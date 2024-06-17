/// @ts-check

import { derive_keys } from "./crypto/client.js"

// ## DOM

/** @template {HTMLElement} T @param {string} id @returns T */
function getStaticElemById(id) {
	return /** @type {T} */(document.getElementById(id) ?? never());
}

/** @template {HTMLElement} T @param {string} id @returns T */
function getStaticElem(id) {
	return /** @type {T} */(document.querySelector(id) ?? never());
}

/** @type {ChannelList} */ const channels = getStaticElem("channel-list");

// ## EVENTS

/** @param {HTMLFormElement} elem */
function loginHandler(elem) {
	mnemonic(elem.elements['mnemonic'].value);
	selectPage("servers");
	return false;
}

/** @param {HTMLFormElement} elem */
async function addServerHandler(elem) {

}

Object.assign(window, { loginHandler });

// ## RENDERING

async function render() {
	if (!mnemonic()) {
		selectPage("login");
		return;
	}

	selectPage("servers");
}

/** @param {string} target_server */
async function selectServer(target_server) {
	const server = await Server.get(target_server);
	if (typeof server === "string") errorToast(server);
	channels.switchServer(server);
}

/** @typedef {"login" | "servers"} Page */

/** @param {Page} name */
function selectPage(name) {
	const prevPage = page();
	if (prevPage) getStaticElemById(prevPage).hidden = true;
	page(name); getStaticElemById(name).hidden = false;
}

// ## REQUESTS

class Server {
	/** @type {Map<string, Server>} */
	static cache = new Map();

	/** @param {string} host @param {ServerConfig} config @param {Profile} profile */
	constructor(host, config, profile) {
		this.host = host;
		this.config = config;
		this.profile = profile
	}

	/** @param {string} url @return {Promise<Server | string>} */
	static async get(url) {
		let server; if (server = this.cache.get(url)) return server;

		const profileFut = getProfile(url), configFut = getServerConfig(url);
		const profile = await profileFut, config = await configFut;
		if (typeof config === "string") return config;
		if (typeof profile === "string") return profile;

		this.cache.set(url, server = new Server(url, config, profile));
		return server;
	}


	/** @param {string} name @param {string} token @returns {Promise<true | string>} */
	async register(name, token) {
		const rpk = (await keys()).signing.publicKey;
		const cpk = await compressPublicKey(rpk);
		const pk = toHex(new Uint8Array(cpk, 0, 33));

		const body = JSON.stringify({ name, token, pk });

		const resp = await fetch(`${this.host}/user`, { method: "CREATE", body });
		return resp.status === 200 || await resp.text().catch(() => "") || statusToString(resp.status);
	}
}

/** @param {number} status @returns {string} */
function statusToString(status) {
	switch (status) {
		case 400: return "Bad Request";
		case 401: return "Unauthorized";
		default: return "Unknown Error";
	}
}

/** @typedef {number} RoleId */

/** @typedef {Object} Profile
 * @property {string} name
 * @property {number[]} roles */

/** @typedef {Object} Role
 * @property {RoleId} id
 * @property {string} name
 * @property {string} color */

/** @typedef {Object} RolePermissions
 * @property {RoleId} id
 * @property {boolean} view
 * @property {boolean} write
 * @property {number?} action_rate_limit
 * @property {boolean} moderate
 * @property {boolean} manage */

/** @typedef {Object} Channel
 * @property {number} id
 * @property {string} group
 * @property {string} name
 * @property {RolePermissions[]} roles
 * @property {RolePermissions} default_permissions
 * @property {RolePermissions | undefined} computed_permissions */

/** @typedef {Object} ServerConfig
 * @property {string} name 
 * @property {string[]} roots 
 * @property {Channel[]} channels
 * @property {Role[]} roles */

/** @template {Object} T @param {Response} resp @returns {Promise<T | string>} */
function resolveJsonResponse(resp) {
	return resp.status === 200 ? resp.json() : resp.text().then(t => t || statusToString(resp.status));
}

/** @param {string} server @returns {Promise<ServerConfig | string>} */
async function getServerConfig(server) {
	return resolveJsonResponse(await fetch(`${server}/config`));
}

/** @param {string} server @returns {Promise<Profile | string>} */
async function getProfile(server) {
	return resolveJsonResponse(await fetch(`${server}/user`,
		{ headers: { "Authorization": await nextBearer() } }));
}

// ## STORAGE

/** @type {Keys | undefined} */ let _keys; async function keys() {
	return _keys ||= await deriveKeys(mnemonic() ?? logout("missing mnemonic"));
}

/** @type {{ [name: string]: string }} params */
const queryParams = /** @type {any} */ (new Proxy(new URLSearchParams(window.location.search), {
	get: (target, name) => target.get(name.toString()),
	set: function(target, name, value) {
		if (!value) target.delete(name.toString());
		else target.set(name.toString(), value);

		const path = window.location.origin + window.location.pathname
			+ '?' + target.toString();
		window.history.pushState(null, '', path);
		return true;
	}
}));

/** @param {string} keys @param {{ [key: string]: string }} store @return {((value?: string) => string | undefined)[]} **/
function declStorage(keys, store) {
	return keys.split(';').map(key =>
		(value = undefined) => value ? store[key] = value : store[key]);
}

const [mnemonic] = declStorage("mnemonic", sessionStorage);
const [backups, servers] = declStorage("backups;servers", localStorage);
const [page, server] = declStorage("page;server", queryParams);

// ## CRYPTO

/** @typedef {Object} Keys
 * @property {CryptoKeyPair} signing 
 * @property {CryptoKey} vault */

/** @param {string} mnemonic  @return {Promise<Keys>} */
async function deriveKeys(mnemonic) {
	const keys = derive_keys(mnemonic);
	/** @type {EcKeyImportParams} */ const ecdsa = { name: "ECDSA", namedCurve: "P-256" };
	/** @type {AesKeyAlgorithm} */ const aes = { name: "AES-GCM", length: 256 };
	return {
		signing: {
			publicKey: await crypto.subtle.importKey("raw", keys.vkey, ecdsa, false, ["verify"]),
			privateKey: await crypto.subtle.importKey("raw", keys.skey, ecdsa, false, ["sign"]),
		},
		vault: await crypto.subtle.importKey("raw", keys.vault, aes, false, ["encrypt", "decrypt"]),
	}
}

/** @returns {Promise<string>} */
async function nextBearer() {
	const kys = await keys();

	const final_res = new Uint8Array(1 + 32 + 64 + 8);
	let cursor = 0;

	// nonce
	for (let i = 0, now = new Date().getTime(); i < 8; i++)
		(final_res[cursor++] = now & 0xff, now /= 255);

	const sig = await crypto.subtle.sign({ name: "ECDSA", hash: { name: "SHA-256" } },
		kys.signing.privateKey, new Uint8Array(final_res.buffer, 0, 8));
	for (const b of new Uint8Array(sig)) final_res[cursor++] = b;

	const pk = await compressPublicKey(kys.signing.publicKey);
	for (const b of new Uint8Array(pk, 0, 33)) final_res[cursor++] = b;

	return toHex(final_res);
}

/** @param {CryptoKey} publicKey */
async function compressPublicKey(publicKey) {
	const exp = await crypto.subtle.exportKey("raw", publicKey);
	const view = new Uint8Array(exp);
	view[0] = (view[view.length - 1] & 1) + 2;
	return exp;
}

/** @param {Uint8Array} arr @returns {string} */
function toHex(arr) {
	console.log(arr);

	const hexChars = "0123456789abcdef";

	const hex = new Uint8Array(arr.length * 2);
	for (let i = 0; i < hex.length; i += 2) {
		hex[i] = hexChars.charCodeAt(arr[i / 2] >> 4);
		hex[i + 1] = hexChars.charCodeAt(arr[i / 2] & 0x0f);
	}
	return new TextDecoder().decode(hex);
}

// ## ERROR HANDLING

/** @returns {never} */ function never() { throw new Error("never"); }


/** @param {string} message @return {never} */
function logout(message) {
	localStorage.clear();
	sessionStorage.clear();
	selectPage("login");
	errorToast(message);
}

/** @param {string} message @return {never} */
function errorToast(message) {
	// TODO: make the toast
	throw new Error(message);
}

/** @param {HTMLElement} from @param {HTMLElement} to @param {string[]} list */
function copyAtts(from, to, list) {
	let attr;
	for (const name of list) if (attr = from.getAttribute(name))
		to.setAttribute(name, attr);
}

// ## TEMPLATING

/** @param {string} str */
const pascalToKebab = (str) => str.replace(/([A-Z])/g, '-$1').toLowerCase().slice(1);

/** @param {CustomElementConstructor} cDef */
function defComp(cDef) {
	const template = document.getElementById(pascalToKebab(cDef.name)) ?? never();
	class Component extends cDef { constructor() { super(template); } }
	customElements.define(template.id, Component)
}

function defStaticTemplates() {
	const templates = document.querySelectorAll('template[static]');
	for (const template of /** @type {NodeListOf<HTMLTemplateElement>} */ (templates)) {
		class StaticTemplate extends HTMLElement {
			constructor() { super(); bindTemplate(this, template); }
		}
		customElements.define(template.id, StaticTemplate);
	}

	const aliases = document.querySelectorAll('template[alias]');
	for (const template of /** @type {NodeListOf<HTMLTemplateElement>} */ (aliases)) {
		const Aliased = customElements.get(template.getAttribute('alias') ?? never()) ?? never();
		class Alias extends Aliased { constructor() { super(template); } }
		customElements.define(template.id, Alias);
	}
}

/** @param {HTMLElement} el @param {HTMLTemplateElement} template */
function bindTemplate(el, template) {
	const shadow = el.attachShadow({ mode: 'open' });
	shadow.appendChild(template.content.cloneNode(true));
}

/** @param {HTMLElement} el @param {string} id @returns {HTMLElement} */
function getWCElem(el, id) {
	const shadow = el.shadowRoot ?? never();
	return shadow.getElementById(id) ?? never();
}

// ## REGISTRATION

/** @param {Channel} channel @param {RoleId[]} roles @returns {RolePermissions} */
function myChannelPermissions(channel, roles) {
	if (channel.computed_permissions) return channel.computed_permissions;
	const base = channel.default_permissions;
	for (const role of channel.roles) {
		if (!roles.includes(role.id)) continue;
		for (const key in role) base[key] = Math.max(base[key], role[key]);
	}
	return channel.computed_permissions = base;
}

class ChannelList extends HTMLElement {
	/** @param {HTMLTemplateElement} template */
	constructor(template) {
		super();

		bindTemplate(this, template);
		this.name = getWCElem(this, "name");
		this.groups = getWCElem(this, "groups");
	}


	/** @param {Server} server */
	switchServer(server) {
		this.name.textContent = server.config.name;

		for (const channel of server.config.channels) {
			if (!myChannelPermissions(channel, server.profile.roles).view) continue;

			let group = /** @type {HTMLElement} */ (this.groups.querySelector(`> channel-group[name=${channel.group}]`));
			if (!group) {
				group = document.createElement("channel-group") ?? never();
				getWCElem(group, "name").textContent = channel.group;
				group.setAttribute("name", channel.group);
			}

			const cs = /** @type {HTMLElement} */ (group.querySelector(`> channel-select[name=${channel.name}]`));
			getWCElem(cs, "name").textContent = channel.name;
			cs.id = "channel-" + channel.id;

			group.appendChild(cs);
		}
	}
}
defComp(ChannelList);

const parseBits = /[^X ]+/g;
customElements.define("bit-mage", class extends HTMLElement {
	/** @param {HTMLElement | undefined} origin */
	constructor(origin = undefined) {
		super();

		this.style.display = "flex";

		const raw_bits = (origin ?? this).getAttribute('bits') ?? never();
		const bits = raw_bits.replace(parseBits, '');
		const dim = parseInt((origin ?? this).getAttribute('dim') ?? "8");

		const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');

		svg.setAttribute('fill', 'currentColor');
		for (const attr of this.attributes) if (attr.name !== 'bits')
			svg.setAttribute(attr.name, attr.value);

		/** @param {number} x @param {number} y @param {number} pinned */
		function renderBit(x, y, pinned) {
			const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
			rect.setAttribute('x', pinned + "");
			rect.setAttribute('y', y + "");
			rect.setAttribute('height', "1");
			rect.setAttribute('width', x - pinned + "");
			svg.appendChild(rect);
		}

		let y = 0
		o: for (let pinned = -1; y < dim; y++) {
			for (let x = 0; x < dim; x++) {
				const pos = y * dim + x;
				const oob = pos >= bits.length;
				if (oob || bits.charAt(pos) === ' '.charAt(0)) {
					if (pinned !== -1) (renderBit(x, y, pinned), pinned = -1);
					if (oob) break o;
				} else if (pinned === -1) pinned = x;
			}
			if (pinned !== -1) (renderBit(8, y, pinned), pinned = -1);
		}
		svg.setAttribute('viewBox', `0 0 ${dim} ${y}`);

		this.attachShadow({ mode: 'open' }).appendChild(svg);
	}
});

defStaticTemplates();

// ## RESPONSIVENESS

const adjustHeight = () => document.body.style.height = `${window.innerHeight}px`;
window.onresize = adjustHeight;
adjustHeight();

if (document.body.dataset.env === "debug") {
	let errored;
	const se = new EventSource("/hot-reload");
	se.onmessage = () => location.reload();
	se.onerror = (e) => !errored && (errored = true, console.log(e));
}

render();
