/// @ts-check

import init_crypto, { derive_keys } from "./crypto/client.js"

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
/** @type {NavBar} */ const nav = getStaticElem("nav-bar");
/** @type {ChannelView} */ const channelView = getStaticElem("channel-view");

// ## EVENTS

/** @param {HTMLInputElement} mnemonic */
function validateMnemonic(mnemonic) {
	try { derive_keys(mnemonic.value); }
	catch (e) { return mnemonic.setCustomValidity(e); }
	mnemonic.setCustomValidity('');
}

/** @param {HTMLFormElement} elem */
async function loginHandler(elem) {
	mnemonic(elem.elements['mnemonic'].value);
	selectPage("servers");
}

const addServerHint = getStaticElemById("add-server-hint"),
	addServerPubkey = getStaticElemById("add-server-pubkey");
/** @param {HTMLFormElement} elem */
async function addServerHandler(elem) {
	const url = elem.elements['url'].value;
	const host = new URL(url).hostname;
	const name = elem.elements['username'].value;

	let resp = await fetch(`${url}/user`,
		{ headers: { "Authorization": "Bearer " + await nextProof(host) } })
		.catch(e => e + "");

	if (typeof resp === "string") return errorToast(resp);

	switch (resp.status) {
		case 200: return Server.select(url.toString());
		case 401: errorToast("server fully rejected us, this might be because of incorrect server name");
		case 403: { } break;
		default: unexpectedStatusToast(resp);
	}

	resp = await fetch(`${url}/user`, {
		method: "POST",
		headers: { "Content-Type": "application/json" },
		body: JSON.stringify({
			name,
			proof: await nextProof(host, true),
		}),
	});

	switch (resp.status) {
		case 200: return Server.select(url.toString());
		case 403: {
			addServerPubkey.textContent = toHex(await pubkey())
			addServerHint.hidden = false;
			errorToast("your public key is not allowed");
		}
		case 409: errorToast("username already taken");
		default: unexpectedStatusToast(resp);
	}
}

/** @param {HTMLButtonElement} elem */
async function selectChannelHandler(elem) {
	const id = getWCRoot(elem).id.slice(ChannelList.prefix.length);
	if (id === channel()) return;
	channelView.selectChannel(parseInt(id));
}

/** @param {HTMLTextAreaElement} elem */
function messageInputHandler(elem) {
	const computed = getComputedStyle(elem);
	const padd = parseInt(computed.paddingTop) + parseInt(computed.paddingBottom);
	elem.style.height = "0px";
	elem.style.height = (elem.scrollHeight - padd) + "px";
}

/** @param {HTMLTextAreaElement} elem @param {KeyboardEvent} event */
async function messageKeyDownHandler(elem, event) {
	if (event.key === "Enter" && !event.shiftKey && !event.ctrlKey && elem.value.trim() !== "") {
		event.preventDefault();
		const server = await Server.current();
		const chan = parseInt(channel() ?? never());
		const res = await server.sendMessage(chan, elem.value);
		if (typeof res === "string") errorToast(res);
		elem.value = ""; messageInputHandler(elem);
	}
}

/** @param {string | undefined} id @param {boolean} hide */
function togglePoppup(id, hide) {
	if (!id) return;
	getStaticElemById(id).hidden = hide;
	poppup(hide ? "" : id);
}

/** @param {string} id */ const hidePoppup = (id) => togglePoppup(id, true);
/** @param {string} id */ const showPoppup = (id) => togglePoppup(id, false);

// ## RENDERING

async function render() {
	await init_crypto();

	if (!mnemonic()) {
		selectPage("login");
		return;
	}

	selectPage();
	Server.select(server(), channel());
	togglePoppup(poppup(), false);
}

/** @typedef {"login" | "servers"} Page */

/** @param {Page} [name] */
function selectPage(name = undefined) {
	const prevPage = page();
	if (prevPage) getStaticElemById(prevPage).hidden = true;
	const selected = name ?? prevPage ?? "login";
	page(selected); getStaticElemById(selected).hidden = false;
}

// ## REQUESTS

/** @typedef {"Before" | "After"} TimeQuery */

class Server {
	/** @type {Map<string, Server>} */
	static cache = new Map();

	/** @param {string} host @param {ServerConfig} config @param {Profile} profile */
	constructor(host, config, profile) {
		this.host = host;
		this.hostname = new URL(host).hostname;
		this.config = config;
		this.profile = profile
		/** @type {EventSource | undefined} */
		this.sse = undefined;
		/** @type {AbortController | undefined} */
		this.messageAbort = undefined;

		this.initSse();
	}

	/** @typedef {DefSseEvent<Message, "Message">} SseEvent */
	/** @template {Object} D @template {string} K @typedef {D & { type: K }} DefSseEvent */

	async initSse() {
		this.sse = new EventSource(`${this.host}/sse/${await nextProof(this.hostname)}`);
		this.sse.onmessage = (e) => (this.handleSse(e));
	}

	/** @param {MessageEvent<string>} e */
	handleSse(e) {
		/** @type {SseEvent} */ const data = JSON.parse(e.data);
		switch (data.type) {
			case "Message": {
				const chan = channel();
				if (chan === undefined || data.channel !== parseInt(chan)) return;
				channelView.addMessages(true, data);
			} break;
			default: console.error("Unknown sse event: ", data);
		}
	}

	/** @param {ChannelId} id @return {Channel | undefined} */
	channelById(id) {
		return this.config.channels.find(c => c.id === id);
	}

	/** @param {string} url @return {Promise<Server | string>} */
	static async get(url) {
		let server; if (server = this.cache.get(url)) return server;

		const profileFut = getProfile(url), configFut = getServerConfig(url);
		const profile = await profileFut, config = await configFut;
		if (typeof config === "string") return config;
		if (typeof profile === "string") return profile;

		this.cache.set(url, server = new Server(url, config, profile));
		withServers(servers => servers.add(url));
		return server;
	}

	/** @return {Promise<Server>} */
	static async current() {
		const url = server() ?? never();
		const srver = await Server.get(url);
		if (typeof srver === "string") never();
		return srver;
	}

	/** @param {string | undefined} url @param {number | string | undefined} channel
	 * @return {Promise<void>} */
	static async select(url, channel = undefined) {
		if (!url) return;

		server(url);
		const srver = await Server.get(url);
		if (typeof srver === "string") errorToast(srver);
		channels.selectServer(srver);

		if (typeof channel === "string") channel = parseInt(channel);
		if (channel !== undefined) channelView.selectChannel(channel);
	}

	/** @param {ChannelId} channel @param {TimeQuery} query @param {number | string} time
	 * @returns {Promise<Message[] | string>} */
	async getMessages(channel, query, time) {
		if (this.messageAbort) this.messageAbort.abort();
		this.messageAbort = new AbortController();
		const resp = await fetch(
			`${this.host}/messages?channel=${channel}&kind=${query}&time=${time}`,
			{
				headers: { "Authorization": "Bearer " + await nextProof(this.hostname) },
				signal: this.messageAbort.signal
			}
		);
		this.messageAbort = undefined;
		return resolveJsonResponse(resp);
	}

	/** @param {ChannelId} channel @param {string} content @returns {Promise<true | string>} */
	async sendMessage(channel, content) {
		const resp = await fetch(
			`${this.host}/messages?channel=${channel}`,
			{
				method: "POST",
				headers: {
					"Content-Type": "plain/text",
					"Authorization": "Bearer " + await nextProof(this.hostname)
				},
				body: content,
			}
		);
		return resp.status === 200 || await resp.text().catch(() => null) || resp.statusText;
	}
}


/** @typedef {Object} Message
 * @property {number} id
 * @property {number} channel
 * @property {number} timestamp
 * @property {string} author
 * @property {string} content */

/** @typedef {Object} Profile
 * @property {string} name
 * @property {number[]} roles */

/** @typedef {number} RoleId */
/** @typedef {number} ChannelId */

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
 * @property {string} hostname 
 * @property {string[]} roots 
 * @property {Channel[]} channels
 * @property {Role[]} roles */

/** @template {Object} T @param {Response} resp @returns {Promise<T | string>} */
function resolveJsonResponse(resp) {
	return resp.status === 200 ? resp.json() : resp.text().then(t => t || resp.statusText);
}

/** @param {string} server @returns {Promise<ServerConfig | string>} */
async function getServerConfig(server) {
	return resolveJsonResponse(await fetch(`${server}/config`));
}

/** @param {string} server @returns {Promise<Profile | string>} */
async function getProfile(server) {
	return resolveJsonResponse(await fetch(`${server}/user`,
		{ headers: { "Authorization": "Bearer " + await nextProof(new URL(server).hostname) } }));
}

// ## STORAGE

/** @type {Keys | undefined} */ let _keys; async function keys() {
	return _keys ||= await deriveKeys(mnemonic() ?? logout("missing mnemonic"));
}

/** @template {any} T @param {(chats: Set<string>) => T} cb @returns T */
function withServers(cb) {
	const serves = servers();
	const res = cb(serves);
	localStorage['servers'] = [...serves].join(';');
	nav.render(serves);
	return res;
}

/** @returns {Set<string>} */
function servers() {
	const rawServers = localStorage['servers'] ?? '';
	const servers = new Set(rawServers.split(';'));
	servers.delete('');
	return servers;
}

/** @returns {Promise<Uint8Array>} */
async function pubkey() {
	const kys = await keys();
	return await compressPublicKey(kys.signing.publicKey);
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
		(value = undefined) => value !== undefined ? store[key] = value : store[key]);
}

const [mnemonic] = declStorage("mnemonic", sessionStorage);
const [backups] = declStorage("backups", localStorage);
const [page, server, channel, poppup] = declStorage("page;server;channel;poppup", queryParams);

// ## CRYPTO

/** @typedef {Object} Keys
 * @property {CryptoKeyPair} signing 
 * @property {CryptoKey} vault */

const ecdsaAlg = { name: "ECDSA", namedCurve: "P-256" };
const aesAlg = { name: "AES-GCM", length: 256 };
/** @param {string} mnemonic  @return {Promise<Keys>} */
async function deriveKeys(mnemonic) {
	const keys = derive_keys(mnemonic);
	// I honestly hate you so much W3C
	const jwk = {
		crv: "P-256",
		d: encodeB64UrlSafe(keys.skey),
		key_ops: ["sign"],
		kty: "EC",
		x: encodeB64UrlSafe(new Uint8Array(keys.vkey.buffer, 1, 32)),
		y: encodeB64UrlSafe(new Uint8Array(keys.vkey.buffer, 33, 32)),
	};

	const publicKey = await crypto.subtle.importKey("raw", keys.vkey, ecdsaAlg, true, ["verify"]);
	const privateKey = await crypto.subtle.importKey("jwk", jwk, ecdsaAlg, false, ["sign"]);
	const vault = await crypto.subtle.importKey("raw", keys.vault, aesAlg, false, ["encrypt", "decrypt"]);
	return { signing: { publicKey, privateKey }, vault }
}

const url64TrimPad = /=+$/g, url64Plus = /\+/g, url64Slash = /\//g;
/** @param {Uint8Array} bytes */
function encodeB64UrlSafe(bytes) {
	return btoa(String.fromCharCode(...bytes))
		.replace(url64TrimPad, '').replace(url64Plus, '-').replace(url64Slash, '_');
}

const ecdsaSignAlg = { name: "ECDSA", hash: { name: "SHA-256" } };
/** @param {string} server @returns {Promise<string>} */
async function nextProof(server, zero_nonce = false) {
	const kys = await keys();

	const final_res = new Uint8Array(8 + 33 + 64);
	let cursor = 0;

	// nonce
	for (let i = 0, now = zero_nonce ? 0 : new Date().getTime(); i < 8; i++)
		(final_res[cursor++] = now & 0xff, now /= 256);
	for (let i = 0; i < server.length; i++)
		final_res[cursor + i] = server.charCodeAt(i);

	const toSign = new Uint8Array(final_res.buffer, 0, cursor + server.length);
	const sig = await crypto.subtle.sign(ecdsaSignAlg, kys.signing.privateKey, toSign);

	const pk = await compressPublicKey(kys.signing.publicKey);
	for (const b of pk) final_res[cursor++] = b;

	for (const b of new Uint8Array(sig)) final_res[cursor++] = b;

	return toHex(final_res);
}

/** @param {CryptoKey} publicKey */
async function compressPublicKey(publicKey) {
	const exp = await crypto.subtle.exportKey("raw", publicKey);
	const view = new Uint8Array(exp);
	view[0] = (view[view.length - 1] & 1) + 2;
	return new Uint8Array(exp, 0, 33);
}

const hexChars = "0123456789abcdef";
/** @param {Uint8Array} arr @returns {string} */
function toHex(arr) {
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
	const toast = document.createElement("div");
	toast.classList.add("toast");
	toast.textContent = message;

	const toasts = document.body.querySelectorAll(".toast");
	for (const ot of /** @type {NodeListOf<HTMLElement>} */ (toasts)) {
		ot.style.transform += 'translateY(calc(100% + var(--small-gap)))';
	}

	setTimeout(() => toast.style.transform += 'scale(1)', 10);
	setTimeout(() => toast.style.transform += 'scale(0)', 5400);
	setTimeout(() => toast.remove(), 6000);
	document.body.prepend(toast);

	throw new Error(message);
}

/** @param {Response} resp @return {never} */
function unexpectedStatusToast(resp) {
	errorToast(`internal: unexpected status code: ${resp.status} ${resp.statusText}`);
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

/** @param {CustomElementConstructor[]} cDefs */
function defComp(...cDefs) {
	for (const cDef of cDefs) customElements.define(pascalToKebab(cDef.name), cDef);
}

/** @param {CustomElementConstructor[]} cDefs */
function defTemplateComp(...cDefs) {
	for (const cDef of cDefs) {
		const template = document.getElementById(pascalToKebab(cDef.name)) ?? never();
		class Component extends cDef { constructor() { super(template); } }
		customElements.define(template.id, Component)
	}
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

/** @param {HTMLElement} el @returns {HTMLElement} **/
function getWCRoot(el) {
	const root = /** @type {ShadowRoot} */(el.getRootNode());
	return /** @type {HTMLElement} */(root.host);
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

class NavBar extends HTMLElement {
	/** @param {HTMLTemplateElement} template */
	constructor(template) {
		super();
		bindTemplate(this, template);
		this.serverList = getWCElem(this, "server-list");

		this.render(servers());
	}

	/** @param {Set<string>} servers */
	render(servers) {
		this.serverList.innerHTML = "";

		for (const server of servers) {
			this.serverList.innerHTML += `<button onclick="Server.select('${server}')">
				<mpqcht-logo width="32" height="32"></mpqcht-logo>
			</button>`;
		}
	}
}

class ChannelSelect extends HTMLElement {
	/** @param {HTMLTemplateElement} template */
	constructor(template) {
		super();

		bindTemplate(this, template);
		this.name = getWCElem(this, "name");
	}
}

class ChannelList extends HTMLElement {
	static prefix = "channel-";

	/** @param {HTMLTemplateElement} template */
	constructor(template) {
		super();

		bindTemplate(this, template);
		this.name = getWCElem(this, "name");
		this.groups = getWCElem(this, "groups");
		/** @type {Map<ChannelId, ChannelSelect>} */
		this.channelMap = new Map();
	}

	/** @param {ChannelId} chanId */
	select(chanId) {
		const oldChan = channel();
		if (oldChan) this.getChannel(parseInt(oldChan)).name.classList.remove("selected");
		this.getChannel(chanId).name.classList.add("selected");
		channel(chanId + "");
	}

	/** @param {ChannelId} chanId @returns {ChannelSelect} */
	getChannel(chanId) {
		return this.channelMap.get(chanId) ?? never();
	}

	/** @param {Server} server */
	selectServer(server) {
		this.name.textContent = server.config.hostname;
		this.groups.innerHTML = "";
		this.channelMap.clear();

		for (const channel of server.config.channels) {
			if (!myChannelPermissions(channel, server.profile.roles).view) continue;

			let group = /** @type {HTMLElement} */ (this.groups.querySelector(`:scope > channel-group[name=${channel.group}]`));
			if (!group) {
				group = document.createElement("channel-group") ?? never();
				getWCElem(group, "name").textContent = channel.group;
				group.setAttribute("name", channel.group);
				this.groups.appendChild(group);
			}

			const cs = document.createElement("channel-select") ?? never();
			getWCElem(cs, "name").textContent = channel.name;
			cs.id = ChannelList.prefix + channel.id;

			getWCElem(group, "channels").appendChild(cs);

			this.channelMap.set(channel.id, /** @type {ChannelSelect} */(cs));
		}
	}
}

/** @param {number} x @param {number} y @param {number} p */
const renderRect = (x, y, p) => `<rect x=${p} y=${y} height=1 width=${x - p}></rect>`;
const parseBits = /[^X \n]+/g, nline = '\n'.charCodeAt(0), capX = 'X'.charCodeAt(0);
class BitMage extends HTMLElement {
	/** @param {HTMLElement | undefined} origin */
	constructor(origin = undefined) {
		super();

		this.style.display = "flex";

		const raw_bits = (origin ?? this).getAttribute('bits') ?? never();
		const bits = raw_bits.replace(parseBits, '').trimEnd();

		let y = 0, x = 0, c = -1, mx = 0, ny = Math.min(), p = -1, body = [];
		while (++c < bits.length) {
			const cc = bits.charCodeAt(c);
			if (cc !== capX && p !== -1) body.push(renderRect(x, y - ny, p)), p = -1;
			else if (cc === capX && p === -1) p = x, ny = Math.min(ny, y);
			if (cc === nline) y++, mx = Math.max(mx, x), x = 0; else x++;
		}
		if (p !== -1) body.push(renderRect(x, y - ny, p));

		const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
		svg.setAttribute('viewBox', `0 0 ${mx} ${y - ny + 1}`);
		svg.setAttribute('fill', 'currentColor');
		svg.innerHTML = body.join('');
		for (const attr of this.attributes) if (attr.name !== 'bits')
			svg.setAttribute(attr.name, attr.value);
		this.attachShadow({ mode: 'open' }).append(svg);
	}
}

/** @param {HTMLElement} elem @param {boolean} bottom */
const pickEdge = (bottom, elem) => /** @type {HTMLElement | undefined} */
	(bottom ? elem.lastElementChild : elem.firstElementChild);
/** @param {HTMLElement} elem @param {boolean} bottom @param {HTMLElement} toAppend */
const appendEdge = (bottom, elem, toAppend) =>
	bottom ? elem.append(toAppend) : elem.prepend(toAppend);

class ChannelView extends HTMLElement {
	static maxMessages = 120;

	/** @param {HTMLTemplateElement} template */
	constructor(template) {
		super();

		bindTemplate(this, template);

		this.name = getWCElem(this, "name");
		this.messageList = getWCElem(this, "message-list");

		this.scrollContainer = this.messageList.parentElement ?? never();
		this.scrollContainer.onscroll = () => this.onContainerScroll();

		this.scrollPos = 0;
		this.loadedMessageCount = 0;
		this.maxBatch = 0;
		this.atBottom = true;
		this.atTop = false;
	}

	get scrollBottom() {
		const sc = this.scrollContainer;
		return sc.scrollHeight - sc.scrollTop - sc.clientHeight;
	}

	cacheScroll() { this.scrollPos = this.scrollBottom; }

	/** @param {boolean} bottom */
	restoreScroll(bottom) {
		const sc = this.scrollContainer;
		if (bottom) this.scrollPos < 20 && this.atBottom && (sc.scrollTop = sc.scrollHeight)
		else sc.scrollTop = sc.scrollHeight - this.scrollPos - sc.clientHeight;
	}

	async onContainerScroll() {
		const server = await Server.current();
		const chan = parseInt(channel() ?? never());
		const tolerance = 300;

		if (!server.messageAbort) {
			while (this.scrollContainer.scrollTop < tolerance && !this.atTop) {
				await this.loadMessages(server, chan, false);
			}

			while (this.scrollBottom < tolerance && !this.atBottom) {
				await this.loadMessages(server, chan, true);
			}
		}
	}

	/** @param {Server} server @param {ChannelId} chan @param {boolean} bottom */
	async loadMessages(server, chan, bottom) {
		const query = bottom ? "After" : "Before";
		const messages = await server.getMessages(chan, query, this.getEdgeTimestamp(bottom));
		if (typeof messages === "string") errorToast(messages);
		this.maxBatch = Math.max(this.maxBatch, messages.length);
		const fin = this.maxBatch > messages.length;
		this.addMessages(bottom, ...messages);
		this.atTop ||= fin && !bottom;
		this.atBottom ||= fin && bottom;
	}

	/** @param {boolean} bottom @returns {number | string} */
	getEdgeTimestamp(bottom) {
		return this.getEdgeMessage(bottom)?.dataset?.timestamp ?? new Date().getTime();
	}

	/** @param {boolean} bottom */
	getEdgeMessage(bottom) {
		const lastGroup = pickEdge(bottom, this.messageList);
		if (!lastGroup) return undefined;
		return pickEdge(bottom, getWCElem(lastGroup, "messages"));
	}

	/** @param {Message[]} messages @param {boolean} bottom */
	addMessages(bottom, ...messages) {
		const excess = (this.loadedMessageCount + messages.length) - ChannelView.maxMessages;
		if (excess > 0) this.trimMessages(excess, !bottom);
		this.loadedMessageCount = Math.min(ChannelView.maxMessages, this.loadedMessageCount + messages.length);

		this.cacheScroll();

		let group = pickEdge(bottom, this.messageList);
		for (const message of messages) {
			if (!group || group.dataset.author !== message.author) {
				group = this.createMessageGroup(message.author);
				appendEdge(bottom, this.messageList, group);
			}
			appendEdge(bottom, getWCElem(group, "messages"), this.createMessage(message));
		}

		this.restoreScroll(bottom);
	}

	/** @param {number} count @param {boolean} bottom */
	trimMessages(count, bottom) {
		this.atBottom &&= !bottom;
		this.atTop &&= bottom;
		let latest; while (count && (latest = pickEdge(bottom, this.messageList))) {
			const list = getWCElem(latest, "messages");
			const take = Math.min(list.childElementCount, count);
			if (list.childElementCount < count) latest.remove();
			else for (let i = 0; i < take; i++) pickEdge(bottom, list)?.remove();
			count -= take;
		}
	}

	/** @param {string} author */
	createMessageGroup(author) {
		let group = document.createElement("channel-message-group") ?? never();
		group.dataset.author = author;
		getWCElem(group, "author").textContent = author;
		return group;
	}

	/** @param {Message} message */
	createMessage(message) {
		const messageElem = document.createElement("channel-message") ?? never();
		messageElem.dataset.timestamp = message.timestamp + "";
		getWCElem(messageElem, "content").textContent = message.content;
		return messageElem;
	}

	/** @param {ChannelId} chanId */
	async selectChannel(chanId) {
		this.scrollPos = 0;
		this.loadedMessageCount = 0;
		this.maxBatch = 0;
		this.atBottom = true;
		this.atTop = false;
		this.messageList.innerHTML = "";
		this.name.textContent = "loading...";

		const server = await Server.current();
		const chan = server.channelById(chanId);
		if (!chan) errorToast("channel was probably removed");

		channels.select(chanId);

		this.name.textContent = chan.name;

		do {
			await this.loadMessages(server, chan.id, false);
		} while (this.scrollContainer.scrollHeight === this.scrollContainer.clientHeight
			&& !this.atTop);
	}
}

defTemplateComp(NavBar, ChannelList, ChannelView, ChannelSelect);
defComp(BitMage);
defStaticTemplates();

// ## RESPONSIVENESS

const adjustHeight = () => document.body.style.height = `${window.innerHeight}px`;
window.onresize = adjustHeight;
adjustHeight();

if (document.body.dataset.env === "debug") {
	const se = new EventSource("/hot-reload");
	se.onmessage = () => location.reload();
}

render();

Object.assign(window, {
	hidePoppup, showPoppup,
	loginHandler, validateMnemonic,
	addServerHandler, Server,
	selectChannelHandler,
	messageInputHandler, messageKeyDownHandler,
});
