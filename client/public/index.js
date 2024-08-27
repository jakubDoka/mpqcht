/// @ts-check

/** @param {Object} obj */
function zeroOut(obj) {
	for (const key in obj) {
		switch (typeof obj[key]) {
			case "number": obj[key] = 0; break;
			case "boolean": obj[key] = false; break;
			case "bigint": obj[key] = 0n; break;
			default: console.error("unknown type", typeof obj[key]);
		}
	}
}

/** @template {Record<string, any>} T @param {T} acc @param {T} b @returns {void} */
function maxOf(acc, b) {
	for (const key in acc) acc[key] = acc[key] > b[key] ? acc[key] : b[key];
}

// ## MARKDOWN

/** @template {any} T @param {T[]} list @returns {T | undefined} */
function lastElem(list) {
	if (list.length > 0) return list[list.length - 1];
	return undefined;
}

class LexerBase {
	/** @typedef {Object} TokenMatch
	 * @property {string} kind
	 * @property {number} start
	 * @property {number} end */

	/** @type {RegExp} */ #compiled;
	/** @type {Record<string, RegExp>} */ #patterns;
	/** @type {TokenMatch} */ match = { kind: '', start: 0, end: 0 };
	/** @type {RegExpExecArray} */ rawMatch;

	/** @param {Record<string, RegExp>} patterns */
	constructor(patterns) {
		this.#patterns = patterns;
	}

	/** @returns {RegExp} */
	#getLexer() {
		return this.#compiled ??= new RegExp(Object.keys(this.#patterns)
			.map(name => `(?<${name}>${this.#patterns[name].source})`)
			.join('|'), "dgm");
	}

	/** @param {string} source @return {void} */
	postProcessToken(source) { console.error(source) }

	/** @param {string} source */
	execute(source) {
		const re = this.#getLexer();
		for (let _i = 0; _i < source.length; _i++) {
			const m = re.exec(source); if (m === null) break;
			this.rawMatch = m;
			for (const kind in m['indices'].groups) {
				if (!this.#patterns[kind]) continue;
				const range = m['indices'].groups[kind];
				if (!range) continue;
				this.match.kind = kind;
				this.match.start = range[0];
				this.match.end = range[1];
			}
			this.postProcessToken(source);
			re.lastIndex = this.match.end;
		}
	}

}

/** @type {Record<string, string>} */ let theme;
/** @returns {Record<string, string>} */
function getHighlightTheme() {
	if (theme) return theme;

	const pat = "--code-theme-";
	/** @type { Record<string, string>} */ const records = {};

	if (document.body.computedStyleMap !== undefined) {
		// surprisingly, the fallback does not work on chrome so we keep this
		const properties = document.body.computedStyleMap();
		for (const [key, value] of properties) {
			if (!key.startsWith(pat)) continue;
			records[key.slice(pat.length)] = value.toString();
		}
	} else {
		const properties = getComputedStyle(document.body);
		for (const key of properties) {
			if (!key.startsWith(pat)) continue;
			records[key.slice(pat.length)] = properties.getPropertyValue(key);
		}
	}

	return theme = records;
}

class Highlighter extends LexerBase {
	/** @type {HTMLPreElement} */ #pre;
	/** @type {string | undefined} */ #lastColor;
	/** @type {number} */ #lastIndex;

	/** @param {string} source @return {void} */
	flush(source) {
		const str = source.slice(this.#lastIndex, this.match.start);
		if (this.#lastColor === undefined) {
			this.#pre.append(str);
		} else {
			const span = document.createElement('span');
			span.style.color = this.#lastColor;
			span.textContent = str;
			this.#pre.append(span);
		}
	}

	/** @param {string} source @return {void} */
	postProcessToken(source) {
		const color = getHighlightTheme()[this.match.kind];
		if (this.#lastColor !== color) {
			this.flush(source);
			this.#lastColor = color;
			this.#lastIndex = this.match.start;
		}
	}

	/** @param {HTMLPreElement} pre @param {string} content */
	highlight(pre, content) {
		this.#pre = pre;
		this.#lastColor = undefined;
		this.#lastIndex = 0;

		this.execute(content);
		this.match.start = this.match.end;
		this.flush(content);
	}
}

class Rs extends Highlighter {
	constructor() {
		super({
			ignored: /\s+/,
			keyword: /\b(as|break|const|continue|crate|else|enum|extern|false|fn|for|if|impl|in|let|loop|match|mod|move|mut|pub|ref|return|self|Self|static|struct|super|trait|true|type|unsafe|use|where|while|async|await|dyn)\b/,
			type: /[A-Z][a-zA-Z0-9]*/,
			identifier: /[a-zA-Z][a-zA-Z0-9]*/,
			number: /[0-9]+/,
			string: /"([^"]|\\")*"|'([^']|\\')*'/,
			multilineComment: /\/\*/,
			comment: /\/\/[^\n]*/,
			punctation: /[?:^\+\-*&^%$#@!(){}\[\]\.,/:=><]/,
		});
	}

	/** @param {string} source */
	postProcessToken(source) {
		if (this.match.kind === "multilineComment") {
			/** @type {RegExp} */
			const matcher = window['multilineCommentMatch'] ??= /(\/\*)|(\*\/)/dg;
			matcher.lastIndex = this.match.end;
			let depth = 1; while (depth) {
				const m = matcher.exec(source); if (!m) break;
				if (m[1]) depth++; else depth--;
			}
			this.match.end = matcher.lastIndex;
			this.match.kind = "comment";
		}
		super.postProcessToken(source);
	}
}

/** @typedef {Object} OgData
 * @property {string} [image]
 * @property {string} description
 * @property {string} [site_name]
 * @property {string} title */

/** @param {string} url @returns {Promise<OgData | string>} */
async function extractPreview(url) {
	const resp = await fetch("https://cors-anywhere.herokuapp.com/" + url);
	if (resp.status !== 200) return (await
		resp.text().catch(() => undefined)) || resp.statusText;
	const html = new DOMParser().parseFromString(await resp.text(), 'text/html');

	const result = {};
	for (const elem of html.querySelectorAll("meta[property][content]")) {
		const property = elem.getAttribute("property") ?? never();
		const content = elem.getAttribute("property") ?? never();
		if (!property.startsWith("og:")) continue;
		result[property.slice(3)] = content;
	}

	if (!result.description) return "missing description";
	if (!result.title) return "missing title";

	return /** @type {OgData} */ (result);
}


/** @type {Record<string, Highlighter>} */
const languages = {
	rs: new Rs(),
};

const markdonwEscapePat = /\\([\\\`\*\_\{\}\[\]\<\>\(\)\#\+\-\.\!\|])/g;
class MarkdownRenderer extends LexerBase {
	static #instance = new MarkdownRenderer();

	/** @type {number} */ #t;
	/** @type {HTMLDivElement} */ #container;
	/** @type {HTMLParagraphElement} */ #paragraph;
	/** @type {HTMLElement[]} */ #quoteStack = [];
	/** @type {HTMLElement[]} */ #styleStack = [];
	/** @type {string} */ #content;
	/** @type {boolean} */ #needed;
	/** @type {boolean} */ #hasEscapes;
	/** @type {boolean} */ #tickEscape;


	constructor() {
		// TODO: bullet points
		// TODO: hr
		// TODO: link reference
		// TODO: images
		// TODO: fix the terrible hack
		super({
			escape: markdonwEscapePat,
			heading: /(?<headingNl>^>*)\s*(?<headingLevel>#{1,6}) (?<headingContent>[^\n]+)/,
			codeBlock: /(?<codeBlockNl>^>*)\s*`{3}(?<codeBlockId>[a-z]+)/,
			quote: /^>+/,
			paragraphSep: /\s*(\n\s*){2,}|\s*\n(?<paragraphSepQuote>>+)\s*\n/,
			namedUrl: /\[(?<namedUrlName>[^\]]+)\]\s*\((?<namedUrlContent>[^)]+)\)/,
			url: /<?(?<urlContent>[a-z]{1,10}:\/\/[^)\s]+)>?/,
			italic: /\*{1,3}|_{1,3}/,
			del: /~{1,2}/,
			code: /`{1,2}/,
		});
	}

	#flushText2() {
		const prevT = this.#t; this.#t = this.match.end;
		if (prevT === this.match.start) return;
		let text = this.#content.slice(prevT, this.match.start);
		if (this.#hasEscapes) {
			this.#hasEscapes = false;
			text = text.replace(markdonwEscapePat, "$1");
		}
		this.#activeTextElement().append(text);
	}

	/** @param {string[]} tags */
	#pushStyle2(...tags) {
		this.#flushText2();

		let index = 1000;

		if (tags.length == 0) {
			if (this.#styleStack.length === 0) return;
			index = 0;
		} else {
			for (const tag of tags) {
				const found = this.#styleStack.findIndex((e) => e.tagName === tag);
				if (found === -1) continue;
				index = Math.min(found, index);
			}
			if (index === 1000) {
				for (const tag of tags) {
					this.#styleStack.push(document.createElement(tag));
				}
				return;
			}
		}

		for (let i = this.#styleStack.length - 1; i >= index; i--) {
			const elem = this.#styleStack.pop() ?? never();
			this.#activeTextElement().append(elem);
		}
	}

	#endParagraph2(handleQuote = false) {
		this.#flushText2();
		if (this.#paragraph.childNodes.length !== 0) {
			this.#pushStyle2();
			this.#activeContainer().append(this.#paragraph);
			this.#paragraph = document.createElement('p');
		}
		if (handleQuote) {
			const depth = this.rawMatch.groups?.[this.match.kind + "Nl"]?.length ?? 0;
			this.#setQuoteDepth(depth);
		}
	}

	/** @param {string} content @param {string} name  */
	#pushLink(content, name) {
		try { new URL(content) } catch (_) { return };

		const a = document.createElement('a');
		a.href = content;
		a.target = "_blank";
		a.referrerPolicy = "noopener noreferrer";
		a.textContent = name;

		this.#activeTextElement().append(a);
		this.#t = this.match.end;
	}

	/** @param {number} depth  */
	#setQuoteDepth(depth) {
		if (depth > this.#quoteStack.length) this.#endParagraph2();
		for (let i = this.#quoteStack.length; i < depth; i++)
			this.#quoteStack.push(document.createElement('section'));
		while (this.#quoteStack.length > depth) {
			this.#normalize();
			const elem = this.#quoteStack.pop() ?? never();
			this.#activeContainer().append(elem);
		}
	}

	/** @param {string} source @return {void} */
	postProcessToken(source) {
		this.#needed = true;
		switch (this.match.kind) {
			case "escape": {
				this.#hasEscapes = true;
			} break;
			case "heading": {
				this.#endParagraph2(true);
				const headingLevel = this.rawMatch.groups?.headingLevel.length;
				const heading = document.createElement(`h${headingLevel}`);
				heading.textContent = this.rawMatch.groups?.headingContent ?? "";
				this.#activeContainer().append(heading);
			} break;
			case "codeBlock": {
				this.#endParagraph2(true);
				const language = this.rawMatch.groups?.codeBlockId ?? "";

				let index = source.indexOf('```', this.match.end);
				if (index === -1) index = source.length;
				const content = source.slice(this.match.end, index);

				const pre = document.createElement('pre');
				if (languages[language]) {
					languages[language].highlight(pre, content.trim());
				} else pre.textContent = content;
				this.#activeContainer().append(pre);

				this.match.end = this.#t = Math.min(index + 3, source.length);
			} break;
			case "quote": {
				this.#flushText2();
				this.#setQuoteDepth(this.match.end - this.match.start);
				this.#t = this.match.end;
			} break;
			case "url": {
				const content = this.rawMatch.groups?.urlContent ?? "";
				this.#pushLink(content, content);
			} break;
			case "namedUrl": {
				const content = this.rawMatch.groups?.namedUrlContent ?? "";
				const name = this.rawMatch.groups?.namedUrlName ?? "";
				this.#pushLink(content, name);
			} break;
			case "paragraphSep": {
				this.#endParagraph2();
				const count = this.rawMatch.groups?.paragraphSepQuote?.length ?? 0;
				this.#setQuoteDepth(count);
			} break;
			case "italic": switch (this.match.end - this.match.start) {
				case 1: this.#pushStyle2('EM'); break;
				case 2: this.#pushStyle2('STRONG'); break;
				case 3: this.#pushStyle2('EM', 'STRONG'); break;
			} break;
			case "del": {
				this.#pushStyle2('DEL');
			} break;
			case "code": {
				if (this.match.end - this.match.start === 2) {
					this.#tickEscape = !this.#tickEscape;
				} else if (this.#tickEscape) break;
				this.#pushStyle2('CODE');
			} break;
			default: console.error(this.match);
		}
	}

	#activeTextElement() { return lastElem(this.#styleStack) || this.#paragraph; };
	#activeContainer() { return lastElem(this.#quoteStack) || this.#container; };

	#reset() {
		this.#t = 0;
		this.#container = document.createElement('div');
		this.#paragraph = document.createElement('p');
		this.#quoteStack.length = 0;
		this.#styleStack.length = 0;
		this.#content = '';
		this.#needed = false;
		this.#hasEscapes = false;
	}

	#parse2() {
		this.execute(this.#content);
	}

	#normalize() {
		const container = this.#activeContainer();
		if (container.childElementCount == 1 &&
			container.children[0].tagName === "P") {
			const p = container.children[0];
			container.append(...p.childNodes);
			p.remove();
		}
	}

	/** @param {string} content @returns {HTMLDivElement | string} */
	static render(content) {
		const inst = MarkdownRenderer.#instance;
		inst.#reset();
		inst.#content = content;
		inst.#parse2();
		inst.match.start = inst.match.end = content.length;
		inst.#endParagraph2();
		inst.#setQuoteDepth(0);
		inst.#normalize();
		return inst.#needed ? inst.#container : content;
	}
}

// ## DOM

/** @template {HTMLElement} T @param {string} id @returns T */
function getStaticElemById(id) {
	return /** @type {T} */(document.getElementById(id) ?? never());
}

// ## EVENTS

/** @type {MediaTrackConstraints} */
const audioSettings = {
	autoGainControl: true,
	echoCancellation: true,
	noiseSuppression: true,
	channelCount: 2,
};
const enabledConstarainst = { video: true, audio: audioSettings };

/** @type {MediaStreamConstraints} */
const inputConstraints = { video: false, audio: false };

/** @type {MediaStream | undefined} */ let prevMedia;
/** @param {MediaStreamConstraints} constraints */
async function getUserMedia(constraints) {
	if (prevMedia) for (const track of prevMedia.getTracks()) track.stop();
	prevMedia = undefined;
	if (!constraints.video && !constraints.audio) return undefined;
	return prevMedia ||= await navigator.mediaDevices.getUserMedia(constraints);
}

async function hangUp() {
	VoiceView.inst.close();
	(await Server.current()).closeVoiceSession();
}

/** @param {HTMLButtonElement} elem */
function toggleCam(elem) { toggleInput("video", elem); }

/** @param {HTMLButtonElement} elem */
function toggleMic(elem) { toggleInput("audio", elem); }

/** @param {"audio" | "video"} kind @param {HTMLButtonElement} elem */
async function toggleInput(kind, elem) {
	const enable = !inputConstraints[kind];
	inputConstraints[kind] =
		enable ? enabledConstarainst[kind] : false;
	await (await Server.current()).voice?.updateAllSessionTracks(inputConstraints);
	for (const child of /** @type {*} */(elem.children)) {
		child.toggleAttribute("hidden");
	}
	elem.classList.toggle("disabled");

	switch (kind) {
		case "audio": {
			VoiceView.inst.addPane("me").muted.classList.toggle("hidden", enable);
		} break;
		case "video": {
			VoiceView.inst.addPane("me").elem.classList.toggle("has-video", enable);
		} break;
	}
}

/** @param {HTMLFormElement} elem */
async function loginHandler(elem) {
	username(elem.elements['username'].value);
	password(elem.elements['password'].value);
	selectPage("servers");
}

/** @param {HTMLFormElement} elem */
async function inviteUserHandler(elem) {
	const pk = elem.elements['pubkey'].value;
	const roles = /** @type {HTMLElement[]} */
		([...getStaticElemById("selected-roles").children])
			.map(e => e.dataset.id);
	const server = await Server.current();
	let resp = await fetch(`${server.host}/user/invite`, {
		method: "POST",
		headers: {
			"Authorization": "Bearer " + await nextProof(server.hostname),
			"Content-Type": "application/json",
		},
		body: JSON.stringify({ pk, roles }),
	}).catch(e => e + "");

	if (typeof resp === "string") return errorToast(resp);

	switch (resp.status) {
		case 200: toast("user invited", "success"); break;
		case 409: errorToast("user already invited");
		case 403: errorToast("you are not allowed to invite users");
		default: unexpectedStatusToast(resp);
	}
}

const addServerHint = getStaticElemById("add-server-hint"),
	addServerPubkey = getStaticElemById("add-server-pubkey");
/** @param {HTMLFormElement} elem */
async function addServerHandler(elem) {
	const url = new URL(elem.elements['url'].value).origin;
	const host = new URL(url).hostname;
	const name = elem.elements['username'].value;

	let resp = await fetch(`${url}/user`,
		{ headers: { "Authorization": "Bearer " + await nextProof(host) } })
		.catch(e => e + "");

	if (typeof resp === "string") return errorToast(resp);

	switch (resp.status) {
		case 200: {
			toast("server recollected", "success");
			return Server.select(url.toString());
		}
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
		case 200: {
			toast("invite used", "success");
			return Server.select(url);
		}
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
	const id = elem.id.slice(ChannelList.prefix.length);
	if (id === channel()) return;
	ChannelView.inst.selectChannel(parseInt(id));
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
async function togglePoppup(id, hide) {
	if (!id) return;

	if (!hide) switch (id) {
		case "profile-settings": {
			getStaticElemById(id + "-pubkey").textContent = toHex(await pubkey());
		} break;
		case "invite-user": await initInvieUserPoppup(); break;
	}

	getStaticElemById(id).hidden = hide;
	poppup(hide ? "" : id);
}

async function initInvieUserPoppup() {
	const server = await Server.current();
	const unselectedRoles = getStaticElemById("unselected-roles");
	const selectedRoles = getStaticElemById("selected-roles");

	for (const ctnr of [selectedRoles, unselectedRoles]) ctnr.innerHTML = "";

	for (const role of server.config.roles) {
		const roleElem = document.createElement("div");
		roleElem.onmousedown = () => {
			const parent = roleElem.parentElement ?? never();
			const moveTo = getStaticElemById(
				parent.getAttribute("toggle-to")
				?? never());
			if (moveTo.childElementCount == 0) {
				moveTo.innerHTML = "";
			}
			moveTo.append(roleElem);
			if (parent.childElementCount == 0) {
				parent.innerHTML = parent.getAttribute("placeholder") ?? never();
			}
		};
		roleElem.dataset.id = role.id + "";
		roleElem.textContent = role.name;
		roleElem.style.color = invertColor(role.color);
		roleElem.style.background = role.color;
		unselectedRoles.appendChild(roleElem);
	}

	for (const ctnr of [selectedRoles, unselectedRoles])
		if (ctnr.childElementCount == 0)
			ctnr.innerHTML = ctnr.getAttribute("placeholder") ?? never();
}

/** @param {string} color */
function invertColor(color) {
	return "#" + (0xFFFFFF - parseInt(color.slice(1), 16))
		.toString(16).padStart(6, "0");
}

/** @param {string} id */ const hidePoppup = (id) => togglePoppup(id, true);
/** @param {string} id */ const showPoppup = (id) => togglePoppup(id, false);

// ## RENDERING

async function render() {
	getCrypto();
	renderComponents(document.body);

	if (username() === undefined || password() === undefined) {
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

/** @template {any} T @param {(T | undefined | null)[]} items @returns {T[]} */
function filter(items) {
	let valid = 0;
	for (let i = 0; i < items.length; i++)
		if (items[i]) items[valid++] = items[i];
	items.length = valid;
	return /** @type {T[]} */ (items);
}

class VoiceChat {
	/** @template {string} K @template {Object} B @typedef {Object} SignalT
	 * @property {K} type
	 * @property {string} from
	 * @property {string} to
	 * @property {B} body */

	/** @typedef {Object} ReadySignal
	 * @property {"ready"} type */

	/** @typedef {Object} PresentSignal
	 * @property {"present"} type
	 * @property {ListenerId} to
	 * @property {Auth[]} participants */

	/** @typedef {SignalT<"offer", RTCSessionDescriptionInit> |
	 * SignalT<"candidate", RTCIceCandidateInit> | ReadySignal} Signal */

	/** @typedef {Signal | SignalT<"left" | "joined", User> |
	 * PresentSignal} SystemSignal

	/** @typedef {Object} RTCSession
	 * @property {RTCPeerConnection} pc 
	 * @property {boolean} polite 
	 * @property {boolean} makingOffer
	 * @property {boolean} ignoreOffer
	 * @property {RTCRtpSender[]} tracks
	 * @property {() => void} [onconnect] */

	/** @typedef {Object} TurnCreds
	 * @property {number} expiry
	 * @property {string} username
	 * @property {string} password */

	/** @typedef {string} ListenerId */

	/** @type {Server} */ #server;
	#backof = 1;
	/** @type {WebSocket} */ #ws;
	/** @type {ListenerId} */ #id;
	/** @type {TurnCreds | undefined} */ #turnCreds;
	/** @type {Map<ListenerId, Participant>} */ participants = new Map();

	/** @typedef {User & RTCSession} Participant */

	/** @param {WebSocket} ws
	 * @param {Server} config
	 * @param {ListenerId} id 
	 * @param {ChannelId} channel */
	constructor(ws, config, id, channel) {
		this.#ws = ws;
		this.#server = config;
		this.#id = id;
		this.channel = channel;

		this.bindWs();
	}

	/** @param {Server} server @param {ChannelId} chan
	 * @returns {Promise<string | VoiceChat>} */
	static async connect(server, chan) {
		const proof = await server.nextProof();
		const ws = new WebSocket(`${server.wsHost}/voice/${chan}/ws/${proof}`);
		const ths = new VoiceChat(ws, server, toHex(await pubkey()), chan);
		await new Promise((resolve, reject) => {
			ws.onopen = () => resolve(ws);
			ws.onerror = (e) => reject(e);
		})
		return ths;
	}

	bindWs() {
		this.#ws.onmessage = (e) => { this.handleWsMessage(JSON.parse(e.data)); };
		this.#ws.onerror = (e) => this.handleWsError(e);
		this.#ws.onclose = () => this.handleWsClose();
	}

	/** @returns {Promise<string | TurnCreds>} */
	async getCreds() {
		if (this.#turnCreds && new Date().getTime() < this.#turnCreds.expiry) return this.#turnCreds;

		const creds = await this.#server.getTurnCreds();
		if (typeof creds === "string") return creds;
		return this.#turnCreds = creds;
	}

	/** @param {UserPk} id */
	removeRTC(id) {
		const session = this.participants.get(id); if (!session) return;
		this.participants.delete(id);
		session.pc.onnegotiationneeded = null;
		session.pc.onicecandidate = null;
		session.pc.onconnectionstatechange = null;
		session.pc.onsignalingstatechange = null
		session.pc.ontrack = null;
		VoiceView.inst.removePane(id);
	}

	/** @param {ListenerId} other
	 * @param {User} user
	 * @param {boolean} polite
	 * @param {() => void} [onconnect] */
	async createRTC(other, user, polite, onconnect) {
		const creds = await this.getCreds();
		if (typeof creds === "string") errorToast(creds);
		const { username, password } = creds;

		/** @type {Participant} */
		const session = {
			polite,
			onconnect,

			pc: new RTCPeerConnection({
				iceTransportPolicy: "relay",
				iceServers: [{
					username,
					credential: password,
					urls: this.#server.turnHost,
				}],
			}),
			tracks: [],
			makingOffer: false,
			ignoreOffer: false,

			...user
		};

		this.participants.set(other, session);
		const from = this.#id;

		session.pc.onnegotiationneeded = async () => {
			try {
				session.makingOffer = true;
				await session.pc.setLocalDescription();
				this.send({ type: "offer", from, to: other, body: session.pc.localDescription ?? never() });
			} catch (err) {
				console.error(err);
			} finally {
				session.makingOffer = false;
			}
		};

		session.pc.onicecandidate = async ({ candidate }) => {
			if (!candidate) return;
			this.send({ type: "candidate", from, to: other, body: candidate });
		};

		session.pc.onconnectionstatechange = () => {
			if (session.pc.connectionState === "closed") this.removeRTC(other);
		};

		session.pc.onsignalingstatechange = () => {
			if (session.pc.signalingState === "closed") this.removeRTC(other);
		}

		session.pc.ontrack = async ({ track, streams: [stream] }) => {
			if (!stream) return console.warn("stream was null");
			VoiceView.inst.addPane(other).addTrack(track, stream);
		};

		if (!session.polite) {
			session.pc.onnegotiationneeded(new Event(""));
		}

		this.updateSessionTracks(session, inputConstraints);
		VoiceView.inst.addPane(other).name.textContent = session.name;
	}

	/** @param {Participant} session @param {MediaStreamConstraints} constraints */
	async updateSessionTracks(session, constraints) {
		const stream = await getUserMedia(constraints);
		for (const sender of session.tracks) session.pc.removeTrack(sender);
		session.tracks = stream?.getTracks()
			?.map(track => session.pc.addTrack(track, stream)) || [];
		this.mountMe(VoiceView.inst.addPane("me"), stream);
	}

	/** @param {MediaStreamConstraints} constraints */
	async updateAllSessionTracks(constraints) {
		const me = VoiceView.inst.addPane("me");
		const stream = await getUserMedia(constraints);
		for (const session of this.participants.values()) {
			for (const sender of session.tracks) {
				me.cachedRemoveCb({ track: sender.track ?? never() });
				session.pc.removeTrack(sender);
			}
			session.tracks = stream?.getTracks()
				?.map(track => session.pc.addTrack(track, stream)) || [];
		}
		this.mountMe(me, stream);
	}

	/** @param {VoicePane} me @param {MediaStream | undefined} stream */
	async mountMe(me, stream) {
		me.name.textContent = "you";
		me.audio.muted = true;
		me.video.style.transform = 'scale(-1, 1)';
		if (stream) for (const track of stream.getTracks())
			await me.addTrack(track, stream);
	}

	/** @param {SystemSignal} msg */
	async handleWsMessage(msg) {
		const from = this.#id;
		/** @param {UserPk} to */
		const warnMissingP = (to) => console.warn("missing participant", to);
		switch (msg.type) {
			case "left": this.removeRTC(msg.from); break;
			case "present": {
				for (const { id, user } of msg.participants)
					this.createRTC(id, user, true);
				this.send({ type: "ready" });
			} break;
			case "joined": this.createRTC(msg.from, msg.body, false); break;
			case "offer": {
				const p = this.participants.get(msg.from);
				if (!p) return warnMissingP(msg.from);
				const offerCollision = msg.body.type === "offer" &&
					(p.makingOffer || p.pc.signalingState !== "stable");

				p.ignoreOffer = offerCollision && !p.polite;
				if (p.ignoreOffer) return console.warn("ignoring offer");

				await p.pc.setRemoteDescription(msg.body);
				if (msg.body.type === "offer") {
					await p.pc.setLocalDescription();
					this.send({ type: "offer", from, to: msg.from, body: p.pc.localDescription ?? never() });
				}

				p.polite = true;
				p.onconnect?.();
				p.onconnect = undefined;
			} break;
			case "candidate": {
				const p = this.participants.get(msg.from);
				if (!p) return warnMissingP(msg.from);
				try {
					await p.pc.addIceCandidate(msg.body);
				} catch (e) {
					if (!p.ignoreOffer) throw e;
				}
			} break;
			case "ready": console.warn("there is a bug in the server"); break;
			default: console.warn("unknown signal type", msg);
		}
	}

	/** @param {Event} e */
	handleWsError(e) { console.error(e); }

	handleWsClose() {
		setTimeout(async () => {
			if (!this.#ws.onclose) return;
			const proof = await this.#server.nextProof();
			this.#ws = new WebSocket(`${this.#server.wsHost}/voice/${this.channel}/ws/${proof}`);
			this.#ws.onopen = () => this.#backof = 1;
			this.bindWs();
		}, 500 * (this.#backof *= 2));
	}

	/** @param {Signal} signal */
	send(signal) { this.#ws.send(JSON.stringify(signal)); }

	close() {
		for (const part of this.participants.values()) part.pc.close();
		this.#ws.onclose = null;
		this.#ws.close();
	}
}

class Server {
	/** @type {Map<string, Server>} */ static cache = new Map();

	/** @type {VoiceChat | undefined} */ voice;
	/** @type {EventSource} */ #sse;
	/** @type {number} */ #sseBackof = 1;
	/** @type {AbortController | undefined} */ messageAbort;

	/** @param {string} host
	 * @param {ServerConfig} config
	 * @param {User} profile
	 * @param {string} pubkey */
	constructor(host, config, profile, pubkey) {
		this.host = new URL(host).origin;
		this.wsHost = Server.#hostToWs(host);
		this.turnHost = config.turn || Server.#hostToTurn(host);
		this.hostname = new URL(host).hostname;
		this.config = config;
		this.profile = profile;
		this.pubkey = pubkey;

		this.#initSse();
	}

	/** @param {string} host  @returns {string} */
	static #hostToTurn(host) {
		const url = new URL(host);
		return `turn:turn.${url.hostname}`;
	}

	/** @param {string} host */
	static #hostToWs(host) {
		const url = new URL(host);
		url.protocol = url.protocol.replace(/^http/, "ws");
		return url.toString().slice(0, -1);
	}

	/** @typedef {DefSseEvent<Message, "Message">} SseEvent */
	/** @template {Object} D @template {string} K @typedef {D & { type: K }} DefSseEvent */

	/** @param {string} url @return {Promise<Server | string>} */
	static async get(url) {
		let server; if (server = this.cache.get(url)) return server;

		const profileFut = getProfile(url), configFut = getServerConfig(url);
		const profile = await profileFut, config = await configFut;
		if (typeof config === "string") return config;
		if (typeof profile === "string") return profile;

		this.cache.set(url, server = new Server(url, config, profile, toHex(await pubkey())));
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

	/** @param {string | undefined} url @param {number | string | undefined} chan
	 * @return {Promise<void>} */
	static async select(url, chan = undefined) {
		if (!url) return;

		server(url);
		const srver = await Server.get(url);
		if (typeof srver === "string") errorToast(srver);
		ChannelList.inst.selectServer(srver);

		chan ??= channel();
		if (typeof chan === "string") chan = parseInt(chan);
		if (typeof chan === "number") await ChannelView.inst.selectChannel(chan);
	}

	/** @returns {boolean} */
	isRoot() {
		if (this.config.is_root !== undefined) return this.config.is_root;
		return this.config.is_root = this.config.roots.includes(this.pubkey);
	}

	/** @param {UserPk} id @return {boolean} */
	isUserRoot(id) {
		return this.config.roots.includes(id);
	}

	static maxRolePerms = { can_manage_users: true };
	/** @returns {RolePermissions} */
	permissions() {
		if (this.isRoot()) return Server.maxRolePerms;
		if (this.config.computed_perms) return this.config.computed_perms;
		const base = Object.assign({}, Server.maxRolePerms); zeroOut(base);
		for (const role of this.config.roles) {
			if (!this.profile.roles.includes(role.id)) continue;
			maxOf(base, role.perms);
		}
		return this.config.computed_perms = base;
	}

	static maxChannelPerms =
		{ id: 0, view: true, write: true, moderate: true, manage: true };
	/** @param {Channel} channel @returns {ChannelPermissions} */
	channelPermissions(channel) {
		if (this.isRoot()) return Server.maxChannelPerms;
		if (channel.computed_permissions) return channel.computed_permissions;
		const base = channel.default_permissions;
		for (const role of channel.roles) {
			if (!this.profile.roles.includes(role.id)) continue;
			maxOf(base, role);
		}
		return channel.computed_permissions = base;
	}

	async nextProof() {
		return nextProof(this.hostname);
	}

	/** @param {MessageEvent<string>} e */
	sseOnMessage = (e) => this.#handleSse(e);
	sseOnOpen = () => this.#sseBackof = 1;
	sseOnError = () => {
		this.#sse.close();
		setTimeout(() => this.#initSse(), 1000 * (this.#sseBackof - 1));
		this.#sseBackof = Math.min(this.#sseBackof * 2, 64);
	};

	async #initSse() {
		this.#sse = new EventSource(`${this.host}/sse/${await this.nextProof()}`);
		this.#sse.onmessage = this.sseOnMessage;
		this.#sse.onopen = this.sseOnOpen;
		this.#sse.onerror = this.sseOnError;
	}

	/** @param {MessageEvent<string>} e */
	#handleSse(e) {
		/** @type {SseEvent} */ const data = JSON.parse(e.data);
		switch (data.type) {
			case "Message": {
				const chan = channel();
				if (chan === undefined || data.channel !== parseInt(chan)) return;
				ChannelView.inst.addMessages(true, data);
			} break;
			default: console.error("Unknown sse event: ", data);
		}
	}

	/** @param {ChannelId} chan @returns {Promise<string | void>} */
	async openVoiceSession(chan) {
		if (this.voice?.channel === chan) return;
		this.closeVoiceSession();

		const sig = await VoiceChat.connect(this, chan);
		if (typeof sig === "string") return sig;
		this.voice = sig;
	}

	closeVoiceSession() {
		this.voice?.close();
		this.voice = undefined;
	}

	/** @param {ChannelId} id @return {Channel | undefined} */
	channelById(id) {
		return this.config.channels.find(c => c.id === id);
	}

	/** @param {ChannelId} channel @param {TimeQuery} query @param {number | string} time
	 * @returns {Promise<Message[] | string>} */
	async getMessages(channel, query, time) {
		if (this.messageAbort) this.messageAbort.abort();
		this.messageAbort = new AbortController();
		const resp = await fetch(
			`${this.host}/messages?channel=${channel}&kind=${query}&time=${time}`,
			{
				headers: { "Authorization": "Bearer " + await this.nextProof() },
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
					"Authorization": "Bearer " + await this.nextProof()
				},
				body: content,
			}
		);
		return resp.status === 200 || await resp.text().catch(() => null) || resp.statusText;
	}

	/** @param {ChannelId} channel @returns {Promise<string | Auth[]>} */
	async getVoicePeers(channel) {
		const resp = await fetch(`${this.host}/voice/${channel}/peers`, {
			headers: { "Authorization": "Bearer " + await this.nextProof() },
		});
		return resolveJsonResponse(resp);
	}

	/** @returns {Promise<string | TurnCreds>} */
	async getTurnCreds() {
		const resp = await fetch(`${this.host}/voice/turn-auth`, {
			headers: { "Authorization": "Bearer " + await this.nextProof() },
		});
		return resolveJsonResponse(resp);
	}

}

/** @typedef {string} UserPk */

/** @typedef {Object} Message
 * @property {number} id
 * @property {number} channel
 * @property {number} timestamp
 * @property {UserPk} author_pk
 * @property {string} author
 * @property {string} content */

/** @typedef {Object} User
 * @property {string} name
 * @property {number[]} roles */

/** @typedef {Object} Auth
 * @property {UserPk} id
 * @property {User} user */

/** @typedef {number} RoleId */
/** @typedef {number} ChannelId */

/** @typedef {Object} RolePermissions
 * @property {boolean} can_manage_users */

/** @typedef {Object} Role
 * @property {RoleId} id
 * @property {string} name
 * @property {string} color
 * @property {RolePermissions} perms */

/** @typedef {Object} ChannelPermissions
 * @property {RoleId} id
 * @property {boolean} view
 * @property {boolean} write
 * @property {number} [action_rate_limit]
 * @property {boolean} moderate
 * @property {boolean} manage */

/** @typedef {Object} VoiceChannel
 * @property {number} max_participants */

/** @typedef {Object} Channel
 * @property {number} id
 * @property {string} group
 * @property {string} name
 * @property {ChannelPermissions[]} roles
 * @property {ChannelPermissions} default_permissions
 * @property {ChannelPermissions} [computed_permissions]
 * @property {VoiceChannel} [voice] */

/** @typedef {Object} Theme
 * @property {string} primary
 * @property {string} secondary
 * @property {string} tertiary
 * @property {string} neutral
 * @property {string} success
 * @property {string} warning
 * @property {string} error */

/** @typedef {Object} ServerConfig
 * @property {string} hostname 
 * @property {string} [turn]
 * @property {string[]} roots 
 * @property {Theme} dark_theme
 * @property {Theme} light_theme
 * @property {Channel[]} channels
 * @property {RolePermissions} [computed_perms]
 * @property {boolean} [is_root] 
 * @property {Role[]} roles */

/** @param {Theme} theme */
function applyTheme(theme) {
	for (const [key, value] of Object.entries(theme)) {
		document.documentElement?.style.setProperty(`--${key}`, value);
	}
}

/** @template {Object} T @param {Response} resp @returns {Promise<T | string>} */
function resolveJsonResponse(resp) {
	return resp.status === 200 ? resp.json() : resp.text().then(t => t || resp.statusText);
}

/** @param {string} server @returns {Promise<ServerConfig | string>} */
async function getServerConfig(server) {
	return resolveJsonResponse(await fetch(`${server}/config`));
}

/** @param {string} server @returns {Promise<User | string>} */
async function getProfile(server) {
	return resolveJsonResponse(await fetch(`${server}/user`,
		{ headers: { "Authorization": "Bearer " + await nextProof(new URL(server).hostname) } }));
}

// ## STORAGE

/** @type {Keys | undefined} */ let _keys; async function keys() {
	return _keys ||= await deriveKeys(
		username() ?? logout("missing username"),
		password() ?? logout("missing password")
	);
}

/** @template {any} T @param {(chats: Set<string>) => T} cb @returns T */
function withServers(cb) {
	const serves = servers();
	const res = cb(serves);
	localStorage['servers'] = [...serves].join(';');
	NavBar.inst.render(serves);
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

const [username, password] = declStorage("username;password", sessionStorage);
const [backups] = declStorage("backups", localStorage);
const [page, server, channel, poppup, invoice] =
	declStorage("page;server;channel;poppup;invoice", queryParams);

// ## CRYPTO

/** @typedef {Object} Keys
 * @property {CryptoKeyPair} signing 
 * @property {CryptoKey} vault */


/** @type {WebAssembly.Instance} */
let wasmInstance;
/** @type {Promise<WebAssembly.Instance>} */
let wasmInstancePromise;
/** @returns {Promise<WebAssembly.Instance>} */
async function getCrypto() {
	wasmInstancePromise ||= fetch("crypto.wasm")
		.then(r => r.arrayBuffer())
		.then(b => WebAssembly.instantiate(b, {
			env: {
				"__linear_memory": new WebAssembly.Memory({ initial: 1 }),
				"__stack_pointer": new WebAssembly.Global({ value: "i32", mutable: true }, 0),
			}
		})).then(i => i.instance)
	return wasmInstance ||= await wasmInstancePromise;
}

const ecdsaAlg = { name: "ECDSA", namedCurve: "P-256" };
const aesAlg = { name: "AES-GCM", length: 256 };
/** @param {string} username @param {string} password @return {Promise<Keys>} */
async function deriveKeys(username, password) {
	const cryptoLib = await getCrypto();

	const {
		username: username_loc, username_len, password: passsword_loc, password_len,
		derive_keys, clear_secrets, memory, ecdsa_vkey, ecdsa_skey, vault: vault_sec,
	} = cryptoLib.exports;

	if (!(username_len instanceof WebAssembly.Global)) never();
	if (!(password_len instanceof WebAssembly.Global)) never();

	if (typeof derive_keys !== "function") never();
	if (typeof clear_secrets !== "function") never();

	if (!(memory instanceof WebAssembly.Memory)) never();
	const view = new DataView(memory.buffer);

	/** @param {WebAssembly.ExportValue} g @param {number} len @return {Uint8Array} */
	function globalView(g, len, offset = 0) {
		if (!(memory instanceof WebAssembly.Memory)) never();
		if (!(g instanceof WebAssembly.Global)) never();
		return new Uint8Array(memory.buffer, g.value + offset, len);
	}

	/** @param {Uint8Array} dst @param {string} src */
	function cpy(dst, src) {
		for (let i = 0; i < src.length; i++) dst[i] = src.charCodeAt(i);
	}

	try {
		cpy(globalView(username_loc, 32), username);
		view.setUint32(username_len.value, username.length, true);
		cpy(globalView(passsword_loc, 256), password);
		view.setUint32(password_len.value, password.length, true);

		if (derive_keys() !== 0) never();

		const vault_key = globalView(vault_sec, 32);

		// I honestly hate you so much W3C
		const jwk = {
			crv: "P-256",
			d: encodeB64UrlSafe(globalView(ecdsa_skey, 32)),
			key_ops: ["sign"],
			kty: "EC",
			x: encodeB64UrlSafe(globalView(ecdsa_vkey, 32, 1)),
			y: encodeB64UrlSafe(globalView(ecdsa_vkey, 32, 33)),
		};

		const vkey = globalView(ecdsa_vkey, 65);
		const sc = crypto.subtle;
		const publicKey = await sc.importKey("raw", vkey, ecdsaAlg, true, ["verify"]);
		const privateKey = await sc.importKey("jwk", jwk, ecdsaAlg, false, ["sign"]);
		const vault = await sc.importKey("raw", vault_key, aesAlg, false,
			["encrypt", "decrypt"]);

		return { signing: { publicKey, privateKey }, vault }
	} catch (e) {
		console.error(e);
		throw new Error("error deriving keys");
	} finally {
		clear_secrets();
	}

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
	toast(message, "error");
	throw new Error(message);
}

/** @param {string} message @param {"error" | "success"} type */
function toast(message, type) {
	const toast = document.createElement("div");
	toast.style.background = `var(--${type})`;
	if (type === "success") toast.style.color = 'black';
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
}

/** @param {Response} resp @return {never} */
function unexpectedStatusToast(resp) {
	errorToast(`internal: unexpected status code: ${resp.status} ${resp.statusText}`);
}

// ## TEMPLATING

/** @param {string} str */
const pascalToKebab = (str) => str.replace(/([A-Z])/g, '-$1').toLowerCase().slice(1);

/** @typedef {{ new (root: HTMLElement, attrs: NamedNodeMap): any }} Component */
/** @type {Map<string, Component>} */
const components = new Map();

/** @param {Component[]} cDefs */
function defComp(...cDefs) {
	for (const cDef of cDefs) components.set(pascalToKebab(cDef.name), cDef);
}

/** @param {HTMLElement} inside */
function renderComponents(inside) {
	const componens = inside.querySelectorAll('wbr');
	for (const comp of componens) {
		const name = comp.getAttribute('comp') ?? never();
		comp.replaceWith(createComponent(name, comp.attributes));
	}
}

const scratchSpace = getStaticElemById('scratch-space');

/** @template {ComponentBase} T  @param {Element} elem @returns {T} */
function cpnnt(elem) {
	return elem['component'] ?? never();
}

const dummyMap = document.createElement('div').attributes;

/** @param {Attr} attr */
function cloneAttr(attr) {
	return /** @type {Attr} */(attr.cloneNode());
}

/** @param {string} name @param {NamedNodeMap} attrs @returns {HTMLElement} */
function createComponent(name, attrs = dummyMap) {
	const excludedAttrs = "id comp alias";

	/** @type {HTMLTemplateElement} */
	let template = getStaticElemById(name);

	let alias, prevTemplate; if (alias = template.getAttribute('alias')) {
		prevTemplate = template;
		template = getStaticElemById(alias);
		name = alias;
	}

	scratchSpace.appendChild(template.content.cloneNode(true));
	const finalElem = /** @type {HTMLElement} */(scratchSpace.lastElementChild);
	finalElem.setAttribute('comp', name);
	renderComponents(finalElem);
	if (!template.hasAttribute('static')) {
		if (prevTemplate)
			for (const attr of prevTemplate.attributes) {
				if (!attrs.getNamedItem(attr.name)
					&& !excludedAttrs.includes(attr.name))
					attrs.setNamedItem(cloneAttr(attr));
			}

		const component = components.get(name) ?? never();
		finalElem['component'] = new component(finalElem, attrs);

		if (prevTemplate) if (attrs === dummyMap)
			for (const attr of attrs) attrs.removeNamedItem(attr.name);

	}


	return finalElem;
}

class ComponentBase {
	/** @param {HTMLElement} elem @param {NamedNodeMap} attrs */
	constructor(elem, attrs = dummyMap) {
		for (const attr of attrs) if (attr.name !== 'comp')
			elem.attributes.setNamedItem(cloneAttr(attr));
		this.elem = elem;
	}

	/** @param {string} id @returns {HTMLElement} */
	getElem(id) {
		return getElem(this.elem, id);
	}
}

// ## REGISTRATION

class NavBar extends ComponentBase {
	/** @type {NavBar} */ static inst;
	/** @param {HTMLElement} elem */
	constructor(elem) {
		super(elem);
		this.serverList = this.getElem("server-list");
		this.render(servers());

		NavBar.inst = this;
	}

	/** @param {Set<string>} servers */
	render(servers) {
		this.serverList.innerHTML = "";

		for (const server of servers) {
			const button = createComponent("server-select") ?? never();
			button.onclick = () => Server.select(server);
			this.serverList.appendChild(button);
		}
	}
}

class ChannelSelect extends ComponentBase {
	/** @param {HTMLElement} elem */
	constructor(elem) {
		super(elem);
	}
}

class ChannelList extends ComponentBase {
	/** @type {ChannelList} */ static inst;
	static prefix = "channel-";

	/** @param {HTMLElement} elem */
	constructor(elem) {
		super(elem);
		this.name = this.getElem("name");
		this.groups = this.getElem("groups");
		this.inviteBtn = this.getElem("invite-btn");
		/** @type {Map<ChannelId, ChannelSelect>} */
		this.channelMap = new Map();

		ChannelList.inst = this;
	}

	/** @param {ChannelId} chanId */
	select(chanId) {
		const oldChan = channel();
		if (oldChan && chanId != parseInt(oldChan)) this.getChannel(parseInt(oldChan))?.elem.classList.remove("selected");
		this.getChannel(chanId)?.elem.classList.add("selected");
		channel(chanId + "");
	}

	/** @param {ChannelId} chanId @returns {ChannelSelect | undefined} */
	getChannel(chanId) {
		return this.channelMap.get(chanId);
	}

	/** @param {Server} server */
	selectServer(server) {
		this.name.textContent = server.config.hostname;
		this.groups.innerHTML = "";
		this.inviteBtn.hidden = !server.permissions().can_manage_users;
		this.channelMap.clear();
		applyTheme(server.config.dark_theme);

		for (const channel of server.config.channels) {
			if (!server.channelPermissions(channel).view) continue;

			let group = /** @type {HTMLElement} */ (this.groups.querySelector(`:scope > [name=${channel.group}]`));
			if (!group) {
				group = createComponent("channel-group") ?? never();
				getElem(group, "name").textContent = channel.group;
				getElem(group, "name").textContent = channel.group;
				group.setAttribute("name", channel.group);
				this.groups.appendChild(group);
			}

			const cs = createComponent("channel-select") ?? never();
			cs.textContent = channel.name;
			cs.id = ChannelList.prefix + channel.id;

			getElem(group, "channels").appendChild(cs);

			this.channelMap.set(channel.id, cpnnt(cs));
		}
	}
}

/** @param {HTMLElement | SVGElement} el @param {string} id @returns {HTMLElement} */
function getElem(el, id) {
	return el.querySelector(`#${id}`) ?? never();
}

/** @param {number} x @param {number} y @param {number} p */
const renderRect = (x, y, p) => `<rect x=${p} y=${y} height=1 width=${x - p}></rect>`;
const parseBits = /[^X \n]+/g, nline = '\n'.charCodeAt(0), capX = 'X'.charCodeAt(0);
class BitMage extends ComponentBase {
	/** @param {HTMLElement} elem @param {NamedNodeMap} attrs */
	constructor(elem, attrs) {
		super(elem);

		const raw_bits = attrs.getNamedItem('bits')?.value ?? never();
		const bits = raw_bits.replace(parseBits, '').trimEnd() + '\n';

		let y = 0, x = 0, c = -1, mx = 0, ny = Math.min(), p = -1, body = [];
		while (++c < bits.length) {
			const cc = bits.charCodeAt(c);
			if (cc !== capX && p !== -1) body.push(renderRect(x, y - ny, p)), p = -1;
			else if (cc === capX && p === -1) p = x, ny = Math.min(ny, y);
			if (cc === nline) y++, mx = Math.max(mx, x), x = 0; else x++;
		}
		if (p !== -1) body.push(renderRect(x, y - ny, p));

		elem.setAttribute('viewBox', `0 0 ${mx} ${y - ny}`);
		elem.innerHTML = body.join('');
		for (const attr of attrs) if (attr.name !== 'bits')
			elem.attributes.setNamedItem(cloneAttr(attr));
	}
}

/** @param {HTMLElement} elem @param {boolean} bottom */
const pickEdge = (bottom, elem) => /** @type {HTMLElement | undefined} */
	(bottom ? elem.lastElementChild : elem.firstElementChild);
/** @param {HTMLElement} elem @param {boolean} bottom @param {HTMLElement} toAppend */
const appendEdge = (bottom, elem, toAppend) =>
	bottom ? elem.append(toAppend) : elem.prepend(toAppend);

class ChannelView extends ComponentBase {
	/** @type {ChannelView} */ static inst;
	static maxMessages = 120;

	/** @type {Server} */
	server;

	/** @param {HTMLElement} elem */
	constructor(elem) {
		super(elem);
		this.name = this.getElem("name");
		this.messageList = this.getElem("message-list");
		this.joinVoice = this.getElem("join-voice");
		this.joinVoice.onmousedown = () => this.open();

		this.scrollContainer = this.messageList.parentElement ?? never();
		this.scrollContainer.onscroll = () => this.onContainerScroll();

		this.scrollPos = 0;
		this.loadedMessageCount = 0;
		this.maxBatch = 0;
		this.atBottom = true;
		this.atTop = false;
		this.chanId = NaN;

		ChannelView.inst = this;
	}

	open() {
		this.elem.hidden = true;
		VoiceView.inst.open(this.chanId);
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
				await this.loadMessages(chan, false);
			}

			while (this.scrollBottom < tolerance && !this.atBottom) {
				await this.loadMessages(chan, true);
			}
		}
	}

	/** @param {ChannelId} chan @param {boolean} bottom */
	async loadMessages(chan, bottom) {
		const query = bottom ? "After" : "Before";
		const messages = await this.server.getMessages(chan, query, this.getEdgeTimestamp(bottom));
		if (typeof messages === "string") errorToast(messages);
		this.maxBatch = Math.max(this.maxBatch, messages.length);
		const fin = this.maxBatch > messages.length || messages.length === 0;
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
		return pickEdge(bottom, getElem(lastGroup, "messages"));
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
				group = this.createMessageGroup(message.author, message.author_pk);
				appendEdge(bottom, this.messageList, group);
			}
			appendEdge(bottom, getElem(group, "messages"), this.createMessage(message));
		}

		this.restoreScroll(bottom);
	}

	/** @param {number} count @param {boolean} bottom */
	trimMessages(count, bottom) {
		this.atBottom &&= !bottom;
		this.atTop &&= bottom;
		let latest; while (count && (latest = pickEdge(bottom, this.messageList))) {
			const list = getElem(latest, "messages");
			const take = Math.min(list.childElementCount, count);
			if (list.childElementCount < count) latest.remove();
			else for (let i = 0; i < take; i++) pickEdge(bottom, list)?.remove();
			count -= take;
		}
	}

	/** @param {string} author @param {UserPk} authorPk */
	createMessageGroup(author, authorPk) {
		const group = createComponent("channel-message-group") ?? never();
		group.dataset.author = author;
		const authorElem = getElem(group, "author");
		if (author === "") {
			authorElem.textContent = "(this user has empty username)";
			authorElem.style.color = 'var(--error)';
		} else authorElem.textContent = author;
		if (this.server.isUserRoot(authorPk)) {
			getElem(group, "root-icon").removeAttribute('hidden');
		}
		return group;
	}

	/** @param {Message} message */
	createMessage(message) {
		const messageElem = createComponent("channel-message") ?? never();
		messageElem.dataset.timestamp = message.timestamp + "";
		const rendered = MarkdownRenderer.render(message.content);
		const content = getElem(messageElem, "content");
		content.innerHTML = '';
		if (typeof rendered === 'string') {
			content.textContent = rendered;
		} else {
			content.append(...rendered.childNodes);
			content.classList.add("markdown");
		}
		getElem(messageElem, "timestamp").textContent =
			new Date(message.timestamp * 1000).toLocaleString();
		return messageElem;
	}

	/** @param {ChannelId} chanId */
	async selectChannel(chanId) {
		this.chanId = chanId;
		this.scrollPos = 0;
		this.loadedMessageCount = 0;
		this.maxBatch = 0;
		this.atBottom = true;
		this.atTop = false;
		this.messageList.innerHTML = "";
		this.name.textContent = "loading...";
		this.server = await Server.current();

		const chan = this.server.channelById(chanId);
		if (!chan) errorToast("channel was probably removed");

		this.joinVoice.hidden = !chan.voice;
		if (!chan.voice) VoiceView.inst.close();
		if (invoice()) this.open();
		ChannelList.inst.select(chanId);
		this.name.textContent = chan.name;

		do {
			await this.loadMessages(chan.id, false);
		} while (this.scrollContainer.scrollHeight === this.scrollContainer.clientHeight
			&& !this.atTop);
	}
}

class VoiceView extends ComponentBase {
	/** @type {VoiceView} */
	static inst;

	/** @param {HTMLElement} elem @param {NamedNodeMap} attrs */
	constructor(elem, attrs) {
		super(elem, attrs);
		this.panes = this.getElem("voice-panes");
		this.backToChat = this.getElem("back-to-chat");
		this.backToChat.onmousedown = () => this.close();

		VoiceView.inst = this;
	}

	close() {
		this.elem.hidden = true;
		ChannelView.inst.elem.hidden = false;
		this.panes.innerHTML = "";
		invoice("");
	}

	/** @param {ChannelId} chanId */
	async open(chanId) {
		this.elem.hidden = false;
		const server = await Server.current();
		this.addPane("me");
		server.openVoiceSession(chanId);
		invoice("true");
	}

	/** @param {UserPk} peer @returns {VoicePane} */
	addPane(peer) {
		let existing = this.panes.querySelector(`#pane-${peer}`);
		if (!existing) {
			existing = createComponent("voice-pane");
			existing.id = "pane-" + peer;
			this.panes.appendChild(existing);
		}
		return cpnnt(existing);
	}

	/** @param {UserPk} to */
	removePane(to) {
		this.panes.querySelector(`#pane-${to}`)?.remove();
	}
}

class VoicePane extends ComponentBase {
	/** @type {AudioContext | undefined} */
	static audioCtx;

	/** @type {AnalyserNode} */
	audioAnalizer;
	/** @type {MediaStreamAudioSourceNode | undefined} */
	audioMediaSource;
	/** @type {Float32Array | undefined} */
	audioDataBuffer;

	closedInTheMeanTime = false;

	// I do not want to create this on each animation frame
	cachedCb = () => this.updateAudioVis();

	/** @param {{ track: MediaStreamTrack }} e */
	cachedRemoveCb = ({ track }) => {
		this.closedInTheMeanTime = true;
		switch (track.kind) {
			case "audio": {
				this.audio.srcObject = null;
				this.audioMediaSource?.disconnect();
				this.audioMediaSource = this.audioDataBuffer = undefined;
				this.muted.classList.remove("hidden");
			} break;
			case "video": {
				this.video.srcObject = null;
				this.elem.classList.remove("has-video");
			} break;
			default: console.error("unknown track kind", track.kind);
		};
	};

	/** @param {HTMLElement} elem */
	constructor(elem) {
		super(elem);
		this.video = /** @type {HTMLVideoElement} */ (this.getElem("video"));
		this.audio = /** @type {HTMLAudioElement} */ (this.getElem("audio"));
		this.muted = /** @type {HTMLElement} */ (this.getElem("muted"));
		this.name = /** @type {HTMLElement} */ (this.getElem("name"));
		this.pfp = this.getElem("pfp");
	}

	/** @param {MediaStreamTrack} track @param {MediaStream} stream */
	async addTrack(track, stream) {
		this.closedInTheMeanTime = false;
		stream.onremovetrack = this.cachedRemoveCb;

		await ensureActivation();
		if (this.closedInTheMeanTime) return;

		switch (track.kind) {
			case "audio": {
				this.audio.srcObject = stream;
				VoicePane.audioCtx ||= new AudioContext();
				this.audioMediaSource = VoicePane.audioCtx.createMediaStreamSource(stream);
				this.audioAnalizer = VoicePane.audioCtx.createAnalyser();
				this.audioMediaSource.connect(this.audioAnalizer);
				this.audioDataBuffer = new Float32Array(this.audioAnalizer.fftSize);
				this.muted.classList.add("hidden");
				this.updateAudioVis();
			} break;
			case "video": {
				this.video.muted = true;
				this.video.srcObject = stream;
				this.elem.classList.add("has-video");
			} break;
			default: console.error("unknown track kind", track.kind);
		}
	}

	updateAudioVis() {
		const TRASHOLD = 0.005;

		if (!this.audioDataBuffer) return;

		this.audioAnalizer.getFloatTimeDomainData(this.audioDataBuffer);

		let fold = 0.0; for (const amp of this.audioDataBuffer) fold += amp * amp;
		const volume = Math.sqrt(fold / this.audioDataBuffer.length);
		this.elem.classList.toggle("talking", volume > TRASHOLD);

		window.requestAnimationFrame(this.cachedCb);
	}
}

defComp(
	BitMage,
	NavBar,
	ChannelList, ChannelSelect,
	ChannelView,
	VoiceView, VoicePane,
);

let requestActFuture;
async function ensureActivation() {
	const requestActDiv = getStaticElemById("request-activation");
	const requestActBtn = getStaticElemById("request-activation-btn");
	if (navigator.userActivation.hasBeenActive) return;
	requestActDiv.hidden = false;
	requestActFuture ||= new Promise(resolve => requestActBtn.onmousedown = () => {
		requestActDiv.hidden = true;
		resolve(undefined);
	});
	await requestActFuture;
	requestActFuture = undefined;
}

// ## RESPONSIVENESS

const adjustHeight = () => document.body.style.height = `${window.innerHeight}px`;
window.onresize = adjustHeight;
adjustHeight();

if (window.location.hostname === "localhost") {
	const se = new EventSource("/hot-reload");
	se.onmessage = () => location.reload();
}

// ## KEYBIND

/** @param {HTMLElement} el */
function isHidden(el) {
	if (el.hidden) return true;
	if (!el.parentElement) return false;
	return isHidden(el.parentElement);
}

let keyBuf = "";
/** @param {KeyboardEvent} e */
function catchUnhandledKeys(e) {
	if (document.activeElement?.matches(':is(textarea, input[type=text], input[type=password])')) return;

	switch (e.key) {
		case "Backspace": keyBuf = keyBuf.slice(0, -1); break;
		case "Enter": keyBuf = ""; break;
		default: keyBuf += e.key;
	}

	/** @type {NodeListOf<HTMLElement>} */
	const cachedKeyElems = document.querySelectorAll('[kb]');
	let incomplete = false;
	for (const elem of cachedKeyElems) {
		if (isHidden(elem)) continue;
		const pat = elem.getAttribute('kb') ?? never();
		if (pat === keyBuf) {
			elem.onmousedown?.(new MouseEvent(""));
			if (incomplete) never();
			break;
		}
		incomplete ||= pat.startsWith(keyBuf);
	}

	if (!incomplete) keyBuf = "";
}

document.onkeydown = catchUnhandledKeys;

render();
