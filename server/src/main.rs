#![feature(impl_trait_in_assoc_type)]
#![feature(if_let_guard)]
#![feature(array_chunks)]
#![feature(iter_array_chunks)]
#![feature(iter_next_chunk)]

#[macro_export]
macro_rules! ensure {
    ($e:expr, $status:ident) => {
        if !$e {
            return Err($crate::Error($crate::SC::$status));
        }
    };
}

use axum::extract::Json;
use axum::extract::Query;
use axum::extract::State as StPat;
use axum::http::StatusCode as SC;
use std::ffi::OsStr;
use std::net::Ipv4Addr;
use std::path::Path;

type St = axum::extract::State<State>;

fn now() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

fn default_port() -> u16 {
    8080
}

#[cfg(feature = "tls")]
fn default_cert_path() -> String {
    "ssc/cert.pem".to_string()
}

#[cfg(feature = "tls")]
fn default_key_path() -> String {
    "ssc/key.pem".to_string()
}

struct Color([u8; 4]);

impl Color {
    const WHITE: Self = Self(0xffffffffu32.to_le_bytes());
}

impl std::str::FromStr for Color {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.strip_prefix('#').unwrap_or(s);

        Ok(match s.len() {
            6 => {
                let [r, g, b] = s.parse::<crate::Hex<3>>()?.0;
                Self([r, g, b, 0xff])
            }
            8 => Self(s.parse::<crate::Hex<4>>()?.0),
            _ => {
                return Err(format!(
                    "valid lengths for color are 6, or 8, got {}",
                    s.len()
                ))
            }
        })
    }
}

impl<'de> serde::Deserialize<'de> for Color {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct ColorVisitor;

        impl<'de> serde::de::Visitor<'de> for ColorVisitor {
            type Value = Color;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a hex color")
            }

            fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                v.parse::<Color>().map_err(E::custom)
            }
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                v.parse::<Color>().map_err(E::custom)
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                v.parse::<Color>().map_err(E::custom)
            }
        }

        deserializer.deserialize_any(ColorVisitor)
    }
}

impl serde::Serialize for Color {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        const CHARS: &[u8] = b"0123456789abcdef";

        let mut bytes = [0u8; 9];
        bytes[0] = b'#';

        for (b, d) in self.0.iter().zip(bytes[1..].array_chunks_mut::<2>()) {
            *d = [CHARS[(b >> 4) as usize], CHARS[(b & 0xf) as usize]];
        }

        let len = bytes.len() - 2 * (self.0[3] == 0xff) as usize;
        serializer.serialize_str(unsafe { std::str::from_utf8_unchecked(&bytes[..len]) })
    }
}

fn default_color() -> Color {
    Color::WHITE
}

#[derive(serde::Deserialize, serde::Serialize)]
struct Role {
    id: usize,
    name: String,
    #[serde(default = "default_color")]
    color: Color,
}

#[derive(serde::Deserialize, serde::Serialize)]
struct Channel {
    id: usize,
    group: String,
    name: String,
    roles: Vec<RolePermissions>,
    default_permissions: RolePermissions,
}

fn default_true() -> bool {
    true
}

#[derive(serde::Deserialize, serde::Serialize, Clone, Copy)]
struct RolePermissions {
    id: usize,
    #[serde(default = "default_true")]
    view: bool,
    #[serde(default = "default_true")]
    write: bool,
    #[serde(default)]
    action_rate_limit: Option<u64>,
    #[serde(default)]
    moderate: bool,
    #[serde(default)]
    manage: bool,
}

impl RolePermissions {
    fn max(a: Self, b: &Self) -> Self {
        Self {
            id: std::cmp::max(a.id, b.id),
            view: std::cmp::max(a.view, b.view),
            write: std::cmp::max(a.write, b.write),
            action_rate_limit: std::cmp::max(a.action_rate_limit, b.action_rate_limit),
            moderate: std::cmp::max(a.moderate, b.moderate),
            manage: std::cmp::max(a.manage, b.manage),
        }
    }
}

#[derive(serde::Deserialize, serde::Serialize)]
struct Config {
    name: String,
    roots: Vec<Hex<33>>,

    #[serde(default = "default_port")]
    port: u16,
    #[cfg(feature = "tls")]
    #[serde(default = "default_cert_path")]
    cert: String,
    #[cfg(feature = "tls")]
    #[serde(default = "default_key_path")]
    key: String,

    channels: Vec<Channel>,
    roles: Vec<Role>,
}

impl Config {
    fn sort(&mut self) {
        self.roles.sort_unstable_by_key(|r| r.id);
        self.channels.sort_unstable_by_key(|c| c.id);
        self.channels
            .iter_mut()
            .for_each(|c| c.roles.sort_unstable_by_key(|r| r.id));
    }

    fn channel_perms(&self, channel: usize, roles: &[usize]) -> Option<RolePermissions> {
        let channel = bin_find(&self.channels, channel, |c| c.id)?;
        let role = roles
            .iter()
            .filter_map(|r| bin_find(&channel.roles, *r, |r| r.id))
            .fold(channel.default_permissions, RolePermissions::max);
        Some(role)
    }
}

fn bin_find<T, F: FnMut(&T) -> usize>(space: &[T], needle: usize, key: F) -> Option<&T> {
    space
        .binary_search_by_key(&needle, key)
        .ok()
        .map(|i| &space[i])
}

mod config {
    use std::sync::Arc;

    use crate::user;

    pub async fn get(
        crate::StPat(state): crate::St,
    ) -> Result<crate::Json<Arc<crate::Config>>, crate::Error> {
        Ok(crate::Json(state.config.load_full()))
    }

    pub async fn update(
        user::Auth { id, .. }: user::Auth,
        crate::StPat(state): crate::St,
        crate::Json(mut body): crate::Json<crate::Config>,
    ) -> Result<(), crate::Error> {
        crate::ensure!(
            state.config.load().roots.contains(&crate::Hex(id)),
            UNAUTHORIZED
        );
        body.sort();
        std::fs::write("config.toml", toml::to_string_pretty(&body).unwrap()).unwrap();
        state.config.store(body.into());
        Ok(())
    }
}

fn connect_db() -> rusqlite::Connection {
    let file = std::env::var("DB_PATH").unwrap_or("db.db3".to_string());
    rusqlite::Connection::open(file).unwrap()
}

thread_local! {
    static DB_CONN: std::cell::RefCell<OwnedState> = std::cell::RefCell::new({
        let conn = Box::leak(Box::new(connect_db()));
        OwnedState {
            user: user::Queries::new(conn).unwrap(),
            messages: messages::Queries::new(conn).unwrap(),
            _conn: conn,
        }
    });
}

fn char_to_hex((i, c): (usize, char)) -> Result<u8, String> {
    Ok(match c {
        '0'..='9' => c as u8 - b'0',
        'a'..='f' => c as u8 - b'a' + 10,
        'A'..='F' => c as u8 - b'A' + 10,
        _ => return Err(format!("expected hex character at index {i}, got {c}",)),
    })
}

#[derive(PartialEq, Eq)]
struct Hex<const N: usize>([u8; N]);

impl<const N: usize> std::str::FromStr for Hex<N> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != N * 2 {
            return Err(format!(
                "expected {} hex characters, got {}",
                N * 2,
                s.len()
            ));
        }

        let mut bytes = [0u8; N];

        for (i, [a, b]) in s.chars().enumerate().array_chunks::<2>().enumerate() {
            bytes[i] = char_to_hex(a)? << 4 | char_to_hex(b)?;
        }

        Ok(Self(bytes))
    }
}

impl<const N: usize> rusqlite::ToSql for Hex<N> {
    fn to_sql(&self) -> rusqlite::Result<rusqlite::types::ToSqlOutput<'_>> {
        Ok(rusqlite::types::ToSqlOutput::Borrowed(
            rusqlite::types::ValueRef::Blob(&self.0),
        ))
    }
}

impl<'de, const N: usize> serde::Deserialize<'de> for Hex<N> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct HexVisitor<const N: usize>;

        impl<'de, const N: usize> serde::de::Visitor<'de> for HexVisitor<N> {
            type Value = Hex<N>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a hex string")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                v.parse::<Hex<N>>().map_err(E::custom)
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                v.parse::<Hex<N>>().map_err(E::custom)
            }
        }

        deserializer.deserialize_any(HexVisitor::<N>)
    }
}

impl<const N: usize> serde::Serialize for Hex<N> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        const CHARS: &[u8] = b"0123456789abcdef";

        unsafe {
            let mut array = [0u16; N];
            let mem = std::slice::from_raw_parts_mut(array.as_mut_ptr().cast::<u8>(), N * 2);

            for (chunk, byte) in mem.array_chunks_mut::<2>().zip(self.0.iter()) {
                *chunk = [byte >> 4, byte & 0xf].map(|b| CHARS[b as usize]);
            }

            serializer.serialize_str(std::str::from_utf8_unchecked(mem))
        }
    }
}

#[derive(Debug)]
pub struct Error(crate::SC);

impl axum::response::IntoResponse for Error {
    fn into_response(self) -> axum::response::Response {
        self.0.into_response()
    }
}

impl From<rusqlite::Error> for Error {
    fn from(e: rusqlite::Error) -> Self {
        match &e {
            rusqlite::Error::SqliteFailure(err, _) => match err.code {
                rusqlite::ErrorCode::ConstraintViolation => return Error(crate::SC::CONFLICT),
                rusqlite::ErrorCode::Unknown
                    if let Ok(sc) = crate::SC::from_u16(err.extended_code as _) =>
                {
                    return Error(sc)
                }
                _ => {}
            },
            _ => todo!(),
        }

        eprintln!("uncategorized error: {e:#?}");
        Error(crate::SC::INTERNAL_SERVER_ERROR)
    }
}

fn with_db<T, F: FnOnce(&mut OwnedState) -> rusqlite::Result<T>>(f: F) -> rusqlite::Result<T> {
    DB_CONN.with(|c| f(&mut c.borrow_mut()))
}

struct OwnedState {
    _conn: &'static rusqlite::Connection,
    user: user::Queries,
    messages: messages::Queries,
}

mod messages {
    use crate::user;

    pub fn init_tables(conn: &rusqlite::Connection) -> rusqlite::Result<()> {
        conn.execute_batch(
            "\
                CREATE TABLE IF NOT EXISTS messages (\
                    channel INTEGER NOT NULL,\
                    timestamp INTEGER NOT NULL,\
                    author TEXT NOT NULL,\
                    content BLOB NOT NULL\
                );

                CREATE INDEX IF NOT EXISTS message_ordering ON
                    messages (channel, timestamp DESC);
            ",
        )
    }

    pub struct Queries {
        get_after: rusqlite::Statement<'static>,
        get_before: rusqlite::Statement<'static>,
        create: rusqlite::Statement<'static>,
    }

    impl Queries {
        pub fn new(conn: &'static rusqlite::Connection) -> rusqlite::Result<Self> {
            Ok(Self {
                get_after: conn.prepare(
                    "SELECT channel, timestamp, author, content FROM messages\
                        WHERE channel = ? AND timestamp > ?",
                )?,
                get_before: conn.prepare(
                    "SELECT channel, timestamp, author, content FROM messages\
                        WHERE channel = ? AND timestamp < ?",
                )?,
                create: conn.prepare("INSERT INTO messages (channel, timestamp, author, content) VALUES (?, ?, ?, ?)")?,
            })
        }
    }

    #[derive(serde::Serialize)]
    pub struct Message {
        channel: usize,
        timestamp: u64,
        author: crate::Hex<33>,
        content: String,
    }

    #[derive(serde::Deserialize)]
    #[serde(untagged)]
    enum TimeQuery {
        After { after: u64 },
        Before { before: u64 },
    }

    #[derive(serde::Deserialize)]
    pub struct MessageQuery {
        channel: usize,
        #[serde(flatten)]
        time: TimeQuery,
    }

    pub async fn get(
        crate::Query(q): crate::Query<MessageQuery>,
    ) -> Result<crate::Json<Vec<Message>>, crate::Error> {
        const MESSAGE_FETCH_LIMIT: usize = 40;
        crate::with_db(|db| {
            let mut query = match q.time {
                TimeQuery::After { after } => db.messages.get_after.query((q.channel, after)),
                TimeQuery::Before { before } => db.messages.get_before.query((q.channel, before)),
            }?;

            let mut messages = Vec::with_capacity(MESSAGE_FETCH_LIMIT);

            while let Some(row) = query.next()? {
                let content: Vec<u8> = row.get(3)?;
                let dec = trained_compression::decompress(&content).unwrap();
                let content = String::from_utf8(dec).unwrap();
                messages.push(Message {
                    channel: row.get(0)?,
                    timestamp: row.get(1)?,
                    author: crate::Hex(row.get(2)?),
                    content,
                });
            }

            Ok(crate::Json(messages))
        })
        .map_err(crate::Error::from)
    }

    #[derive(serde::Deserialize)]
    pub struct CreateReq {
        channel: usize,
        content: String,
    }

    pub async fn create(
        user::Auth { id, roles }: user::Auth,
        crate::StPat(state): crate::St,
        crate::Json(body): crate::Json<CreateReq>,
    ) -> Result<(), crate::Error> {
        let writable = state
            .config
            .load()
            .channel_perms(body.channel, &roles)
            .ok_or(crate::Error(crate::SC::NOT_FOUND))?
            .write;

        ensure!(writable, FORBIDDEN);

        crate::with_db(|db| {
            let compressed = trained_compression::compress(body.content.as_bytes());
            let args = (body.channel, crate::now(), id, compressed);
            db.messages.create.insert(args)
        })?;

        Ok(())
    }
}

mod user {

    const RUSQLITE_UNAUTORIZED: rusqlite::Error = rusqlite::Error::SqliteFailure(
        rusqlite::ffi::Error {
            code: rusqlite::ErrorCode::Unknown,
            extended_code: unsafe { std::mem::transmute::<crate::SC, u16>(crate::SC::UNAUTHORIZED) }
                as _,
        },
        None,
    );

    pub fn init_tables(conn: &rusqlite::Connection) -> rusqlite::Result<()> {
        conn.execute_batch(
            "\
                CREATE TABLE IF NOT EXISTS users (\
                    pk BLOB PRIMARY KEY,\
                    name TEXT NOT NULL UNIQUE,\
                    nonce INTEGER NOT NULL DEFAULT 0,\
                    roles BLOB NOT NULL DEFAULT ''\
                );\
                \
                CREATE TABLE IF NOT EXISTS invited (\
                    pk BLOB PRIMARY KEY,\
                    roles BLOB NOT NULL DEFAULT ''\
                );\
            ",
        )
    }

    pub struct Queries {
        eat_token: rusqlite::Statement<'static>,
        create: rusqlite::Statement<'static>,
        get: rusqlite::Statement<'static>,
        bump_nonce: rusqlite::Statement<'static>,
    }

    impl Queries {
        pub fn new(conn: &'static rusqlite::Connection) -> rusqlite::Result<Self> {
            Ok(Self {
                eat_token: conn.prepare("DELETE FROM invited WHERE pk = ? RETURNING init_roles")?,
                create: conn
                    .prepare("INSERT INTO users (pk, name, init_roles) VALUES (?, ?, ?)")?,
                get: conn.prepare("SELECT name FROM users WHERE pk = ?")?,
                bump_nonce: conn.prepare(
                    "UPDATE users SET nonce = ?1 WHERE pk = ?2 AND nonce < ?1 RETURNING roles",
                )?,
            })
        }
    }

    pub struct Auth {
        pub id: [u8; 33],
        pub roles: Vec<usize>,
    }

    #[axum::async_trait]
    impl axum::extract::FromRequestParts<crate::State> for Auth {
        type Rejection = crate::Error;

        async fn from_request_parts(
            parts: &mut axum::http::request::Parts,
            state: &crate::State,
        ) -> Result<Self, Self::Rejection> {
            #[repr(C)]
            #[allow(dead_code)]
            struct Params([u8; 8], [u8; 33], [u8; 64]);

            const SIZE: usize = std::mem::size_of::<Params>();

            let s = parts
                .headers
                .get("Authorization")
                .and_then(|h| h.to_str().ok()?.strip_prefix("Bearer "))
                .and_then(|s| s.parse::<crate::Hex<SIZE>>().ok())
                .ok_or(crate::Error(crate::SC::UNAUTHORIZED))?;

            let Params(nonce, id, sig) =
                unsafe { std::mem::transmute::<crate::Hex<SIZE>, Params>(s) };

            let mut nonce = nonce.to_vec();
            nonce.extend(state.config.load().name.as_bytes());

            let pk = p256::ecdsa::VerifyingKey::from_sec1_bytes(&id)?;
            let sig = p256::ecdsa::Signature::from_bytes(&sig.into())?;
            p256::ecdsa::signature::Verifier::verify(&pk, &nonce, &sig)?;

            let encoded_roles: Vec<u8> = crate::with_db(|db| {
                db.user
                    .bump_nonce
                    .query((nonce, id))?
                    .next()?
                    .ok_or(RUSQLITE_UNAUTORIZED)?
                    .get(0)
            })?;

            Ok(Self {
                id,
                roles: decode_roles(encoded_roles)?,
            })
        }
    }

    fn decode_roles(encoded_roles: Vec<u8>) -> Result<Vec<usize>, crate::Error> {
        let count = encoded_roles.iter().filter(|&&b| b & 0x80 == 0).count();
        let mut roles = Vec::with_capacity(count);
        let mut cursor = encoded_roles.into_iter();

        while !cursor.as_slice().is_empty() {
            let mut value = 0usize;
            for num in cursor.by_ref() {
                value = (value << 7) | (num & 0x7f) as usize;
                if num & 0x80 == 0 {
                    break;
                }
            }
            roles.push(value);
        }

        Ok(roles)
    }

    impl From<p256::ecdsa::Error> for crate::Error {
        fn from(e: p256::ecdsa::Error) -> Self {
            eprintln!("ECDSA error: {e:#?}");
            Self(crate::SC::BAD_REQUEST)
        }
    }

    #[derive(serde::Serialize)]
    pub struct MyUser {
        name: String,
    }

    pub async fn get(Auth { id, .. }: Auth) -> Result<super::Json<MyUser>, crate::Error> {
        Ok(super::with_db(|db| {
            db.user
                .get
                .query_row([id], |row| Ok(MyUser { name: row.get(0)? }))
                .map(super::Json)
        })?)
    }

    #[derive(serde::Deserialize)]
    pub struct CreateReq {
        name: String,
        pk: crate::Hex<33>,
    }

    pub async fn create(
        crate::StPat(state): crate::St,
        super::Json(body): super::Json<CreateReq>,
    ) -> Result<(), super::Error> {
        super::with_db(|db| {
            let init_roles = if state.config.load().roots.contains(&body.pk) {
                vec![]
            } else {
                let mut eaten = db.user.eat_token.query([body.pk.0])?;
                eaten.next()?.ok_or(RUSQLITE_UNAUTORIZED)?.get(0)?
            };

            db.user.create.insert((body.pk, body.name, init_roles))
        })?;

        Ok(())
    }

    pub async fn update() {
        todo!()
    }

    pub async fn delete() {
        todo!()
    }
}

mod other_user {
    pub async fn get() {
        todo!()
    }

    pub async fn update() {
        todo!()
    }

    pub async fn delete() {
        todo!()
    }
}

#[cfg(not(feature = "tls"))]
async fn serve(addr: std::net::SocketAddr, app: axum::Router) {
    let socket = tokio::net::TcpListener::bind(addr).await.unwrap();
    axum::serve(socket, app).await.unwrap();
}

#[cfg(feature = "tls")]
async fn serve(addr: std::net::SocketAddr, app: axum::Router) {
    use axum_server::tls_rustls::RustlsConfig;

    let config = Config::get();
    let config = RustlsConfig::from_pem_file(&config.cert, &config.key)
        .await
        .unwrap();

    axum_server::bind_rustls(addr, config)
        .serve(app.into_make_service())
        .await
        .unwrap();
}

#[derive(Clone)]
struct StaticFiles;

impl tower_service::Service<axum::http::Request<axum::body::Body>> for StaticFiles {
    type Response = axum::http::Response<axum::body::Body>;
    type Error = std::convert::Infallible;
    type Future = impl std::future::Future<Output = Result<Self::Response, Self::Error>> + Send;

    fn poll_ready(
        &mut self,
        _: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        std::task::Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: axum::http::Request<axum::body::Body>) -> Self::Future {
        async move {
            if req.uri().path().contains("..") {
                return Ok(status(crate::SC::NOT_ACCEPTABLE));
            }

            let mut path = format!(".{}", req.uri().path());
            if path == "./" {
                path.push_str("index.html");
            }

            let mime = match Path::new(&path).extension().and_then(OsStr::to_str) {
                Some(ext @ ("html" | "css")) => format!("text/{}", ext),
                Some("js") => "application/javascript".to_string(),
                Some(ext @ "wasm") => format!("application/{}", ext),
                _ => "application/octet-stream".to_string(),
            };

            // we only load small files
            match std::fs::read(path) {
                Ok(data) => Ok({
                    let mut resp = axum::http::Response::builder();
                    if cfg!(feature = "gzip") {
                        resp = resp.header("Content-Encoding", "gzip");
                    }
                    resp.header("Content-Type", mime).body(data.into()).unwrap()
                }),
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                    Ok(status(crate::SC::NOT_FOUND))
                }
                Err(e) => {
                    eprintln!("Error loading file: {:?}, uri: {:?}", e, req.uri());
                    Ok(status(crate::SC::INTERNAL_SERVER_ERROR))
                }
            }
        }
    }
}

fn status(status: crate::SC) -> axum::http::Response<axum::body::Body> {
    axum::http::Response::builder()
        .status(status)
        .body(axum::body::Body::empty())
        .unwrap()
}

#[derive(Clone)]
struct State {
    config: &'static arc_swap::ArcSwap<Config>,
}

impl State {
    fn new() -> Result<Self, String> {
        let file = std::fs::read_to_string("config.toml")
            .map_err(|_| "cant load the config.toml".to_owned())?;
        let mut config: Config =
            toml::from_str(&file).map_err(|e| format!("error in config.toml: {e:#}"))?;
        config.sort();

        Ok(State {
            config: Box::leak(Box::new(arc_swap::ArcSwap::from_pointee(config))),
        })
    }

    #[cfg(test)]
    fn test() -> Self {
        let config = Config {
            name: "test".to_string(),
            roots: vec![],
            port: 0,
            channels: vec![],
            roles: vec![],
        };

        State {
            config: Box::leak(Box::new(arc_swap::ArcSwap::from_pointee(config))),
        }
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    use axum::routing::{delete, get, patch, post};

    let state = match State::new() {
        Ok(s) => s,
        Err(e) => panic!("{}", e),
    };

    {
        let db = connect_db();

        // TODO: include migrations

        user::init_tables(&db).unwrap();
        messages::init_tables(&db).unwrap();
    }

    let app = axum::Router::new()
        .route("/user/:id", get(other_user::get))
        .route("/user/:id", patch(other_user::update))
        .route("/user/:id", delete(other_user::delete))
        .route("/user", get(user::get))
        .route("/user", post(user::create))
        .route("/user", patch(user::update))
        .route("/user", delete(user::delete))
        .route("/messages", get(messages::get))
        .route("/messages", post(messages::create))
        .route("/config", get(config::get))
        .route("/config", patch(config::update))
        .nest_service("/", StaticFiles)
        .with_state(state.clone());

    #[cfg(feature = "hot-reload")]
    let app = app.route(
        "/hot-reload",
        get(|| async {
            use axum::response::sse;
            use notify::Watcher;
            use std::sync::atomic::{AtomicBool, Ordering};
            use std::sync::Arc;

            struct Notify {
                not: Option<notify::RecommendedWatcher>,
                waken: Arc<AtomicBool>,
            }

            impl futures::Stream for Notify {
                type Item = Result<sse::Event, std::convert::Infallible>;
                fn poll_next(
                    self: std::pin::Pin<&mut Self>,
                    cx: &mut std::task::Context<'_>,
                ) -> std::task::Poll<Option<Self::Item>> {
                    let s = self.get_mut();

                    s.not.get_or_insert_with(|| {
                        let waker = cx.waker().clone();
                        let waken = s.waken.clone();
                        let mut not = notify::recommended_watcher(move |_| {
                            waken.store(true, Ordering::Relaxed);
                            waker.wake_by_ref();
                        })
                        .unwrap();
                        not.watch(Path::new("."), notify::RecursiveMode::Recursive)
                            .unwrap();
                        not
                    });

                    if !s.waken.swap(false, Ordering::Relaxed) {
                        return std::task::Poll::Pending;
                    }

                    std::task::Poll::Ready(Some(Ok(sse::Event::default().data(""))))
                }
            }

            static HAPPENED: AtomicBool = AtomicBool::new(false);
            let happened = HAPPENED.swap(true, Ordering::Relaxed);
            sse::Sse::new(Notify {
                not: None,
                waken: Arc::new(AtomicBool::new(!happened)),
            })
        }),
    );

    let port = std::env::var("PORT")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(state.config.load().port);

    serve((Ipv4Addr::UNSPECIFIED, port).into(), app).await;
}

#[cfg(test)]
mod tests {
    use axum::extract::FromRequestParts;

    use crate::user::Auth;

    #[tokio::test]
    async fn decode() {
        let input = "8e2838c096010000024d6a2d5333517ed65c67828a9377f4ef931a249ac3c03b0c7670ddb4c32d43cb9efc59eb8646188ab14b3118c384fa742590aa123799aee024fb1ce2fe0830e25b718d3fe8d05551a2e95caec06d1240289532e4c7d6ad54ac69411348355fe0";

        let req = axum::http::request::Builder::new()
            .header("Authorization", format!("Bearer {}", input))
            .body(())
            .unwrap();

        let mut parts = req.into_parts().0;
        let state = crate::State::test();

        Auth::from_request_parts(&mut parts, &state).await.unwrap();
    }
}
