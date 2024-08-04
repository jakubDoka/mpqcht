#![feature(let_chains)]
#![feature(impl_trait_in_assoc_type)]
#![feature(type_alias_impl_trait)]
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

#[macro_export]
macro_rules! schema {
    (
        name: $name:ident,
        tables: $tables:literal,
        $($field:ident: $query:literal,)*
    ) => {
        pub struct $name {
            $(pub $field: sqlt::Statement<'static>,)*
        }

        impl $name {
            pub fn new(conn: &'static sqlt::Connection) -> rusqlite::Result<Self> {
                Ok(Self {
                    $($field: conn.prepare($query).map_err(|e| $crate::append_query(e, $query))?,)*
                })
            }

            pub fn init_tables(conn: &sqlt::Connection) -> rusqlite::Result<()> {
                conn.execute_batch($tables)
            }
        }
    };
}

extern crate rusqlite as sqlt;

use {
    self::{sse::Event, user::RoleId},
    axum::{
        extract::{Json, Query, State as StPat},
        http::StatusCode as SC,
    },
    sqlt::types as stys,
    std::{ffi::OsStr, net::Ipv4Addr, path::Path},
    tokio::sync::broadcast,
};

fn append_query(e: sqlt::Error, query: &str) -> rusqlite::Error {
    match e {
        sqlt::Error::SqliteFailure(err, Some(message)) => {
            sqlt::Error::SqliteFailure(err, Some(format!("prepare({}) -> {}", message, query)))
        }
        e => e,
    }
}

struct FromStrVisitor<T>(std::marker::PhantomData<T>);

impl<T> FromStrVisitor<T> {
    fn new() -> Self {
        Self(std::marker::PhantomData)
    }
}

impl<'de, T> serde::de::Visitor<'de> for FromStrVisitor<T>
where
    T: std::str::FromStr<Err: std::fmt::Display>,
{
    type Value = T;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a ")?;
        formatter.write_str(std::any::type_name::<T>())?;
        formatter.write_str("(as string)")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        v.parse().map_err(E::custom)
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        v.parse().map_err(E::custom)
    }

    fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        v.parse().map_err(E::custom)
    }
}

type St = axum::extract::State<State>;

fn now() -> u64 {
    std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_secs()
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

impl From<u32> for Color {
    fn from(i: u32) -> Self {
        Color(i.to_be_bytes())
    }
}

impl std::str::FromStr for Color {
    type Err = FromHexError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.strip_prefix('#').unwrap_or(s);

        Ok(match s.len() {
            6 => {
                let [r, g, b] = s.parse::<crate::Hex<3>>()?.0;
                Self([r, g, b, 0xff])
            }
            8 => Self(s.parse::<crate::Hex<4>>()?.0),
            _ => {
                return Err(FromHexError::InvalidLength { got: s.len() as _, want: 7 });
            }
        })
    }
}

impl<'de> serde::Deserialize<'de> for Color {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(FromStrVisitor::<Self>::new())
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
    id: RoleId,
    name: String,
    #[serde(default = "default_color")]
    color: Color,
    voice_tier: Option<voice::Tier>,
    #[serde(default)]
    perms: RolePermissions,
}

#[derive(serde::Deserialize, serde::Serialize, Default)]
struct RolePermissions {
    #[serde(default)]
    can_manage_users: bool,
}

impl RolePermissions {
    fn max(a: Self, b: &Self) -> Self {
        Self { can_manage_users: std::cmp::max(a.can_manage_users, b.can_manage_users) }
    }
}

type ChannelId = u16;

#[derive(serde::Deserialize, serde::Serialize)]
struct Channel {
    id: ChannelId,
    group: String,
    name: String,
    #[serde(default)]
    roles: Vec<ChannelPermissions>,
    #[serde(default)]
    default_permissions: ChannelPermissions,
    #[cfg(feature = "voice")]
    #[serde(default, skip_serializing_if = "Option::is_none")]
    voice: Option<voice::Channel>,
}

fn default_true() -> bool {
    true
}

fn default_message_length() -> usize {
    1024 * 4
}

#[derive(serde::Deserialize, serde::Serialize, Clone, Copy)]
struct ChannelPermissions {
    #[serde(default)]
    id: RoleId,
    #[serde(default = "default_true")]
    view: bool,
    #[serde(default = "default_true")]
    write: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    action_rate_limit: Option<u64>,
    #[serde(default)]
    moderate: bool,
    #[serde(default)]
    manage: bool,
    #[serde(default = "default_message_length")]
    max_message_length: usize,
}

impl Default for ChannelPermissions {
    fn default() -> Self {
        Self {
            id: RoleId::default(),
            view: true,
            write: true,
            action_rate_limit: None,
            moderate: false,
            manage: false,
            max_message_length: default_message_length(),
        }
    }
}

impl ChannelPermissions {
    fn max(a: Self, b: &Self) -> Self {
        Self {
            id: std::cmp::max(a.id, b.id),
            view: std::cmp::max(a.view, b.view),
            write: std::cmp::max(a.write, b.write),
            action_rate_limit: std::cmp::max(a.action_rate_limit, b.action_rate_limit),
            moderate: std::cmp::max(a.moderate, b.moderate),
            manage: std::cmp::max(a.manage, b.manage),
            max_message_length: std::cmp::max(a.max_message_length, b.max_message_length),
        }
    }
}

#[derive(serde::Deserialize, serde::Serialize)]
struct Theme {
    primary: Color,
    secondary: Color,
    tertiary: Color,
    font: Color,
    neutral: Color,
    success: Color,
    warning: Color,
    error: Color,
}

impl Default for Theme {
    fn default() -> Self {
        Self {
            primary: Color::from(0x2C254FFF),
            secondary: Color::from(0x594B9EFF),
            tertiary: Color::from(0x8874EEFF),
            font: Color::from(0xEDEDEDFF),
            neutral: Color::from(0xF6B248FF),
            success: Color::from(0xC2F353FF),
            warning: Color::from(0xCC931EFF),
            error: Color::from(0xD64E3CFF),
        }
    }
}

fn default_light_theme() -> Theme {
    Theme::default()
}

fn default_public_path() -> String {
    "public".to_string()
}

#[derive(serde::Deserialize, serde::Serialize)]
#[serde(deny_unknown_fields)]
struct Config {
    hostname: user::Name,
    #[cfg(feature = "voice")]
    #[serde(default, skip_serializing_if = "Option::is_none")]
    turn: Option<user::Name>,
    roots: Vec<Hex<33>>,
    #[serde(default = "default_public_path")]
    public: String,

    #[serde(default)]
    dark_theme: Theme,
    #[serde(default = "default_light_theme")]
    light_theme: Theme,

    #[serde(default = "default_port")]
    port: u16,
    #[cfg(feature = "tls")]
    #[serde(default = "default_cert_path")]
    cert: String,
    #[cfg(feature = "tls")]
    #[serde(default = "default_key_path")]
    key: String,

    #[serde(default)]
    channels: Vec<Channel>,
    #[serde(default)]
    roles: Vec<Role>,
}

impl Config {
    fn sort(&mut self) {
        self.roles.sort_unstable_by_key(|r| r.id);
        self.channels.sort_unstable_by_key(|c| c.id);
        self.channels.iter_mut().for_each(|c| c.roles.sort_unstable_by_key(|r| r.id));
    }

    fn channel(&self, id: ChannelId) -> Option<&Channel> {
        bin_find(&self.channels, id, |c| c.id)
    }

    fn role_perms(&self, role: user::Roles) -> RolePermissions {
        self.roles
            .iter()
            .filter(|r| role.contains(r.id))
            .map(|r| &r.perms)
            .fold(RolePermissions::default(), RolePermissions::max)
    }

    fn channel_perms(&self, channel: ChannelId, roles: user::Roles) -> Option<ChannelPermissions> {
        let channel = self.channel(channel)?;
        let role = channel
            .roles
            .iter()
            .filter(|r| roles.contains(r.id))
            .fold(channel.default_permissions, ChannelPermissions::max);
        Some(role)
    }

    fn best_tier(&self, roles: user::Roles) -> Option<voice::Tier> {
        roles
            .filter_map(|r| bin_find(self.roles.as_slice(), r, |r| r.id))
            .filter_map(|r| r.voice_tier)
            .min()
    }
}

fn bin_find<T, K: Ord, F: FnMut(&T) -> K>(space: &[T], needle: K, key: F) -> Option<&T> {
    space.binary_search_by_key(&needle, key).ok().map(|i| &space[i])
}

mod config {
    use {crate::user, std::sync::Arc};

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
        crate::ensure!(state.config.load().roots.contains(&id), UNAUTHORIZED);
        body.sort();
        std::fs::write("config.toml", toml::to_string_pretty(&body).unwrap()).unwrap();
        state.config.store(body.into());
        Ok(())
    }
}

pub enum FromHexError {
    InvalidChar { c: char, i: u16 },
    InvalidLength { got: u16, want: u16 },
}

impl std::fmt::Display for FromHexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FromHexError::InvalidChar { c, i } => {
                write!(f, "invalid hex character at index {i}, got {c}")
            }
            FromHexError::InvalidLength { got, want } => {
                write!(f, "valid lengths for hex are {want}, got {got}")
            }
        }
    }
}

fn char_to_hex((i, c): (usize, char)) -> Result<u8, FromHexError> {
    Ok(match c {
        '0'..='9' => c as u8 - b'0',
        'a'..='f' => c as u8 - b'a' + 10,
        'A'..='F' => c as u8 - b'A' + 10,
        _ => return Err(FromHexError::InvalidChar { c, i: i as _ }),
    })
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
struct Hex<const N: usize>([u8; N]);

impl<const N: usize> std::str::FromStr for Hex<N> {
    type Err = FromHexError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != N * 2 {
            return Err(FromHexError::InvalidLength { got: s.len() as _, want: N as u16 * 2 });
        }

        let mut bytes = [0u8; N];

        for (i, [a, b]) in s.chars().enumerate().array_chunks::<2>().enumerate() {
            bytes[i] = char_to_hex(a)? << 4 | char_to_hex(b)?;
        }

        Ok(Self(bytes))
    }
}

impl<const N: usize> sqlt::ToSql for Hex<N> {
    fn to_sql(&self) -> sqlt::Result<rusqlite::types::ToSqlOutput<'_>> {
        sqlt::ToSql::to_sql(&self.0)
    }
}

impl<'de, const N: usize> serde::Deserialize<'de> for Hex<N> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(FromStrVisitor::<Self>::new())
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

impl From<sqlt::Error> for Error {
    fn from(e: sqlt::Error) -> Self {
        if let sqlt::Error::SqliteFailure(err, _) = &e {
            match err.code {
                sqlt::ErrorCode::ConstraintViolation => return Error(crate::SC::CONFLICT),
                sqlt::ErrorCode::Unknown
                    if let Ok(sc) = crate::SC::from_u16(err.extended_code as _) =>
                {
                    return Error(sc)
                }
                _ => {}
            }
        }

        eprintln!("uncategorized error: {e:#}");
        Error(crate::SC::INTERNAL_SERVER_ERROR)
    }
}

impl From<crate::SC> for crate::Error {
    fn from(e: crate::SC) -> Self {
        Self(e)
    }
}

mod db {
    use std::mem::ManuallyDrop;

    pub struct State {
        pub conn: ManuallyDrop<Box<sqlt::Connection>>,
        pub users: ManuallyDrop<crate::user::Queries>,
        pub messages: ManuallyDrop<crate::messages::Queries>,
    }

    impl State {
        fn new() -> sqlt::Result<Self> {
            let conn = Box::new(connect());
            let sconn: &'static _ = unsafe { &*(&*conn as *const _) };
            Ok(State {
                users: ManuallyDrop::new(crate::user::Queries::new(sconn)?),
                messages: ManuallyDrop::new(crate::messages::Queries::new(sconn)?),
                conn: ManuallyDrop::new(conn),
            })
        }
    }

    impl Drop for State {
        fn drop(&mut self) {
            unsafe {
                ManuallyDrop::drop(&mut self.users);
                ManuallyDrop::drop(&mut self.messages);

                // keep this last
                ManuallyDrop::drop(&mut self.conn);
            }
        }
    }

    pub fn connect() -> sqlt::Connection {
        let file = std::env::var("DB_PATH").unwrap_or("db.db3".to_string());
        sqlt::Connection::open(file).unwrap()
    }

    pub fn with<T, F: FnOnce(&mut State) -> sqlt::Result<T>>(f: F) -> rusqlite::Result<T> {
        thread_local! {
            static DB_CONN: std::cell::RefCell<State> =
                std::cell::RefCell::new(State::new().unwrap());
        }
        DB_CONN.with(|c| f(&mut c.borrow_mut()))
    }
}

mod messages {

    use {
        crate::{sse::Event, stys, user, ChannelId},
        std::sync::Arc,
    };

    schema! {
        name: Queries,
        tables: "
            CREATE TABLE IF NOT EXISTS messages (
                channel INTEGER NOT NULL,
                timestamp INTEGER NOT NULL,
                author BLOB NOT NULL,
                content BLOB NOT NULL,
                FOREIGN KEY (author) REFERENCES users(pk) ON DELETE CASCADE
            );

            CREATE INDEX IF NOT EXISTS message_ordering ON
                messages (channel, timestamp DESC);
        ",
        get_after: "
            SELECT
                rowid, channel, timestamp, author,
                users.name as author_name, content
            FROM
                messages INNER JOIN users ON users.pk = author
            WHERE
                channel = ? AND timestamp > ?
            ORDER BY
                timestamp ASC
        ",
        get_before: "
            SELECT
                rowid, channel, timestamp, author,
                users.name as author_name, content
            FROM
                messages INNER JOIN users ON users.pk = author
            WHERE
                channel = ? AND timestamp < ?
        ",
        create: "
            INSERT INTO messages(channel, timestamp, author, content)
            VALUES (?, ?, ?, ?)
        ",
    }

    #[derive(serde::Serialize, serde::Deserialize)]
    #[serde(transparent)]
    pub struct Compressed(String);

    impl sqlt::ToSql for Compressed {
        fn to_sql(&self) -> sqlt::Result<rusqlite::types::ToSqlOutput<'_>> {
            Ok(stys::ToSqlOutput::Owned(sqlt::types::Value::Blob(trained_compression::compress(
                self.0.as_bytes(),
            ))))
        }
    }

    impl stys::FromSql for Compressed {
        fn column_result(value: stys::ValueRef<'_>) -> sqlt::types::FromSqlResult<Self> {
            trained_compression::decompress(value.as_blob()?)
                .and_then(|arr| String::from_utf8(arr).ok().map(Self))
                .ok_or(stys::FromSqlError::InvalidType)
        }
    }

    #[derive(serde::Serialize)]
    pub struct Message {
        pub id: u32,
        pub channel: u16,
        pub timestamp: u64,
        pub author_pk: crate::Hex<33>,
        pub author: user::Name,
        pub content: Compressed,
    }

    #[derive(serde::Deserialize, Default)]
    enum TimeQuery {
        #[default]
        Before,
        After,
    }

    #[derive(serde::Deserialize)]
    pub struct MessageQuery {
        channel: usize,
        #[serde(default)]
        kind: TimeQuery,
        time: u64,
    }

    pub async fn get(
        crate::Query(q): crate::Query<MessageQuery>,
    ) -> Result<crate::Json<Vec<Message>>, crate::Error> {
        const MESSAGE_FETCH_LIMIT: usize = 40;
        crate::db::with(|db| {
            let query = match q.kind {
                TimeQuery::Before => &mut db.messages.get_before,
                TimeQuery::After => &mut db.messages.get_after,
            };

            let mut rows = query.query((q.channel, q.time))?;
            let mut messages = Vec::with_capacity(MESSAGE_FETCH_LIMIT);

            while let Some(row) = rows.next()?
                && messages.len() < MESSAGE_FETCH_LIMIT
            {
                messages.push(Message {
                    id: row.get(0)?,
                    channel: row.get(1)?,
                    timestamp: row.get(2)?,
                    author_pk: crate::Hex(row.get(3)?),
                    author: row.get(4)?,
                    content: row.get(5)?,
                });
            }

            Ok(crate::Json(messages))
        })
        .map_err(crate::Error::from)
    }

    #[derive(serde::Deserialize)]
    pub struct CreateQery {
        channel: ChannelId,
    }

    pub async fn create(
        user::Auth { id, user }: user::Auth,
        crate::StPat(state): crate::St,
        crate::Query(q): crate::Query<CreateQery>,
        content: String,
    ) -> Result<(), crate::Error> {
        {
            let perms = state
                .config
                .load()
                .channel_perms(q.channel, user.roles)
                .ok_or(crate::Error(crate::SC::NOT_FOUND))?;

            ensure!(perms.write, FORBIDDEN);
            ensure!(perms.max_message_length >= content.len(), PAYLOAD_TOO_LARGE);
            // TODO: rate limiting per channel
        }

        crate::db::with(|db| {
            let compressed = trained_compression::compress(content.as_bytes());
            let timestamp = crate::now();
            let args = (q.channel, timestamp, id, compressed);
            let rowid = db.messages.create.insert(args)?;
            _ = state.pubsub.send(Event::Message(Arc::new(Message {
                id: rowid as _,
                channel: q.channel,
                timestamp: crate::now(),
                author_pk: id,
                author: user.name,
                content: Compressed(content),
            })));
            Ok(())
        })?;

        Ok(())
    }
}

mod user {
    use {
        crate::{stys, FromHexError},
        serde::ser::SerializeSeq,
    };

    const fn status_err(status: crate::SC) -> sqlt::Error {
        sqlt::Error::SqliteFailure(
            sqlt::ffi::Error {
                code: sqlt::ErrorCode::Unknown,
                extended_code: unsafe { std::mem::transmute::<crate::SC, u16>(status) } as _,
            },
            None,
        )
    }

    crate::schema! {
        name: Queries,
        tables: "
            CREATE TABLE IF NOT EXISTS users (
                pk BLOB PRIMARY KEY,
                name TEXT NOT NULL UNIQUE,
                nonce INTEGER NOT NULL DEFAULT 0,
                roles INTEGER NOT NULL DEFAULT 0
            ) WITHOUT ROWID;
            
            CREATE TABLE IF NOT EXISTS invited (
                pk BLOB PRIMARY KEY,
                roles INTEGER NOT NULL DEFAULT 0
            ) WITHOUT ROWID;
        ",
        eat_token: "DELETE FROM invited WHERE pk = ? RETURNING roles",
        create: "INSERT INTO users (pk, name, roles) VALUES (?, ?, ?)",
        get: "SELECT name, roles FROM users WHERE pk = ?",
        bump_nonce: "UPDATE users SET nonce = ?1
            WHERE pk = ?2 AND nonce < ?1 + 600 RETURNING name, roles",
        invite: "INSERT INTO invited (pk, roles) VALUES (?, ?)",
    }

    #[derive(Clone, Copy, serde::Serialize, serde::Deserialize)]
    pub struct User {
        pub name: Name,
        pub roles: Roles,
    }

    impl User {
        pub fn from_row(row: &rusqlite::Row) -> sqlt::Result<Self> {
            Ok(Self { name: row.get(0)?, roles: row.get(1)? })
        }
    }

    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, Default)]
    #[repr(transparent)]
    pub struct RoleId(u8);

    impl<'de> serde::Deserialize<'de> for RoleId {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de>,
        {
            let id = u8::deserialize(deserializer)?;
            if id >= 64 {
                return Err(serde::de::Error::custom(
                    "invalid role id, expected integer from 0 to 63 (inclusive)",
                ));
            }
            Ok(Self(id))
        }
    }

    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
    pub struct Roles(i64);

    impl Iterator for Roles {
        type Item = RoleId;

        fn next(&mut self) -> Option<Self::Item> {
            if self.0 == 0 {
                return None;
            }
            let role = RoleId(self.0.trailing_zeros() as u8);
            self.0 &= self.0 - 1;
            Some(role)
        }
    }

    impl serde::Serialize for Roles {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            let mut seq = serializer.serialize_seq(Some(self.0.count_ones() as _))?;
            for role in *self {
                seq.serialize_element(&role)?;
            }
            seq.end()
        }
    }

    impl<'de> serde::Deserialize<'de> for Roles {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de>,
        {
            struct Visitor;

            impl<'de> serde::de::Visitor<'de> for Visitor {
                type Value = Roles;

                fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                    formatter.write_str("a list of role ids")
                }

                fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                where
                    A: serde::de::SeqAccess<'de>,
                {
                    let mut bits = 0u64;
                    while let Some(role) = seq.next_element::<RoleId>()? {
                        bits |= 1 << role.0;
                    }
                    Ok(Roles(bits as _))
                }
            }

            deserializer.deserialize_seq(Visitor)
        }
    }

    impl From<Vec<RoleId>> for Roles {
        fn from(roles: Vec<RoleId>) -> Self {
            let mut bits = 0;
            for role in roles {
                bits |= 1 << role.0;
            }
            Self(bits)
        }
    }

    impl From<Roles> for Vec<RoleId> {
        fn from(value: Roles) -> Self {
            let mut roles = Vec::with_capacity(value.0.count_ones() as usize);
            for i in 0..64 {
                if value.0 & (1 << i) != 0 {
                    roles.push(RoleId(i));
                }
            }
            roles
        }
    }

    impl Roles {
        pub fn contains(&self, role: RoleId) -> bool {
            self.0 & (1 << role.0) != 0
        }
    }

    impl stys::FromSql for Roles {
        fn column_result(value: stys::ValueRef<'_>) -> stys::FromSqlResult<Self> {
            i64::column_result(value).map(Self)
        }
    }

    impl sqlt::ToSql for Roles {
        fn to_sql(&self) -> sqlt::Result<rusqlite::types::ToSqlOutput<'_>> {
            self.0.to_sql()
        }
    }

    #[derive(Clone, Copy, Default)]
    pub struct Name {
        bytes: [u8; 32],
    }

    impl Name {
        pub fn as_str(&self) -> &str {
            unsafe { std::str::from_utf8_unchecked(&self.bytes[..self.len()]) }
        }

        pub fn len(&self) -> usize {
            self.bytes.iter().rposition(|&b| b != 0).map_or(0, |i| i + 1)
        }
    }

    impl std::ops::Deref for Name {
        type Target = str;

        fn deref(&self) -> &Self::Target {
            self.as_str()
        }
    }

    impl std::str::FromStr for Name {
        type Err = &'static str;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            if s.len() > 32 {
                return Err("username too long (max 32 bytes)");
            }
            let mut bytes = [0; 32];
            bytes[..s.len()].copy_from_slice(s.as_bytes());
            Ok(Self { bytes })
        }
    }

    impl<'de> serde::Deserialize<'de> for Name {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de>,
        {
            deserializer.deserialize_any(crate::FromStrVisitor::<Self>::new())
        }
    }

    impl serde::Serialize for Name {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            serializer.serialize_str(self)
        }
    }

    impl sqlt::ToSql for Name {
        fn to_sql(&self) -> sqlt::Result<rusqlite::types::ToSqlOutput<'_>> {
            sqlt::ToSql::to_sql(&**self)
        }
    }

    impl stys::FromSql for Name {
        fn column_result(value: stys::ValueRef<'_>) -> sqlt::types::FromSqlResult<Self> {
            value.as_str()?.parse::<Self>().map_err(Into::into).map_err(stys::FromSqlError::Other)
        }
    }

    #[derive(Clone, Copy, serde::Serialize, serde::Deserialize)]
    pub struct Auth {
        pub id: crate::Hex<33>,
        pub user: User,
    }

    impl Auth {
        pub fn from_params(params: AuthParams, state: &crate::State) -> Result<Self, crate::Error> {
            params.verify(&state.config.load().hostname)?;

            let user = crate::db::with(|db| {
                db.users
                    .bump_nonce
                    .query((u64::from_le_bytes(params.nonce), params.pk))?
                    .mapped(User::from_row)
                    .next()
                    .ok_or(status_err(crate::SC::FORBIDDEN))?
            })?;

            Ok(Self { id: crate::Hex(params.pk), user })
        }
    }

    #[repr(C)]
    pub struct AuthParams {
        nonce: [u8; 8],
        pk: [u8; 33],
        sig: [u8; 64],
    }

    impl std::str::FromStr for AuthParams {
        type Err = FromHexError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            type HexRepr = crate::Hex<{ std::mem::size_of::<AuthParams>() }>;
            let hex = s.parse::<HexRepr>()?;
            Ok(unsafe { std::mem::transmute::<HexRepr, AuthParams>(hex) })
        }
    }

    impl<'de> serde::Deserialize<'de> for AuthParams {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de>,
        {
            deserializer.deserialize_any(crate::FromStrVisitor::<Self>::new())
        }
    }

    impl AuthParams {
        fn verify(&self, server: &str) -> Result<(), p256::ecdsa::Error> {
            let mut buff = [0u8; 8 + 32];
            buff[..8].copy_from_slice(&self.nonce);
            buff[8..][..server.len()].copy_from_slice(server.as_bytes());

            let pk = p256::ecdsa::VerifyingKey::from_sec1_bytes(&self.pk)?;
            let sig = p256::ecdsa::Signature::from_bytes(&self.sig.into())?;
            p256::ecdsa::signature::Verifier::verify(&pk, &buff[..8 + server.len()], &sig)
        }
    }

    #[axum::async_trait]
    impl axum::extract::FromRequestParts<crate::State> for Auth {
        type Rejection = crate::Error;

        async fn from_request_parts(
            parts: &mut axum::http::request::Parts,
            state: &crate::State,
        ) -> Result<Self, Self::Rejection> {
            let params = parts
                .headers
                .get("Authorization")
                .and_then(|h| h.to_str().ok()?.strip_prefix("Bearer "))
                .and_then(|s| s.parse::<AuthParams>().ok())
                .ok_or(crate::Error(crate::SC::UNAUTHORIZED))?;

            Self::from_params(params, state)
        }
    }

    impl From<p256::ecdsa::Error> for crate::Error {
        fn from(e: p256::ecdsa::Error) -> Self {
            eprintln!("ECDSA error: {e:#?}");
            Self(crate::SC::BAD_REQUEST)
        }
    }

    pub async fn get(Auth { id, .. }: Auth) -> Result<super::Json<User>, crate::Error> {
        Ok(super::db::with(|db| db.users.get.query_row([id], User::from_row).map(super::Json))?)
    }

    #[derive(serde::Deserialize)]
    pub struct CreateReq {
        name: Name,
        proof: AuthParams,
    }

    pub async fn create(
        crate::StPat(state): crate::St,
        super::Json(body): super::Json<CreateReq>,
    ) -> Result<(), super::Error> {
        crate::ensure!(body.proof.nonce == [0; 8], BAD_REQUEST);

        let config = state.config.load();
        body.proof.verify(&config.hostname)?;

        super::db::with(|db| {
            let init_roles = if state.config.load().roots.contains(&crate::Hex(body.proof.pk)) {
                Roles::default()
            } else {
                let mut eaten = db.users.eat_token.query([body.proof.pk])?;
                eaten.next()?.ok_or(status_err(crate::SC::FORBIDDEN))?.get(0)?
            };

            db.users.create.insert((body.proof.pk, body.name, init_roles))
        })?;

        Ok(())
    }

    #[derive(serde::Deserialize)]
    pub struct AddToken {
        pk: crate::Hex<33>,
        roles: Roles,
    }

    pub async fn invite(
        Auth { user, .. }: Auth,
        crate::StPat(state): crate::St,
        crate::Json(body): crate::Json<AddToken>,
    ) -> Result<(), crate::Error> {
        let perms = state.config.load().role_perms(user.roles);
        ensure!(perms.can_manage_users, FORBIDDEN);
        crate::db::with(|db| db.users.invite.insert((body.pk, body.roles)))?;
        Ok(())
    }
}

#[cfg(feature = "voice")]
mod voice {
    use {
        crate::{
            user::{self, Auth, User},
            ChannelId, Config,
        },
        axum::{extract::ws::Message, Json},
        futures::{FutureExt, StreamExt},
        std::{collections::HashMap, sync::RwLock},
        tokio::sync::mpsc,
    };

    pub type Tier = u8;

    #[derive(serde::Deserialize, serde::Serialize, Clone, Copy)]
    pub struct TierSpec {
        pub id: Tier,
        pub bandwidth: usize,
        pub session_duration_secs: usize,
    }

    #[derive(serde::Serialize, serde::Deserialize)]
    pub struct TurnCreds {
        expiry: u64,
        username: String,
        password: String,
    }

    pub mod coturn {
        use hmac::{digest::KeyInit, Mac};

        pub struct TurnServer {
            secret: hmac::Hmac<sha1::Sha1>,
        }

        impl TurnServer {
            pub fn from_env() -> Self {
                let secret = std::env::var("TURN_SECRET").expect("TURN_SECRET env var is not set");
                Self::new(&secret)
            }

            pub fn new(secret: &str) -> Self {
                Self {
                    secret: <hmac::Hmac<sha1::Sha1> as KeyInit>::new_from_slice(secret.as_bytes())
                        .unwrap(),
                }
            }

            pub async fn generate_credentials(
                &self,
                _tier: super::Tier,
                duration: std::time::Duration,
            ) -> Result<super::TurnCreds, crate::Error> {
                let expiry = std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .saturating_add(duration)
                    .as_secs();
                let username = format!("{expiry}");
                let mut sha = self.secret.clone();
                sha.update(username.as_bytes());
                use base64::Engine;
                let password =
                    base64::engine::general_purpose::STANDARD.encode(sha.finalize().into_bytes());
                Ok(super::TurnCreds { username, password, expiry })
            }
        }
    }

    #[derive(serde::Deserialize, serde::Serialize, Clone, Copy)]
    pub struct Channel {
        max_participants: usize,
    }

    struct Peer {
        auth: user::Auth,
        sender: mpsc::Sender<String>,
    }

    pub struct Session {
        id: ChannelId,
        peers: RwLock<HashMap<[u8; 33], Peer>>,
        channel: Channel,
    }

    impl Session {
        pub fn init(config: &Config) -> Vec<Self> {
            config
                .channels
                .iter()
                .filter_map(|ch| {
                    ch.voice.map(|channel| Session {
                        id: ch.id,
                        peers: Default::default(),
                        channel,
                    })
                })
                .collect()
        }
    }

    #[derive(serde::Serialize, serde::Deserialize, Debug)]
    struct PeekTo {
        to: crate::Hex<33>,
    }

    #[derive(serde::Serialize, serde::Deserialize, Debug)]
    struct PeekType<'a> {
        r#type: &'a str,
    }

    #[allow(non_camel_case_types)]
    #[derive(serde::Serialize, serde::Deserialize)]
    #[serde(tag = "type")]
    enum SystemEvent {
        left { to: crate::Hex<33>, from: crate::Hex<33>, body: User },
        joined { to: crate::Hex<33>, from: crate::Hex<33>, body: User },
        present { to: crate::Hex<33>, participants: Vec<Auth> },
    }

    pub async fn ws(
        axum::extract::Path((channel, params)): axum::extract::Path<(ChannelId, user::AuthParams)>,
        ws: axum::extract::WebSocketUpgrade,
        crate::StPat(state): crate::St,
    ) -> Result<axum::response::Response, crate::Error> {
        #[derive(Debug)]
        enum Event {
            Recv(String),
            Send(String),
        }

        let auth = user::Auth::from_params(params, &state)?;

        if state.config.load().channel_perms(channel, auth.user.roles).map_or(true, |p| !p.view) {
            return Err(crate::SC::FORBIDDEN.into());
        }

        let sessions = state.voice_sessions.load_full();
        let session_index =
            sessions.binary_search_by_key(&channel, |s| s.id).ok().ok_or(crate::SC::NOT_FOUND)?;

        {
            let session = &sessions[session_index];
            if session.peers.read().unwrap().len() == session.channel.max_participants {
                return Err(crate::SC::TOO_MANY_REQUESTS.into());
            }
        }

        fn to_message<T: serde::Serialize>(e: T) -> String {
            serde_json::to_string(&e).unwrap_or_else(|e| {
                eprintln!("error serializing event: {e:#}");
                String::new()
            })
        }

        Ok(ws.on_upgrade(move |mut socket| async move {
            let session = &sessions[session_index];

            {
                let peers = session
                    .peers
                    .read()
                    .unwrap()
                    .iter()
                    .map(|(&id, peer)| Auth { id: crate::Hex(id), user: peer.auth.user })
                    .collect::<Vec<_>>();
                if socket
                    .send(Message::Text(to_message(SystemEvent::present {
                        to: auth.id,
                        participants: peers,
                    })))
                    .await
                    .is_err()
                {
                    return;
                }
            }

            let mut recv = {
                let mut peers = session.peers.write().unwrap();
                let (sender, receiver) = mpsc::channel(10);
                peers.insert(auth.id.0, Peer { auth, sender });
                receiver
            };

            loop {
                let event = futures::select! {
                    msg = socket.next().fuse() => {
                        let Some(Ok(Message::Text(msg))) = msg else { break };
                        Event::Send(msg)
                    },
                    msg = recv.recv().fuse() => {
                        let Some(msg) = msg else { break };
                        Event::Recv(msg)
                    },
                };

                if match event {
                    Event::Recv(msg) => socket.send(Message::Text(msg)).await.is_err(),
                    Event::Send(msg) => {
                        if let Ok(PeekType { r#type: "ready" }) = serde_json::from_str(&msg) {
                            let peers = session.peers.read().unwrap();
                            for (&id, peer) in peers.iter() {
                                if id == auth.id.0 {
                                    continue;
                                }
                                _ = peer.sender.try_send(to_message(SystemEvent::joined {
                                    from: auth.id,
                                    to: crate::Hex(id),
                                    body: auth.user,
                                }));
                            }
                            continue;
                        }
                        let Ok(PeekTo { to }) = serde_json::from_str(&msg) else { break };
                        let peer =
                            session.peers.read().unwrap().get(&to.0).map(|s| s.sender.clone());
                        if let Some(peer) = peer {
                            _ = peer.try_send(msg);
                        }
                        false
                    }
                } {
                    break;
                }
            }

            let mut peers = session.peers.write().unwrap();
            peers.remove(&auth.id.0);
            for (&id, peer) in peers.iter() {
                _ = peer.sender.try_send(to_message(SystemEvent::left {
                    from: auth.id,
                    to: crate::Hex(id),
                    body: auth.user,
                }));
            }
        }))
    }

    pub async fn peers(
        axum::extract::Path((channel,)): axum::extract::Path<(ChannelId,)>,
        Auth { id, .. }: Auth,
        crate::StPat(state): crate::St,
    ) -> Result<Json<Vec<Auth>>, crate::Error> {
        let sessions = state.voice_sessions.load();
        let index =
            sessions.binary_search_by_key(&channel, |s| s.id).ok().ok_or(crate::SC::NOT_FOUND)?;
        let vec = sessions[index]
            .peers
            .read()
            .unwrap()
            .values()
            .map(|p| p.auth)
            .filter(|a| a.id != id)
            .collect();
        Ok(Json(vec))
    }

    pub async fn turn_auth(
        Auth { user, id }: Auth,
        crate::StPat(state): crate::St,
    ) -> Result<Json<TurnCreds>, crate::Error> {
        let is_root = state.config.load().roots.contains(&id);

        let realm = if is_root {
            0
        } else {
            state.config.load().best_tier(user.roles).ok_or(crate::SC::FORBIDDEN)?
        };

        let timeout = std::time::Duration::from_secs(3600);
        state.turn.generate_credentials(realm, timeout).await.map(Json)
    }
}

mod sse {
    use {
        crate::{messages, user},
        axum::response::{sse, Sse},
        std::sync::Arc,
        stream::*,
        tokio::sync::broadcast,
    };

    #[derive(Clone, serde::Serialize)]
    #[serde(tag = "type")]
    pub enum Event {
        Message(Arc<messages::Message>),
    }

    mod stream {
        use {super::*, n::*, std::future::Future};

        struct State {
            auth: user::Auth,
            state: crate::State,
            events: broadcast::Receiver<Event>,
        }

        mod n {
            use super::*;
            pub type Nexter = impl Future<Output = Option<(State, Event)>>;

            impl State {
                pub fn next(mut self) -> Nexter {
                    async move {
                        loop {
                            let ev = self.events.recv().await.ok()?;
                            if self.should_send(&ev) {
                                break Some((self, ev));
                            }
                        }
                    }
                }

                pub fn should_send(&self, ev: &Event) -> bool {
                    match ev {
                        Event::Message(msg) => self
                            .state
                            .config
                            .load()
                            .channel_perms(msg.channel, self.auth.user.roles)
                            .map_or(false, |p| p.view),
                    }
                }
            }
        }

        pub struct Stream(Nexter);

        impl Stream {
            pub fn new(auth: user::Auth, state: crate::State) -> Self {
                Self(State { auth, state, events: state.pubsub.subscribe() }.next())
            }
        }

        impl futures::Stream for Stream {
            type Item = Result<sse::Event, std::convert::Infallible>;

            fn poll_next(
                self: std::pin::Pin<&mut Self>,
                cx: &mut std::task::Context<'_>,
            ) -> std::task::Poll<Option<Self::Item>> {
                let s = unsafe { self.get_unchecked_mut() };
                let Some((state, ev)) =
                    std::task::ready!(unsafe { std::pin::Pin::new_unchecked(&mut s.0) }.poll(cx))
                else {
                    return std::task::Poll::Ready(None);
                };

                s.0 = state.next();

                std::task::Poll::Ready(Some(Ok(sse::Event::default().json_data(ev).unwrap())))
            }
        }
    }

    pub async fn get(
        axum::extract::Path((params,)): axum::extract::Path<(user::AuthParams,)>,
        crate::StPat(state): crate::St,
    ) -> Result<
        Sse<impl futures::Stream<Item = Result<sse::Event, std::convert::Infallible>>>,
        crate::Error,
    > {
        let auth = user::Auth::from_params(params, &state)?;
        Ok(sse::Sse::new(Stream::new(auth, state)))
    }
}

#[cfg(not(feature = "tls"))]
async fn serve(port: u16, app: axum::Router, _: State) {
    let socket = tokio::net::TcpListener::bind((Ipv4Addr::LOCALHOST, port)).await.unwrap();
    axum::serve(socket, app).await.unwrap();
}

#[cfg(feature = "tls")]
async fn serve(port: u16, app: axum::Router, state: State) {
    use axum_server::tls_rustls::RustlsConfig;

    let config = state.config.load();
    let config = RustlsConfig::from_pem_file(&config.cert, &config.key).await.unwrap_or_else(|e| {
        panic!("error loading tls config ({} {}): {e}", config.cert, config.key)
    });

    axum_server::bind_rustls((Ipv4Addr::UNSPECIFIED, port).into(), config)
        .serve(app.into_make_service())
        .await
        .unwrap();
}

#[derive(Clone)]
struct StaticFiles(State);

impl tower_service::Service<axum::http::Request<axum::body::Body>> for StaticFiles {
    type Error = std::convert::Infallible;
    type Response = axum::http::Response<axum::body::Body>;

    type Future = impl std::future::Future<Output = Result<Self::Response, Self::Error>> + Send;

    fn poll_ready(
        &mut self,
        _: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        std::task::Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: axum::http::Request<axum::body::Body>) -> Self::Future {
        let state = self.0;
        async move {
            if req.uri().path().contains("..") {
                return Ok(status(crate::SC::NOT_ACCEPTABLE));
            }

            let mut path = format!("{}/{}", state.config.load().public, req.uri().path());
            if req.uri().path() == "/" {
                path.push_str("index.html");
            }

            let mime = match Path::new(&path).extension().and_then(OsStr::to_str) {
                Some(ext @ ("html" | "css")) => format!("text/{}", ext),
                Some("js") => "application/javascript".to_string(),
                Some(ext @ "wasm") => format!("application/{}", ext),
                _ => "application/octet-stream".to_string(),
            };

            if cfg!(feature = "gzip") {
                path.push_str(".gz");
            }

            // we only load small files
            Ok(match std::fs::read(path) {
                Ok(data) => {
                    let mut resp = axum::http::Response::builder();
                    if cfg!(feature = "gzip") {
                        resp = resp.header("Content-Encoding", "gzip");
                    }
                    resp.header("Content-Type", mime).body(data.into()).unwrap()
                }
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => status(crate::SC::NOT_FOUND),
                Err(e) => {
                    eprintln!("Error loading file: {e:?}, uri: {:?}", req.uri());
                    status(crate::SC::INTERNAL_SERVER_ERROR)
                }
            })
        }
    }
}

fn status(status: crate::SC) -> axum::http::Response<axum::body::Body> {
    axum::http::Response::builder().status(status).body(axum::body::Body::empty()).unwrap()
}

type State = &'static OwnedState;

struct OwnedState {
    config: arc_swap::ArcSwap<Config>,
    pubsub: broadcast::Sender<Event>,
    #[cfg(feature = "voice")]
    voice_sessions: arc_swap::ArcSwap<Vec<voice::Session>>,
    #[cfg(feature = "voice")]
    turn: voice::coturn::TurnServer,
}

impl OwnedState {
    fn new() -> Result<Self, String> {
        let file = std::fs::read_to_string("config.toml")
            .map_err(|_| "cant load the config.toml".to_owned())?;
        let mut config: Config =
            toml::from_str(&file).map_err(|e| format!("error in config.toml: {e:#}"))?;
        config.sort();

        Ok(OwnedState {
            #[cfg(feature = "voice")]
            voice_sessions: arc_swap::ArcSwap::from_pointee(voice::Session::init(&config)),
            #[cfg(feature = "voice")]
            turn: voice::coturn::TurnServer::from_env(),
            config: arc_swap::ArcSwap::from_pointee(config),
            pubsub: broadcast::channel(100).0,
        })
    }
}

#[cfg(feature = "hot-reload")]
mod hot_reload {
    use {
        axum::response::{sse, Sse},
        notify::Watcher,
        std::sync::{
            atomic::{AtomicBool, Ordering},
            Arc,
        },
    };

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
                let mut not = notify::recommended_watcher(move |e| {
                    if matches!(e, Ok(notify::Event { paths, .. })
                        if paths.iter().all(|p|
                            p.to_str().unwrap().contains("db")))
                    {
                        return;
                    }
                    waken.store(true, Ordering::Relaxed);
                    waker.wake_by_ref();
                })
                .unwrap();
                not.watch(std::path::Path::new("."), notify::RecursiveMode::Recursive).unwrap();
                not
            });

            if !s.waken.swap(false, Ordering::Relaxed) {
                return std::task::Poll::Pending;
            }

            std::task::Poll::Ready(Some(Ok(sse::Event::default().data(""))))
        }
    }

    pub async fn get(
    ) -> Sse<impl futures::Stream<Item = Result<sse::Event, std::convert::Infallible>>> {
        // We want to reload when server is recompiled
        static FIRST_TIME: AtomicBool = AtomicBool::new(true);
        let first_time = FIRST_TIME.swap(false, Ordering::Relaxed);
        sse::Sse::new(Notify { not: None, waken: Arc::new(AtomicBool::new(first_time)) })
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    use axum::routing::{get, patch, post};

    let state = match OwnedState::new() {
        Ok(s) => s,
        Err(e) => panic!("{e}"),
    };
    let state: &'static OwnedState = unsafe { &*(&state as *const _) };

    {
        let db = db::connect();

        // TODO: include migrations

        user::Queries::init_tables(&db).unwrap();
        messages::Queries::init_tables(&db).unwrap();

        // validates queries early
        #[cfg(debug_assertions)]
        db::with(|_| Ok(())).unwrap();
    }

    let app = axum::Router::new()
        .route("/user", get(user::get))
        .route("/user", post(user::create))
        .route("/user/invite", post(user::invite))
        .route("/messages", get(messages::get))
        .route("/messages", post(messages::create))
        .route("/config", get(config::get))
        .route("/config", patch(config::update))
        .route("/sse/:auth", get(sse::get))
        .nest_service("/", StaticFiles(state));

    #[cfg(feature = "voice")]
    let app = app
        .route("/voice/:channel/ws/:auth", get(voice::ws))
        .route("/voice/:channel/peers", get(voice::peers))
        .route("/voice/turn-auth", get(voice::turn_auth));

    #[cfg(feature = "hot-reload")]
    let app = app.route("/hot-reload", get(hot_reload::get));

    let app = app.layer(tower_http::cors::CorsLayer::permissive());

    serve(state.config.load().port, app.with_state(state), state).await;
}
