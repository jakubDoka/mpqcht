const std = @import("std");
const sql = @import("sqlite");
const zap = @import("zap");
const opt = @import("options");
//const cf = @import("cf.zig");

const ws = zap.WebSockets.Handler(WsContext);
const WsHandle = zap.WebSockets.WsHandle;
const Ecdsa = std.crypto.sign.ecdsa.EcdsaP256Sha256;

const shared = struct {
    pub const ServerConfig = Config;
};

const codec = struct {
    pub const Reader = std.io.FixedBufferStream([]const u8).Reader;
    pub const ParseError = Reader.NoEofError || error{Overflow};
    pub const Writer = std.io.FixedBufferStream([]u8).Writer;
    pub const FormatError = Writer.Error;

    pub fn format(value: anytype, writer: Writer) FormatError!void {
        return formatLow(value, writer);
    }

    fn formatLow(value: anytype, writer: Writer) FormatError!void {
        const T = @TypeOf(value);
        if (T == []const u8) {
            try formatLow(value.len, writer);
            try writer.writeAll(value);
            return;
        }

        return switch (@typeInfo(T)) {
            .Int => |i| if (i.bits <= 8)
                try writer.writeByte(value)
            else
                try writer.writeInt(T, value, .big),
            .Union => switch (value) {
                inline else => |v, t| {
                    try formatLow(t, writer);
                    try formatLow(v, writer);
                },
            },
            .Enum => try formatLow(@intFromEnum(value), writer),
            .Struct => |s| inline for (s.fields) |field| {
                try formatLow(@field(value, field.name), writer);
            },
            inline else => |_, t| @compileError("unhabdled message type: " ++
                @tagName(t) ++ " (" ++ @typeName(T) ++ ")"),
        };
    }

    pub fn parseFromBytes(comptime T: type, bytes: []const u8) ParseError!T {
        var reader = std.io.fixedBufferStream(bytes);
        return parse(T, reader.reader());
    }

    pub fn parse(comptime T: type, reader: Reader) ParseError!T {
        return parseLow(T, reader);
    }

    inline fn parseLow(comptime T: type, reader: Reader) ParseError!T {
        if (T == []const u8) {
            const len = try parseLow(usize, reader);
            if (reader.context.pos +| len > reader.context.buffer.len) return error.EndOfStream;
            const slice = reader.context.buffer[reader.context.pos..][0..len];
            reader.context.pos += len;
            return slice;
        }

        return switch (@typeInfo(T)) {
            .Int => |i| if (i.bits <= 8)
                std.math.cast(T, try reader.readByte()) orelse error.Overflow
            else
                try reader.readInt(T, .big),
            .Union => |u| {
                const tag_type = u.tag_type orelse
                    @compileError("tagless unions are impossible to handle: " ++ @typeName(T));
                return switch (try parseLow(tag_type, reader)) {
                    inline else => |t| @unionInit(T, @tagName(t), try parseLow(std.meta.TagPayload(T, t), reader)),
                };
            },
            .Enum => |e| {
                inline for (e.fields, 0..) |field, i| {
                    if (field.value != i) @compileError("Only continuous enums are supported: " ++ @typeName(T));
                }
                const repr = try parseLow(e.tag_type, reader);
                if (repr >= e.fields.len) return error.Overflow;
                return @enumFromInt(repr);
            },
            .Struct => |s| {
                var ret: T = undefined;
                inline for (s.fields) |field| {
                    @field(ret, field.name) = try parseLow(field.type, reader);
                }
                return ret;
            },
            inline else => |_, t| @compileError("unhabdled message type: " ++
                @tagName(t) ++ " (" ++ @typeName(T) ++ ")"),
        };
    }
};

var config = Config{};
var global_allocator = std.heap.GeneralPurposeAllocator(.{}){};
var db: sql.Db = undefined;
const MessageSchema = Schema(struct {
    pub const tables =
        \\CREATE TABLE IF NOT EXISTS messages (
        \\    channel INTEGER NOT NULL,
        \\    timestamp INTEGER NOT NULL,
        \\    author BLOB NOT NULL,
        \\    content BLOB NOT NULL,
        \\    FOREIGN KEY (author) REFERENCES users(pk) ON DELETE CASCADE
        \\);
        \\CREATE INDEX IF NOT EXISTS message_ordering ON
        \\    messages (channel, timestamp DESC);
    ;
    pub const queries = struct {
        pub const get_after =
            \\SELECT
            \\    rowid, channel, timestamp, author,
            \\    users.name as author_name, content
            \\FROM
            \\    messages INNER JOIN users ON users.pk = author
            \\WHERE
            \\    channel = ? AND timestamp > ?
            \\ORDER BY
            \\    timestamp ASC
        ;
        pub const get_before =
            \\SELECT
            \\    rowid, channel, timestamp, author,
            \\    users.name as author_name, content
            \\FROM
            \\    messages INNER JOIN users ON users.pk = author
            \\WHERE
            \\    channel = ? AND timestamp < ?
        ;
        pub const create =
            \\INSERT INTO messages(channel, timestamp, author, content)
            \\VALUES (?, ?, ?, ?)
        ;
    };
});
const UserSchema = Schema(struct {
    pub const tables =
        \\CREATE TABLE IF NOT EXISTS users (
        \\    pk BLOB PRIMARY KEY,
        \\    name TEXT NOT NULL UNIQUE,
        \\    nonce INTEGER NOT NULL DEFAULT 0,
        \\    roles INTEGER NOT NULL DEFAULT 0
        \\) WITHOUT ROWID;
        \\
        \\CREATE TABLE IF NOT EXISTS invited (
        \\    pk BLOB PRIMARY KEY,
        \\    roles INTEGER NOT NULL DEFAULT 0
        \\) WITHOUT ROWID;
    ;
    pub const queries = struct {
        pub const eat_token =
            \\DELETE FROM invited WHERE pk = ? RETURNING roles
        ;
        pub const create =
            \\INSERT INTO users (pk, name, roles) VALUES (?, ?, ?)
        ;
        pub const get =
            \\SELECT name, roles FROM users WHERE pk = ?
        ;
        pub const bump_nonce =
            \\UPDATE users SET nonce = ?nonce
            \\  WHERE pk = ? AND nonce < ?nonce + 600 RETURNING name, roles
        ;
        pub const invite =
            \\INSERT INTO invited (pk, roles) SELECT * FROM (VALUES (?1, ?2))
            \\  WHERE NOT EXISTS (SELECT 1 FROM users WHERE pk = ?1)
        ;
    };
});

fn Schema(comptime Blueptint: type) type {
    const tinfo = @typeInfo(Blueptint.queries).Struct;
    const Queries = b: {
        var fields: [tinfo.decls.len]std.builtin.Type.StructField = undefined;
        for (&fields, tinfo.decls) |*res, decl| {
            const Ty = sql.StatementType(.{}, @field(Blueptint.queries, decl.name));
            res.* = .{
                .type = Ty,
                .alignment = @alignOf(Ty),
                .name = decl.name,
                .is_comptime = false,
                .default_value = null,
            };
        }

        break :b @Type(.{ .Struct = .{
            .layout = .auto,
            .fields = &fields,
            .decls = &.{},
            .is_tuple = false,
        } });
    };

    return struct {
        var queries: Queries = undefined;

        fn init() !void {
            inline for (tinfo.decls) |decl| {
                @field(queries, decl.name) = try db.prepare(@field(Blueptint.queries, decl.name));
            }
        }
    };
}

fn on_request(r: zap.Request) void {
    r.setStatus(.not_found);
}

const max_username_len = 32;
const Username = std.BoundedArray(u8, max_username_len);
const UserPk = [Ecdsa.PublicKey.compressed_sec1_encoded_length]u8;

const AuthParams = extern struct {
    nonce: [8]u8,
    pk: UserPk,
    sig: [Ecdsa.Signature.encoded_length]u8,

    /// returns nonce
    fn isValid(self: AuthParams, server_name: []const u8) !i64 {
        // NOTE: we wont validate the nonce against timestamp, bad idea

        var buff: [8 + 64]u8 = undefined;
        buff[0..8].* = self.nonce;
        @memcpy(buff[8..][0..server_name.len], server_name);

        const pk = try Ecdsa.PublicKey.fromSec1(&self.pk);
        try Ecdsa.Signature.fromBytes(self.sig).verify(buff[0 .. 8 + server_name.len], pk);

        return std.mem.readInt(i64, &self.nonce, .little);
    }
};

const User = struct {
    name: Username,
    pk: UserPk,
    roles: Roles,

    const Roles = u64;

    fn auth(pk: UserPk, nonce: i64) !User {
        var name = Username.init(max_username_len) catch unreachable;

        var allc = std.heap.FixedBufferAllocator.init(name.slice());
        const name_slice, const roles = try UserSchema.queries.bump_nonce.oneAlloc(
            struct { []const u8, i64 },
            allc.allocator(),
            .{},
            .{ nonce, pk },
        ) orelse return error.UserNotFound;
        std.debug.assert(name_slice.ptr == name.slice().ptr);

        name.len = @intCast(allc.end_index);
        return .{ .name = name, .pk = pk, .roles = @bitCast(roles) };
    }
};

const WsContext = struct {
    user: User,
    settings: ws.WebSocketSettings,
    message_sub: ws.SubscribeArgs,

    const Msg = union(enum) {
        Message: struct {
            channel: u32,
            content: []const u8,
        },
    };

    fn create(gpa: std.mem.Allocator, user: User) !*WsContext {
        const self = try gpa.create(WsContext);
        self.* = .{
            .user = user,
            .settings = .{
                .on_open = on_open,
                .on_message = on_message,
                .on_close = on_close,
                .context = self,
            },
            .message_sub = .{
                .channel = "messages",
                .force_binary = true,
                .context = self,
            },
        };
        return self;
    }

    fn on_open(context: ?*WsContext, handle: WsHandle) void {
        const self = context orelse return weAreNull();

        const id = ws.subscribe(handle, &self.message_sub) catch |err| {
            std.log.warn("unable to subscribe to message channel: {!}", .{err});
            ws.close(handle);
            return;
        };

        if (id == 0) {
            std.log.warn("unable to subscribe to message channel", .{});
            ws.close(handle);
            return;
        }
    }

    fn on_message(context: ?*WsContext, handle: WsHandle, message: []const u8, is_text: bool) void {
        onMessage(context orelse return weAreNull(), handle, message, is_text) catch {};
    }

    fn onMessage(self: *WsContext, handle: WsHandle, message: []const u8, is_text: bool) !void {
        errdefer |e| {
            std.log.warn("on message error: {!}", .{e});
            ws.close(handle);
        }

        if (is_text) return error.ExpectedTextMessage;
        const msg = codec.parseFromBytes(Msg, message) catch return error.InvalidMessage;

        switch (msg) {
            .Message => |m| {
                try MessageSchema.queries.create.exec(.{}, .{
                    m.channel,
                    self.user.name.slice(),
                    std.time.timestamp(),
                    m.content,
                });
            },
        }
    }

    fn on_close(context: ?*WsContext, uuid: isize) void {
        _ = context;
        _ = uuid;
    }

    fn weAreNull() void {
        std.log.err("wy the fuck we are null now", .{});
    }
};

const Message = struct {};

fn on_upgrade(r: zap.Request, protocol: []const u8) void {
    if (!std.mem.eql(u8, protocol, "websocket")) {
        std.log.warn("received illegal protocol: {s}", .{protocol});
        r.setStatus(.not_acceptable);
        return;
    }

    const path = r.path orelse {
        std.log.warn("received request without path", .{});
        r.setStatus(.bad_request);
        return;
    };

    if (path.len != 1 + @sizeOf(AuthParams) * 2) {
        std.log.warn("receiver incorrect auth format of path: {s}", .{path});
        r.setStatus(.bad_request);
        return;
    }

    var auth_params_bytes: [@sizeOf(AuthParams)]u8 = undefined;
    _ = std.fmt.hexToBytes(&auth_params_bytes, path[1..]) catch |err| {
        std.log.warn("invalid auth params hex encoding: {s} {s}", .{ path[1..], @errorName(err) });
        r.setStatus(.bad_request);
        return;
    };
    const auth_params: AuthParams = @bitCast(auth_params_bytes);

    const nonce = auth_params.isValid(config.net.domain) catch |err| {
        std.log.warn("invalid authentication params: {!}", .{err});
        r.setStatus(.forbidden);
        return;
    };

    const user = User.auth(auth_params.pk, nonce) catch |err| {
        std.log.warn("user failed to authorize: {!}", .{err});
        r.setStatus(.unauthorized);
        return;
    };

    const ctx = WsContext.create(global_allocator.allocator(), user) catch |err| {
        std.log.warn("failed to create ws session: {s}", .{@errorName(err)});
        r.setStatus(.internal_server_error);
        return;
    };

    ws.upgrade(r.h, &ctx.settings) catch |err| {
        std.log.warn("websocket upgrade failed: {s}", .{@errorName(err)});
        r.setStatus(.internal_server_error);
        return;
    };
}

fn getFlag(gpa: std.mem.Allocator, name: []const u8) !?[]const u8 {
    return std.process.getEnvVarOwned(gpa, name) catch |e| switch (e) {
        error.EnvironmentVariableNotFound => return null,
        else => return e,
    };
}

const Config = struct {
    net: struct {
        port: u16 = 8080,
        log: bool = false,
        domain: []const u8 = "localhost",
        turn: []const u8 = "turn.localhost",
        public_dir: ?[]const u8 = null,
    } = .{},
    tls: if (opt.tls) struct {
        cert_path: []const u8 = "ssc/cert.pem",
        key_path: []const u8 = "ssc/key.pem",
    } else void = if (opt.tls) .{},
    roots: [][@sizeOf(UserPk) * 2]u8 = &.{},
    channels: []Channel = &.{},
    roles: []Role = &.{},

    const Role = struct {
        id: Id,
        name: []const u8,
        color: []const u8 = "#FFFFFF",
        voice_tier: ?u7 = null,
        perms: packed struct(u8) {
            manage_users: bool = false,
            _: u7 = undefined,
        } = .{},

        const Id = u6;
    };

    const Channel = struct {
        id: Id,
        name: []const u8,
        group: []const u8 = "",
        role_perms: []Permissions = &.{},
        default_perms: Permissions = .{},
        voice: ?VoiceChannel = null,

        const Id = u32;
        const Permissions = struct {
            id: Role.Id = 0,
            flags: packed struct(u8) {
                view: bool = true,
                write: bool = true,
                moderate: bool = false,
                manage: bool = false,
                _: u4 = undefined,
            } = .{},
            action_rate_limit: u64 = 0,
            max_message_length: u16 = 1024 * 4,
        };
    };

    const VoiceChannel = struct {
        max_participants: u16,
    };
};

pub fn main() !void {
    var gen = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        std.debug.assert(!gen.detectLeaks());
        std.debug.assert(gen.deinit() == .ok);
    }
    const gpa = gen.allocator();

    db = try sql.Db.init(.{ .mode = .{ .File = "db.db3" } });

    var tmp = std.heap.ArenaAllocator.init(gpa);
    defer tmp.deinit();
    const arena = tmp.allocator();
    const config_path = try getFlag(arena, "MPQCHT_CONFIG_PATH") orelse "mpqcht-config.json";
    const config_file = try std.fs.cwd().readFileAlloc(arena, config_path, 1024 * 100);
    const cfg = .{ .allocate = .alloc_if_needed };
    config = try std.json.parseFromSliceLeaky(Config, arena, config_file, cfg);
    std.log.info("loaded config from: {s}", .{config_path});

    var router = zap.Router.init(gpa, .{});
    defer router.deinit();

    var listener = zap.HttpListener.init(.{
        .port = config.net.port,
        .on_request = on_request,
        .on_upgrade = on_upgrade,
        .log = config.net.log,
    });
    try listener.listen();

    if (opt.tls) _ = try zap.Tls.init(.{
        .private_key_file = try arena.dupeZ(u8, config.tls.key_path),
        .public_certificate_file = try arena.dupeZ(u8, config.tls.cert_path),
    });

    std.log.info("Listening on 0.0.0.0:{d}\n", .{config.net.port});

    // start worker threads
    zap.start(.{ .threads = 2, .workers = 2 });
}
