const std = @import("std");
const native_endian = @import("builtin").target.cpu.arch.endian();

const Ecdsa = @import("modified-ecdsa.zig").EcdsaP256Sha256;

const globs = struct {
    pub export var username: [32]u8 = undefined;
    pub export var username_len: usize = undefined;
    pub export var password: [256]u8 = undefined;
    pub export var password_len: usize = undefined;

    pub export var vault: [32]u8 = undefined;

    pub export var ecdsa_vkey: [Ecdsa.PublicKey.uncompressed_sec1_encoded_length]u8 = undefined;
    pub export var ecdsa_skey: [Ecdsa.SecretKey.encoded_length]u8 = undefined;
};

export fn derive_keys() i32 {
    const username_str = globs.username[0..globs.username_len];
    const password_str = globs.password[0..globs.password_len];

    const key = std.crypto.kdf.hkdf.HkdfSha256.extract(username_str, password_str);
    var rng = std.rand.ChaCha.init(key);

    {
        rng.fill(&globs.vault);
    }

    {
        var ecdsa_seed: [Ecdsa.KeyPair.seed_length]u8 = undefined;
        rng.fill(&ecdsa_seed);
        const keypair = Ecdsa.KeyPair.create(ecdsa_seed) catch return 1;
        globs.ecdsa_vkey = keypair.public_key.toUncompressedSec1();
        globs.ecdsa_skey = keypair.secret_key.toBytes();
    }

    return 0;
}

export fn clear_secrets() void {
    comptime var size: usize = 0;
    inline for (@typeInfo(globs).Struct.decls) |decl| {
        size += @sizeOf(@TypeOf(@field(globs, decl.name)));
    }
    @memset(@as([*]u8, @ptrCast(&@field(globs, @typeInfo(globs).Struct.decls[0].name)))[0..size], 0);
}

export fn __multi3(a: i128, b: i128) i128 {
    return mulX(i128, a, b);
}

export fn memset(ptr: [*]u8, value: usize, len: usize) [*]u8 {
    for (ptr[0..len]) |*p| p.* = @intCast(value);
    return ptr;
}

export fn memcpy(dest: [*]u8, src: [*]const u8, len: usize) [*]u8 {
    for (dest[0..len], src[0..len]) |*d, s| d.* = s;
    return dest;
}

inline fn mulX(comptime T: type, a: T, b: T) T {
    const word_t = HalveInt(T, false);
    const x = word_t{ .all = a };
    const y = word_t{ .all = b };
    var r = switch (T) {
        i64, i128 => word_t{ .all = muldXi(word_t.HalfT, x.s.low, y.s.low) },
        else => unreachable,
    };
    r.s.high +%= x.s.high *% y.s.low +% x.s.low *% y.s.high;
    return r.all;
}

pub fn HalveInt(comptime T: type, comptime signed_half: bool) type {
    return extern union {
        pub const bits = @divExact(@typeInfo(T).Int.bits, 2);
        pub const HalfTU = std.meta.Int(.unsigned, bits);
        pub const HalfTS = std.meta.Int(.signed, bits);
        pub const HalfT = if (signed_half) HalfTS else HalfTU;

        all: T,
        s: if (native_endian == .little)
            extern struct { low: HalfT, high: HalfT }
        else
            extern struct { high: HalfT, low: HalfT },
    };
}

fn DoubleInt(comptime T: type) type {
    return switch (T) {
        u32 => i64,
        u64 => i128,
        i32 => i64,
        i64 => i128,
        else => unreachable,
    };
}

fn muldXi(comptime T: type, a: T, b: T) DoubleInt(T) {
    const DT = DoubleInt(T);
    const word_t = HalveInt(DT, false);
    const bits_in_word_2 = @sizeOf(T) * 8 / 2;
    const lower_mask = (~@as(T, 0)) >> bits_in_word_2;

    var r: word_t = undefined;
    r.s.low = (a & lower_mask) *% (b & lower_mask);
    var t: T = r.s.low >> bits_in_word_2;
    r.s.low &= lower_mask;
    t += (a >> bits_in_word_2) *% (b & lower_mask);
    r.s.low +%= (t & lower_mask) << bits_in_word_2;
    r.s.high = t >> bits_in_word_2;
    t = r.s.low >> bits_in_word_2;
    r.s.low &= lower_mask;
    t +%= (b >> bits_in_word_2) *% (a & lower_mask);
    r.s.low +%= (t & lower_mask) << bits_in_word_2;
    r.s.high +%= t >> bits_in_word_2;
    r.s.high +%= (a >> bits_in_word_2) *% (b >> bits_in_word_2);
    return r.all;
}
