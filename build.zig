const std = @import("std");

pub fn build(b: *std.Build) void {
    var static_buf: [256]u8 = undefined;
    var buf = std.ArrayListUnmanaged(u8){
        .items = static_buf[0..0],
        .capacity = static_buf.len,
    };

    const no_gzip = b.option(bool, "no-gzip", "gzip the public files and build server for gzipped content") orelse false;
    const no_tls = b.option(bool, "no-tls", "build server with builting TLS") orelse false;
    const no_voice = b.option(bool, "no-voice", "build server with voice support") orelse false;
    const debug = b.option(bool, "debug", "build debug binary") orelse false;
    const no_wasm_opt = b.option(bool, "no-wasm-opt", "disable wasm-opt") orelse false;
    const target = b.option([]const u8, "server-target", "target triple of the server");

    const optimize = b.standardOptimizeOption(.{});
    const zig_target = b.standardTargetOptions(.{});

    // TODO: we can reduce the sqlite library size by ommiting some features we dont need
    const sqlite = b.dependency("sqlite", .{
        .target = zig_target,
        .optimize = optimize,
    });
    const zap = b.dependency("zap", .{
        .target = zig_target,
        .optimize = optimize,
        .openssl = !no_tls, // set to true to enable TLS support
    });

    buf.items.len = 0;
    buf.fixedWriter().print("{}", .{b.cache_root}) catch unreachable;

    const crypto_wasm = b.addExecutable(.{
        .name = "lib",
        .root_source_file = b.path("client/crypto.zig"),
        .target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .freestanding,
        }),
        .optimize = .ReleaseSmall,
    });
    crypto_wasm.entry = .disabled;
    crypto_wasm.root_module.export_symbol_names = &.{
        "derive_keys",
        "password",
        "password_len",
        "username",
        "username_len",
        "vault",
        "ecdsa_vkey",
        "ecdsa_skey",
        "clear_secrets",
    };

    var wasm_path = crypto_wasm.getEmittedBin();
    if (!no_wasm_opt) {
        const wasm_opt = b.addSystemCommand(&.{ "wasm-opt", "-Oz" });
        wasm_opt.addFileArg(wasm_path);
        wasm_opt.addArg("-o");
        wasm_path = wasm_opt.addOutputFileArg("crypto.wasm");
    }

    const pub_files = [_][]const u8{
        "index.html",
        "index.js",
        "index.css",
        "crypto.wasm",
    };
    const pub_out_dir = "public/";
    const crypto_file = pub_out_dir ++ "crypto.wasm";
    const pub_dir = "client/public/";

    var pub_file_install_steps: [pub_files.len]*std.Build.Step.InstallFile = undefined;

    inline for (
        pub_files[0 .. pub_files.len - 1],
        pub_file_install_steps[0 .. pub_files.len - 1],
    ) |file, *install_file| {
        install_file.* = b.addInstallFile(b.path(pub_dir ++ file), "public/" ++ file);
    }
    pub_file_install_steps[pub_files.len - 1] =
        b.addInstallFile(wasm_path, crypto_file);

    if (!no_gzip) {
        const gzip_command = b.addSystemCommand(&.{ "gzip", "-f" });
        b.getInstallStep().dependOn(&gzip_command.step);
        inline for (pub_file_install_steps, pub_files) |install_file, file| {
            gzip_command.step.dependOn(&install_file.step);
            gzip_command.addArg(b.getInstallPath(.prefix, pub_out_dir ++ file));
        }
    } else {
        for (pub_file_install_steps) |install_file|
            b.getInstallStep().dependOn(&install_file.step);
    }

    const cargo = b.addSystemCommand(&.{ "cargo", "zigbuild", "-p", "server" });
    if (!debug) cargo.addArg("--release");
    if (target) |t| cargo.addArgs(&.{ "--target", t });

    buf.items.len = 0;
    if (!no_gzip) buf.appendSliceAssumeCapacity(",gzip");
    if (!no_voice) buf.appendSliceAssumeCapacity(",voice");
    if (!no_tls) buf.appendSliceAssumeCapacity(",tls");
    if (buf.items.len > 0) cargo.addArgs(&.{ "--features", buf.items });

    const profile = if (debug) "debug" else "release";
    const target_seg = target orelse "";
    const install_server = b.addInstallFile(
        b.path(b.pathJoin(&.{ "target", target_seg, profile, "server" })),
        "server",
    );
    install_server.step.dependOn(&cargo.step);

    const server_options = b.addOptions();
    server_options.addOption(bool, "tls", !no_tls);

    const server = b.addExecutable(.{
        .name = "zig-server",
        .root_source_file = b.path("zig-server/main.zig"),
        .target = zig_target,
        .optimize = optimize,
    });
    server.root_module.addImport("zap", zap.module("zap"));
    server.root_module.addImport("sqlite", sqlite.module("sqlite"));
    server.root_module.addOptions("options", server_options);

    b.installArtifact(server);
    b.getInstallStep().dependOn(&install_server.step);

    const test_step = b.step("test", "stub for now");
    test_step.dependOn(b.getInstallStep());
}
