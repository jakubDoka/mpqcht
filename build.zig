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

    // const crypto_wasm = b.addExecutable(.{
    //     .name = "crypto",
    //     .root_source_file = b.path("client/crypto.zig"),
    //     .target = b.resolveTargetQuery(.{
    //         .cpu_arch = .wasm32,
    //         .os_tag = .freestanding,
    //     }),
    //     .optimize = .ReleaseSmall,
    // });

    buf.items.len = 0;
    buf.fixedWriter().print("{}", .{b.cache_root}) catch unreachable;

    const crypto_wasm = b.addSystemCommand(&.{
        "zig",
        "build-exe",
        "--cache-dir",
        buf.items,
        "-target",
        "wasm32-freestanding",
        "-fno-entry",
        "-OReleaseSmall",
        "--export=derive_keys",
        "--export=clear_secrets",
        "--export=username",
        "--export=username_len",
        "--export=password",
        "--export=password_len",
        "--export=vault",
        "--export=ecdsa_vkey",
        "--export=ecdsa_skey",
    });

    crypto_wasm.addFileArg(b.path("client/crypto.zig"));
    var wasm_path = crypto_wasm.addPrefixedOutputFileArg("-femit-bin=", "crypto.wasm");

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
    const target_seg = if (target) |t| t else "";
    const install_server = b.addInstallFile(
        b.path(b.pathJoin(&.{ "target", target_seg, profile, "server" })),
        "server",
    );
    install_server.step.dependOn(&cargo.step);

    b.getInstallStep().dependOn(&install_server.step);
}
