# mpqcht

Mpqcht is an attempt to make a chat server with minimal system requirements, complemented with performant web client that integrates multiple servers into single website. Every user has a unique identify based on crypto signature (ECDSA due to browser support), which allows authentication architecture similar to SSH server with allowed keys.

The goal is to allow people to host curated online communities with minimal resources.

## Features

Ordered by priority

- [ ] Unencrypted channels (message threads with assigned name)
    - [ ] Voice channels
    - [ ] Dynamic channels (channels under messages)
    - [ ] Advanced content search (with access control)
- [ ] Access control and rate-limiting trough roles
- [ ] App configuration backups (server list, theming)
- [x] Markdown in messages
- [ ] Custom CSS/theming (I am unable to make beautiful things)
- [ ] Server currency (monetization of whatever you want) (sweet dreams)
- [ ] E2EE P2P conversations (would be kind of cool)
- [ ] PoW DDOS protection (because its cool)
- [ ] Load balancing (splitting the community into multiple servers) (I would love to actually need this)
- [ ] What people demand (probably wont happen since nobody will use this)

## Building The App

### Pre-requirements

- Zig (`zig` command)
- Rust toolchain (`cargo` command)
 - `cargo-zigbuild` (`cargo install cargo-zigbuild`)

### Optional Dependencies

- Rust target `x86_64-unknown-linux-musl` target (`rustup target add x86_64-unknown-linux-musl`) to ensure self contained build
- WasmOpt (`wasm-opt` command)
- Gzip (`gzip` command)

### Build Commands

```sh
# in the repo root with all optional dependencies
zig build -Dserver-target=x86_64-unknown-linux-musl
# without any optional dependencies
zig build -Dno-gzip -Dno-wasm-opt
```

All app assets will be in `/zig-out/`, that includes frontend and server.

## Tested Deployment

1. Rent a droplet on digital ocean (If you find more cost efficient solution, let me know)
2. Assuming you have public/private ssh auth:
```sh
scp -r zig-out/* root@<server-ip>:/root/
```
3. (optional) If you did not use `-Dno-voice` during build, server will expect `TURN_SECRET` variable and I ll also need a turn server setup, here is an example config for coturn:
```
# /etc/turnserver.conf

listening-port=3478
tls-listening-port=5349
fingerprint
use-auth-secret
server-name=trun.<SERVER_DOMAIN>
realm=turn.<SERVER_DOMAIN>
static-auth-secret=<$TURN_SECRET>

# TODO: make example for the server supported realms
total-quota=100
stale-nonce=600

# assuming you use certbot
cert=/etc/letsencrypt/live/turn.<SERVER_DOMAIN>/cert.pem
pkey=/etc/letsencrypt/live/turn.<SERVER_DOMAIN>/privkey.pem
cipher-list="ECDHE-RSA-AES256-GCM-SHA512:DHE-RSA-AES256-GCM-SHA512:ECDHE-RSA-AES256-GCM-SHA384:DHE-RSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-SHA384"
```
4. Create server configuration that server will use to enforce the structure of the community. Example configuration is in `client/config.toml`
