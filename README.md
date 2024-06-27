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
- [ ] Markdown in messages
- [ ] Custom CSS/theming (I am unable to make beautiful things)
- [ ] Server currency (monetization of whatever you want) (sweet dreams)
- [ ] E2EE P2P conversations (would be kind of cool)
- [ ] PoW DDOS protection (because its cool)
- [ ] Load balancing (splitting the community into multiple servers) (I would love to actually need this)
- [ ] What people demand (probably wont happen since nobody will use this)
