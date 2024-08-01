#!/bin/bash

agree() {
	read -p "  $1 [y/N] " -n 1 -r && echo
	[[ $REPLY =~ ^[Yy]$ ]] && return 0 || return 1
}

echo "Do you want:"
agree "server to handle TLS?" && export TLS="tls"
agree "server to assume static assets are gzipped" && export GZIP="gzip"
agree "to priortize binary-size over performance" && export PROFILE="minimal-size" || export PROFILE="release"

cargo build -p server --profile=$PROFILE --features $TLS,$GZIP || exit 1

BIN_PATH="$(pwd)/target/$PROFILE/server"

echo "Compilation finished:"
echo "  path: $BIN_PATH"
echo "  binary-size: $(cat $BIN_PATH | wc -c)b"
