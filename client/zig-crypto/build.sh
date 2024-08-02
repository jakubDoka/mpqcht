zig build-exe crypto.zig -target wasm32-freestanding -fno-entry -OReleaseSmall\
	--export=derive_keys\
	--export=clear_secrets\
	--export=username\
	--export=username_len\
	--export=password\
	--export=password_len\
	--export=vault\
	--export=ecdsa_vkey\
	--export=ecdsa_pkey
wasm-opt -Oz -o ../public/crypto.wasm crypto.wasm
mv crypto.wasm ../public
find ../public/ | xargs -I{} sh -c 'gzip -q {} --stdout > ../public-gz/$(basename {}).gz'
