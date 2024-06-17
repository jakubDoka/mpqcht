use std::str::FromStr;

use bip39::rand::{RngCore, SeedableRng};
//use pqc_kyber::{CryptoRng, RngCore};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
/// @throws {string}
pub fn mnemonic() -> Result<String, Error> {
    Ok(bip39::Mnemonic::generate(24)?.to_string())
}

#[wasm_bindgen(getter_with_clone)]
pub struct UserKeys {
    pub vault: Vec<u8>,
    pub vkey: Vec<u8>,
    pub skey: Vec<u8>,
}

#[wasm_bindgen]
pub fn derive_keys(mnemonic: String) -> Result<UserKeys, Error> {
    let mnemonic = bip39::Mnemonic::from_str(&mnemonic)?;
    let seed = mnemonic.to_seed("torsion");
    let mut rng =
        rand_chacha::ChaChaRng::from_seed(std::array::from_fn(|i| seed[i] ^ seed[i + 32]));

    let mut vault = [0u8; 32];
    rng.fill_bytes(&mut vault);
    let skey = p256::ecdsa::SigningKey::random(&mut rng);
    let vkey = p256::ecdsa::VerifyingKey::from(&skey);

    Ok(UserKeys {
        vault: vault.to_vec(),
        vkey: vkey.to_encoded_point(true).as_bytes().to_vec(),
        skey: skey.to_bytes().to_vec(),
    })
}

#[derive(Debug)]
pub struct Error(JsValue);

impl From<Error> for JsValue {
    fn from(e: Error) -> Self {
        e.0
    }
}

impl<E: std::error::Error> From<E> for Error {
    fn from(value: E) -> Self {
        Error(JsValue::from_str(&value.to_string()))
    }
}
