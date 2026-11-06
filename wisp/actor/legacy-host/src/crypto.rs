//! The `wisp-crypto` interface: sha256, ed25519 verification, and signing with
//! keys only the daemon holds; Web Push (VAPID and RFC 8291), sealed boxes
//! (X25519), scrypt password hashes, HMAC, and random bytes. A key is a file in the configured
//! directory: `<name>.key` an ed25519 seed as hex, `<name>.p256` a P-256 key as
//! a PKCS#8 PEM, `<name>.x25519` an X25519 secret as hex. Each call reads the
//! file it needs, so nothing is cached, and no function returns a private key:
//! sessions name keys.
use aes_gcm::Aes128Gcm;
use anyhow::Result;
use base64::Engine;
use base64::alphabet::URL_SAFE;
use base64::engine::{DecodePaddingMode, GeneralPurpose, GeneralPurposeConfig};
use chacha20poly1305::ChaCha20Poly1305;
use chacha20poly1305::aead::{Aead, KeyInit};
use ed25519_dalek::{Signature, Signer, SigningKey, VerifyingKey};
use hkdf::Hkdf;
use hmac::{Hmac, Mac};
use p256::elliptic_curve::sec1::ToEncodedPoint;
use p256::pkcs8::{DecodePrivateKey, EncodePrivateKey, LineEnding};
use sha1::Sha1;
use sha2::{Digest, Sha256};
use std::fs::{self, DirBuilder, File, OpenOptions};
use std::io::{ErrorKind, Write};
use std::os::unix::fs::{DirBuilderExt, OpenOptionsExt};
use std::path::{Path, PathBuf};
use theater::handler::{Handler, HandlerContext};
use theater::pack_bridge::{HostImports, InterfaceImpl, Value, ValueType, host_fn, parse_pact};

/// Where the signing keys live. With no directory the key verbs return an error;
/// `sha256` and `verify` need none.
#[derive(Clone, Default)]
pub struct KeyAccess {
    pub dir: Option<PathBuf>,
}

fn to_hex(bytes: &[u8]) -> String {
    bytes.iter().map(|byte| format!("{byte:02x}")).collect()
}

fn hex_bytes(text: &str) -> Option<Vec<u8>> {
    let digits = text.as_bytes();
    if !digits.len().is_multiple_of(2) {
        return None;
    }
    digits
        .chunks(2)
        .map(|pair| {
            let high = (pair[0] as char).to_digit(16)?;
            let low = (pair[1] as char).to_digit(16)?;
            Some((high * 16 + low) as u8)
        })
        .collect()
}

fn from_hex<const N: usize>(text: &str) -> Option<[u8; N]> {
    hex_bytes(text)?.try_into().ok()
}

/// base64url as Web Push uses it: written without padding, read with or without.
const BASE64URL: GeneralPurpose = GeneralPurpose::new(
    &URL_SAFE,
    GeneralPurposeConfig::new()
        .with_encode_padding(false)
        .with_decode_padding_mode(DecodePaddingMode::Indifferent),
);

fn fill_random(out: &mut [u8]) -> Result<(), String> {
    getrandom::getrandom(out).map_err(|error| format!("no randomness: {error}"))
}

fn random<const N: usize>() -> Result<[u8; N], String> {
    let mut out = [0; N];
    fill_random(&mut out)?;
    Ok(out)
}

/// The most `random-bytes` gives in one call.
const RANDOM_BYTES_MAX: u32 = 1024;

fn random_bytes(n: u32) -> Result<Vec<u8>, String> {
    if n == 0 || n > RANDOM_BYTES_MAX {
        return Err(format!(
            "random-bytes takes 1 to {RANDOM_BYTES_MAX} bytes, not {n}"
        ));
    }
    let mut out = vec![0; n as usize];
    fill_random(&mut out)?;
    Ok(out)
}

fn hkdf<const N: usize>(salt: &[u8], secret: &[u8], info: &[u8]) -> [u8; N] {
    let mut out = [0; N];
    Hkdf::<Sha256>::new(Some(salt), secret)
        .expand(info, &mut out)
        .expect("an HKDF-SHA256 output this short");
    out
}

fn sha256(data: &[u8]) -> String {
    to_hex(&Sha256::digest(data))
}

// HMAC (RFC 2104) takes a key of any length.
fn hmac_sha1(key: &[u8], data: &[u8]) -> String {
    let mut mac = <Hmac<Sha1> as Mac>::new_from_slice(key).expect("any key length");
    mac.update(data);
    to_hex(&mac.finalize().into_bytes())
}

fn hmac_sha256(key: &[u8], data: &[u8]) -> String {
    let mut mac = <Hmac<Sha256> as Mac>::new_from_slice(key).expect("any key length");
    mac.update(data);
    to_hex(&mac.finalize().into_bytes())
}

/// Strict: a low-order public key, which any signature can satisfy under the
/// lax check, is refused.
fn verify(public_key: &str, signature: &str, data: &[u8]) -> bool {
    let (Some(public_key), Some(signature)) = (from_hex(public_key), from_hex(signature)) else {
        return false;
    };
    VerifyingKey::from_bytes(&public_key).is_ok_and(|key| {
        key.verify_strict(data, &Signature::from_bytes(&signature))
            .is_ok()
    })
}

fn valid_name(name: &str) -> bool {
    (1..=64).contains(&name.len())
        && name
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'.' | b'_' | b'-'))
}

fn public_hex(key: &SigningKey) -> String {
    to_hex(key.verifying_key().as_bytes())
}

/// What a key file holds, which gives its suffix and how errors name it.
#[derive(Clone, Copy)]
enum Kind {
    Ed25519,
    P256,
    X25519,
}

impl Kind {
    fn suffix(self) -> &'static str {
        match self {
            Kind::Ed25519 => "key",
            Kind::P256 => "p256",
            Kind::X25519 => "x25519",
        }
    }

    /// "key", "p256 key", "x25519 key": the ed25519 messages are as they were.
    fn noun(self) -> &'static str {
        match self {
            Kind::Ed25519 => "key",
            Kind::P256 => "p256 key",
            Kind::X25519 => "x25519 key",
        }
    }
}

fn p256_public(key: &p256::SecretKey) -> String {
    BASE64URL.encode(key.public_key().to_encoded_point(false).as_bytes())
}

/// A random 32 bytes is a valid scalar but for about 1 in 2^32 tries.
fn p256_random() -> Result<p256::SecretKey, String> {
    loop {
        if let Ok(key) = p256::SecretKey::from_slice(&random::<32>()?) {
            return Ok(key);
        }
    }
}

fn x25519_public(secret: &x25519_dalek::StaticSecret) -> String {
    to_hex(x25519_dalek::PublicKey::from(secret).as_bytes())
}

impl KeyAccess {
    fn dir(&self) -> Result<&Path, String> {
        self.dir
            .as_deref()
            .ok_or_else(|| "no key directory configured".to_string())
    }

    fn path(&self, kind: Kind, name: &str) -> Result<PathBuf, String> {
        let dir = self.dir()?;
        if !valid_name(name) {
            return Err(format!(
                "bad key name '{name}': use 1-64 of A-Z a-z 0-9 . _ -"
            ));
        }
        Ok(dir.join(format!("{name}.{}", kind.suffix())))
    }

    fn read(&self, kind: Kind, name: &str) -> Result<String, String> {
        let noun = kind.noun();
        fs::read_to_string(self.path(kind, name)?).map_err(|error| match error.kind() {
            ErrorKind::NotFound => format!("no {noun} named '{name}'"),
            _ => format!("reading {noun} '{name}': {error}"),
        })
    }

    fn load(&self, name: &str) -> Result<SigningKey, String> {
        let seed = from_hex(self.read(Kind::Ed25519, name)?.trim())
            .ok_or_else(|| format!("key file for '{name}' is not a 32-byte hex seed"))?;
        Ok(SigningKey::from_bytes(&seed))
    }

    fn load_p256(&self, name: &str) -> Result<p256::SecretKey, String> {
        p256::SecretKey::from_pkcs8_pem(&self.read(Kind::P256, name)?)
            .map_err(|_| format!("key file for '{name}' is not a PKCS#8 PEM P-256 key"))
    }

    fn load_x25519(&self, name: &str) -> Result<x25519_dalek::StaticSecret, String> {
        let secret = from_hex::<32>(self.read(Kind::X25519, name)?.trim())
            .ok_or_else(|| format!("key file for '{name}' is not a 32-byte hex secret"))?;
        Ok(x25519_dalek::StaticSecret::from(secret))
    }

    fn create(&self, name: &str) -> Result<String, String> {
        let seed = random::<32>()?;
        let public = public_hex(&SigningKey::from_bytes(&seed));
        self.store(Kind::Ed25519, name, &to_hex(&seed), public)
    }

    fn create_p256(&self, name: &str) -> Result<String, String> {
        let key = p256_random()?;
        let pem = key
            .to_pkcs8_pem(LineEnding::LF)
            .map_err(|error| format!("encoding p256 key '{name}': {error}"))?;
        self.store(Kind::P256, name, &pem, p256_public(&key))
    }

    fn create_x25519(&self, name: &str) -> Result<String, String> {
        let secret = random::<32>()?;
        let public = x25519_public(&x25519_dalek::StaticSecret::from(secret));
        self.store(Kind::X25519, name, &to_hex(&secret), public)
    }

    /// Whole or not at all: the key is on disk under a temporary name before a
    /// hard link gives it its real one, and the link refuses an existing key, so
    /// a crash can't leave a key file without a key and a race can't replace one.
    /// Returns `public`.
    fn store(
        &self,
        kind: Kind,
        name: &str,
        contents: &str,
        public: String,
    ) -> Result<String, String> {
        let path = self.path(kind, name)?;
        let dir = self.dir()?;
        let noun = kind.noun();
        let exists = || format!("{noun} '{name}' exists");
        if path.exists() {
            return Err(exists());
        }
        let failed = |error: std::io::Error| format!("creating {noun} '{name}': {error}");
        DirBuilder::new()
            .recursive(true)
            .mode(0o700)
            .create(dir)
            .map_err(failed)?;
        // Unique per key, so a crash's leftover never blocks a later create. It
        // doesn't end in a key suffix, so it isn't listed.
        let temporary = dir.join(format!("{public}.tmp"));
        let written = OpenOptions::new()
            .write(true)
            .create_new(true)
            .mode(0o600)
            .open(&temporary)
            .and_then(|mut file| {
                file.write_all(contents.as_bytes())?;
                file.sync_all()
            })
            .and_then(|()| fs::hard_link(&temporary, &path));
        let _ = fs::remove_file(&temporary);
        match written {
            Ok(()) => {}
            Err(error) if error.kind() == ErrorKind::AlreadyExists => return Err(exists()),
            Err(error) => return Err(failed(error)),
        }
        File::open(dir)
            .and_then(|dir| dir.sync_all())
            .map_err(failed)?;
        Ok(public)
    }

    /// The key names, sorted. A directory not made yet just has no keys.
    fn list(&self) -> Result<Vec<String>, String> {
        let entries = match fs::read_dir(self.dir()?) {
            Ok(entries) => entries,
            Err(error) if error.kind() == ErrorKind::NotFound => return Ok(Vec::new()),
            Err(error) => return Err(format!("listing keys: {error}")),
        };
        let mut names = Vec::new();
        for entry in entries {
            let entry = entry.map_err(|error| format!("listing keys: {error}"))?;
            if let Some(name) = entry
                .file_name()
                .to_str()
                .and_then(|f| f.strip_suffix(".key"))
                && valid_name(name)
            {
                names.push(name.to_string());
            }
        }
        names.sort();
        Ok(names)
    }
}

fn string_result(value: Result<String, String>) -> Value {
    result(ValueType::String, value.map(Value::String))
}

fn result(ok_type: ValueType, value: Result<Value, String>) -> Value {
    Value::Result {
        ok_type,
        err_type: ValueType::String,
        value: value
            .map(Box::new)
            .map_err(|error| Box::new(Value::String(error))),
    }
}

fn bytes(value: &Value) -> Option<Vec<u8>> {
    let Value::List { items, .. } = value else {
        return None;
    };
    items
        .iter()
        .map(|item| match item {
            Value::U8(byte) => Some(*byte),
            _ => None,
        })
        .collect()
}

fn name_of(input: &Value) -> Result<&str, String> {
    match input {
        Value::String(name) => Ok(name),
        _ => Err("expected a key name string".to_string()),
    }
}

fn sign(keys: &KeyAccess, input: &Value) -> Result<String, String> {
    let Value::Tuple(args) = input else {
        return Err("sign expects a key name and bytes".to_string());
    };
    let [Value::String(name), data] = args.as_slice() else {
        return Err("sign expects a key name and bytes".to_string());
    };
    let data = bytes(data).ok_or("sign expects a key name and bytes")?;
    Ok(to_hex(&keys.load(name)?.sign(&data).to_bytes()))
}

/// The arguments of a host call: one is passed bare, several as a tuple.
fn args(input: &Value) -> &[Value] {
    match input {
        Value::Tuple(args) => args,
        one => std::slice::from_ref(one),
    }
}

fn bytes_result(value: Result<Vec<u8>, String>) -> Value {
    let list = value.map(|bytes| Value::List {
        elem_type: ValueType::U8,
        items: bytes.into_iter().map(Value::U8).collect(),
    });
    result(ValueType::List(Box::new(ValueType::U8)), list)
}

// --- Web Push ---------------------------------------------------------------

/// An RFC 8292 `Authorization` value: an ES256 JWT for `audience` (the push
/// service's origin) and `subject` (a contact URL), good for 12 hours.
fn vapid_header(key: &p256::SecretKey, audience: &str, subject: &str, now: u64) -> String {
    let head = BASE64URL.encode(br#"{"typ":"JWT","alg":"ES256"}"#);
    let claims = serde_json::json!({ "aud": audience, "exp": now + 12 * 3600, "sub": subject });
    let body = BASE64URL.encode(claims.to_string());
    let signed = format!("{head}.{body}");
    let signature: p256::ecdsa::Signature =
        p256::ecdsa::SigningKey::from(key).sign(signed.as_bytes());
    format!(
        "vapid t={signed}.{}, k={}",
        BASE64URL.encode(signature.to_bytes()),
        p256_public(key)
    )
}

/// The largest payload one record carries within the 4096-byte body every push
/// service takes: 4096 - 86 header - 16 tag - 1 padding delimiter.
const WEBPUSH_MAX_PAYLOAD: usize = 3993;

/// RFC 8291: `payload` as one aes128gcm record (RFC 8188) for the subscription
/// whose keys are `p256dh` (its uncompressed point) and `auth`, from `sender`'s
/// ephemeral key with `salt`.
fn webpush_encrypt(
    p256dh: &[u8],
    auth: &[u8],
    payload: &[u8],
    sender: &p256::SecretKey,
    salt: &[u8; 16],
) -> Result<Vec<u8>, String> {
    let receiver = p256::PublicKey::from_sec1_bytes(p256dh)
        .ok()
        .filter(|_| p256dh.len() == 65)
        .ok_or("p256dh is not an uncompressed P-256 point")?;
    if auth.len() != 16 {
        return Err("auth is not 16 bytes".into());
    }
    if payload.len() > WEBPUSH_MAX_PAYLOAD {
        return Err(format!(
            "payload is {} bytes; at most {WEBPUSH_MAX_PAYLOAD} fit",
            payload.len()
        ));
    }
    let shared = p256::ecdh::diffie_hellman(sender.to_nonzero_scalar(), receiver.as_affine());
    let sender_public = sender.public_key().to_encoded_point(false);
    let key_info = [b"WebPush: info\0", p256dh, sender_public.as_bytes()].concat();
    let ikm = hkdf::<32>(auth, shared.raw_secret_bytes(), &key_info);
    let cek = hkdf::<16>(salt, &ikm, b"Content-Encoding: aes128gcm\0");
    let nonce = hkdf::<12>(salt, &ikm, b"Content-Encoding: nonce\0");
    let record = [payload, &[2]].concat();
    let sealed = Aes128Gcm::new(&cek.into())
        .encrypt(&nonce.into(), record.as_slice())
        .map_err(|_| "aes128gcm failed")?;
    let mut out = Vec::with_capacity(86 + sealed.len());
    out.extend_from_slice(salt);
    out.extend_from_slice(&4096u32.to_be_bytes());
    out.push(65);
    out.extend_from_slice(sender_public.as_bytes());
    out.extend_from_slice(&sealed);
    Ok(out)
}

fn unbase64url(text: &str, what: &str) -> Result<Vec<u8>, String> {
    BASE64URL
        .decode(text)
        .map_err(|_| format!("{what} is not base64url"))
}

// --- sealed boxes -------------------------------------------------------------

/// HKDF's info for a sealed box's key.
const SEAL_INFO: &[u8] = b"wisp sealed box v1";

/// The ChaCha20-Poly1305 key for one box. None when the peer's key is of low
/// order, so the shared secret is zero and anyone could compute it.
fn seal_key(
    secret: &x25519_dalek::StaticSecret,
    peer: &[u8; 32],
    ephemeral: &[u8; 32],
    recipient: &[u8; 32],
) -> Option<[u8; 32]> {
    let shared = secret.diffie_hellman(&x25519_dalek::PublicKey::from(*peer));
    shared.was_contributory().then(|| {
        hkdf::<32>(
            &[*ephemeral, *recipient].concat(),
            shared.as_bytes(),
            SEAL_INFO,
        )
    })
}

/// ephemeral public (32) || ChaCha20-Poly1305(plaintext) with its 16-byte tag.
/// The nonce is zero: the key is used for this box only.
fn seal(
    recipient: &[u8; 32],
    plaintext: &[u8],
    ephemeral: &x25519_dalek::StaticSecret,
) -> Result<Vec<u8>, String> {
    let ephemeral_public = x25519_dalek::PublicKey::from(ephemeral).to_bytes();
    let key = seal_key(ephemeral, recipient, &ephemeral_public, recipient)
        .ok_or("recipient key is of low order")?;
    let sealed = ChaCha20Poly1305::new(&key.into())
        .encrypt(&[0; 12].into(), plaintext)
        .map_err(|_| "chacha20poly1305 failed")?;
    Ok([&ephemeral_public[..], &sealed].concat())
}

fn open(secret: &x25519_dalek::StaticSecret, sealed: &[u8]) -> Option<Vec<u8>> {
    let (ephemeral, sealed) = sealed.split_first_chunk::<32>()?;
    let recipient = x25519_dalek::PublicKey::from(secret).to_bytes();
    let key = seal_key(secret, ephemeral, ephemeral, &recipient)?;
    ChaCha20Poly1305::new(&key.into())
        .decrypt(&[0; 12].into(), sealed)
        .ok()
}

// --- scrypt -------------------------------------------------------------------

/// auth-helper's parameters: N = 2^15, r = 8, p = 1, a 32-byte hash.
const SCRYPT_LOG_N: u8 = 15;
const SCRYPT_R: u32 = 8;
const SCRYPT_P: u32 = 1;

/// The parameters of a stored line, or None for one that is malformed or would
/// cost too much: N above 2^17, p above 16, or more than 256 MiB (128 * r * N).
fn scrypt_params(n: u64, r: u32, p: u32, len: usize) -> Option<scrypt::Params> {
    let fits = n.is_power_of_two()
        && (2..=1 << 17).contains(&n)
        && (1..=16).contains(&p)
        && r >= 1
        && 128 * u64::from(r) * n <= 256 << 20
        && (16..=64).contains(&len);
    fits.then(|| scrypt::Params::new(n.trailing_zeros() as u8, r, p, len).ok())
        .flatten()
}

fn scrypt_derive(password: &[u8], salt: &[u8], params: &scrypt::Params, len: usize) -> Vec<u8> {
    let mut out = vec![0; len];
    scrypt::scrypt(password, salt, params, &mut out).expect("output length is in range");
    out
}

/// A line as auth-helper writes it: scrypt$N$r$p$salt-hex$hash-hex.
fn scrypt_hash(password: &[u8]) -> Result<String, String> {
    let salt = random::<16>()?;
    let params = scrypt::Params::new(SCRYPT_LOG_N, SCRYPT_R, SCRYPT_P, 32).expect("valid");
    Ok(format!(
        "scrypt${}${SCRYPT_R}${SCRYPT_P}${}${}",
        1u32 << SCRYPT_LOG_N,
        to_hex(&salt),
        to_hex(&scrypt_derive(password, &salt, &params, 32))
    ))
}

/// A stored line's parameters, salt and hash; None for one that is malformed
/// or too costly.
fn scrypt_line(line: &str) -> Option<(scrypt::Params, Vec<u8>, Vec<u8>)> {
    let parts: Vec<&str> = line.trim().split('$').collect();
    let ["scrypt", n, r, p, salt, hash] = parts.as_slice() else {
        return None;
    };
    let hash = hex_bytes(hash)?;
    let params = scrypt_params(
        n.parse().ok()?,
        r.parse().ok()?,
        p.parse().ok()?,
        hash.len(),
    )?;
    Some((params, hex_bytes(salt)?, hash))
}

/// False for a wrong password and for a line that is malformed or too costly.
fn scrypt_check(password: &[u8], line: &str) -> bool {
    let Some((params, salt, want)) = scrypt_line(line) else {
        return false;
    };
    let got = scrypt_derive(password, &salt, &params, want.len());
    // Constant time: every byte is compared.
    got.iter().zip(&want).fold(0, |diff, (a, b)| diff | (a ^ b)) == 0
}

// --- the new verbs, by their arguments ---------------------------------------

fn call_p256_key_new(keys: &KeyAccess, input: &Value) -> Value {
    string_result(name_of(input).and_then(|name| keys.create_p256(name)))
}

fn call_p256_public(keys: &KeyAccess, input: &Value) -> Value {
    string_result(
        name_of(input)
            .and_then(|name| keys.load_p256(name))
            .map(|key| p256_public(&key)),
    )
}

fn call_vapid_header(keys: &KeyAccess, input: &Value) -> Value {
    let header = match args(input) {
        [
            Value::String(name),
            Value::String(audience),
            Value::String(subject),
        ] => keys.load_p256(name).map(|key| {
            let now = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map_or(0, |since| since.as_secs());
            vapid_header(&key, audience, subject, now)
        }),
        _ => Err("vapid-header expects a key name, an audience and a subject".into()),
    };
    string_result(header)
}

fn call_webpush_encrypt(_: &KeyAccess, input: &Value) -> Value {
    let encrypt = || match args(input) {
        [Value::String(p256dh), Value::String(auth), payload] => {
            let payload = bytes(payload).ok_or("webpush-encrypt expects bytes")?;
            let p256dh = unbase64url(p256dh, "p256dh")?;
            let auth = unbase64url(auth, "auth")?;
            webpush_encrypt(&p256dh, &auth, &payload, &p256_random()?, &random()?)
        }
        _ => Err("webpush-encrypt expects p256dh, auth and a payload".into()),
    };
    bytes_result(encrypt())
}

fn call_x25519_key_new(keys: &KeyAccess, input: &Value) -> Value {
    string_result(name_of(input).and_then(|name| keys.create_x25519(name)))
}

fn call_x25519_public(keys: &KeyAccess, input: &Value) -> Value {
    string_result(
        name_of(input)
            .and_then(|name| keys.load_x25519(name))
            .map(|secret| x25519_public(&secret)),
    )
}

fn call_seal(_: &KeyAccess, input: &Value) -> Value {
    let sealed = || match args(input) {
        [Value::String(recipient), plaintext] => {
            let plaintext = bytes(plaintext).ok_or("seal expects bytes")?;
            let recipient =
                from_hex::<32>(recipient).ok_or("recipient is not a 32-byte hex public key")?;
            seal(
                &recipient,
                &plaintext,
                &x25519_dalek::StaticSecret::from(random::<32>()?),
            )
        }
        _ => Err("seal expects a recipient public key and a plaintext".into()),
    };
    bytes_result(sealed())
}

fn call_open(keys: &KeyAccess, input: &Value) -> Value {
    let opened = || match args(input) {
        [Value::String(name), sealed] => {
            let sealed = bytes(sealed).ok_or("open expects bytes")?;
            open(&keys.load_x25519(name)?, &sealed).ok_or_else(|| "cannot open".to_string())
        }
        _ => Err("open expects a key name and a sealed box".into()),
    };
    bytes_result(opened())
}

fn call_scrypt_hash(_: &KeyAccess, input: &Value) -> Value {
    string_result(
        bytes(input)
            .ok_or_else(|| "scrypt-hash expects a password".to_string())
            .and_then(|password| scrypt_hash(&password)),
    )
}

fn call_scrypt_check(_: &KeyAccess, input: &Value) -> Value {
    Value::Bool(match args(input) {
        [password, Value::String(line)] => {
            bytes(password).is_some_and(|password| scrypt_check(&password, line))
        }
        _ => false,
    })
}

fn call_random_bytes(_: &KeyAccess, input: &Value) -> Value {
    bytes_result(match input {
        Value::U32(n) => random_bytes(*n),
        _ => Err("random-bytes expects a count".into()),
    })
}

/// The actor checks the arguments; bad ones hash as empty, as for `sha256`.
fn hmac_args(input: &Value) -> (Vec<u8>, Vec<u8>) {
    match args(input) {
        [key, data] => (
            bytes(key).unwrap_or_default(),
            bytes(data).unwrap_or_default(),
        ),
        _ => Default::default(),
    }
}

fn call_hmac_sha1(_: &KeyAccess, input: &Value) -> Value {
    let (key, data) = hmac_args(input);
    Value::String(hmac_sha1(&key, &data))
}

fn call_hmac_sha256(_: &KeyAccess, input: &Value) -> Value {
    let (key, data) = hmac_args(input);
    Value::String(hmac_sha256(&key, &data))
}

type Verb = fn(&KeyAccess, &Value) -> Value;

/// A verb run on the blocking pool: key files are read from disk, and scrypt
/// takes a good part of a second.
fn define(imports: &mut HostImports, keys: &KeyAccess, name: &str, call: Verb) {
    let keys = keys.clone();
    imports.define(
        "wisp-crypto",
        name,
        host_fn(move |input| {
            let keys = keys.clone();
            async move {
                tokio::task::spawn_blocking(move || call(&keys, &input))
                    .await
                    .map_err(|error| format!("wisp-crypto: {error}").into())
            }
        }),
    );
}

impl Handler for KeyAccess {
    fn create_instance(
        &self,
        _: Option<&theater::config::actor_manifest::HandlerConfig>,
    ) -> Box<dyn Handler> {
        Box::new(self.clone())
    }
    fn name(&self) -> &str {
        "wisp-crypto"
    }
    fn imports(&self) -> Option<Vec<String>> {
        Some(vec!["wisp-crypto".into()])
    }
    fn exports(&self) -> Option<Vec<String>> {
        None
    }
    fn supports_composite(&self) -> bool {
        true
    }
    fn interfaces(&self) -> Vec<InterfaceImpl> {
        vec![InterfaceImpl::from_pact(
            &parse_pact(include_str!("../../crypto.pact")).expect("crypto interface"),
        )]
    }
    fn register_host_functions(
        &mut self,
        imports: &mut HostImports,
        ctx: &mut HandlerContext,
    ) -> Result<()> {
        if ctx.is_satisfied("wisp-crypto") {
            return Ok(());
        }
        imports.define(
            "wisp-crypto",
            "sha256",
            host_fn(|input| async move {
                // The actor checks the argument; a bad one hashes as empty.
                Ok(Value::String(sha256(&bytes(&input).unwrap_or_default())))
            }),
        );
        imports.define(
            "wisp-crypto",
            "verify",
            host_fn(|input| async move {
                let checked = match &input {
                    Value::Tuple(args) => match args.as_slice() {
                        [Value::String(public_key), Value::String(signature), data] => {
                            bytes(data).is_some_and(|data| verify(public_key, signature, &data))
                        }
                        _ => false,
                    },
                    _ => false,
                };
                Ok(Value::Bool(checked))
            }),
        );
        let keys = self.clone();
        imports.define(
            "wisp-crypto",
            "key-new",
            host_fn(move |input| {
                let keys = keys.clone();
                async move { Ok(string_result(name_of(&input).and_then(|n| keys.create(n)))) }
            }),
        );
        let keys = self.clone();
        imports.define(
            "wisp-crypto",
            "key-public",
            host_fn(move |input| {
                let keys = keys.clone();
                async move {
                    let public = name_of(&input)
                        .and_then(|name| keys.load(name))
                        .map(|key| public_hex(&key));
                    Ok(string_result(public))
                }
            }),
        );
        let keys = self.clone();
        imports.define(
            "wisp-crypto",
            "key-list",
            host_fn(move |_input| {
                let keys = keys.clone();
                async move {
                    let names = keys.list().map(|names| Value::List {
                        elem_type: ValueType::String,
                        items: names.into_iter().map(Value::String).collect(),
                    });
                    Ok(result(ValueType::List(Box::new(ValueType::String)), names))
                }
            }),
        );
        let keys = self.clone();
        imports.define(
            "wisp-crypto",
            "sign",
            host_fn(move |input| {
                let keys = keys.clone();
                async move { Ok(string_result(sign(&keys, &input))) }
            }),
        );
        let verbs: [(&str, Verb); 13] = [
            ("p256-key-new", call_p256_key_new),
            ("p256-public", call_p256_public),
            ("vapid-header", call_vapid_header),
            ("webpush-encrypt", call_webpush_encrypt),
            ("x25519-key-new", call_x25519_key_new),
            ("x25519-public", call_x25519_public),
            ("seal", call_seal),
            ("open", call_open),
            ("scrypt-hash", call_scrypt_hash),
            ("scrypt-check", call_scrypt_check),
            ("random-bytes", call_random_bytes),
            ("hmac-sha1", call_hmac_sha1),
            ("hmac-sha256", call_hmac_sha256),
        ];
        for (name, call) in verbs {
            define(imports, self, name, call);
        }
        ctx.mark_satisfied("wisp-crypto");
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn b64(text: &str) -> Vec<u8> {
        BASE64URL.decode(text).unwrap()
    }

    fn hex(text: &str) -> Vec<u8> {
        hex_bytes(text).unwrap()
    }

    fn p256_key(text: &str) -> p256::SecretKey {
        p256::SecretKey::from_slice(&b64(text)).unwrap()
    }

    // RFC 8291 section 5 and appendix A.
    const UA_PUBLIC: &str =
        "BCVxsr7N_eNgVRqvHtD0zTZsEc6-VV-JvLexhqUzORcxaOzi6-AYWXvTBHm4bjyPjs7Vd8pZGH6SRpkNtoIAiw4";
    const UA_PRIVATE: &str = "q1dXpw3UpT5VOmu_cf_v6ih07Aems3njxI-JWgLcM94";
    const AS_PUBLIC: &str =
        "BP4z9KsN6nGRTbVYI_c7VJSPQTBtkgcy27mlmlMoZIIgDll6e3vCYLocInmYWAmS6TlzAC8wEqKK6PBru3jl7A8";
    const AS_PRIVATE: &str = "yfWPiYE-n46HLnH0KqZOF1fJJU3MYrct3AELtAQ-oRw";
    const AUTH: &str = "BTBZMqHH6r4Tts7J_aSIgg";
    const SALT: &str = "DGv6ra1nlYgDCS1FRnbzlw";
    const MESSAGE: &str = "DGv6ra1nlYgDCS1FRnbzlwAAEABBBP4z9KsN6nGRTbVYI_c7VJSPQTBtkgcy27mlmlMoZIIgDll6e3vCYLocInmYWAmS6TlzAC8wEqKK6PBru3jl7A_yl95bQpu6cVPTpK4Mqgkf1CXztLVBSt2Ks3oZwbuwXPXLWyouBWLVWGNWQexSgSxsj_Qulcy4a-fN";
    const PLAINTEXT: &[u8] = b"When I grow up, I want to be a watermelon";

    /// The user agent's side of RFC 8291, to check what we send can be read.
    fn webpush_decrypt(ua: &p256::SecretKey, auth: &[u8], message: &[u8]) -> Option<Vec<u8>> {
        let (salt, rest) = message.split_first_chunk::<16>()?;
        let (rs, rest) = rest.split_first_chunk::<4>()?;
        let (&[65], rest) = rest.split_first_chunk::<1>()? else {
            return None;
        };
        let (sender, sealed) = rest.split_first_chunk::<65>()?;
        assert_eq!(u32::from_be_bytes(*rs), 4096);
        let sender = p256::PublicKey::from_sec1_bytes(sender).ok()?;
        let shared = p256::ecdh::diffie_hellman(ua.to_nonzero_scalar(), sender.as_affine());
        let ua_public = ua.public_key().to_encoded_point(false);
        let sender_public = sender.to_encoded_point(false);
        let info = [
            b"WebPush: info\0",
            ua_public.as_bytes(),
            sender_public.as_bytes(),
        ]
        .concat();
        let ikm = hkdf::<32>(auth, shared.raw_secret_bytes(), &info);
        let cek = hkdf::<16>(salt, &ikm, b"Content-Encoding: aes128gcm\0");
        let nonce = hkdf::<12>(salt, &ikm, b"Content-Encoding: nonce\0");
        let mut record = Aes128Gcm::new(&cek.into())
            .decrypt(&nonce.into(), sealed)
            .ok()?;
        (record.pop() == Some(2)).then_some(record)
    }

    #[test]
    fn webpush_matches_rfc_8291() {
        let sender = p256_key(AS_PRIVATE);
        assert_eq!(p256_public(&sender), AS_PUBLIC);
        assert_eq!(p256_public(&p256_key(UA_PRIVATE)), UA_PUBLIC);
        let salt: [u8; 16] = b64(SALT).try_into().unwrap();
        let got = webpush_encrypt(&b64(UA_PUBLIC), &b64(AUTH), PLAINTEXT, &sender, &salt).unwrap();
        assert_eq!(BASE64URL.encode(&got), MESSAGE);
        // Padded base64url, as some subscriptions are written, reads the same.
        assert_eq!(
            unbase64url(&format!("{AUTH}=="), "auth").unwrap(),
            b64(AUTH)
        );
    }

    #[test]
    fn webpush_round_trips_and_refuses_bad_keys() {
        let ua = p256_random().unwrap();
        let ua_public = ua.public_key().to_encoded_point(false);
        let auth = random::<16>().unwrap();
        for payload in [
            &b""[..],
            b"{\"title\":\"hi\"}",
            &[0xff; WEBPUSH_MAX_PAYLOAD],
        ] {
            let sent = webpush_encrypt(
                ua_public.as_bytes(),
                &auth,
                payload,
                &p256_random().unwrap(),
                &random().unwrap(),
            )
            .unwrap();
            assert_eq!(sent.len(), 86 + payload.len() + 17);
            assert_eq!(webpush_decrypt(&ua, &auth, &sent).unwrap(), payload);
            assert!(webpush_decrypt(&ua, &[0; 16], &sent).is_none());
        }
        let sender = p256_random().unwrap();
        let salt = [0; 16];
        let too_big = vec![0; WEBPUSH_MAX_PAYLOAD + 1];
        let err = |p256dh: &[u8], auth: &[u8], payload: &[u8]| {
            webpush_encrypt(p256dh, auth, payload, &sender, &salt).unwrap_err()
        };
        assert!(err(ua_public.as_bytes(), &auth, &too_big).contains("at most 3993"));
        assert!(err(ua_public.as_bytes(), &auth[..15], b"").contains("16 bytes"));
        let compressed = ua.public_key().to_encoded_point(true);
        assert!(err(compressed.as_bytes(), &auth, b"").contains("uncompressed"));
        assert!(err(&[4; 65], &auth, b"").contains("uncompressed"));
    }

    #[test]
    fn vapid_header_is_an_es256_jwt_for_the_audience() {
        use p256::ecdsa::signature::Verifier;
        let key = p256_key(AS_PRIVATE);
        let header = vapid_header(&key, "https://push.example.net", "mailto:a@b.c", 1000);
        let (token, public) = header
            .strip_prefix("vapid t=")
            .and_then(|rest| rest.split_once(", k="))
            .unwrap();
        assert_eq!(public, AS_PUBLIC);
        let (signed, signature) = token.rsplit_once('.').unwrap();
        let (head, claims) = signed.split_once('.').unwrap();
        let head: serde_json::Value = serde_json::from_slice(&b64(head)).unwrap();
        assert_eq!(head, serde_json::json!({"typ": "JWT", "alg": "ES256"}));
        let claims: serde_json::Value = serde_json::from_slice(&b64(claims)).unwrap();
        assert_eq!(
            claims,
            serde_json::json!({"aud": "https://push.example.net", "exp": 1000 + 43200, "sub": "mailto:a@b.c"})
        );
        let signature = p256::ecdsa::Signature::from_slice(&b64(signature)).unwrap();
        let verifier = p256::ecdsa::VerifyingKey::from(key.public_key());
        assert!(verifier.verify(signed.as_bytes(), &signature).is_ok());
        assert!(verifier.verify(b"other", &signature).is_err());
    }

    // RFC 7748 section 6.1.
    const ALICE_PRIVATE: &str = "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a";
    const ALICE_PUBLIC: &str = "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a";
    const BOB_PRIVATE: &str = "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb";
    const BOB_PUBLIC: &str = "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f";
    const SHARED: &str = "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742";

    fn x25519_key(text: &str) -> x25519_dalek::StaticSecret {
        x25519_dalek::StaticSecret::from(from_hex::<32>(text).unwrap())
    }

    #[test]
    fn x25519_matches_rfc_7748_and_seals_with_it() {
        let alice = x25519_key(ALICE_PRIVATE);
        let bob = x25519_key(BOB_PRIVATE);
        assert_eq!(x25519_public(&alice), ALICE_PUBLIC);
        assert_eq!(x25519_public(&bob), BOB_PUBLIC);
        let bob_public = from_hex::<32>(BOB_PUBLIC).unwrap();
        let shared = alice.diffie_hellman(&x25519_dalek::PublicKey::from(bob_public));
        assert_eq!(to_hex(shared.as_bytes()), SHARED);

        // A box from Alice's key to Bob: the format, written out by hand.
        let sealed = seal(&bob_public, b"hello", &alice).unwrap();
        let salt = [hex(ALICE_PUBLIC), hex(BOB_PUBLIC)].concat();
        let key = hkdf::<32>(&salt, &hex(SHARED), b"wisp sealed box v1");
        let body = ChaCha20Poly1305::new(&key.into())
            .encrypt(&[0; 12].into(), &b"hello"[..])
            .unwrap();
        assert_eq!(sealed, [hex(ALICE_PUBLIC), body].concat());
        assert_eq!(sealed.len(), 32 + 5 + 16);
        assert_eq!(open(&bob, &sealed).unwrap(), b"hello");
    }

    #[test]
    fn sealed_boxes_open_only_for_their_recipient_unchanged() {
        let recipient = x25519_dalek::StaticSecret::from(random::<32>().unwrap());
        let public = x25519_dalek::PublicKey::from(&recipient).to_bytes();
        let fresh = || x25519_dalek::StaticSecret::from(random::<32>().unwrap());
        for plaintext in [&b""[..], b"an event", &[7; 5000]] {
            let sealed = seal(&public, plaintext, &fresh()).unwrap();
            assert_eq!(open(&recipient, &sealed).unwrap(), plaintext);
            assert!(open(&fresh(), &sealed).is_none());
            for at in [0, 31, 32, sealed.len() - 1] {
                let mut bent = sealed.clone();
                bent[at] ^= 1;
                assert!(open(&recipient, &bent).is_none(), "byte {at}");
            }
            assert!(open(&recipient, &sealed[..sealed.len() - 1]).is_none());
        }
        // Each box has its own ephemeral key.
        let one = seal(&public, b"same", &fresh()).unwrap();
        assert_ne!(one, seal(&public, b"same", &fresh()).unwrap());
        assert!(open(&recipient, &[0; 47]).is_none());
        // A low-order recipient key would give a shared secret of zero.
        assert!(seal(&[0; 32], b"m", &fresh()).is_err());
        assert!(open(&recipient, &[0; 64]).is_none());
    }

    #[test]
    fn scrypt_matches_rfc_7914_and_auth_helper() {
        // RFC 7914 section 12, the first 32 bytes of each 64 (PBKDF2's output
        // is a prefix of the longer one).
        assert!(scrypt_check(
            b"password",
            "scrypt$1024$8$16$4e61436c$fdbabe1c9d3472007856e7190d01e9fe7c6ad7cbc8237830e77376634b373162"
        ));
        let rfc = "scrypt$16384$8$1$536f6469756d43686c6f72696465$7023bdcb3afd7348461c06cd81fd38ebfda8fbba904f8e3ea9b543f6545da1f2d5432955613f0fcf62d49705242a9af9e61e85dc0d651e40dfcf017b45575887";
        assert!(scrypt_check(b"pleaseletmein", rfc));
        assert!(!scrypt_check(b"pleaseletmeim", rfc));
        // Python's hashlib.scrypt at auth-helper's parameters, with a newline
        // as its file has.
        let line = "scrypt$32768$8$1$000102030405060708090a0b0c0d0e0f$c72e940880b3f15bff3fa26a3a49140f80ce9a1c4dd2d5d27bab10014071ba94\n";
        assert!(scrypt_check(b"correct horse battery", line));
        assert!(!scrypt_check(b"correct horse batterz", line));
        assert!(!scrypt_check(b"", line));
    }

    #[test]
    fn scrypt_hash_writes_auth_helpers_format() {
        let line = scrypt_hash(b"a password").unwrap();
        let parts: Vec<&str> = line.split('$').collect();
        assert_eq!(parts[..4], ["scrypt", "32768", "8", "1"]);
        assert_eq!(parts[4].len(), 32);
        assert_eq!(parts[5].len(), 64);
        assert!(scrypt_check(b"a password", &line));
        assert!(!scrypt_check(b"a passwore", &line));
        assert_ne!(line, scrypt_hash(b"a password").unwrap(), "fresh salt");
    }

    #[test]
    fn scrypt_refuses_costly_or_malformed_lines() {
        let hash = "00".repeat(32);
        for line in [
            format!("scrypt$262144$1$1$00${hash}"),           // N > 2^17
            format!("scrypt$131072$17$1$00${hash}"),          // 272 MiB
            format!("scrypt$1024$8$17$00${hash}"),            // p > 16
            format!("scrypt$1000$8$1$00${hash}"),             // N not a power of two
            format!("scrypt$1$8$1$00${hash}"),                // N < 2
            format!("scrypt$1024$0$1$00${hash}"),             // r = 0
            format!("scrypt$1024$8$0$00${hash}"),             // p = 0
            format!("scrypt$1024$8$1$00${}", "00".repeat(8)), // short hash
            format!("scrypt$1024$8$1$0${hash}"),              // odd hex
            format!("scrypt$1024$8$1$zz${hash}"),             // not hex
            format!("pbkdf2$1024$8$1$00${hash}"),
            "scrypt$1024$8$1$00".to_string(),
            format!("scrypt$1024$8$1$00${hash}$"),
            String::new(),
        ] {
            assert!(scrypt_line(&line).is_none(), "{line}");
            assert!(!scrypt_check(b"", &line), "{line}");
        }
        // The largest allowed: exactly 256 MiB.
        assert!(scrypt_line(&format!("scrypt$131072$16$1$00${hash}")).is_some());
    }
}
