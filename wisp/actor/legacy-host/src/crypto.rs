//! The `wisp-crypto` interface: sha256, ed25519 verification, and signing with
//! keys only the daemon holds. A key is a file `<name>.key` in the configured
//! directory, its 32-byte seed as hex. Each call reads the file it needs, so
//! nothing is cached, and no function returns a seed: sessions name keys.
use anyhow::Result;
use ed25519_dalek::{Signature, Signer, SigningKey, VerifyingKey};
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

fn from_hex<const N: usize>(text: &str) -> Option<[u8; N]> {
    let digits = text.as_bytes();
    if digits.len() != N * 2 {
        return None;
    }
    let mut out = [0; N];
    for (byte, pair) in out.iter_mut().zip(digits.chunks(2)) {
        let high = (pair[0] as char).to_digit(16)?;
        let low = (pair[1] as char).to_digit(16)?;
        *byte = (high * 16 + low) as u8;
    }
    Some(out)
}

fn sha256(data: &[u8]) -> String {
    to_hex(&Sha256::digest(data))
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

impl KeyAccess {
    fn dir(&self) -> Result<&Path, String> {
        self.dir
            .as_deref()
            .ok_or_else(|| "no key directory configured".to_string())
    }

    fn path(&self, name: &str) -> Result<PathBuf, String> {
        let dir = self.dir()?;
        if !valid_name(name) {
            return Err(format!(
                "bad key name '{name}': use 1-64 of A-Z a-z 0-9 . _ -"
            ));
        }
        Ok(dir.join(format!("{name}.key")))
    }

    fn load(&self, name: &str) -> Result<SigningKey, String> {
        let text = fs::read_to_string(self.path(name)?).map_err(|error| match error.kind() {
            ErrorKind::NotFound => format!("no key named '{name}'"),
            _ => format!("reading key '{name}': {error}"),
        })?;
        let seed = from_hex(text.trim())
            .ok_or_else(|| format!("key file for '{name}' is not a 32-byte hex seed"))?;
        Ok(SigningKey::from_bytes(&seed))
    }

    /// Whole or not at all: the seed is on disk under a temporary name before a
    /// hard link gives it its real one, and the link refuses an existing key, so
    /// a crash can't leave a key file without a seed and a race can't replace one.
    fn create(&self, name: &str) -> Result<String, String> {
        let path = self.path(name)?;
        let dir = self.dir()?;
        let exists = || format!("key '{name}' exists");
        if path.exists() {
            return Err(exists());
        }
        let failed = |error: std::io::Error| format!("creating key '{name}': {error}");
        DirBuilder::new()
            .recursive(true)
            .mode(0o700)
            .create(dir)
            .map_err(failed)?;
        let mut seed = [0; 32];
        getrandom::getrandom(&mut seed).map_err(|error| format!("no randomness: {error}"))?;
        let key = SigningKey::from_bytes(&seed);
        let public = public_hex(&key);
        // Unique per key, so a crash's leftover never blocks a later create. It
        // doesn't end in .key, so it isn't listed.
        let temporary = dir.join(format!("{public}.tmp"));
        let written = OpenOptions::new()
            .write(true)
            .create_new(true)
            .mode(0o600)
            .open(&temporary)
            .and_then(|mut file| {
                file.write_all(to_hex(&seed).as_bytes())?;
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
        ctx.mark_satisfied("wisp-crypto");
        Ok(())
    }
}
