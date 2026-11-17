//! Checking a release before `theater-repl upgrade` installs it (experiment
//! field/SPEC-release.md 6).
//!
//! A release is made latest by a `release.approve` event: a field event signed
//! by an approver, whose body names each artifact's sha256 and size. The site
//! serves that event file as `releases/<repo>/latest/APPROVAL`. This module
//! checks it the way a field node checks any event, against approver keys built
//! into this binary (so the server it came from is not trusted), and returns the
//! tag, sha256 and size of the artifact asked for. The caller then checks the
//! downloaded bytes against those.
//!
//! What "verified" means, honestly (lux): a binary is installed if one of the
//! keys below signed an approval naming its sha256, and nothing more. The client
//! does not repeat the server's other checks (the builder's key, the tag at its
//! commit, GitHub's build matching). Today any agent on the desktop can sign as
//! tova, and a key built into the binary can't be revoked by a route an attacker
//! can't also use. So the client is as strong as the weakest approver key here:
//! no worse than GitHub's releases before it, and stronger once the approvers are
//! the phone alone.

use anyhow::{Context, Result, bail, ensure};
use ed25519_dalek::{Signature, VerifyingKey};
use sha2::{Digest, Sha256};

/// The keys whose approval makes a wisp release installable: the same as the
/// `approvers` of [repo.wisp] in server3's root copy (experiment
/// site/server3/field-releases/repos.toml). A new approver needs a new release.
pub const APPROVERS: &[(&str, &str)] = &[
    (
        "tova",
        "1908c021edbb19d46c8474cac5cd50731a097a822628c26345491b70ec514082",
    ),
    (
        "colin's phone",
        "e24834e51845dc25438c5ba006e0db03ecf0273a624e5345f370e1bede907c3e",
    ),
];

/// What an approval says about one artifact.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Approved {
    pub tag: String,
    pub commit: String,
    pub sha256: String,
    pub size: u64,
    pub approver: String,
}

fn is_hex(s: &str, n: usize) -> bool {
    s.len() == n
        && s.bytes()
            .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
}

/// Check an APPROVAL file (`sig <hex>\n` + the event's canonical text) for
/// `repo`, with `keys` the approver public keys (hex), and return what it
/// approves for the artifact `name`.
pub fn check_approval(file: &str, repo: &str, name: &str, keys: &[&str]) -> Result<Approved> {
    let (first, text) = file.split_once('\n').context("an empty approval")?;
    let sig = first
        .strip_prefix("sig ")
        .context("the approval has no signature line")?;
    ensure!(is_hex(sig, 128), "the signature is not 128 hex characters");
    let id = hex::encode(Sha256::digest(text.as_bytes()));

    // the event's meta lines, up to `body <n>` (SPEC-node 2)
    let (meta, rest) = text
        .split_once("\nbody ")
        .context("the approval has no body")?;
    let (len, body) = rest
        .split_once('\n')
        .context("the approval's body has no length line")?;
    let len: usize = len
        .parse()
        .context("the approval's body length is not a number")?;
    ensure!(
        body.len() == len,
        "the approval's body is {} bytes, not the {len} it says",
        body.len()
    );
    let mut author = "";
    let (mut scope, mut kind) = ("", "");
    for line in meta.lines() {
        match line.split_once(' ') {
            Some(("author", v)) => author = v,
            Some(("scope", v)) => scope = v,
            Some(("kind", v)) => kind = v,
            _ => {}
        }
    }
    ensure!(is_hex(author, 64), "the approval names no author key");
    ensure!(
        keys.contains(&author),
        "the approval is signed by {}…, which is not an approver built into this binary",
        &author[..16]
    );
    let key: [u8; 32] = hex::decode(author)?
        .try_into()
        .map_err(|_| anyhow::anyhow!("a bad key"))?;
    let sig: [u8; 64] = hex::decode(sig)?
        .try_into()
        .map_err(|_| anyhow::anyhow!("a bad signature"))?;
    let ok = VerifyingKey::from_bytes(&key).is_ok_and(|k| {
        k.verify_strict(id.as_bytes(), &Signature::from_bytes(&sig))
            .is_ok()
    });
    ensure!(ok, "the approval's signature does not check");
    ensure!(
        kind == "release.approve",
        "the event is a {kind}, not a release.approve"
    );
    ensure!(
        scope == format!("release/approve/{repo}"),
        "the approval is for {scope}, not release/approve/{repo}"
    );

    // its body: headers, a blank line, then `<name> <sha256> <size>` lines
    let (headers, artifacts) = body
        .split_once("\n\n")
        .context("the approval lists no artifacts")?;
    let header = |k: &str| {
        headers
            .lines()
            .find_map(|l| l.strip_prefix(k).and_then(|v| v.strip_prefix(' ')))
            .unwrap_or("")
    };
    ensure!(
        header("repo") == repo,
        "the approval is for repo {:?}",
        header("repo")
    );
    let (tag, commit) = (header("tag").to_string(), header("commit").to_string());
    ensure!(is_hex(&commit, 40), "the approval names no commit");
    let mut found = None;
    for line in artifacts.lines().filter(|l| !l.trim().is_empty()) {
        let w: Vec<&str> = line.split(' ').collect();
        ensure!(
            w.len() == 3
                && is_hex(w[1], 64)
                && !w[2].is_empty()
                && w[2].bytes().all(|b| b.is_ascii_digit()),
            "a bad artifact line {line:?}"
        );
        if w[0] == name {
            ensure!(found.is_none(), "{name} is listed twice");
            found = Some((w[1].to_string(), w[2].parse::<u64>()?));
        }
    }
    let Some((sha256, size)) = found else {
        bail!("the approval does not list {name}")
    };
    Ok(Approved {
        tag,
        commit,
        sha256,
        size,
        approver: author.to_string(),
    })
}

/// The bytes are the approved artifact: their size and sha256.
pub fn check_bytes(bytes: &[u8], a: &Approved) -> Result<()> {
    ensure!(
        bytes.len() as u64 == a.size,
        "{} bytes, not the approved {}",
        bytes.len(),
        a.size
    );
    let got = hex::encode(Sha256::digest(bytes));
    ensure!(
        got == a.sha256,
        "the bytes hash to {got}, not the approved {}",
        a.sha256
    );
    Ok(())
}

/// "v0.1.40" -> (0, 1, 40); anything else -> None.
pub fn version(v: &str) -> Option<(u32, u32, u32)> {
    let mut p = v.strip_prefix('v')?.split('.');
    let t = (
        p.next()?.parse().ok()?,
        p.next()?.parse().ok()?,
        p.next()?.parse().ok()?,
    );
    p.next().is_none().then_some(t)
}

#[cfg(test)]
mod tests {
    use super::*;
    use ed25519_dalek::{Signer, SigningKey};

    const NAME: &str = "theater-repl-linux-x86_64";

    fn key(seed: u8) -> (SigningKey, String) {
        let k = SigningKey::from_bytes(&[seed; 32]);
        let p = hex::encode(k.verifying_key().to_bytes());
        (k, p)
    }

    fn event(k: &SigningKey, pubhex: &str, scope: &str, kind: &str, body: &str) -> String {
        let text = format!(
            "field 1\nauthor {pubhex}\nscope {scope}\nkind {kind}\ntime 1\nparent {}\nbody {}\n{body}",
            "a".repeat(64),
            body.len()
        );
        let id = hex::encode(Sha256::digest(text.as_bytes()));
        format!(
            "sig {}\n{text}",
            hex::encode(k.sign(id.as_bytes()).to_bytes())
        )
    }

    fn body(repo: &str, arts: &str) -> String {
        format!(
            "from tova\nrequest {}\nrepo {repo}\ntag v0.1.40\ncommit {}\n\n{arts}",
            "b".repeat(64),
            "c".repeat(40)
        )
    }

    fn good_arts() -> String {
        format!("{NAME} {} 3\n", hex::encode(Sha256::digest(b"abc")))
    }

    #[test]
    fn a_good_approval_names_the_artifact() {
        let (k, p) = key(1);
        let f = event(
            &k,
            &p,
            "release/approve/wisp",
            "release.approve",
            &body("wisp", &good_arts()),
        );
        let a = check_approval(&f, "wisp", NAME, &[&p]).unwrap();
        assert_eq!((a.tag.as_str(), a.size), ("v0.1.40", 3));
        check_bytes(b"abc", &a).unwrap();
        assert!(check_bytes(b"abd", &a).is_err());
        assert!(check_bytes(b"abcd", &a).is_err());
    }

    #[test]
    fn refused() {
        let (k, p) = key(1);
        let (k2, p2) = key(2);
        let ok = |f: &str| check_approval(f, "wisp", NAME, &[&p]);
        let good = event(
            &k,
            &p,
            "release/approve/wisp",
            "release.approve",
            &body("wisp", &good_arts()),
        );
        // a key not built in, even with a good signature
        assert!(
            ok(&event(
                &k2,
                &p2,
                "release/approve/wisp",
                "release.approve",
                &body("wisp", &good_arts())
            ))
            .is_err()
        );
        // one byte of the text changed
        assert!(ok(&good.replace("v0.1.40", "v0.1.99")).is_err());
        // a signature by another key under the approver's name
        let forged = event(
            &k2,
            &p,
            "release/approve/wisp",
            "release.approve",
            &body("wisp", &good_arts()),
        );
        assert!(ok(&forged).is_err());
        // another kind, scope or repo, signed by the approver
        assert!(
            ok(&event(
                &k,
                &p,
                "release/approve/wisp",
                "release.request",
                &body("wisp", &good_arts())
            ))
            .is_err()
        );
        assert!(
            ok(&event(
                &k,
                &p,
                "release/approve/other",
                "release.approve",
                &body("wisp", &good_arts())
            ))
            .is_err()
        );
        assert!(
            ok(&event(
                &k,
                &p,
                "release/approve/wisp",
                "release.approve",
                &body("other", &good_arts())
            ))
            .is_err()
        );
        // the artifact missing, listed twice, or a bad line
        assert!(
            ok(&event(
                &k,
                &p,
                "release/approve/wisp",
                "release.approve",
                &body("wisp", "other 1 2\n")
            ))
            .is_err()
        );
        let twice = format!("{}{}", good_arts(), good_arts());
        assert!(
            ok(&event(
                &k,
                &p,
                "release/approve/wisp",
                "release.approve",
                &body("wisp", &twice)
            ))
            .is_err()
        );
        let sup = format!("{NAME} {} ²\n", "d".repeat(64));
        assert!(
            ok(&event(
                &k,
                &p,
                "release/approve/wisp",
                "release.approve",
                &body("wisp", &sup)
            ))
            .is_err()
        );
        // no signature line, or garbage
        assert!(ok(good.split_once('\n').unwrap().1).is_err());
        assert!(ok("").is_err());
        assert!(ok("sig zz\nfield 1").is_err());
    }

    #[test]
    fn versions() {
        assert_eq!(version("v0.1.40"), Some((0, 1, 40)));
        assert!(version("v0.1.40") > version("v0.1.39"));
        assert_eq!(version("v0.0.0-dev"), None);
        assert_eq!(version("0.1.4"), None);
    }
}
