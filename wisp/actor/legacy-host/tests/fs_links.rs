//! A symlink inside a session's filesystem root that points out of it can't be
//! followed: Theater checks each path's real location (theater#260, ticket
//! 0156), so a link planted in a data dir doesn't widen any grant.

use std::path::{Path, PathBuf};
use wisp_interpreter_actor::{FsAccess, Grants, Runtime, source::SourceBundle};

fn actor_path() -> PathBuf {
    std::env::var_os("WISP_ACTOR_WASM")
        .map(PathBuf::from)
        .unwrap_or_else(|| Path::new(env!("CARGO_MANIFEST_DIR")).join("../actor.wasm"))
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn a_link_out_of_the_root_is_refused() -> anyhow::Result<()> {
    let base = std::env::temp_dir().join(format!("wisp-fs-links-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    let (root, outside) = (base.join("root"), base.join("outside"));
    std::fs::create_dir_all(root.join("data"))?;
    std::fs::create_dir_all(&outside)?;
    std::fs::write(outside.join("secret"), "host")?;
    std::fs::write(root.join("data/mine"), "mine")?;
    std::os::unix::fs::symlink(outside.join("secret"), root.join("data/link"))?;
    std::os::unix::fs::symlink(&outside, root.join("data/out"))?;

    let grants = Grants {
        fs: FsAccess {
            root: Some(root.clone()),
            read: true,
            write: true,
        },
        ..Default::default()
    };
    let runtime = Runtime::new(SourceBundle::default(), grants).await?;
    let session = runtime.spawn(std::fs::read(actor_path())?).await?;
    let eval = |form: &'static str| session.evaluate(form);

    assert!(eval("(fs-read-text \"data/mine\")").await?.contains("mine"));
    for form in [
        "(fs-read-text \"data/link\")",
        "(fs-write \"data/link\" \"overwritten\")",
        "(fs-read-text \"data/out/secret\")",
        "(fs-write \"data/out/planted\" \"x\")",
    ] {
        let got = eval(form).await?;
        assert!(
            got.contains("escapes the sandbox root via a symlink"),
            "{form}: {got}"
        );
    }
    assert_eq!(std::fs::read_to_string(outside.join("secret"))?, "host");
    assert!(!outside.join("planted").exists());
    runtime.shutdown().await?;
    std::fs::remove_dir_all(&base)?;
    Ok(())
}
