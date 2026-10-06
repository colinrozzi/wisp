//! Immutable, bounded source bundles. Guest paths never access the filesystem.
use anyhow::{Result, ensure};
use std::collections::BTreeMap;
use std::sync::Arc;
use theater::handler::{Handler, HandlerContext};
use theater::pack_bridge::{HostImports, InterfaceImpl, Value, ValueType, host_fn, parse_pact};

#[derive(Clone, Default)]
pub struct SourceBundle(Arc<BTreeMap<String, String>>);

impl SourceBundle {
    pub fn new(sources: BTreeMap<String, String>) -> Result<Self> {
        ensure!(sources.len() <= 256, "bundle exceeds 256 files");
        ensure!(
            sources.values().map(String::len).sum::<usize>() <= 1024 * 1024,
            "bundle exceeds 1 MiB"
        );
        let mut canonical = BTreeMap::new();
        for (path, source) in sources {
            ensure!(source.len() <= 65536, "bundle file exceeds 64 KiB: {path}");
            let path = normalize("", &path).map_err(anyhow::Error::msg)?;
            ensure!(
                canonical.insert(path.clone(), source).is_none(),
                "duplicate bundle path: {path}"
            );
        }
        Ok(Self(Arc::new(canonical)))
    }
}

fn normalize(base: &str, path: &str) -> Result<String, String> {
    if path.is_empty() || path.contains('\0') || path.contains('\\') {
        return Err("invalid bundle path".into());
    }
    let joined = if path.starts_with('/') || base.is_empty() {
        path.to_string()
    } else {
        format!(
            "{}/{path}",
            base.rsplit_once('/').map_or("", |(dir, _)| dir)
        )
    };
    let mut parts = Vec::new();
    for part in joined.split('/') {
        match part {
            "" | "." => {}
            ".." => {
                if parts.pop().is_none() {
                    return Err("path escapes bundle".into());
                }
            }
            other => parts.push(other),
        }
    }
    if parts.is_empty() {
        return Err("bundle path is not a file".into());
    }
    Ok(format!("/{}", parts.join("/")))
}

fn result(value: Result<String, String>) -> Value {
    Value::Result {
        ok_type: ValueType::String,
        err_type: ValueType::String,
        value: value
            .map(|s| Box::new(Value::String(s)))
            .map_err(|s| Box::new(Value::String(s))),
    }
}

impl Handler for SourceBundle {
    fn create_instance(
        &self,
        _: Option<&theater::config::actor_manifest::HandlerConfig>,
    ) -> Box<dyn Handler> {
        Box::new(self.clone())
    }
    fn name(&self) -> &str {
        "wisp-source"
    }
    fn imports(&self) -> Option<Vec<String>> {
        Some(vec!["wisp-source".into()])
    }
    fn exports(&self) -> Option<Vec<String>> {
        None
    }
    fn supports_composite(&self) -> bool {
        true
    }
    fn interfaces(&self) -> Vec<InterfaceImpl> {
        vec![InterfaceImpl::from_pact(
            &parse_pact(include_str!("../source.pact")).expect("source interface"),
        )]
    }
    fn register_host_functions(
        &mut self,
        imports: &mut HostImports,
        ctx: &mut HandlerContext,
    ) -> Result<()> {
        if ctx.is_satisfied("wisp-source") {
            return Ok(());
        }
        let bundle = self.0.clone();
        imports.define(
            "wisp-source",
            "resolve-path",
            host_fn(move |input| {
                let bundle = bundle.clone();
                async move {
                    let resolved = match input {
                        Value::Tuple(args) => match args.as_slice() {
                            [Value::String(base), Value::String(path)] => normalize(base, path)
                                .and_then(|path| {
                                    if bundle.contains_key(&path) {
                                        Ok(path)
                                    } else {
                                        Err(format!("source is not in bundle: {path}"))
                                    }
                                }),
                            _ => Err("resolve-path expects base and path strings".into()),
                        },
                        _ => Err("resolve-path expects two arguments".into()),
                    };
                    Ok(result(resolved))
                }
            }),
        );
        let bundle = self.0.clone();
        imports.define(
            "wisp-source",
            "read-source",
            host_fn(move |input| {
                let bundle = bundle.clone();
                async move {
                    let source = match input {
                        Value::String(path) => bundle
                            .get(&path)
                            .cloned()
                            .ok_or_else(|| format!("source is not in bundle: {path}")),
                        _ => Err("read-source expects a path string".into()),
                    };
                    Ok(result(source))
                }
            }),
        );
        ctx.mark_satisfied("wisp-source");
        Ok(())
    }
}
