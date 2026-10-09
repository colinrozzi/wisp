use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, anyhow, bail};
use wat::parse_str;
use wit_component::{ComponentEncoder, StringEncoding, embed_component_metadata};

mod tokenizer;
pub(crate) use tokenizer::*;
mod codegen;
use codegen::*;
pub use codegen::{compile_repl_expr, compile_repl_expr_pack, compile_repl_expr_pack_wat};
mod eval;
pub use eval::{Host, NullHost, Value, eval_repl_expr, eval_source, eval_source_with_host};
mod typecheck;
pub(crate) use typecheck::*;
mod macros;
pub(crate) use macros::*;
mod lower;
pub(crate) use lower::*;
mod parser;
pub(crate) use parser::*;
use wit_parser::Resolve;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    S32,
    S64,
    F32,
    F64,
    Record(String),               // Named record type
    Variant(String),              // Named variant type
    Option(Box<Type>),            // option<T> - some or none
    Result(Box<Type>, Box<Type>), // result<T, E> - ok or err
    List(Box<Type>),              // list<T> - dynamic list
    Str,                          // UTF-8 string
    Resource(String),             // resource handle (opaque i32)
    Borrow(Box<Type>),            // borrow<T> - borrowed reference
    Tuple(Vec<Type>),             // tuple<T1, T2, ...> - product type
    U8,                           // unsigned 8-bit integer
    Bool,                         // boolean (i32 in WASM; 1-byte CGRF payload like u8)
    U16,                          // unsigned 16-bit integer (i32 in WASM; 2-byte CGRF payload)
    U32,                          // unsigned 32-bit integer (i32 in WASM; 4-byte CGRF payload)
    U64,                          // unsigned 64-bit integer (i64 in WASM; like s64)
    Any,                          // Pack dynamic `value` - self-describing CGRF blob
}

/// A value that can be inlined during REPL compilation
#[derive(Debug, Clone)]
pub enum InlineValue {
    // Scalars
    S32(i32),
    S64(i64),
    F32(f32),
    F64(f64),

    // String
    Str(String),

    // Compound types WITH explicit type info
    List {
        elem_type: Type,
        items: Vec<InlineValue>,
    },
    Option {
        inner_type: Type,
        value: Option<Box<InlineValue>>,
    },
    Result {
        ok_type: Type,
        err_type: Type,
        value: std::result::Result<Box<InlineValue>, Box<InlineValue>>,
    },

    // User-defined types - ordered fields, multi-value payload
    Record {
        type_name: String,
        fields: Vec<(String, InlineValue)>,
    },
    Variant {
        type_name: String,
        case: String,
        payload: Vec<InlineValue>,
    },
}

impl InlineValue {
    /// Get the type of this value - uses explicit type fields
    pub fn get_type(&self) -> Type {
        match self {
            InlineValue::S32(_) => Type::S32,
            InlineValue::S64(_) => Type::S64,
            InlineValue::F32(_) => Type::F32,
            InlineValue::F64(_) => Type::F64,
            InlineValue::Str(_) => Type::Str,
            InlineValue::List { elem_type, .. } => Type::List(Box::new(elem_type.clone())),
            InlineValue::Option { inner_type, .. } => Type::Option(Box::new(inner_type.clone())),
            InlineValue::Result {
                ok_type, err_type, ..
            } => Type::Result(Box::new(ok_type.clone()), Box::new(err_type.clone())),
            InlineValue::Record { type_name, .. } => Type::Record(type_name.clone()),
            InlineValue::Variant { type_name, .. } => Type::Variant(type_name.clone()),
        }
    }
}

/// Unique identifier for a lexical scope (used for hygiene)
type ScopeId = u64;

/// Thread-local counter for generating unique scope IDs
use std::sync::atomic::{AtomicU64, Ordering};
static SCOPE_COUNTER: AtomicU64 = AtomicU64::new(1);

fn fresh_scope() -> ScopeId {
    SCOPE_COUNTER.fetch_add(1, Ordering::SeqCst)
}

/// Set of scopes attached to an identifier for hygiene tracking
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(crate) struct ScopeSet {
    scopes: HashSet<ScopeId>,
}

impl ScopeSet {
    /// Create a scope set with the base scope (scope 0)
    fn base() -> Self {
        let mut scopes = HashSet::new();
        scopes.insert(0);
        Self { scopes }
    }

    /// Add a scope to this set
    fn with_scope(&self, scope: ScopeId) -> Self {
        let mut new_scopes = self.scopes.clone();
        new_scopes.insert(scope);
        Self { scopes: new_scopes }
    }

    /// Check if this scope set is a subset of another
    fn is_subset_of(&self, other: &ScopeSet) -> bool {
        self.scopes.is_subset(&other.scopes)
    }
}

/// Source location information for error reporting and hygiene
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub line: usize,
    pub column: usize,
    pub length: usize,
    scopes: ScopeSet,
}

impl Span {
    fn new(line: usize, column: usize, length: usize) -> Self {
        Self {
            line,
            column,
            length,
            scopes: ScopeSet::base(),
        }
    }

    /// Create a dummy span for generated code
    fn dummy() -> Self {
        Self {
            line: 0,
            column: 0,
            length: 0,
            scopes: ScopeSet::base(),
        }
    }

    /// Create a span with additional scope (for macro hygiene)
    fn with_scope(&self, scope: ScopeId) -> Self {
        Self {
            line: self.line,
            column: self.column,
            length: self.length,
            scopes: self.scopes.with_scope(scope),
        }
    }

    /// Merge two spans (from start of first to end of second)
    fn merge(&self, other: &Span) -> Span {
        if self.line == 0 && self.column == 0 {
            return other.clone();
        }
        if other.line == 0 && other.column == 0 {
            return self.clone();
        }
        // For simplicity, just use the start of self
        // A proper implementation would compute the full range
        Span {
            line: self.line,
            column: self.column,
            length: 1, // Simplified
            scopes: self.scopes.clone(),
        }
    }
}

/// A compilation error with source location information
#[derive(Debug)]
pub(crate) struct CompileError {
    message: String,
    span: Span,
    note: Option<String>,
}

impl CompileError {
    fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
            note: None,
        }
    }

    fn with_note(mut self, note: impl Into<String>) -> Self {
        self.note = Some(note.into());
        self
    }

    /// Format the error with source context
    fn format(&self, source: &str, file_path: &str) -> String {
        let mut out = String::new();

        // Error header
        out.push_str(&format!("error: {}\n", self.message));

        // Location line
        out.push_str(&format!(
            "  --> {}:{}:{}\n",
            file_path, self.span.line, self.span.column
        ));

        // Get the source line
        if let Some(line) = source.lines().nth(self.span.line.saturating_sub(1)) {
            let line_num_width = self.span.line.to_string().len();

            // Blank line with separator
            out.push_str(&format!("{:width$} |\n", "", width = line_num_width));

            // Source line
            out.push_str(&format!("{} | {}\n", self.span.line, line));

            // Caret line pointing to the error
            let padding = " ".repeat(self.span.column.saturating_sub(1));
            let carets = "^".repeat(self.span.length.max(1));
            out.push_str(&format!(
                "{:width$} | {}{}\n",
                "",
                padding,
                carets,
                width = line_num_width
            ));
        }

        // Optional note
        if let Some(note) = &self.note {
            out.push_str(&format!("  = note: {}\n", note));
        }

        out
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} at line {}, column {}",
            self.message, self.span.line, self.span.column
        )
    }
}

impl std::error::Error for CompileError {}

/// Context for error reporting during compilation
pub(crate) struct CompileContext {
    source: String,
    file_path: String,
}

impl CompileContext {
    fn new(source: String, file_path: String) -> Self {
        Self { source, file_path }
    }

    fn error(&self, message: impl Into<String>, span: &Span) -> anyhow::Error {
        let err = CompileError::new(message, span.clone());
        anyhow::anyhow!("{}", err.format(&self.source, &self.file_path))
    }

    fn error_with_note(
        &self,
        message: impl Into<String>,
        span: &Span,
        note: impl Into<String>,
    ) -> anyhow::Error {
        let err = CompileError::new(message, span.clone()).with_note(note);
        anyhow::anyhow!("{}", err.format(&self.source, &self.file_path))
    }
}

#[derive(Debug)]
pub struct CompileArtifacts {
    pub wasm: PathBuf,
    /// Written only when requested (`--emit-wat`); a readable disassembly of the wasm.
    pub wat: Option<PathBuf>,
    /// Written only when requested (`--emit-pact`); a text view of the interface that
    /// is also embedded in the wasm.
    pub pact: Option<PathBuf>,
}

/// Which optional, human-readable views to write alongside the `.wasm`.
#[derive(Debug, Clone, Copy, Default)]
pub struct EmitOptions {
    pub wat: bool,
    pub pact: bool,
}

/// Splice `(include "path")` top-level forms in place, reading each referenced
/// file relative to the including file's directory. Runs before macro/generic
/// expansion, so included traits, macros, and functions are all visible. A file
/// is included at most once (keyed by canonical path), which also breaks cycles.
fn expand_includes(
    forms: Vec<SExpr>,
    base_dir: &Path,
    visited: &mut HashSet<PathBuf>,
    ctx: &CompileContext,
) -> Result<Vec<SExpr>> {
    let mut out = Vec::new();
    for form in forms {
        let is_include = matches!(&form,
            SExpr::List(items, _) if head_sym(items) == Some("include"));
        if !is_include {
            out.push(form);
            continue;
        }
        let (items, span) = match &form {
            SExpr::List(items, span) => (items, span),
            _ => unreachable!(),
        };
        let rel = match items.get(1) {
            Some(SExpr::Str(s, _)) if items.len() == 2 => s,
            _ => {
                return Err(ctx.error(
                    "include expects a string path: (include \"file.wisp\")",
                    span,
                ));
            }
        };
        let path = base_dir.join(rel);
        let canon = path.canonicalize().map_err(|e| {
            ctx.error(
                format!("include: cannot open '{}': {}", path.display(), e),
                span,
            )
        })?;
        if !visited.insert(canon.clone()) {
            continue; // already included
        }
        let inc_src = fs::read_to_string(&canon).map_err(|e| {
            ctx.error(
                format!("include: cannot read '{}': {}", canon.display(), e),
                span,
            )
        })?;
        let toks = tokenize(&inc_src);
        let mut inc_forms = Vec::new();
        let mut pos = 0;
        while pos < toks.len() {
            let (s, next) = parse_sexpr(&toks, pos);
            inc_forms.push(s);
            pos = next;
        }
        let inc_dir = canon
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or_else(|| base_dir.to_path_buf());
        out.extend(expand_includes(inc_forms, &inc_dir, visited, ctx)?);
    }
    Ok(out)
}

/// The shared front-end + middle of the pipeline: tokenize, parse, expand includes
/// and macros, derive, lower/monomorphize generics, then type-check. Produces the
/// typed `Program` (and its signatures) that either back-end consumes — `codegen`
/// (WAT/Pack) in `compile`, or the tree-walking `eval`.
pub(crate) fn analyze(
    src: &str,
    base_dir: &Path,
    visited: &mut HashSet<PathBuf>,
    ctx: &CompileContext,
) -> Result<(Program, HashMap<String, Signature>)> {
    let tokens = tokenize(src);
    let mut forms = Vec::new();
    let mut pos = 0;
    while pos < tokens.len() {
        let (sexpr, next) = parse_sexpr(&tokens, pos);
        forms.push(sexpr);
        pos = next;
    }
    if forms.is_empty() {
        bail!("no function definitions found in source");
    }
    let forms = expand_includes(forms, base_dir, visited, ctx)?;
    let macros = collect_macros(&forms);
    let expanded_forms = expand_all_macros(forms, &macros);
    let expanded_forms = expand_derives(expanded_forms, ctx)?;
    let expanded_forms = expand_generics(expanded_forms, ctx)?;
    let prog = parse_program(expanded_forms, ctx)?;
    let signatures = collect_signatures(&prog)?;
    type_check(&prog, &signatures, ctx)?;
    Ok((prog, signatures))
}

pub fn compile(source_path: &Path, out_base: &Path, emit: EmitOptions) -> Result<CompileArtifacts> {
    let src = fs::read_to_string(source_path)
        .with_context(|| format!("failed to read source file {}", source_path.display()))?;

    let file_path = source_path.display().to_string();
    let ctx = CompileContext::new(src.clone(), file_path);

    // Splice any `(include "...")` files before macro/generic expansion.
    let base_dir = source_path
        .parent()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| PathBuf::from("."));
    let mut visited = HashSet::new();
    if let Ok(c) = source_path.canonicalize() {
        visited.insert(c);
    }

    // The shared front + middle (parse -> expand -> lower -> type-check). Both
    // back-ends -- codegen below and the tree-walking `eval` -- consume its output.
    let (prog, signatures) = analyze(&src, &base_dir, &mut visited, &ctx)?;

    // Generate Pack-compatible WAT (raw module with Pack/Graph ABI). This text is
    // always produced because the wasm is built from it, but it is only *written*
    // when requested — it is a readable disassembly the wasm can regenerate.
    let wat = generate_wat_pack(&prog, &signatures);

    // Ensure the output directory (e.g. `compiled/`) exists.
    if let Some(dir) = out_base.parent().filter(|d| !d.as_os_str().is_empty()) {
        fs::create_dir_all(dir)
            .with_context(|| format!("failed to create output directory {}", dir.display()))?;
    }

    let mut wasm_path = out_base.to_path_buf();
    wasm_path.set_extension("wasm");

    // The wasm is the one true artifact; it also embeds the interface metadata.
    let wasm_bytes = parse_str(&wat).context("failed to convert generated WAT to wasm")?;
    fs::write(&wasm_path, &wasm_bytes)
        .with_context(|| format!("failed to write {}", wasm_path.display()))?;

    // Optional view: the readable WAT disassembly.
    let wat_path = if emit.wat {
        let mut p = out_base.to_path_buf();
        p.set_extension("wat");
        fs::write(&p, &wat).with_context(|| format!("failed to write {}", p.display()))?;
        Some(p)
    } else {
        None
    };

    // Optional view: the text interface (also embedded in the wasm as CGRF metadata).
    let pact_path = if emit.pact {
        let interface_name = source_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("wisp");
        let pact = generate_pact(&prog, interface_name);
        let mut p = out_base.to_path_buf();
        p.set_extension("pact");
        fs::write(&p, &pact).with_context(|| format!("failed to write {}", p.display()))?;
        Some(p)
    } else {
        None
    };

    Ok(CompileArtifacts {
        wasm: wasm_path,
        wat: wat_path,
        pact: pact_path,
    })
}

fn encode_component(
    module: &[u8],
    wit_source: &str,
    world_config: Option<&WorldConfig>,
    source_path: &Path,
) -> Result<Vec<u8>> {
    let mut resolve = Resolve::new();

    // If we have external WIT dependencies, load them first
    if let Some(config) = world_config
        && let Some(wit_deps) = &config.wit_deps
    {
        // Resolve wit_deps path relative to the source file
        let deps_path = if wit_deps.is_absolute() {
            wit_deps.clone()
        } else {
            source_path
                .parent()
                .unwrap_or(Path::new("."))
                .join(wit_deps)
        };

        if deps_path.exists() {
            // Load all WIT packages from the deps directory
            resolve
                .push_path(&deps_path)
                .with_context(|| format!("failed to load WIT deps from {}", deps_path.display()))?;
        } else {
            bail!("WIT deps path not found: {}", deps_path.display());
        }
    }

    // Parse our generated WIT (which may reference the loaded external packages)
    let pkg_id = resolve
        .push_str(Path::new("generated.wit"), wit_source)
        .context("failed to parse generated WIT")?;
    let world_id = resolve.packages[pkg_id]
        .worlds
        .values()
        .next()
        .cloned()
        .context("generated WIT is missing a world declaration")?;
    let mut module_with_metadata = module.to_vec();
    embed_component_metadata(
        &mut module_with_metadata,
        &resolve,
        world_id,
        StringEncoding::UTF8,
    )
    .context("failed to embed component metadata")?;
    let bytes = ComponentEncoder::default()
        .module(&module_with_metadata)
        .context("failed to prepare module for component encoding")?
        .validate(true)
        .encode()
        .context("failed to encode component")?;
    Ok(bytes)
}

#[derive(Debug, Clone)]
pub enum SExpr {
    Sym(String, Span),
    Int { value: i64, ty: Type, span: Span },
    Float { value: f64, ty: Type, span: Span },
    Str(String, Span), // String literal
    List(Vec<SExpr>, Span),
    Quasiquote(Box<SExpr>, Span),
    Unquote(Box<SExpr>, Span),
    UnquoteSplice(Box<SExpr>, Span),
    // Syntax object forms for syntax-case
    SyntaxQuote(Box<SExpr>, Span),    // #'expr - creates syntax object
    Quasisyntax(Box<SExpr>, Span),    // #`template - syntax template
    Unsyntax(Box<SExpr>, Span),       // #,expr - unquote in syntax
    UnsyntaxSplice(Box<SExpr>, Span), // #,@expr - splice in syntax
}

impl SExpr {
    fn span(&self) -> &Span {
        match self {
            SExpr::Sym(_, span) => span,
            SExpr::Int { span, .. } => span,
            SExpr::Float { span, .. } => span,
            SExpr::Str(_, span) => span,
            SExpr::List(_, span) => span,
            SExpr::Quasiquote(_, span) => span,
            SExpr::Unquote(_, span) => span,
            SExpr::UnquoteSplice(_, span) => span,
            SExpr::SyntaxQuote(_, span) => span,
            SExpr::Quasisyntax(_, span) => span,
            SExpr::Unsyntax(_, span) => span,
            SExpr::UnsyntaxSplice(_, span) => span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int {
        value: i64,
        ty: Type,
    },
    Float {
        value: f64,
        ty: Type,
    },
    StringLiteral(String),
    Ascribe {
        expr: Box<Expr>,
        ty: Type,
    },
    Var(String),
    Call {
        name: String,
        args: Vec<Expr>,
    },
    If {
        cond: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    Let {
        name: String,
        value: Box<Expr>,
        body: Box<Expr>,
        /// Substructural multiplicity of the binding: `Lin`/`Aff` when the value is
        /// a linear/affine resource (a linear variant, or an explicit `(lin T)`
        /// annotation), so the body must consume it exactly/at-most once.
        mult: Multiplicity,
    },
    Begin {
        exprs: Vec<Expr>,
    },
    WasmInstr {
        name: String,
        args: Vec<Expr>,
    },
    GlobalGet {
        name: String,
    },
    GlobalSet {
        name: String,
        value: Box<Expr>,
    },
    /// Construct a record: (point 10 20)
    RecordConstruct {
        record_name: String,
        fields: Vec<Expr>,
    },
    /// Access a record field: (record.field-name expr)
    RecordAccess {
        record_name: String,
        field_name: String,
        expr: Box<Expr>,
    },
    /// Construct a variant: (circle 5) or (point)
    VariantConstruct {
        variant_name: String,
        case_name: String,
        payload: Vec<Expr>,
    },
    /// Match on a variant: (match expr ((case1 vars...) body1) ...)
    Match {
        expr: Box<Expr>,
        cases: Vec<MatchArm>,
    },
    /// Option constructors
    Some {
        inner_type: Type,
        value: Box<Expr>,
    },
    None {
        inner_type: Type,
    },
    /// Result constructors
    Ok {
        ok_type: Type,
        err_type: Type,
        value: Box<Expr>,
    },
    Err {
        ok_type: Type,
        err_type: Type,
        value: Box<Expr>,
    },
    /// List operations
    ListNew {
        elem_type: Type,
    },
    ListPush {
        list: Box<Expr>,
        value: Box<Expr>,
    },
    ListGet {
        list: Box<Expr>,
        index: Box<Expr>,
    },
    ListLen {
        list: Box<Expr>,
    },
    /// String operations
    StringLen {
        string: Box<Expr>,
    },
    /// Get character at index: (string-ref s idx) -> s32
    StringRef {
        string: Box<Expr>,
        index: Box<Expr>,
    },
    /// Extract substring: (substring s start end) -> string
    Substring {
        string: Box<Expr>,
        start: Box<Expr>,
        end: Box<Expr>,
    },
    /// Concatenate strings: (string-append s1 s2) -> string
    StringAppend {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// String equality: (string=? s1 s2) -> s32
    StringEq {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// Create string from bytes: (string-from-bytes bytes) -> string
    StringFromBytes {
        bytes: Box<Expr>,
    },
    /// Construct a dynamic `any` from an s32: (any-s32 n) -> any
    AnyFromS32 {
        value: Box<Expr>,
    },
    /// Read the s32 payload of a dynamic `any`: (any-as-s32 x) -> s32
    AnyToS32 {
        value: Box<Expr>,
    },
    /// Construct a dynamic `any` from a string: (any-string s) -> any
    AnyFromString {
        value: Box<Expr>,
    },
    /// Read the string payload of a dynamic `any`: (any-as-string x) -> string
    AnyToString {
        value: Box<Expr>,
    },
    /// Allocate n bytes on the compiler heap: (heap-alloc n) -> s32 (pointer).
    HeapAlloc {
        size: Box<Expr>,
    },
    /// Reinterpret an `any` as the address of its [len:u32][cgrf] blob:
    /// (any-addr x) -> s32. The inverse of any-from-addr.
    AnyAddr {
        value: Box<Expr>,
    },
    /// Reinterpret a blob address as an `any`: (any-from-addr p) -> any. The
    /// address must point to a [len:u32][cgrf] buffer (e.g. built via heap-alloc).
    AnyFromAddr {
        value: Box<Expr>,
    },
    /// Reinterpret a string as the address of its [len:u32][bytes] buffer:
    /// (string-addr s) -> s32. Lets Wisp copy string bytes into a CGRF buffer.
    StringAddr {
        value: Box<Expr>,
    },
    /// Reinterpret an address as a string: (string-from-addr p) -> string. The
    /// address must point to a [len:u32][bytes] buffer (e.g. a CGRF string payload).
    StringFromAddr {
        value: Box<Expr>,
    },
    /// Invoke a host import's raw CGRF entry point with a pre-encoded args blob:
    /// `args-any` is a len-prefixed CGRF blob (built by `marshal`), passed straight
    /// to the import's raw symbol, and the returned CGRF is wrapped into an `any`.
    /// `module`+`import` name the import (e.g. "theater:simple/store"+"get"); the raw
    /// symbol is qualified by interface so same-named functions in different
    /// interfaces (store.exists vs filesystem.exists) don't collide.
    /// `(call-raw args)` is sugar for rpc.call; `(raw-invoke "iface" "name" args)`
    /// targets any declared import. This is the raw-CGRF calling convention: the
    /// import is declared with its real typed signature (so the interface hash
    /// matches Theater), but values cross as CGRF and are bridged by marshal/unmarshal.
    RawInvoke {
        module: String,
        import: String,
        value: Box<Expr>,
    },
    /// Convert string to bytes: (string-to-bytes string) -> list<u8>
    StringToBytes {
        string: Box<Expr>,
    },
    /// Construct a tuple: (tuple e1 e2 ...)
    TupleConstruct {
        values: Vec<Expr>,
    },
    /// Mint a capability for the duration of a scope: (with-cap (c Cap) body).
    /// `name` is bound to a fresh linear capability token of type `cap` (a resource
    /// name); `body` must consume it exactly once. Zero runtime cost: the token is
    /// an i32 witness and the form lowers to its body.
    WithCap {
        name: String,
        cap: String,
        body: Box<Expr>,
    },
    /// Consume (release) a capability token: (release-cap c) -> s32. The terminal
    /// consumer that discharges an owned capability's linear obligation (for caps
    /// obtained by transfer; a with-cap-bound cap is released by its scope).
    ReleaseCap {
        value: Box<Expr>,
    },
    /// Borrow a capability without consuming it: (& c) -> (borrow Cap). A borrow is
    /// unrestricted (usable any number of times) and does not count toward the
    /// capability's single consuming use. Zero runtime cost: the same i32 witness.
    BorrowCap {
        name: String,
    },
}

/// A single arm in a match expression
#[derive(Debug, Clone)]
pub struct MatchArm {
    case_name: String,
    bindings: Vec<String>, // Variable names to bind payload values
    /// Substructural multiplicity of each binding (parallel to `bindings`), taken
    /// from the matched case's payload. A linear payload binding must be consumed
    /// exactly once in the arm body — this is the type-state narrowing.
    binding_mult: Vec<Multiplicity>,
    body: Expr,
}

/// Expand a `_` wildcard arm into explicit arms for every case the concrete arms
/// don't cover — each with fresh, unused bindings matching that case's payload
/// arity and the wildcard's body. Non-wildcard matches pass through unchanged.
/// Both the type checker and codegen call this so they see the same arm set, and
/// the gnarly discriminant codegen needs no special wildcard handling.
fn expand_match_wildcard(
    cases: &[MatchArm],
    expr_ty: &Type,
    variants: &HashMap<String, VariantDef>,
) -> Vec<MatchArm> {
    let Some(wild) = cases.iter().find(|a| a.case_name == "_") else {
        return cases.to_vec();
    };
    let covered: std::collections::HashSet<&str> = cases
        .iter()
        .filter(|a| a.case_name != "_")
        .map(|a| a.case_name.as_str())
        .collect();
    let full: Vec<(String, usize)> = match expr_ty {
        Type::Option(_) => vec![("some".to_string(), 1), ("none".to_string(), 0)],
        Type::Result(_, _) => vec![("ok".to_string(), 1), ("err".to_string(), 1)],
        Type::Variant(name) => match variants.get(name) {
            Some(v) => v
                .cases
                .iter()
                .map(|c| (c.name.clone(), c.payload.len()))
                .collect(),
            None => return cases.to_vec(),
        },
        _ => return cases.to_vec(),
    };
    let mut out: Vec<MatchArm> = cases
        .iter()
        .filter(|a| a.case_name != "_")
        .cloned()
        .collect();
    for (name, arity) in full {
        if !covered.contains(name.as_str()) {
            let bindings: Vec<String> = (0..arity).map(|i| format!("_wild_{name}_{i}")).collect();
            let binding_mult = vec![Multiplicity::Un; bindings.len()];
            out.push(MatchArm {
                case_name: name,
                bindings,
                binding_mult,
                body: wild.body.clone(),
            });
        }
    }
    out
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Type,
    pub body: Expr,
}

/// A variable binding for hygiene tracking
#[derive(Debug, Clone)]
pub(crate) struct Binding {
    name: String,
    scopes: ScopeSet,
}

impl Binding {
    fn new(name: String, scopes: ScopeSet) -> Self {
        Self { name, scopes }
    }

    /// Check if this binding can be referenced by a reference with the given scopes.
    /// A binding is visible to a reference if the binding's scopes are a subset
    /// of the reference's scopes.
    fn is_visible_from(&self, ref_scopes: &ScopeSet) -> bool {
        self.scopes.is_subset_of(ref_scopes)
    }

    /// Get a unique mangled name that includes scope information.
    /// This ensures variables with the same name but different scopes
    /// are distinct in the generated code.
    fn mangled_name(&self) -> String {
        if self.scopes.scopes.len() <= 1 && self.scopes.scopes.contains(&0) {
            // Base scope only - no mangling needed
            self.name.clone()
        } else {
            // Include non-base scopes in the name
            let mut scope_ids: Vec<_> = self
                .scopes
                .scopes
                .iter()
                .filter(|&&s| s != 0)
                .cloned()
                .collect();
            scope_ids.sort();
            format!(
                "{}__hyg{}",
                self.name,
                scope_ids
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join("_")
            )
        }
    }
}

/// Substructural multiplicity — how many times a value may be used.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Multiplicity {
    /// Unrestricted: use any number of times (the default).
    Un,
    /// Affine: use at most once (may be dropped). `(aff T)`.
    Aff,
    /// Linear: use exactly once. `(lin T)` and capability types.
    Lin,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub ty: Type,
    scopes: ScopeSet,
    /// Multiplicity on the substructural axis: `(lin T)` is used exactly once,
    /// `(aff T)` at most once, unqualified is unrestricted.
    pub mult: Multiplicity,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub module: String,
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Type,
}

#[derive(Debug, Clone)]
pub struct Global {
    pub name: String,
    pub ty: Type,
    pub mutable: bool,
    pub init_value: i64, // For simplicity, we'll only support integer constants initially
}

/// A field in a record type
#[derive(Debug, Clone)]
pub struct RecordField {
    pub name: String,
    pub ty: Type,
}

/// A record type definition
#[derive(Debug, Clone)]
pub struct RecordDef {
    pub name: String,
    pub fields: Vec<RecordField>,
}

impl RecordDef {
    /// Calculate the size of this record in bytes
    fn size(&self) -> usize {
        self.fields.iter().map(|f| type_size(&f.ty)).sum()
    }

    /// Calculate the offset of a field by index
    fn field_offset(&self, index: usize) -> usize {
        self.fields[..index].iter().map(|f| type_size(&f.ty)).sum()
    }
}

/// A case in a variant type
#[derive(Debug, Clone)]
pub struct VariantCase {
    pub name: String,
    pub payload: Vec<Type>, // Can have 0, 1, or more payload types
    /// Substructural multiplicity of each payload (parallel to `payload`). A
    /// `(lin T)` / `(aff T)` payload makes the slot linear/affine; a variant with
    /// any such payload is itself a linear value (infectious linearity).
    pub payload_mult: Vec<Multiplicity>,
}

/// A variant type definition (sum type)
#[derive(Debug, Clone)]
pub struct VariantDef {
    pub name: String,
    pub cases: Vec<VariantCase>,
}

impl VariantDef {
    /// Calculate the size of this variant in bytes (discriminant + max payload)
    fn size(&self) -> usize {
        let discriminant_size = 4; // i32 discriminant
        let max_payload_size = self
            .cases
            .iter()
            .map(|c| c.payload.iter().map(type_size).sum::<usize>())
            .max()
            .unwrap_or(0);
        discriminant_size + max_payload_size
    }

    /// Find a case by name and return its index
    fn find_case(&self, name: &str) -> Option<(usize, &VariantCase)> {
        self.cases.iter().enumerate().find(|(_, c)| c.name == name)
    }
}

/// Find a variant definition that contains a case with the given name
fn find_variant_by_case<'a>(
    case_name: &str,
    variants: &'a HashMap<String, VariantDef>,
) -> Option<&'a VariantDef> {
    variants.values().find(|v| v.find_case(case_name).is_some())
}

/// The multiplicity a variant has *by its type*: `Lin` if any payload is linear,
/// else `Aff` if any is affine, else `Un`. A value of such a variant is infectiously
/// linear/affine (it can't be freely copied and re-matched).
fn variant_multiplicity(def: &VariantDef) -> Multiplicity {
    let mut m = Multiplicity::Un;
    for c in &def.cases {
        for pm in &c.payload_mult {
            match pm {
                Multiplicity::Lin => return Multiplicity::Lin,
                Multiplicity::Aff if m == Multiplicity::Un => m = Multiplicity::Aff,
                _ => {}
            }
        }
    }
    m
}

/// The multiplicity of a `let` value expression (for infectious linear `let`): a
/// direct constructor of a linear variant, or a call to a function returning one,
/// yields that variant's multiplicity. Everything else is unrestricted.
fn value_multiplicity(
    value: &SExpr,
    functions: &HashMap<String, Signature>,
    variants: &HashMap<String, VariantDef>,
) -> Multiplicity {
    let SExpr::List(items, _) = value else {
        return Multiplicity::Un;
    };
    let Some(head) = head_sym(items) else {
        return Multiplicity::Un;
    };
    if let Some(def) = find_variant_by_case(head, variants) {
        return variant_multiplicity(def);
    }
    if let Some(sig) = functions.get(head)
        && let Type::Variant(name) = &sig.result
        && let Some(def) = variants.get(name)
    {
        return variant_multiplicity(def);
    }
    Multiplicity::Un
}

/// A resource type definition (opaque handle managed externally)
#[derive(Debug, Clone)]
pub struct ResourceDef {
    pub name: String,
}

/// Get the size of a type in bytes (for memory layout)
fn type_size(ty: &Type) -> usize {
    match ty {
        Type::S32 | Type::F32 => 4,
        Type::S64 | Type::F64 => 8,
        Type::U8 => 4, // stored as i32 in Wisp memory; byte-packing only in list<u8> data arrays
        Type::Bool => 4, // i32 in Wisp memory (0/1); 1-byte only in CGRF payload
        Type::U16 | Type::U32 => 4, // i32 in Wisp memory; narrower only in CGRF payload
        Type::U64 => 8, // i64 in Wisp memory
        // Records, variants, options, results, lists, strings, and tuples are pointer-sized
        Type::Record(_)
        | Type::Variant(_)
        | Type::Option(_)
        | Type::Result(_, _)
        | Type::List(_)
        | Type::Str
        | Type::Tuple(_)
        | Type::Any => 4,
        // Resources and borrows are i32 handles
        Type::Resource(_) | Type::Borrow(_) => 4,
    }
}

/// Check if a type requires heap allocation
fn type_needs_heap(ty: &Type) -> bool {
    match ty {
        Type::S32
        | Type::S64
        | Type::F32
        | Type::F64
        | Type::U8
        | Type::Bool
        | Type::U16
        | Type::U32
        | Type::U64 => false,
        Type::Record(_)
        | Type::Variant(_)
        | Type::Option(_)
        | Type::Result(_, _)
        | Type::List(_)
        | Type::Str
        | Type::Tuple(_)
        | Type::Any => true,
        // Resources don't need heap - they're opaque handles managed externally
        Type::Resource(_) | Type::Borrow(_) => false,
    }
}

/// Estimate the CGRF node size for an element type (used for list buffer allocation).
/// Returns the approximate size in bytes of a single CGRF node for this type.
fn cgrf_element_node_size(ty: &Type) -> usize {
    match ty {
        // Scalars: node header (8) + payload (4 or 8)
        Type::S32 | Type::F32 | Type::U8 | Type::Bool | Type::U16 | Type::U32 => 12,
        Type::S64 | Type::F64 | Type::U64 => 16,
        // Strings: node header (8) + length (4) + average string data (~32)
        Type::Str => 44,
        // Lists: node header (8) + child indices (~16) + nested elements
        Type::List(inner) => 24 + cgrf_element_node_size(inner),
        // Options: node header (8) + presence (1) + optional child
        Type::Option(inner) => 16 + cgrf_element_node_size(inner),
        // Tuples: node header (8) + child indices
        Type::Tuple(elems) => {
            8 + 4 * elems.len() + elems.iter().map(cgrf_element_node_size).sum::<usize>()
        }
        // Records, variants, results: estimate conservatively
        Type::Record(_) | Type::Variant(_) | Type::Result(_, _) => 64,
        // Dynamic value: a self-contained CGRF blob; estimate conservatively
        Type::Any => 64,
        // Resources/borrows: just a handle
        Type::Resource(_) | Type::Borrow(_) => 12,
    }
}

/// Check if an expression uses heap allocation
fn expr_uses_heap(expr: &Expr) -> bool {
    match expr {
        Expr::Int { .. } | Expr::Float { .. } | Expr::Var(_) | Expr::GlobalGet { .. } => false,
        Expr::StringLiteral(_) => true,
        Expr::Ascribe { expr, .. } => expr_uses_heap(expr),
        Expr::Call { args, .. } => args.iter().any(expr_uses_heap),
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => expr_uses_heap(cond) || expr_uses_heap(then_branch) || expr_uses_heap(else_branch),
        Expr::Let { value, body, .. } => expr_uses_heap(value) || expr_uses_heap(body),
        Expr::Begin { exprs } => exprs.iter().any(expr_uses_heap),
        Expr::WasmInstr { args, .. } => args.iter().any(expr_uses_heap),
        Expr::GlobalSet { value, .. } => expr_uses_heap(value),
        Expr::RecordConstruct { .. } => true, // records need heap
        Expr::RecordAccess { expr, .. } => expr_uses_heap(expr),
        Expr::VariantConstruct { .. } => true, // variants need heap
        Expr::Match { expr, cases } => {
            expr_uses_heap(expr) || cases.iter().any(|c| expr_uses_heap(&c.body))
        }
        Expr::Some { .. } | Expr::None { .. } => true,
        Expr::Ok { .. } | Expr::Err { .. } => true,
        Expr::ListNew { .. } | Expr::ListPush { .. } => true,
        Expr::ListGet { list, index } => expr_uses_heap(list) || expr_uses_heap(index),
        Expr::ListLen { list } => expr_uses_heap(list),
        Expr::StringLen { string } => expr_uses_heap(string),
        Expr::StringRef { string, index } => expr_uses_heap(string) || expr_uses_heap(index),
        Expr::AnyFromS32 { .. } | Expr::AnyFromString { .. } => true, // allocate a CGRF blob
        Expr::AnyToS32 { value } | Expr::AnyToString { value } => expr_uses_heap(value),
        Expr::HeapAlloc { .. } => true, // allocates
        Expr::AnyAddr { value }
        | Expr::AnyFromAddr { value }
        | Expr::StringAddr { value }
        | Expr::StringFromAddr { value } => expr_uses_heap(value),
        Expr::RawInvoke { .. } => true, // allocates the result blob
        Expr::Substring { .. }
        | Expr::StringAppend { .. }
        | Expr::StringFromBytes { .. }
        | Expr::StringToBytes { .. } => true, // allocate new strings/lists
        Expr::StringEq { left, right } => expr_uses_heap(left) || expr_uses_heap(right),
        Expr::TupleConstruct { .. } => true,
        // A capability witness is an i32 constant; with-cap/release-cap only touch
        // the heap if their body/value does; a borrow is a bare local.get.
        Expr::WithCap { body, .. } => expr_uses_heap(body),
        Expr::ReleaseCap { value } => expr_uses_heap(value),
        Expr::BorrowCap { .. } => false,
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Macro {
    name: String,
    params: Vec<String>,
    template: SExpr,
}

/// Pattern for syntax-rules matching
#[derive(Debug, Clone)]
pub(crate) enum Pattern {
    /// Pattern variable (matches anything, binds to name)
    Variable(String),
    /// Literal symbol (matches exactly this symbol)
    Literal(String),
    /// Wildcard _ (matches anything, doesn't bind)
    Wildcard,
    /// List pattern without ellipsis
    List(Vec<Pattern>),
    /// List pattern with ellipsis: (p1 p2 ... pN pN+1)
    /// before: patterns before the repeated element
    /// repeated: the pattern that repeats (before ...)
    /// after: patterns after the ...
    ListWithEllipsis {
        before: Vec<Pattern>,
        repeated: Box<Pattern>,
        after: Vec<Pattern>,
    },
}

/// Template for syntax-rules expansion
#[derive(Debug, Clone)]
pub(crate) enum Template {
    /// Pattern variable reference
    Variable(String),
    /// Literal symbol (not a pattern variable)
    Symbol(String),
    /// Literal number or other atom
    Atom(SExpr),
    /// List template without ellipsis
    List(Vec<Template>),
    /// Element followed by ellipsis: t ...
    /// This expands the template for each value in the binding
    Ellipsis(Box<Template>),
}

/// Binding from pattern matching - either single value or list (from ellipsis)
#[derive(Debug, Clone)]
pub(crate) enum PatternBinding {
    Single(SExpr),
    List(Vec<SExpr>),
}

/// A single syntax-rules rule (pattern -> template)
#[derive(Debug, Clone)]
pub(crate) struct SyntaxRule {
    pattern: Pattern,
    template: Template,
}

/// A syntax-rules macro definition
#[derive(Debug, Clone)]
pub(crate) struct SyntaxRulesMacro {
    name: String,
    literals: Vec<String>,
    rules: Vec<SyntaxRule>,
}

/// A syntax-case clause with optional guard
#[derive(Debug, Clone)]
pub(crate) struct SyntaxCaseClause {
    pattern: Pattern,
    guard: Option<CompileTimeExpr>,
    template: CompileTimeExpr,
}

/// A syntax-case macro definition (syntax-case-lambda)
#[derive(Debug, Clone)]
pub(crate) struct SyntaxCaseMacro {
    name: String,
    _param: String, // Reserved for the syntax-case input binding
    literals: Vec<String>,
    clauses: Vec<SyntaxCaseClause>,
}

/// Expressions evaluated at compile time (for syntax-case macros)
#[derive(Debug, Clone)]
pub(crate) enum CompileTimeExpr {
    /// A quoted syntax object: #'expr
    Syntax(SExpr),
    /// A quasisyntax template: #`template with #, and #,@
    Quasisyntax(SExpr),
    /// Reference to a pattern binding or macro parameter
    Var(String),
    /// Function application: (func args...)
    App {
        func: String,
        args: Vec<CompileTimeExpr>,
    },
    /// Conditional: (if cond then else)
    If {
        cond: Box<CompileTimeExpr>,
        then_branch: Box<CompileTimeExpr>,
        else_branch: Box<CompileTimeExpr>,
    },
    /// Let binding: (let (name value) body)
    Let {
        name: String,
        value: Box<CompileTimeExpr>,
        body: Box<CompileTimeExpr>,
    },
    /// Literal value (number, boolean)
    Literal(SExpr),
}

/// Result of compile-time evaluation
#[derive(Debug, Clone)]
pub(crate) enum CompileTimeValue {
    Syntax(SExpr),
    Bool(bool),
    Int(i64),
    List(Vec<CompileTimeValue>),
}

pub(crate) struct PendingFunction {
    name: String,
    params: Vec<Parameter>,
    return_type: Type,
    body: SExpr,
    span: Span,
}

/// External WIT interface reference (e.g., "theater:simple/runtime")
#[derive(Debug, Clone)]
pub struct ExternalInterface {
    pub package: String,   // e.g., "theater:simple"
    pub interface: String, // e.g., "runtime"
}

impl ExternalInterface {
    fn parse(s: &str) -> Option<Self> {
        // Parse "package:namespace/interface" format
        let parts: Vec<&str> = s.split('/').collect();
        if parts.len() == 2 {
            Some(ExternalInterface {
                package: parts[0].to_string(),
                interface: parts[1].to_string(),
            })
        } else {
            None
        }
    }

    fn to_wit_ref(&self) -> String {
        format!("{}/{}", self.package, self.interface)
    }
}

/// World configuration for external WIT
#[derive(Debug, Clone, Default)]
pub struct WorldConfig {
    pub name: String,
    pub wit_deps: Option<PathBuf>, // Path to wit deps directory
    pub external_imports: Vec<ExternalInterface>, // e.g., theater:simple/runtime
    pub external_exports: Vec<ExternalInterface>, // e.g., theater:simple/actor
}

/// An exported function, possibly with an alias name.
#[derive(Debug, Clone)]
pub struct ExportDef {
    /// The name this function is exported as (may differ from func_name for aliased exports)
    pub export_name: String,
    /// The internal function name
    pub func_name: String,
}

impl ExportDef {
    fn simple(name: String) -> Self {
        ExportDef {
            export_name: name.clone(),
            func_name: name,
        }
    }

    fn aliased(export_name: String, func_name: String) -> Self {
        ExportDef {
            export_name,
            func_name,
        }
    }
}

#[derive(Debug, Clone)]
pub struct DataSegment {
    pub offset: i32,
    pub bytes: Vec<u8>,
}

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
    pub imports: Vec<Import>,
    pub exports: Vec<ExportDef>,
    pub globals: Vec<Global>,
    pub records: Vec<RecordDef>,
    pub variants: Vec<VariantDef>,
    pub resources: Vec<ResourceDef>,
    /// Names of declared capability types (a subset of resource names). Bindings
    /// of these types are linear and may only be minted by `with-cap`.
    pub capabilities: HashSet<String>,
    pub world_config: Option<WorldConfig>,
    pub data_segments: Vec<DataSegment>,
}

#[derive(Debug, Clone)]
pub(crate) struct Signature {
    params: Vec<Type>,
    result: Type,
}

pub(crate) struct WasmInstrInfo {
    params: Vec<Type>,
    result: Type,
}

fn lookup_wasm_instr(name: &str) -> Option<WasmInstrInfo> {
    // Arithmetic instructions
    match name {
        // i32 arithmetic
        "i32.add" | "i32.sub" | "i32.mul" | "i32.div_s" | "i32.div_u" | "i32.rem_s"
        | "i32.rem_u" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S32],
            result: Type::S32,
        }),
        // i64 arithmetic
        "i64.add" | "i64.sub" | "i64.mul" | "i64.div_s" | "i64.div_u" | "i64.rem_s"
        | "i64.rem_u" => Some(WasmInstrInfo {
            params: vec![Type::S64, Type::S64],
            result: Type::S64,
        }),
        // f32 arithmetic
        "f32.add" | "f32.sub" | "f32.mul" | "f32.div" => Some(WasmInstrInfo {
            params: vec![Type::F32, Type::F32],
            result: Type::F32,
        }),
        // f64 arithmetic
        "f64.add" | "f64.sub" | "f64.mul" | "f64.div" => Some(WasmInstrInfo {
            params: vec![Type::F64, Type::F64],
            result: Type::F64,
        }),

        // i32 bitwise operations
        "i32.and" | "i32.or" | "i32.xor" | "i32.shl" | "i32.shr_s" | "i32.shr_u" | "i32.rotl"
        | "i32.rotr" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S32],
            result: Type::S32,
        }),
        // i64 bitwise operations
        "i64.and" | "i64.or" | "i64.xor" | "i64.shl" | "i64.shr_s" | "i64.shr_u" | "i64.rotl"
        | "i64.rotr" => Some(WasmInstrInfo {
            params: vec![Type::S64, Type::S64],
            result: Type::S64,
        }),

        // i32 comparisons (return i32)
        "i32.eq" | "i32.ne" | "i32.lt_s" | "i32.lt_u" | "i32.gt_s" | "i32.gt_u" | "i32.le_s"
        | "i32.le_u" | "i32.ge_s" | "i32.ge_u" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S32],
            result: Type::S32,
        }),
        // i64 comparisons (return i32)
        "i64.eq" | "i64.ne" | "i64.lt_s" | "i64.lt_u" | "i64.gt_s" | "i64.gt_u" | "i64.le_s"
        | "i64.le_u" | "i64.ge_s" | "i64.ge_u" => Some(WasmInstrInfo {
            params: vec![Type::S64, Type::S64],
            result: Type::S32,
        }),
        // f32 comparisons (return i32)
        "f32.eq" | "f32.ne" | "f32.lt" | "f32.gt" | "f32.le" | "f32.ge" => Some(WasmInstrInfo {
            params: vec![Type::F32, Type::F32],
            result: Type::S32,
        }),
        // f64 comparisons (return i32)
        "f64.eq" | "f64.ne" | "f64.lt" | "f64.gt" | "f64.le" | "f64.ge" => Some(WasmInstrInfo {
            params: vec![Type::F64, Type::F64],
            result: Type::S32,
        }),

        // Constants (0 params, return typed value)
        "i32.const" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S32,
        }),
        "i64.const" => Some(WasmInstrInfo {
            params: vec![Type::S64],
            result: Type::S64,
        }),
        "f32.const" => Some(WasmInstrInfo {
            params: vec![Type::F32],
            result: Type::F32,
        }),
        "f64.const" => Some(WasmInstrInfo {
            params: vec![Type::F64],
            result: Type::F64,
        }),

        // Type conversions
        "i32.wrap_i64" => Some(WasmInstrInfo {
            params: vec![Type::S64],
            result: Type::S32,
        }),
        "i64.extend_i32_s" | "i64.extend_i32_u" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S64,
        }),
        "f32.demote_f64" => Some(WasmInstrInfo {
            params: vec![Type::F64],
            result: Type::F32,
        }),
        "f64.promote_f32" => Some(WasmInstrInfo {
            params: vec![Type::F32],
            result: Type::F64,
        }),
        "i32.trunc_f32_s" | "i32.trunc_f32_u" => Some(WasmInstrInfo {
            params: vec![Type::F32],
            result: Type::S32,
        }),
        "i32.trunc_f64_s" | "i32.trunc_f64_u" => Some(WasmInstrInfo {
            params: vec![Type::F64],
            result: Type::S32,
        }),
        "i64.trunc_f32_s" | "i64.trunc_f32_u" => Some(WasmInstrInfo {
            params: vec![Type::F32],
            result: Type::S64,
        }),
        "i64.trunc_f64_s" | "i64.trunc_f64_u" => Some(WasmInstrInfo {
            params: vec![Type::F64],
            result: Type::S64,
        }),
        "f32.convert_i32_s" | "f32.convert_i32_u" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::F32,
        }),
        "f32.convert_i64_s" | "f32.convert_i64_u" => Some(WasmInstrInfo {
            params: vec![Type::S64],
            result: Type::F32,
        }),
        "f64.convert_i32_s" | "f64.convert_i32_u" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::F64,
        }),
        "f64.convert_i64_s" | "f64.convert_i64_u" => Some(WasmInstrInfo {
            params: vec![Type::S64],
            result: Type::F64,
        }),

        // Memory operations
        "memory.size" => Some(WasmInstrInfo {
            params: vec![],
            result: Type::S32,
        }),
        "memory.grow" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S32,
        }),

        // Load instructions (address -> value)
        "i32.load" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S32,
        }),
        "i64.load" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S64,
        }),
        "f32.load" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::F32,
        }),
        "f64.load" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::F64,
        }),

        // Store instructions (address, value -> value)
        // Note: In WASM, stores don't return values, but for our expression-based
        // language we make them return the value that was stored for composability
        "i32.store" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S32],
            result: Type::S32,
        }),
        "i64.store" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S64],
            result: Type::S64,
        }),
        "f32.store" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::F32],
            result: Type::F32,
        }),
        "f64.store" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::F64],
            result: Type::F64,
        }),

        // Byte-level load operations
        "i32.load8_s" | "i32.load8_u" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S32,
        }),
        "i32.load16_s" | "i32.load16_u" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S32,
        }),
        "i64.load8_s" | "i64.load8_u" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S64,
        }),
        "i64.load16_s" | "i64.load16_u" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S64,
        }),
        "i64.load32_s" | "i64.load32_u" => Some(WasmInstrInfo {
            params: vec![Type::S32],
            result: Type::S64,
        }),

        // Byte-level store operations
        "i32.store8" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S32],
            result: Type::S32,
        }),
        "i32.store16" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S32],
            result: Type::S32,
        }),
        "i64.store8" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S64],
            result: Type::S64,
        }),
        "i64.store16" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S64],
            result: Type::S64,
        }),
        "i64.store32" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S64],
            result: Type::S64,
        }),

        _ => None,
    }
}
