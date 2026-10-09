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
struct ScopeSet {
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
struct CompileError {
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
struct CompileContext {
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

pub fn compile(source_path: &Path, out_base: &Path, emit: EmitOptions) -> Result<CompileArtifacts> {
    let src = fs::read_to_string(source_path)
        .with_context(|| format!("failed to read source file {}", source_path.display()))?;

    let file_path = source_path.display().to_string();
    let ctx = CompileContext::new(src.clone(), file_path);

    let tokens = tokenize(&src);
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

    // Splice any `(include "...")` files before macro/generic expansion.
    let base_dir = source_path
        .parent()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| PathBuf::from("."));
    let mut visited = HashSet::new();
    if let Ok(c) = source_path.canonicalize() {
        visited.insert(c);
    }
    let forms = expand_includes(forms, &base_dir, &mut visited, &ctx)?;

    // Collect macro definitions (both defmacro and define-syntax) and expand macros
    let macros = collect_macros(&forms);
    let expanded_forms = expand_all_macros(forms, &macros);

    // Compile-time deriving: `(derive Trait Type)` -> a generated trait instance.
    let expanded_forms = expand_derives(expanded_forms, &ctx)?;

    // Lower traits / instances / generics to plain monomorphic forms.
    let expanded_forms = expand_generics(expanded_forms, &ctx)?;

    let prog = parse_program(expanded_forms, &ctx)?;
    let signatures = collect_signatures(&prog)?;
    type_check(&prog, &signatures, &ctx)?;

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
struct Binding {
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
struct Macro {
    name: String,
    params: Vec<String>,
    template: SExpr,
}

/// Pattern for syntax-rules matching
#[derive(Debug, Clone)]
enum Pattern {
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
enum Template {
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
enum PatternBinding {
    Single(SExpr),
    List(Vec<SExpr>),
}

/// A single syntax-rules rule (pattern -> template)
#[derive(Debug, Clone)]
struct SyntaxRule {
    pattern: Pattern,
    template: Template,
}

/// A syntax-rules macro definition
#[derive(Debug, Clone)]
struct SyntaxRulesMacro {
    name: String,
    literals: Vec<String>,
    rules: Vec<SyntaxRule>,
}

/// A syntax-case clause with optional guard
#[derive(Debug, Clone)]
struct SyntaxCaseClause {
    pattern: Pattern,
    guard: Option<CompileTimeExpr>,
    template: CompileTimeExpr,
}

/// A syntax-case macro definition (syntax-case-lambda)
#[derive(Debug, Clone)]
struct SyntaxCaseMacro {
    name: String,
    _param: String, // Reserved for the syntax-case input binding
    literals: Vec<String>,
    clauses: Vec<SyntaxCaseClause>,
}

/// Expressions evaluated at compile time (for syntax-case macros)
#[derive(Debug, Clone)]
enum CompileTimeExpr {
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
enum CompileTimeValue {
    Syntax(SExpr),
    Bool(bool),
    Int(i64),
    List(Vec<CompileTimeValue>),
}

struct PendingFunction {
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
struct Signature {
    params: Vec<Type>,
    result: Type,
}

struct WasmInstrInfo {
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

fn type_check(
    prog: &Program,
    signatures: &HashMap<String, Signature>,
    _ctx: &CompileContext,
) -> Result<()> {
    // Build global type map
    let mut globals_map = HashMap::new();
    for global in &prog.globals {
        globals_map.insert(global.name.clone(), (global.ty.clone(), global.mutable));
    }

    // Build records and variants maps for type checking
    let records_map: HashMap<String, RecordDef> = prog
        .records
        .iter()
        .map(|r| (r.name.clone(), r.clone()))
        .collect();
    let variants_map: HashMap<String, VariantDef> = prog
        .variants
        .iter()
        .map(|v| (v.name.clone(), v.clone()))
        .collect();

    // A variant is linear (or affine) by its type if any payload slot is — so a
    // value holding a linear resource cannot be copied and re-matched. `Lin`
    // dominates `Aff` dominates `Un`.
    let variant_mult: HashMap<String, Multiplicity> = prog
        .variants
        .iter()
        .map(|v| (v.name.clone(), variant_multiplicity(v)))
        .collect();

    for func in &prog.functions {
        let mut env = HashMap::new();
        for param in &func.params {
            env.insert(param.name.clone(), param.ty.clone());
        }
        let body_ty = check_expr(
            &func.body,
            &env,
            signatures,
            &globals_map,
            &records_map,
            &variants_map,
        )?;
        if body_ty != func.return_type {
            bail!(
                "function '{}' returns {:?} but body has type {:?}",
                func.name,
                func.return_type,
                body_ty
            );
        }
        // A borrow is transient and tied to its source; it must not escape by being
        // returned (that would outlive the borrowed capability).
        if matches!(func.return_type, Type::Borrow(_)) {
            bail!("function '{}' cannot return a borrow", func.name);
        }
        check_fn_linearity(func, &prog.capabilities, &variant_mult)?;
    }
    Ok(())
}

/// The direct sub-expressions of an expression. Exhaustive so that adding a new
/// `Expr` variant forces a decision here (the linearity use-count must see every
/// value position, or it would undercount and admit a double-use).
fn expr_children(e: &Expr) -> Vec<&Expr> {
    match e {
        Expr::Int { .. }
        | Expr::Float { .. }
        | Expr::StringLiteral(_)
        | Expr::Var(_)
        | Expr::GlobalGet { .. }
        | Expr::None { .. }
        | Expr::ListNew { .. } => vec![],
        Expr::Ascribe { expr, .. }
        | Expr::RecordAccess { expr, .. }
        | Expr::ListLen { list: expr }
        | Expr::StringLen { string: expr }
        | Expr::StringFromBytes { bytes: expr }
        | Expr::StringToBytes { string: expr }
        | Expr::AnyFromS32 { value: expr }
        | Expr::AnyToS32 { value: expr }
        | Expr::AnyFromString { value: expr }
        | Expr::AnyToString { value: expr }
        | Expr::HeapAlloc { size: expr }
        | Expr::AnyAddr { value: expr }
        | Expr::AnyFromAddr { value: expr }
        | Expr::StringAddr { value: expr }
        | Expr::StringFromAddr { value: expr }
        | Expr::RawInvoke { value: expr, .. }
        | Expr::GlobalSet { value: expr, .. }
        | Expr::Some { value: expr, .. }
        | Expr::Ok { value: expr, .. }
        | Expr::ReleaseCap { value: expr }
        | Expr::Err { value: expr, .. } => vec![expr],
        Expr::Call { args, .. }
        | Expr::WasmInstr { args, .. }
        | Expr::VariantConstruct { payload: args, .. }
        | Expr::RecordConstruct { fields: args, .. }
        | Expr::Begin { exprs: args }
        | Expr::TupleConstruct { values: args } => args.iter().collect(),
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => vec![cond, then_branch, else_branch],
        Expr::Let { value, body, .. } => vec![value, body],
        Expr::WithCap { body, .. } => vec![body],
        // A borrow references a name, not a sub-expression; it never counts as a
        // consuming use (that is the whole point), so it has no children here.
        Expr::BorrowCap { .. } => vec![],
        Expr::Match { expr, cases } => {
            let mut v = vec![expr.as_ref()];
            v.extend(cases.iter().map(|a| &a.body));
            v
        }
        Expr::ListPush { list, value } => vec![list, value],
        Expr::ListGet { list, index } => vec![list, index],
        Expr::StringRef { string, index } => vec![string, index],
        Expr::Substring { string, start, end } => vec![string, start, end],
        Expr::StringAppend { left, right } | Expr::StringEq { left, right } => vec![left, right],
    }
}

/// Count how many times each tracked (linear or affine) binding is referenced in
/// `e`. `if`/`match` branches are combined per multiplicity: a *linear* value must
/// be used identically on every path (its total is otherwise undefined), while an
/// *affine* value may be used on some paths and dropped on others (per-path max).
/// Only names in `tracked` are counted (`match`/`let`-bound names are unrestricted).
fn linear_uses(
    e: &Expr,
    tracked: &HashMap<String, Multiplicity>,
) -> Result<HashMap<String, usize>> {
    fn merge_sum(
        mut a: HashMap<String, usize>,
        b: &HashMap<String, usize>,
    ) -> HashMap<String, usize> {
        for (k, v) in b {
            *a.entry(k.clone()).or_insert(0) += v;
        }
        a
    }
    fn merge_branches(
        a: &HashMap<String, usize>,
        b: &HashMap<String, usize>,
        tracked: &HashMap<String, Multiplicity>,
    ) -> Result<HashMap<String, usize>> {
        let mut out = HashMap::new();
        for (name, mult) in tracked {
            let ca = a.get(name).copied().unwrap_or(0);
            let cb = b.get(name).copied().unwrap_or(0);
            let merged = match mult {
                Multiplicity::Lin => {
                    if ca != cb {
                        bail!(
                            "linear value '{}' is used {} time(s) on one branch but {} on another; it must be consumed the same way on every path",
                            name,
                            ca,
                            cb
                        );
                    }
                    ca
                }
                // Affine (and unrestricted) take the worst-case path.
                Multiplicity::Aff | Multiplicity::Un => ca.max(cb),
            };
            if merged > 0 {
                out.insert(name.clone(), merged);
            }
        }
        Ok(out)
    }
    match e {
        Expr::Var(name) => {
            let mut m = HashMap::new();
            if tracked.contains_key(name) {
                m.insert(name.clone(), 1);
            }
            Ok(m)
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let c = linear_uses(cond, tracked)?;
            let t = linear_uses(then_branch, tracked)?;
            let f = linear_uses(else_branch, tracked)?;
            Ok(merge_sum(c, &merge_branches(&t, &f, tracked)?))
        }
        Expr::Match { expr, cases } => {
            // The scrutinee is consumed here (a linear scrutinee counts as one use).
            let acc = linear_uses(expr, tracked)?;
            // Each arm's linear/affine payload bindings are local obligations: they
            // are tracked within the arm, enforced (exactly/at-most once), and then
            // removed before the arm's outer-binding uses are branch-merged. This is
            // the type-state narrowing — a linear resource extracted by the match
            // cannot be dropped or duplicated in the arm.
            let mut merged: Option<HashMap<String, usize>> = None;
            for arm in cases {
                let mut inner = tracked.clone();
                for (b, m) in arm.bindings.iter().zip(&arm.binding_mult) {
                    if *m != Multiplicity::Un {
                        inner.insert(b.clone(), *m);
                    }
                }
                let mut u = linear_uses(&arm.body, &inner)?;
                for (b, m) in arm.bindings.iter().zip(&arm.binding_mult) {
                    let c = u.get(b).copied().unwrap_or(0);
                    match m {
                        Multiplicity::Lin if c != 1 => bail!(
                            "match binding '{}' is linear and must be used exactly once (used {} time(s))",
                            b,
                            c
                        ),
                        Multiplicity::Aff if c > 1 => bail!(
                            "match binding '{}' is affine and must be used at most once (used {} time(s))",
                            b,
                            c
                        ),
                        _ => {}
                    }
                    u.remove(b);
                }
                merged = Some(match merged {
                    None => u,
                    Some(prev) => merge_branches(&prev, &u, tracked)?,
                });
            }
            Ok(merge_sum(acc, &merged.unwrap_or_default()))
        }
        Expr::WithCap { name, body, .. } => {
            // RAII: the scope owns the capability and releases it at the end, so the
            // body may only *borrow* it (via `(& c)`, which does not count) — never
            // consume it by value. A by-value use would be a second release and, worse,
            // releasing early then borrowing would be use-after-release; zero by-value
            // uses keeps the scope's release strictly last, which is sound. The cap
            // binding is linear; outer bindings still count toward their obligations.
            let mut inner = tracked.clone();
            inner.insert(name.clone(), Multiplicity::Lin);
            let mut uses = linear_uses(body, &inner)?;
            let by_value = uses.get(name).copied().unwrap_or(0);
            if by_value > 0 {
                bail!(
                    "capability '{}' may only be borrowed (& {}) inside with-cap, not consumed; the scope releases it (used by value {} time(s))",
                    name,
                    name,
                    by_value
                );
            }
            uses.remove(name);
            Ok(uses)
        }
        Expr::Let {
            name,
            value,
            body,
            mult,
        } => {
            // The value may itself consume outer linear bindings. A linear/affine
            // binding is a local obligation: tracked in the body, enforced, then
            // removed before returning the body's outer-binding uses.
            let v = linear_uses(value, tracked)?;
            let mut inner = tracked.clone();
            if *mult != Multiplicity::Un {
                inner.insert(name.clone(), *mult);
            }
            let mut b = linear_uses(body, &inner)?;
            if *mult != Multiplicity::Un {
                let c = b.get(name).copied().unwrap_or(0);
                match mult {
                    Multiplicity::Lin if c != 1 => bail!(
                        "linear binding '{}' must be used exactly once (used {} time(s))",
                        name,
                        c
                    ),
                    Multiplicity::Aff if c > 1 => bail!(
                        "affine binding '{}' must be used at most once (used {} time(s))",
                        name,
                        c
                    ),
                    _ => {}
                }
                b.remove(name);
            }
            Ok(merge_sum(v, &b))
        }
        _ => {
            let mut acc = HashMap::new();
            for child in expr_children(e) {
                acc = merge_sum(acc, &linear_uses(child, tracked)?);
            }
            Ok(acc)
        }
    }
}

/// Confirm every `with-cap` names a declared capability. Walks the whole body.
fn validate_cap_names(e: &Expr, capabilities: &HashSet<String>) -> Result<()> {
    if let Expr::WithCap { cap, .. } = e
        && !capabilities.contains(cap)
    {
        bail!(
            "unknown capability '{}'; declare it with (capability {})",
            cap,
            cap
        );
    }
    for child in expr_children(e) {
        validate_cap_names(child, capabilities)?;
    }
    Ok(())
}

/// Enforce the substructural contract of a function: each `(lin T)` parameter (and
/// every capability parameter, and every `with-cap` binding) must be consumed
/// exactly once, and each `(aff T)` parameter at most once, counting `if`/`match`
/// branches per multiplicity.
fn check_fn_linearity(
    func: &Function,
    capabilities: &HashSet<String>,
    variant_mult: &HashMap<String, Multiplicity>,
) -> Result<()> {
    validate_cap_names(&func.body, capabilities)?;
    // A capability-typed parameter is linear by its type; a variant holding a
    // linear/affine payload is linear/affine by its type (infectious linearity, so
    // it can't be copied and re-matched to extract the resource twice); otherwise
    // the declared `(lin/aff T)` multiplicity applies.
    let is_cap =
        |p: &Parameter| matches!(&p.ty, Type::Resource(name) if capabilities.contains(name));
    let mult_of = |p: &Parameter| -> Multiplicity {
        if is_cap(p) {
            return Multiplicity::Lin;
        }
        if let Type::Variant(name) = &p.ty
            && let Some(m) = variant_mult.get(name)
            && *m != Multiplicity::Un
        {
            // The declared qualifier can only strengthen the inherent one.
            return if p.mult == Multiplicity::Lin {
                Multiplicity::Lin
            } else {
                *m
            };
        }
        p.mult
    };
    let tracked: HashMap<String, Multiplicity> = func
        .params
        .iter()
        .map(|p| (p.name.clone(), mult_of(p)))
        .filter(|(_, m)| *m != Multiplicity::Un)
        .collect();
    // Always walk the body: it may contain `with-cap` bindings even when no
    // parameter is tracked (linear_uses enforces their obligations too).
    let uses = linear_uses(&func.body, &tracked)?;
    for p in &func.params {
        let count = uses.get(&p.name).copied().unwrap_or(0);
        match mult_of(p) {
            Multiplicity::Un => {}
            Multiplicity::Lin => {
                let noun = if is_cap(p) {
                    "capability parameter"
                } else {
                    "linear parameter"
                };
                match count {
                    1 => {}
                    0 => bail!(
                        "{} '{}' of function '{}' is never used; it must be consumed exactly once",
                        noun,
                        p.name,
                        func.name
                    ),
                    n => bail!(
                        "{} '{}' of function '{}' is used {} times but must be used exactly once",
                        noun,
                        p.name,
                        func.name,
                        n
                    ),
                }
            }
            Multiplicity::Aff => {
                if count > 1 {
                    bail!(
                        "affine parameter '{}' of function '{}' is used {} times but must be used at most once",
                        p.name,
                        func.name,
                        count
                    );
                }
            }
        }
    }
    Ok(())
}

fn collect_signatures(prog: &Program) -> Result<HashMap<String, Signature>> {
    let mut signatures = HashMap::new();
    for func in &prog.functions {
        let params = func.params.iter().map(|p| p.ty.clone()).collect();
        let sig = Signature {
            params,
            result: func.return_type.clone(),
        };
        if signatures.insert(func.name.clone(), sig).is_some() {
            bail!("Duplicate function '{}'", func.name);
        }
    }
    for import in &prog.imports {
        let params = import.params.iter().map(|p| p.ty.clone()).collect();
        let sig = Signature {
            params,
            result: import.return_type.clone(),
        };
        // Imports are keyed by bare name only for typed-call resolution, which
        // raw-invoked imports never use. Same-named functions in different
        // interfaces (store.exists / filesystem.exists) are allowed; keep the
        // first signature (its typed wrapper is the one that survives dedup).
        signatures.entry(import.name.clone()).or_insert(sig);
    }
    Ok(signatures)
}

fn check_expr(
    expr: &Expr,
    env: &HashMap<String, Type>,
    signatures: &HashMap<String, Signature>,
    globals: &HashMap<String, (Type, bool)>,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
) -> Result<Type> {
    match expr {
        Expr::Int { ty, .. } => Ok(ty.clone()),
        Expr::Float { ty, .. } => Ok(ty.clone()),
        Expr::StringLiteral(_) => Ok(Type::Str),
        Expr::Ascribe { expr, ty } => {
            let inner_ty = check_expr(expr, env, signatures, globals, records, variants)?;
            // A same-type ascription is an identity annotation (any type is fine);
            // a differing ascription is a numeric cast (both sides must be numeric).
            if inner_ty != *ty {
                ensure_numeric(&inner_ty, "cast requires scalar types")?;
                ensure_numeric(ty, "cast requires scalar types")?;
                if conversion_instr(&inner_ty, ty).is_none() {
                    bail!("unsupported cast from {:?} to {:?}", inner_ty, ty);
                }
            }
            Ok(ty.clone())
        }
        Expr::Var(name) => env
            .get(name)
            .cloned()
            .ok_or_else(|| anyhow!("unknown variable '{}'", name)),
        Expr::Call { name, args } => {
            let sig = signatures
                .get(name)
                .ok_or_else(|| anyhow!("call to unknown function '{}'", name))?;
            if sig.params.len() != args.len() {
                bail!(
                    "function '{}' expects {} arguments but {} were provided",
                    name,
                    sig.params.len(),
                    args.len()
                );
            }
            for (arg, expected_ty) in args.iter().zip(sig.params.iter()) {
                let ty = check_expr(arg, env, signatures, globals, records, variants)?;
                if ty != *expected_ty {
                    bail!(
                        "argument type mismatch calling '{}': expected {:?}, got {:?}",
                        name,
                        expected_ty,
                        ty
                    );
                }
            }
            Ok(sig.result.clone())
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_ty = check_expr(cond, env, signatures, globals, records, variants)?;
            if cond_ty != Type::S32 {
                bail!("if condition must be s32 (0/1), got {:?}", cond_ty);
            }
            let then_ty = check_expr(then_branch, env, signatures, globals, records, variants)?;
            let else_ty = check_expr(else_branch, env, signatures, globals, records, variants)?;
            if then_ty != else_ty {
                bail!(
                    "if branches must return the same type, got {:?} and {:?}",
                    then_ty,
                    else_ty
                );
            }
            Ok(then_ty)
        }
        Expr::Let {
            name, value, body, ..
        } => {
            let value_ty = check_expr(value, env, signatures, globals, records, variants)?;
            let mut next_env = env.clone();
            next_env.insert(name.clone(), value_ty);
            check_expr(body, &next_env, signatures, globals, records, variants)
        }
        Expr::Begin { exprs } => {
            let mut last_ty = Type::S32;
            for expr in exprs {
                last_ty = check_expr(expr, env, signatures, globals, records, variants)?;
            }
            Ok(last_ty)
        }
        Expr::WasmInstr { name, args } => {
            let instr_info = lookup_wasm_instr(name)
                .ok_or_else(|| anyhow!("unknown WASM instruction '{}'", name))?;

            // Special handling for const instructions - they define the type, not check it
            if name.ends_with(".const") {
                if args.len() != 1 {
                    bail!("{} expects exactly 1 argument", name);
                }
                // Just verify it's a literal, don't type check it
                match &args[0] {
                    Expr::Int { .. } | Expr::Float { .. } => {}
                    _ => bail!("{} requires a literal value", name),
                }
                return Ok(instr_info.result);
            }

            if instr_info.params.len() != args.len() {
                bail!(
                    "WASM instruction '{}' expects {} arguments but {} were provided",
                    name,
                    instr_info.params.len(),
                    args.len()
                );
            }
            for (arg, expected_ty) in args.iter().zip(instr_info.params.iter()) {
                let ty = check_expr(arg, env, signatures, globals, records, variants)?;
                // Allow u8 -> s32 coercion since u8 is stored as i32
                let types_compatible =
                    ty == *expected_ty || (*expected_ty == Type::S32 && ty == Type::U8);
                if !types_compatible {
                    bail!(
                        "argument type mismatch in '{}': expected {:?}, got {:?}",
                        name,
                        expected_ty,
                        ty
                    );
                }
            }
            Ok(instr_info.result)
        }
        Expr::GlobalGet { name } => {
            let (ty, _mutable) = globals
                .get(name)
                .ok_or_else(|| anyhow!("unknown global '{}'", name))?;
            Ok(ty.clone())
        }
        Expr::GlobalSet { name, value } => {
            let (expected_ty, mutable) = globals
                .get(name)
                .ok_or_else(|| anyhow!("unknown global '{}'", name))?;
            if !mutable {
                bail!("cannot set immutable global '{}'", name);
            }
            let value_ty = check_expr(value, env, signatures, globals, records, variants)?;
            if value_ty != *expected_ty {
                bail!(
                    "type mismatch setting global '{}': expected {:?}, got {:?}",
                    name,
                    expected_ty,
                    value_ty
                );
            }
            Ok(value_ty)
        }
        Expr::RecordConstruct {
            record_name,
            fields,
        } => {
            let record_def = records
                .get(record_name)
                .ok_or_else(|| anyhow!("unknown record type '{}'", record_name))?;
            if record_def.fields.len() != fields.len() {
                bail!(
                    "record '{}' expects {} fields but {} were provided",
                    record_name,
                    record_def.fields.len(),
                    fields.len()
                );
            }
            for (field_expr, field_def) in fields.iter().zip(record_def.fields.iter()) {
                let ty = check_expr(field_expr, env, signatures, globals, records, variants)?;
                if ty != field_def.ty {
                    bail!(
                        "field '{}' of record '{}': expected {:?}, got {:?}",
                        field_def.name,
                        record_name,
                        field_def.ty,
                        ty
                    );
                }
            }
            Ok(Type::Record(record_name.clone()))
        }
        Expr::RecordAccess {
            record_name,
            field_name,
            expr,
        } => {
            let record_def = records
                .get(record_name)
                .ok_or_else(|| anyhow!("unknown record type '{}'", record_name))?;
            let field = record_def
                .fields
                .iter()
                .find(|f| f.name == *field_name)
                .ok_or_else(|| anyhow!("record '{}' has no field '{}'", record_name, field_name))?;
            let expr_ty = check_expr(expr, env, signatures, globals, records, variants)?;
            if expr_ty != Type::Record(record_name.clone()) {
                bail!(
                    "field access expects record '{}', got {:?}",
                    record_name,
                    expr_ty
                );
            }
            Ok(field.ty.clone())
        }
        Expr::VariantConstruct {
            variant_name,
            case_name,
            payload,
        } => {
            let variant_def = variants
                .get(variant_name)
                .ok_or_else(|| anyhow!("unknown variant type '{}'", variant_name))?;
            let (_, case) = variant_def
                .find_case(case_name)
                .ok_or_else(|| anyhow!("variant '{}' has no case '{}'", variant_name, case_name))?;
            if case.payload.len() != payload.len() {
                bail!(
                    "variant case '{}::{}' expects {} payload values but {} were provided",
                    variant_name,
                    case_name,
                    case.payload.len(),
                    payload.len()
                );
            }
            for (payload_expr, expected_ty) in payload.iter().zip(case.payload.iter()) {
                let ty = check_expr(payload_expr, env, signatures, globals, records, variants)?;
                if ty != *expected_ty {
                    bail!(
                        "payload type mismatch in '{}::{}': expected {:?}, got {:?}",
                        variant_name,
                        case_name,
                        expected_ty,
                        ty
                    );
                }
            }
            Ok(Type::Variant(variant_name.clone()))
        }
        Expr::Match { expr, cases } => {
            let expr_ty = check_expr(expr, env, signatures, globals, records, variants)?;
            // Desugar a `_` wildcard into explicit arms before checking coverage.
            let effective = expand_match_wildcard(cases, &expr_ty, variants);
            let cases = &effective;

            // Handle Option and Result types specially
            match &expr_ty {
                Type::Option(inner_ty) => {
                    // Option can match on 'some' and 'none'
                    let mut result_ty: Option<Type> = None;
                    for arm in cases {
                        // Validate case names and bindings for option
                        let expected_bindings = match arm.case_name.as_str() {
                            "some" => 1,
                            "none" => 0,
                            other => bail!(
                                "option match: unknown case '{}', expected 'some' or 'none'",
                                other
                            ),
                        };
                        if arm.bindings.len() != expected_bindings {
                            bail!(
                                "match arm for '{}' expects {} bindings but {} were provided",
                                arm.case_name,
                                expected_bindings,
                                arm.bindings.len()
                            );
                        }

                        // Extend environment with bound variables
                        let mut arm_env = env.clone();
                        if arm.case_name == "some" && !arm.bindings.is_empty() {
                            arm_env.insert(arm.bindings[0].clone(), (**inner_ty).clone());
                        }

                        let arm_ty = check_expr(
                            &arm.body, &arm_env, signatures, globals, records, variants,
                        )?;
                        match &result_ty {
                            None => result_ty = Some(arm_ty),
                            Some(expected) => {
                                if arm_ty != *expected {
                                    bail!(
                                        "match arms must return the same type, got {:?} and {:?}",
                                        expected,
                                        arm_ty
                                    );
                                }
                            }
                        }
                    }
                    // Exhaustiveness: both 'some' and 'none' must be covered.
                    for req in ["some", "none"] {
                        if !cases.iter().any(|a| a.case_name == req) {
                            bail!("non-exhaustive match on option: missing '{}' case", req);
                        }
                    }
                    result_ty.ok_or_else(|| anyhow!("match expression must have at least one case"))
                }
                Type::Result(ok_ty, err_ty) => {
                    // Result can match on 'ok' and 'err'
                    let mut result_ty: Option<Type> = None;
                    for arm in cases {
                        // Validate case names and bindings for result
                        let (expected_bindings, payload_ty) = match arm.case_name.as_str() {
                            "ok" => (1, (**ok_ty).clone()),
                            "err" => (1, (**err_ty).clone()),
                            other => bail!(
                                "result match: unknown case '{}', expected 'ok' or 'err'",
                                other
                            ),
                        };
                        if arm.bindings.len() != expected_bindings {
                            bail!(
                                "match arm for '{}' expects {} bindings but {} were provided",
                                arm.case_name,
                                expected_bindings,
                                arm.bindings.len()
                            );
                        }

                        // Extend environment with bound variables
                        let mut arm_env = env.clone();
                        if !arm.bindings.is_empty() {
                            arm_env.insert(arm.bindings[0].clone(), payload_ty);
                        }

                        let arm_ty = check_expr(
                            &arm.body, &arm_env, signatures, globals, records, variants,
                        )?;
                        match &result_ty {
                            None => result_ty = Some(arm_ty),
                            Some(expected) => {
                                if arm_ty != *expected {
                                    bail!(
                                        "match arms must return the same type, got {:?} and {:?}",
                                        expected,
                                        arm_ty
                                    );
                                }
                            }
                        }
                    }
                    // Exhaustiveness: both 'ok' and 'err' must be covered.
                    for req in ["ok", "err"] {
                        if !cases.iter().any(|a| a.case_name == req) {
                            bail!("non-exhaustive match on result: missing '{}' case", req);
                        }
                    }
                    result_ty.ok_or_else(|| anyhow!("match expression must have at least one case"))
                }
                Type::Variant(variant_name) => {
                    // User-defined variant
                    let variant_def = variants
                        .get(variant_name)
                        .ok_or_else(|| anyhow!("unknown variant type '{}'", variant_name))?;

                    // Check that all cases exist and have correct bindings
                    let mut result_ty: Option<Type> = None;
                    for arm in cases {
                        let (_, case) = variant_def.find_case(&arm.case_name).ok_or_else(|| {
                            anyhow!("variant '{}' has no case '{}'", variant_name, arm.case_name)
                        })?;
                        if arm.bindings.len() != case.payload.len() {
                            bail!(
                                "match arm for '{}' expects {} bindings but {} were provided",
                                arm.case_name,
                                case.payload.len(),
                                arm.bindings.len()
                            );
                        }

                        // Extend environment with bound variables
                        let mut arm_env = env.clone();
                        for (binding_name, ty) in arm.bindings.iter().zip(case.payload.iter()) {
                            arm_env.insert(binding_name.clone(), ty.clone());
                        }

                        let arm_ty = check_expr(
                            &arm.body, &arm_env, signatures, globals, records, variants,
                        )?;
                        match &result_ty {
                            None => result_ty = Some(arm_ty),
                            Some(expected) => {
                                if arm_ty != *expected {
                                    bail!(
                                        "match arms must return the same type, got {:?} and {:?}",
                                        expected,
                                        arm_ty
                                    );
                                }
                            }
                        }
                    }
                    // Exhaustiveness: every case of the variant must be covered.
                    let missing: Vec<&str> = variant_def
                        .cases
                        .iter()
                        .map(|c| c.name.as_str())
                        .filter(|n| !cases.iter().any(|a| a.case_name == *n))
                        .collect();
                    if !missing.is_empty() {
                        bail!(
                            "non-exhaustive match on variant '{}': missing case(s): {}",
                            variant_name,
                            missing.join(", ")
                        );
                    }
                    result_ty.ok_or_else(|| anyhow!("match expression must have at least one case"))
                }
                _ => bail!(
                    "match expression must be a variant, option, or result type, got {:?}",
                    expr_ty
                ),
            }
        }
        // Option constructors
        Expr::Some { inner_type, value } => {
            let value_ty = check_expr(value, env, signatures, globals, records, variants)?;
            if value_ty != *inner_type {
                bail!(
                    "some value type mismatch: expected {:?}, got {:?}",
                    inner_type,
                    value_ty
                );
            }
            Ok(Type::Option(Box::new(inner_type.clone())))
        }
        Expr::None { inner_type } => Ok(Type::Option(Box::new(inner_type.clone()))),
        // Result constructors
        Expr::Ok {
            ok_type,
            err_type,
            value,
        } => {
            let value_ty = check_expr(value, env, signatures, globals, records, variants)?;
            if value_ty != *ok_type {
                bail!(
                    "ok value type mismatch: expected {:?}, got {:?}",
                    ok_type,
                    value_ty
                );
            }
            Ok(Type::Result(
                Box::new(ok_type.clone()),
                Box::new(err_type.clone()),
            ))
        }
        Expr::Err {
            ok_type,
            err_type,
            value,
        } => {
            let value_ty = check_expr(value, env, signatures, globals, records, variants)?;
            if value_ty != *err_type {
                bail!(
                    "err value type mismatch: expected {:?}, got {:?}",
                    err_type,
                    value_ty
                );
            }
            Ok(Type::Result(
                Box::new(ok_type.clone()),
                Box::new(err_type.clone()),
            ))
        }
        // Tuple constructor
        Expr::TupleConstruct { values } => {
            let elem_types: Vec<Type> = values
                .iter()
                .map(|v| check_expr(v, env, signatures, globals, records, variants))
                .collect::<Result<Vec<_>>>()?;
            Ok(Type::Tuple(elem_types))
        }
        Expr::WithCap { name, cap, body } => {
            // Bind the capability token (a resource-represented handle) and check
            // the body. The borrow-only obligation is enforced by check_fn_linearity.
            let mut next_env = env.clone();
            next_env.insert(name.clone(), Type::Resource(cap.clone()));
            let body_ty = check_expr(body, &next_env, signatures, globals, records, variants)?;
            // A borrow must not outlive the capability it borrows; the scope releases
            // the cap at its end, so the body may not yield a borrow of it.
            if matches!(body_ty, Type::Borrow(_)) {
                bail!("a borrow cannot escape its with-cap scope");
            }
            Ok(body_ty)
        }
        Expr::ReleaseCap { value } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            match ty {
                Type::Resource(_) => Ok(Type::S32),
                other => bail!("release-cap expects a capability, got {:?}", other),
            }
        }
        Expr::BorrowCap { name } => {
            let ty = env
                .get(name)
                .cloned()
                .ok_or_else(|| anyhow!("unknown variable '{}'", name))?;
            match ty {
                Type::Resource(_) => Ok(Type::Borrow(Box::new(ty))),
                other => bail!("(& {}) expects a capability, got {:?}", name, other),
            }
        }
        // List operations
        Expr::ListNew { elem_type } => Ok(Type::List(Box::new(elem_type.clone()))),
        Expr::ListPush { list, value } => {
            let list_ty = check_expr(list, env, signatures, globals, records, variants)?;
            let elem_type = match &list_ty {
                Type::List(inner) => inner.as_ref().clone(),
                _ => bail!("list-push expects a list, got {:?}", list_ty),
            };
            let value_ty = check_expr(value, env, signatures, globals, records, variants)?;
            // Allow s32 -> u8 coercion since u8 is stored as i32 internally
            let types_compatible =
                value_ty == elem_type || (elem_type == Type::U8 && value_ty == Type::S32);
            if !types_compatible {
                bail!(
                    "list-push value type mismatch: expected {:?}, got {:?}",
                    elem_type,
                    value_ty
                );
            }
            Ok(list_ty)
        }
        Expr::ListGet { list, index } => {
            let list_ty = check_expr(list, env, signatures, globals, records, variants)?;
            let elem_type = match &list_ty {
                Type::List(inner) => inner.as_ref().clone(),
                _ => bail!("list-get expects a list, got {:?}", list_ty),
            };
            let index_ty = check_expr(index, env, signatures, globals, records, variants)?;
            if index_ty != Type::S32 {
                bail!("list-get index must be s32, got {:?}", index_ty);
            }
            Ok(elem_type)
        }
        Expr::ListLen { list } => {
            let list_ty = check_expr(list, env, signatures, globals, records, variants)?;
            match &list_ty {
                Type::List(_) => Ok(Type::S32),
                _ => bail!("list-len expects a list, got {:?}", list_ty),
            }
        }
        Expr::StringLen { string } => {
            let str_ty = check_expr(string, env, signatures, globals, records, variants)?;
            match &str_ty {
                Type::Str => Ok(Type::S32),
                _ => bail!("string-len expects a string, got {:?}", str_ty),
            }
        }
        Expr::StringRef { string, index } => {
            let str_ty = check_expr(string, env, signatures, globals, records, variants)?;
            if str_ty != Type::Str {
                bail!("string-ref expects a string, got {:?}", str_ty);
            }
            let idx_ty = check_expr(index, env, signatures, globals, records, variants)?;
            if idx_ty != Type::S32 {
                bail!("string-ref index must be s32, got {:?}", idx_ty);
            }
            Ok(Type::S32)
        }
        Expr::Substring { string, start, end } => {
            let str_ty = check_expr(string, env, signatures, globals, records, variants)?;
            if str_ty != Type::Str {
                bail!("substring expects a string, got {:?}", str_ty);
            }
            let start_ty = check_expr(start, env, signatures, globals, records, variants)?;
            if start_ty != Type::S32 {
                bail!("substring start index must be s32, got {:?}", start_ty);
            }
            let end_ty = check_expr(end, env, signatures, globals, records, variants)?;
            if end_ty != Type::S32 {
                bail!("substring end index must be s32, got {:?}", end_ty);
            }
            Ok(Type::Str)
        }
        Expr::AnyFromS32 { value } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            if ty != Type::S32 {
                bail!("any-s32 expects an s32, got {:?}", ty);
            }
            Ok(Type::Any)
        }
        Expr::AnyToS32 { value } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            if ty != Type::Any {
                bail!("any-as-s32 expects an any, got {:?}", ty);
            }
            Ok(Type::S32)
        }
        Expr::AnyFromString { value } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            if ty != Type::Str {
                bail!("any-string expects a string, got {:?}", ty);
            }
            Ok(Type::Any)
        }
        Expr::AnyToString { value } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            if ty != Type::Any {
                bail!("any-as-string expects an any, got {:?}", ty);
            }
            Ok(Type::Str)
        }
        Expr::HeapAlloc { size } => {
            let ty = check_expr(size, env, signatures, globals, records, variants)?;
            if ty != Type::S32 {
                bail!("heap-alloc expects an s32 size, got {:?}", ty);
            }
            Ok(Type::S32)
        }
        Expr::AnyAddr { value } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            if ty != Type::Any {
                bail!("any-addr expects an any, got {:?}", ty);
            }
            Ok(Type::S32)
        }
        Expr::AnyFromAddr { value } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            if ty != Type::S32 {
                bail!("any-from-addr expects an s32 address, got {:?}", ty);
            }
            Ok(Type::Any)
        }
        Expr::StringAddr { value } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            if ty != Type::Str {
                bail!("string-addr expects a string, got {:?}", ty);
            }
            Ok(Type::S32)
        }
        Expr::StringFromAddr { value } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            if ty != Type::S32 {
                bail!("string-from-addr expects an s32 address, got {:?}", ty);
            }
            Ok(Type::Str)
        }
        Expr::RawInvoke { value, .. } => {
            let ty = check_expr(value, env, signatures, globals, records, variants)?;
            if ty != Type::Any {
                bail!(
                    "raw-invoke expects an any (encoded args tuple), got {:?}",
                    ty
                );
            }
            Ok(Type::Any)
        }
        Expr::StringAppend { left, right } => {
            let left_ty = check_expr(left, env, signatures, globals, records, variants)?;
            if left_ty != Type::Str {
                bail!("string-append expects strings, got {:?}", left_ty);
            }
            let right_ty = check_expr(right, env, signatures, globals, records, variants)?;
            if right_ty != Type::Str {
                bail!("string-append expects strings, got {:?}", right_ty);
            }
            Ok(Type::Str)
        }
        Expr::StringEq { left, right } => {
            let left_ty = check_expr(left, env, signatures, globals, records, variants)?;
            if left_ty != Type::Str {
                bail!("string=? expects strings, got {:?}", left_ty);
            }
            let right_ty = check_expr(right, env, signatures, globals, records, variants)?;
            if right_ty != Type::Str {
                bail!("string=? expects strings, got {:?}", right_ty);
            }
            Ok(Type::S32)
        }
        Expr::StringFromBytes { bytes } => {
            let bytes_ty = check_expr(bytes, env, signatures, globals, records, variants)?;
            match &bytes_ty {
                Type::List(elem) if **elem == Type::U8 => Ok(Type::Str),
                _ => bail!("string-from-bytes expects list<u8>, got {:?}", bytes_ty),
            }
        }
        Expr::StringToBytes { string } => {
            let str_ty = check_expr(string, env, signatures, globals, records, variants)?;
            if str_ty != Type::Str {
                bail!("string-to-bytes expects string, got {:?}", str_ty);
            }
            Ok(Type::List(Box::new(Type::U8)))
        }
    }
}

fn ensure_numeric(ty: &Type, msg: &str) -> Result<()> {
    match ty {
        // Every scalar is castable: the four native WASM numerics plus the
        // i32-backed integer refinements (bool/u8/u16/u32) and u64.
        Type::S32
        | Type::S64
        | Type::F32
        | Type::F64
        | Type::U8
        | Type::U16
        | Type::U32
        | Type::U64
        | Type::Bool => Ok(()),
        Type::Record(name) => bail!("{}: expected scalar type, got record '{}'", msg, name),
        Type::Variant(name) => bail!("{}: expected scalar type, got variant '{}'", msg, name),
        Type::Option(_) => bail!("{}: expected scalar type, got option", msg),
        Type::Result(_, _) => bail!("{}: expected scalar type, got result", msg),
        Type::List(_) => bail!("{}: expected scalar type, got list", msg),
        Type::Str => bail!("{}: expected scalar type, got string", msg),
        Type::Tuple(_) => bail!("{}: expected scalar type, got tuple", msg),
        Type::Any => bail!("{}: expected scalar type, got any", msg),
        Type::Resource(name) => bail!("{}: expected scalar type, got resource '{}'", msg, name),
        Type::Borrow(_) => bail!("{}: expected scalar type, got borrow", msg),
    }
}

pub fn parse_sexpr(tokens: &[Token], pos: usize) -> (SExpr, usize) {
    let token = tokens.get(pos);
    match token.map(|t| (&t.kind, &t.span)) {
        Some((TokenKind::LParen, start_span)) => {
            let mut elems = Vec::new();
            let mut i = pos + 1;
            loop {
                match tokens.get(i).map(|t| (&t.kind, &t.span)) {
                    Some((TokenKind::RParen, end_span)) => {
                        let span = start_span.merge(end_span);
                        return (SExpr::List(elems, span), i + 1);
                    }
                    Some(_) => {
                        let (sexpr, next) = parse_sexpr(tokens, i);
                        elems.push(sexpr);
                        i = next;
                    }
                    None => {
                        panic!(
                            "Unclosed parenthesis at line {}, column {}",
                            start_span.line, start_span.column
                        );
                    }
                }
            }
        }
        Some((TokenKind::RParen, span)) => {
            panic!(
                "Unexpected closing parenthesis at line {}, column {}",
                span.line, span.column
            );
        }
        Some((TokenKind::Symbol(s), span)) => (SExpr::Sym(s.clone(), span.clone()), pos + 1),
        Some((TokenKind::Number(NumericToken::Int { value, ty }), span)) => (
            SExpr::Int {
                value: *value,
                ty: ty.clone(),
                span: span.clone(),
            },
            pos + 1,
        ),
        Some((TokenKind::Number(NumericToken::Float { value, ty }), span)) => (
            SExpr::Float {
                value: *value,
                ty: ty.clone(),
                span: span.clone(),
            },
            pos + 1,
        ),
        Some((TokenKind::String(s), span)) => (SExpr::Str(s.clone(), span.clone()), pos + 1),
        Some((TokenKind::Quasiquote, span)) => {
            let (inner, next) = parse_sexpr(tokens, pos + 1);
            (SExpr::Quasiquote(Box::new(inner), span.clone()), next)
        }
        Some((TokenKind::Unquote, span)) => {
            let (inner, next) = parse_sexpr(tokens, pos + 1);
            (SExpr::Unquote(Box::new(inner), span.clone()), next)
        }
        Some((TokenKind::UnquoteSplice, span)) => {
            let (inner, next) = parse_sexpr(tokens, pos + 1);
            (SExpr::UnquoteSplice(Box::new(inner), span.clone()), next)
        }
        Some((TokenKind::SyntaxQuote, span)) => {
            let (inner, next) = parse_sexpr(tokens, pos + 1);
            (SExpr::SyntaxQuote(Box::new(inner), span.clone()), next)
        }
        Some((TokenKind::Quasisyntax, span)) => {
            let (inner, next) = parse_sexpr(tokens, pos + 1);
            (SExpr::Quasisyntax(Box::new(inner), span.clone()), next)
        }
        Some((TokenKind::Unsyntax, span)) => {
            let (inner, next) = parse_sexpr(tokens, pos + 1);
            (SExpr::Unsyntax(Box::new(inner), span.clone()), next)
        }
        Some((TokenKind::UnsyntaxSplice, span)) => {
            let (inner, next) = parse_sexpr(tokens, pos + 1);
            (SExpr::UnsyntaxSplice(Box::new(inner), span.clone()), next)
        }
        None => panic!("Unexpected end of tokens"),
    }
}

// Collect macro definitions from forms
/// Collected macros from defmacro, define-syntax (syntax-rules), and syntax-case
struct CollectedMacros {
    defmacros: HashMap<String, Macro>,
    syntax_rules: HashMap<String, SyntaxRulesMacro>,
    syntax_case: HashMap<String, SyntaxCaseMacro>,
}

fn collect_macros(forms: &[SExpr]) -> CollectedMacros {
    let mut defmacros = HashMap::new();
    let mut syntax_rules = HashMap::new();
    let mut syntax_case = HashMap::new();

    for form in forms {
        if let SExpr::List(items, _) = form
            && let Some(SExpr::Sym(sym, _)) = items.first()
        {
            if sym == "defmacro" && items.len() >= 4 {
                let mac = parse_defmacro_form(items);
                defmacros.insert(mac.name.clone(), mac);
            } else if sym == "define-syntax" && items.len() >= 3 {
                // Check if it's syntax-rules or syntax-case-lambda
                if let SExpr::List(body_items, _) = &items[2]
                    && let Some(SExpr::Sym(body_sym, _)) = body_items.first()
                {
                    if body_sym == "syntax-rules" {
                        if let Some(mac) = parse_define_syntax_form(items) {
                            syntax_rules.insert(mac.name.clone(), mac);
                        }
                    } else if body_sym == "syntax-case-lambda"
                        && let Some(mac) = parse_syntax_case_form(items)
                    {
                        syntax_case.insert(mac.name.clone(), mac);
                    }
                }
            }
        }
    }

    CollectedMacros {
        defmacros,
        syntax_rules,
        syntax_case,
    }
}

fn parse_defmacro_form(items: &[SExpr]) -> Macro {
    // (defmacro name (params...) template)
    if items.len() != 4 {
        panic!("defmacro must have form: (defmacro name (params...) template)");
    }
    let name = match &items[1] {
        SExpr::Sym(s, _) => s.clone(),
        _ => panic!("Macro name must be a symbol"),
    };
    let params = match &items[2] {
        SExpr::List(params, _) => params
            .iter()
            .map(|p| match p {
                SExpr::Sym(s, _) => s.clone(),
                _ => panic!("Macro parameters must be symbols"),
            })
            .collect(),
        _ => panic!("Macro parameters must be a list"),
    };
    Macro {
        name,
        params,
        template: items[3].clone(),
    }
}

/// Parse a define-syntax form
/// (define-syntax name (syntax-rules (literals...) [pattern template] ...))
fn parse_define_syntax_form(items: &[SExpr]) -> Option<SyntaxRulesMacro> {
    // items[0] = define-syntax
    // items[1] = name
    // items[2] = (syntax-rules ...)
    if items.len() != 3 {
        eprintln!("define-syntax requires exactly 2 arguments");
        return None;
    }

    let name = match &items[1] {
        SExpr::Sym(s, _) => s.clone(),
        _ => {
            eprintln!("define-syntax name must be a symbol");
            return None;
        }
    };

    // Parse (syntax-rules (literals...) rules...)
    let syntax_rules_form = match &items[2] {
        SExpr::List(sr_items, _) => sr_items,
        _ => {
            eprintln!("define-syntax body must be (syntax-rules ...)");
            return None;
        }
    };

    if syntax_rules_form.is_empty() {
        eprintln!("syntax-rules form is empty");
        return None;
    }

    // Check for "syntax-rules" keyword
    match &syntax_rules_form[0] {
        SExpr::Sym(s, _) if s == "syntax-rules" => {}
        _ => {
            eprintln!("Expected syntax-rules");
            return None;
        }
    }

    if syntax_rules_form.len() < 2 {
        eprintln!("syntax-rules requires literals list and at least one rule");
        return None;
    }

    // Parse literals list
    let literals: Vec<String> = match &syntax_rules_form[1] {
        SExpr::List(lits, _) => lits
            .iter()
            .filter_map(|l| match l {
                SExpr::Sym(s, _) => Some(s.clone()),
                _ => None,
            })
            .collect(),
        _ => {
            eprintln!("syntax-rules literals must be a list");
            return None;
        }
    };

    // Parse rules: each rule is [pattern template] or (pattern template)
    let mut rules = Vec::new();
    for rule_form in &syntax_rules_form[2..] {
        if let Some(rule) = parse_syntax_rule(rule_form, &name, &literals) {
            rules.push(rule);
        } else {
            eprintln!("Failed to parse syntax rule");
            return None;
        }
    }

    Some(SyntaxRulesMacro {
        name,
        literals,
        rules,
    })
}

/// Parse a define-syntax form with syntax-case-lambda
/// (define-syntax name (syntax-case-lambda (stx) clauses...))
/// or
/// (define-syntax name (syntax-case-lambda (stx) (syntax-case stx (lits) clauses...)))
fn parse_syntax_case_form(items: &[SExpr]) -> Option<SyntaxCaseMacro> {
    // items[0] = define-syntax
    // items[1] = name
    // items[2] = (syntax-case-lambda (param) ...)
    if items.len() != 3 {
        eprintln!("define-syntax requires exactly 2 arguments");
        return None;
    }

    let name = match &items[1] {
        SExpr::Sym(s, _) => s.clone(),
        _ => {
            eprintln!("define-syntax name must be a symbol");
            return None;
        }
    };

    // Parse (syntax-case-lambda (param) body...)
    let scl_form = match &items[2] {
        SExpr::List(scl_items, _) => scl_items,
        _ => {
            eprintln!("define-syntax body must be (syntax-case-lambda ...)");
            return None;
        }
    };

    if scl_form.len() < 3 {
        eprintln!("syntax-case-lambda requires parameter and at least one clause");
        return None;
    }

    // Check for "syntax-case-lambda" keyword
    match &scl_form[0] {
        SExpr::Sym(s, _) if s == "syntax-case-lambda" => {}
        _ => {
            eprintln!("Expected syntax-case-lambda");
            return None;
        }
    }

    // Parse parameter: (stx)
    let param = match &scl_form[1] {
        SExpr::List(params, _) if params.len() == 1 => match &params[0] {
            SExpr::Sym(s, _) => s.clone(),
            _ => {
                eprintln!("syntax-case-lambda parameter must be a symbol");
                return None;
            }
        },
        _ => {
            eprintln!("syntax-case-lambda requires exactly one parameter");
            return None;
        }
    };

    // Check if body starts with syntax-case or is just clauses
    let (literals, clauses_forms): (Vec<String>, &[SExpr]) =
        if let SExpr::List(inner, _) = &scl_form[2] {
            if let Some(SExpr::Sym(s, _)) = inner.first() {
                if s == "syntax-case" && inner.len() >= 3 {
                    // (syntax-case stx (literals) clauses...)
                    let lits = match &inner[2] {
                        SExpr::List(lits, _) => lits
                            .iter()
                            .filter_map(|l| match l {
                                SExpr::Sym(s, _) => Some(s.clone()),
                                _ => None,
                            })
                            .collect(),
                        _ => vec![],
                    };
                    (lits, &inner[3..])
                } else {
                    // Direct clauses without syntax-case wrapper
                    (vec![], &scl_form[2..])
                }
            } else {
                // Direct clauses
                (vec![], &scl_form[2..])
            }
        } else {
            (vec![], &scl_form[2..])
        };

    // Parse clauses
    let mut clauses = Vec::new();
    for clause_form in clauses_forms {
        if let Some(clause) = parse_syntax_case_clause(clause_form, &name, &literals) {
            clauses.push(clause);
        } else {
            eprintln!("Failed to parse syntax-case clause");
            return None;
        }
    }

    Some(SyntaxCaseMacro {
        name,
        _param: param,
        literals,
        clauses,
    })
}

/// Parse a syntax-case clause: (pattern template) or (pattern guard template)
fn parse_syntax_case_clause(
    form: &SExpr,
    macro_name: &str,
    literals: &[String],
) -> Option<SyntaxCaseClause> {
    let items = match form {
        SExpr::List(items, _) => items,
        _ => {
            eprintln!("Syntax-case clause must be a list");
            return None;
        }
    };

    if items.len() < 2 || items.len() > 3 {
        eprintln!("Syntax-case clause must have 2 or 3 elements: (pattern [guard] template)");
        return None;
    }

    // Collect pattern variables from the pattern
    let mut pattern_vars = HashSet::new();
    let pattern = parse_pattern(&items[0], macro_name, literals, &mut pattern_vars)?;

    if items.len() == 2 {
        // No guard: (pattern template)
        let template = parse_compile_time_expr(&items[1], &pattern_vars)?;
        Some(SyntaxCaseClause {
            pattern,
            guard: None,
            template,
        })
    } else {
        // With guard: (pattern guard template)
        let guard = parse_compile_time_expr(&items[1], &pattern_vars)?;
        let template = parse_compile_time_expr(&items[2], &pattern_vars)?;
        Some(SyntaxCaseClause {
            pattern,
            guard: Some(guard),
            template,
        })
    }
}

/// Parse a compile-time expression from an S-expression
fn parse_compile_time_expr(
    sexpr: &SExpr,
    pattern_vars: &HashSet<String>,
) -> Option<CompileTimeExpr> {
    match sexpr {
        SExpr::SyntaxQuote(inner, _) => {
            // #'expr - syntax quote
            Some(CompileTimeExpr::Syntax(inner.as_ref().clone()))
        }
        SExpr::Quasisyntax(inner, _) => {
            // #`template - quasisyntax
            Some(CompileTimeExpr::Quasisyntax(inner.as_ref().clone()))
        }
        SExpr::Sym(name, _) => {
            // Variable reference (could be pattern var or builtin)
            Some(CompileTimeExpr::Var(name.clone()))
        }
        SExpr::Int { .. } | SExpr::Float { .. } => Some(CompileTimeExpr::Literal(sexpr.clone())),
        SExpr::List(items, _) => {
            if items.is_empty() {
                return Some(CompileTimeExpr::Literal(sexpr.clone()));
            }

            // Check for special forms
            if let SExpr::Sym(name, _) = &items[0] {
                match name.as_str() {
                    "if" if items.len() == 4 => {
                        let cond = parse_compile_time_expr(&items[1], pattern_vars)?;
                        let then_branch = parse_compile_time_expr(&items[2], pattern_vars)?;
                        let else_branch = parse_compile_time_expr(&items[3], pattern_vars)?;
                        return Some(CompileTimeExpr::If {
                            cond: Box::new(cond),
                            then_branch: Box::new(then_branch),
                            else_branch: Box::new(else_branch),
                        });
                    }
                    "let" if items.len() == 3 => {
                        if let SExpr::List(binding, _) = &items[1]
                            && binding.len() == 2
                            && let SExpr::Sym(var_name, _) = &binding[0]
                        {
                            let value = parse_compile_time_expr(&binding[1], pattern_vars)?;
                            let mut extended_vars = pattern_vars.clone();
                            extended_vars.insert(var_name.clone());
                            let body = parse_compile_time_expr(&items[2], &extended_vars)?;
                            return Some(CompileTimeExpr::Let {
                                name: var_name.clone(),
                                value: Box::new(value),
                                body: Box::new(body),
                            });
                        }
                    }
                    _ => {}
                }

                // Function application
                let args: Option<Vec<_>> = items[1..]
                    .iter()
                    .map(|arg| parse_compile_time_expr(arg, pattern_vars))
                    .collect();
                return Some(CompileTimeExpr::App {
                    func: name.clone(),
                    args: args?,
                });
            }

            // Unknown form
            eprintln!("Unknown compile-time expression: {:?}", sexpr);
            None
        }
        _ => {
            eprintln!("Unsupported compile-time expression: {:?}", sexpr);
            None
        }
    }
}

/// Parse a single syntax rule: [pattern template] or (pattern template)
fn parse_syntax_rule(form: &SExpr, macro_name: &str, literals: &[String]) -> Option<SyntaxRule> {
    let items = match form {
        SExpr::List(items, _) => items,
        _ => {
            eprintln!("Syntax rule must be a list");
            return None;
        }
    };

    if items.len() != 2 {
        eprintln!("Syntax rule must have exactly 2 elements: [pattern template]");
        return None;
    }

    // Collect pattern variables from the pattern
    let mut pattern_vars = HashSet::new();
    let pattern = parse_pattern(&items[0], macro_name, literals, &mut pattern_vars)?;
    let template = parse_template(&items[1], &pattern_vars)?;

    Some(SyntaxRule { pattern, template })
}

/// Parse a pattern from an S-expression
fn parse_pattern(
    sexpr: &SExpr,
    macro_name: &str,
    literals: &[String],
    pattern_vars: &mut HashSet<String>,
) -> Option<Pattern> {
    match sexpr {
        SExpr::Sym(s, _) => {
            if s == "_" {
                Some(Pattern::Wildcard)
            } else if s == "..." {
                // Ellipsis shouldn't appear as a standalone pattern
                eprintln!("Unexpected ellipsis in pattern");
                None
            } else if s == macro_name {
                // The macro name itself is treated as a literal in the pattern
                Some(Pattern::Literal(s.clone()))
            } else if literals.contains(s) {
                Some(Pattern::Literal(s.clone()))
            } else {
                // It's a pattern variable
                pattern_vars.insert(s.clone());
                Some(Pattern::Variable(s.clone()))
            }
        }
        SExpr::Int { .. } | SExpr::Float { .. } => {
            // Numbers match literally
            Some(Pattern::Literal(format!("{:?}", sexpr)))
        }
        SExpr::List(items, _) => {
            // Check for ellipsis in the list
            let ellipsis_pos = items
                .iter()
                .position(|item| matches!(item, SExpr::Sym(s, _) if s == "..."));

            if let Some(pos) = ellipsis_pos {
                // Pattern has ellipsis
                if pos == 0 {
                    eprintln!("Ellipsis cannot be first element");
                    return None;
                }

                // Elements before the repeated pattern
                let mut before = Vec::new();
                for item in &items[..pos - 1] {
                    before.push(parse_pattern(item, macro_name, literals, pattern_vars)?);
                }

                // The repeated pattern (element before ...)
                let repeated = parse_pattern(&items[pos - 1], macro_name, literals, pattern_vars)?;

                // Elements after the ellipsis
                let mut after = Vec::new();
                for item in &items[pos + 1..] {
                    after.push(parse_pattern(item, macro_name, literals, pattern_vars)?);
                }

                Some(Pattern::ListWithEllipsis {
                    before,
                    repeated: Box::new(repeated),
                    after,
                })
            } else {
                // No ellipsis - regular list pattern
                let patterns: Option<Vec<_>> = items
                    .iter()
                    .map(|item| parse_pattern(item, macro_name, literals, pattern_vars))
                    .collect();
                Some(Pattern::List(patterns?))
            }
        }
        _ => {
            eprintln!("Unexpected form in pattern");
            None
        }
    }
}

/// Parse a template from an S-expression
fn parse_template(sexpr: &SExpr, pattern_vars: &HashSet<String>) -> Option<Template> {
    match sexpr {
        SExpr::Sym(s, _) => {
            if s == "..." {
                eprintln!("Unexpected ellipsis in template");
                None
            } else if pattern_vars.contains(s) {
                Some(Template::Variable(s.clone()))
            } else {
                Some(Template::Symbol(s.clone()))
            }
        }
        SExpr::Int { .. } | SExpr::Float { .. } => Some(Template::Atom(sexpr.clone())),
        SExpr::List(items, _) => {
            // Check for ellipsis patterns like (t ...)
            let mut templates = Vec::new();
            let mut i = 0;
            while i < items.len() {
                // Check if next item is ellipsis
                if i + 1 < items.len()
                    && let SExpr::Sym(s, _) = &items[i + 1]
                    && s == "..."
                {
                    // This element is repeated
                    let inner = parse_template(&items[i], pattern_vars)?;
                    templates.push(Template::Ellipsis(Box::new(inner)));
                    i += 2; // Skip both element and ellipsis
                    continue;
                }
                // Regular element
                templates.push(parse_template(&items[i], pattern_vars)?);
                i += 1;
            }
            Some(Template::List(templates))
        }
        _ => {
            eprintln!("Unexpected form in template");
            None
        }
    }
}

// Expand macros in all forms
fn expand_all_macros(forms: Vec<SExpr>, macros: &CollectedMacros) -> Vec<SExpr> {
    forms
        .into_iter()
        .filter(|form| {
            // Filter out defmacro and define-syntax forms (they're already collected)
            if let SExpr::List(items, _) = form
                && let Some(SExpr::Sym(sym, _)) = items.first()
            {
                return sym != "defmacro" && sym != "define-syntax";
            }
            true
        })
        .map(|form| expand_macros(form, macros, 0))
        .collect()
}

// Expand macros in a single S-expression
fn expand_macros(expr: SExpr, macros: &CollectedMacros, depth: usize) -> SExpr {
    const MAX_EXPANSION_DEPTH: usize = 100;
    if depth > MAX_EXPANSION_DEPTH {
        panic!("Macro expansion depth exceeded (possible infinite recursion)");
    }

    match expr {
        SExpr::List(items, span) if !items.is_empty() => {
            // Check if this is a macro call
            if let SExpr::Sym(name, _) = &items[0] {
                // First check defmacro
                if let Some(mac) = macros.defmacros.get(name) {
                    // It's a defmacro call - expand it
                    if items.len() - 1 != mac.params.len() {
                        panic!(
                            "Macro '{}' expects {} arguments, got {}",
                            name,
                            mac.params.len(),
                            items.len() - 1
                        );
                    }
                    // Build substitution map
                    let args: Vec<SExpr> = items[1..].to_vec();
                    let substitutions: HashMap<String, SExpr> =
                        mac.params.iter().cloned().zip(args).collect();

                    // Generate fresh scope for this macro expansion (hygiene)
                    let macro_scope = fresh_scope();

                    // Evaluate the template with substitutions
                    // Unwrap the top-level quasiquote if present
                    let template_inner = match &mac.template {
                        SExpr::Quasiquote(inner, _) => inner.as_ref(),
                        other => other,
                    };
                    let expanded =
                        eval_quasiquote(template_inner, &substitutions, &span, Some(macro_scope));

                    // Recursively expand the result
                    return expand_macros(expanded, macros, depth + 1);
                }

                // Then check syntax-rules
                if let Some(sr_mac) = macros.syntax_rules.get(name) {
                    // Try to match against each rule in order
                    let input = SExpr::List(items.clone(), span.clone());
                    for rule in &sr_mac.rules {
                        if let Some(bindings) =
                            match_pattern(&rule.pattern, &input, &sr_mac.literals)
                        {
                            // Generate fresh scope for hygiene
                            let macro_scope = fresh_scope();

                            // Expand the template with bindings
                            let expanded =
                                expand_template(&rule.template, &bindings, &span, macro_scope);

                            // Recursively expand the result
                            return expand_macros(expanded, macros, depth + 1);
                        }
                    }
                    // No rule matched
                    panic!(
                        "No matching rule for macro '{}' with input {:?}",
                        name, items
                    );
                }

                // Then check syntax-case
                if let Some(sc_mac) = macros.syntax_case.get(name) {
                    let input = SExpr::List(items.clone(), span.clone());
                    for clause in &sc_mac.clauses {
                        if let Some(bindings) =
                            match_pattern(&clause.pattern, &input, &sc_mac.literals)
                        {
                            // Generate fresh scope for hygiene
                            let macro_scope = fresh_scope();

                            // Create compile-time environment with pattern bindings
                            let mut ct_env: HashMap<String, CompileTimeValue> = HashMap::new();
                            for (name, binding) in &bindings {
                                match binding {
                                    PatternBinding::Single(sexpr) => {
                                        ct_env.insert(
                                            name.clone(),
                                            CompileTimeValue::Syntax(sexpr.clone()),
                                        );
                                    }
                                    PatternBinding::List(sexprs) => {
                                        let vals = sexprs
                                            .iter()
                                            .map(|s| CompileTimeValue::Syntax(s.clone()))
                                            .collect();
                                        ct_env.insert(name.clone(), CompileTimeValue::List(vals));
                                    }
                                }
                            }

                            // Evaluate guard if present
                            let guard_result = if let Some(guard) = &clause.guard {
                                match eval_compile_time_expr(guard, &ct_env, &span, macro_scope) {
                                    CompileTimeValue::Bool(b) => b,
                                    _ => true, // Non-boolean treated as true
                                }
                            } else {
                                true
                            };

                            if guard_result {
                                // Evaluate the template
                                let result = eval_compile_time_expr(
                                    &clause.template,
                                    &ct_env,
                                    &span,
                                    macro_scope,
                                );

                                // Convert result to SExpr
                                let expanded = match result {
                                    CompileTimeValue::Syntax(sexpr) => sexpr,
                                    other => panic!(
                                        "syntax-case template must return syntax, got {:?}",
                                        other
                                    ),
                                };

                                // Recursively expand the result
                                return expand_macros(expanded, macros, depth + 1);
                            }
                        }
                    }
                    // No clause matched
                    panic!(
                        "No matching clause for syntax-case macro '{}' with input {:?}",
                        name, items
                    );
                }
            }

            // Not a macro call - recursively expand children
            SExpr::List(
                items
                    .into_iter()
                    .map(|item| expand_macros(item, macros, depth))
                    .collect(),
                span,
            )
        }
        SExpr::Quasiquote(inner, span) => {
            // Quasiquote outside of macro - evaluate it directly (no hygiene scope needed)
            eval_quasiquote(&inner, &HashMap::new(), &span, None)
        }
        // Pass through other forms
        other => other,
    }
}

/// Substitute pattern variables in a syntax template
/// Pattern variables bound in the environment are replaced with their values
/// Other symbols get the macro scope added for hygiene
#[allow(clippy::only_used_in_recursion)]
fn substitute_pattern_vars_in_syntax(
    sexpr: &SExpr,
    env: &HashMap<String, CompileTimeValue>,
    span: &Span,
    macro_scope: ScopeId,
) -> SExpr {
    match sexpr {
        SExpr::Sym(name, sym_span) => {
            // Check if this is a pattern variable
            if let Some(val) = env.get(name) {
                match val {
                    CompileTimeValue::Syntax(s) => s.clone(), // Keep original scopes (from call site)
                    CompileTimeValue::Int(i) => SExpr::Int {
                        value: *i,
                        ty: Type::S32,
                        span: sym_span.clone(),
                    },
                    CompileTimeValue::Bool(true) => {
                        SExpr::Sym("#t".to_string(), sym_span.with_scope(macro_scope))
                    }
                    CompileTimeValue::Bool(false) => {
                        SExpr::Sym("#f".to_string(), sym_span.with_scope(macro_scope))
                    }
                    CompileTimeValue::List(_) => {
                        panic!("Cannot substitute list value as single syntax")
                    }
                }
            } else {
                // Not a pattern variable - add macro scope for hygiene
                SExpr::Sym(name.clone(), sym_span.with_scope(macro_scope))
            }
        }
        SExpr::List(items, list_span) => {
            let substituted: Vec<_> = items
                .iter()
                .map(|item| substitute_pattern_vars_in_syntax(item, env, span, macro_scope))
                .collect();
            SExpr::List(substituted, list_span.with_scope(macro_scope))
        }
        SExpr::Int {
            value,
            ty,
            span: int_span,
        } => SExpr::Int {
            value: *value,
            ty: ty.clone(),
            span: int_span.with_scope(macro_scope),
        },
        SExpr::Float {
            value,
            ty,
            span: float_span,
        } => SExpr::Float {
            value: *value,
            ty: ty.clone(),
            span: float_span.with_scope(macro_scope),
        },
        other => add_scope_to_sexpr(other, macro_scope),
    }
}

/// Evaluate a compile-time expression in the given environment
fn eval_compile_time_expr(
    expr: &CompileTimeExpr,
    env: &HashMap<String, CompileTimeValue>,
    span: &Span,
    macro_scope: ScopeId,
) -> CompileTimeValue {
    match expr {
        CompileTimeExpr::Syntax(sexpr) => {
            // #'expr - substitute pattern variables and add macro scope
            let substituted = substitute_pattern_vars_in_syntax(sexpr, env, span, macro_scope);
            CompileTimeValue::Syntax(substituted)
        }
        CompileTimeExpr::Quasisyntax(template) => {
            // #`template - evaluate with #, and #,@
            let expanded = eval_quasisyntax(template, env, span, macro_scope);
            CompileTimeValue::Syntax(expanded)
        }
        CompileTimeExpr::Var(name) => {
            // Look up in environment
            if let Some(val) = env.get(name) {
                val.clone()
            } else {
                // Unbound variable - treat as syntax
                CompileTimeValue::Syntax(SExpr::Sym(name.clone(), span.with_scope(macro_scope)))
            }
        }
        CompileTimeExpr::Literal(sexpr) => match sexpr {
            SExpr::Int { value, .. } => CompileTimeValue::Int(*value),
            SExpr::Sym(s, _) if s == "#t" || s == "true" => CompileTimeValue::Bool(true),
            SExpr::Sym(s, _) if s == "#f" || s == "false" => CompileTimeValue::Bool(false),
            other => CompileTimeValue::Syntax(other.clone()),
        },
        CompileTimeExpr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_val = eval_compile_time_expr(cond, env, span, macro_scope);
            let is_true = match cond_val {
                CompileTimeValue::Bool(b) => b,
                CompileTimeValue::Int(i) => i != 0,
                _ => true, // Non-false values are truthy
            };
            if is_true {
                eval_compile_time_expr(then_branch, env, span, macro_scope)
            } else {
                eval_compile_time_expr(else_branch, env, span, macro_scope)
            }
        }
        CompileTimeExpr::Let { name, value, body } => {
            let val = eval_compile_time_expr(value, env, span, macro_scope);
            let mut new_env = env.clone();
            new_env.insert(name.clone(), val);
            eval_compile_time_expr(body, &new_env, span, macro_scope)
        }
        CompileTimeExpr::App { func, args } => {
            // Evaluate builtin compile-time functions
            let arg_vals: Vec<_> = args
                .iter()
                .map(|a| eval_compile_time_expr(a, env, span, macro_scope))
                .collect();

            match func.as_str() {
                "identifier?" => {
                    // Check if argument is an identifier (symbol syntax)
                    if let Some(CompileTimeValue::Syntax(SExpr::Sym(_, _))) = arg_vals.first() {
                        CompileTimeValue::Bool(true)
                    } else {
                        CompileTimeValue::Bool(false)
                    }
                }
                "number?" => {
                    // Check if argument is a number syntax
                    match arg_vals.first() {
                        Some(CompileTimeValue::Syntax(SExpr::Int { .. })) => {
                            CompileTimeValue::Bool(true)
                        }
                        Some(CompileTimeValue::Syntax(SExpr::Float { .. })) => {
                            CompileTimeValue::Bool(true)
                        }
                        Some(CompileTimeValue::Int(_)) => CompileTimeValue::Bool(true),
                        _ => CompileTimeValue::Bool(false),
                    }
                }
                "syntax->datum" => {
                    // Extract the datum from syntax
                    match arg_vals.first() {
                        Some(CompileTimeValue::Syntax(SExpr::Int { value, .. })) => {
                            CompileTimeValue::Int(*value)
                        }
                        Some(CompileTimeValue::Syntax(SExpr::Sym(s, _))) => {
                            CompileTimeValue::Syntax(SExpr::Sym(s.clone(), Span::dummy()))
                        }
                        Some(v) => v.clone(),
                        None => panic!("syntax->datum requires an argument"),
                    }
                }
                "not" => match arg_vals.first() {
                    Some(CompileTimeValue::Bool(b)) => CompileTimeValue::Bool(!b),
                    Some(CompileTimeValue::Int(0)) => CompileTimeValue::Bool(true),
                    _ => CompileTimeValue::Bool(false),
                },
                "and" => {
                    let result = arg_vals.iter().all(|v| match v {
                        CompileTimeValue::Bool(b) => *b,
                        CompileTimeValue::Int(i) => *i != 0,
                        _ => true,
                    });
                    CompileTimeValue::Bool(result)
                }
                "or" => {
                    let result = arg_vals.iter().any(|v| match v {
                        CompileTimeValue::Bool(b) => *b,
                        CompileTimeValue::Int(i) => *i != 0,
                        _ => true,
                    });
                    CompileTimeValue::Bool(result)
                }
                "+" => {
                    let sum: i64 = arg_vals
                        .iter()
                        .map(|v| match v {
                            CompileTimeValue::Int(i) => *i,
                            CompileTimeValue::Syntax(SExpr::Int { value, .. }) => *value,
                            _ => 0,
                        })
                        .sum();
                    CompileTimeValue::Int(sum)
                }
                "-" => {
                    if arg_vals.len() == 1 {
                        match &arg_vals[0] {
                            CompileTimeValue::Int(i) => CompileTimeValue::Int(-i),
                            _ => CompileTimeValue::Int(0),
                        }
                    } else if arg_vals.len() >= 2 {
                        let first = match &arg_vals[0] {
                            CompileTimeValue::Int(i) => *i,
                            CompileTimeValue::Syntax(SExpr::Int { value, .. }) => *value,
                            _ => 0,
                        };
                        let rest: i64 = arg_vals[1..]
                            .iter()
                            .map(|v| match v {
                                CompileTimeValue::Int(i) => *i,
                                CompileTimeValue::Syntax(SExpr::Int { value, .. }) => *value,
                                _ => 0,
                            })
                            .sum();
                        CompileTimeValue::Int(first - rest)
                    } else {
                        CompileTimeValue::Int(0)
                    }
                }
                "integer?" => match arg_vals.first() {
                    Some(CompileTimeValue::Int(_)) => CompileTimeValue::Bool(true),
                    Some(CompileTimeValue::Syntax(SExpr::Int { .. })) => {
                        CompileTimeValue::Bool(true)
                    }
                    _ => CompileTimeValue::Bool(false),
                },
                "syntax-error" => {
                    let msg = match arg_vals.first() {
                        Some(CompileTimeValue::Syntax(SExpr::Sym(s, _))) => s.clone(),
                        _ => "syntax error".to_string(),
                    };
                    panic!("Compile-time error: {}", msg);
                }
                _ => {
                    // Unknown function - return as syntax application
                    let func_sym = SExpr::Sym(func.clone(), span.with_scope(macro_scope));
                    let arg_sexprs: Vec<_> = arg_vals
                        .iter()
                        .map(|v| match v {
                            CompileTimeValue::Syntax(s) => s.clone(),
                            CompileTimeValue::Bool(true) => {
                                SExpr::Sym("#t".to_string(), span.with_scope(macro_scope))
                            }
                            CompileTimeValue::Bool(false) => {
                                SExpr::Sym("#f".to_string(), span.with_scope(macro_scope))
                            }
                            CompileTimeValue::Int(i) => SExpr::Int {
                                value: *i,
                                ty: Type::S32,
                                span: span.with_scope(macro_scope),
                            },
                            CompileTimeValue::List(items) => {
                                let sexprs: Vec<_> = items
                                    .iter()
                                    .map(|item| match item {
                                        CompileTimeValue::Syntax(s) => s.clone(),
                                        _ => SExpr::Sym(
                                            "?".to_string(),
                                            span.with_scope(macro_scope),
                                        ),
                                    })
                                    .collect();
                                SExpr::List(sexprs, span.with_scope(macro_scope))
                            }
                        })
                        .collect();
                    let mut all_items = vec![func_sym];
                    all_items.extend(arg_sexprs);
                    CompileTimeValue::Syntax(SExpr::List(all_items, span.with_scope(macro_scope)))
                }
            }
        }
    }
}

/// Evaluate quasisyntax template (#`) with #, and #,@
fn eval_quasisyntax(
    template: &SExpr,
    env: &HashMap<String, CompileTimeValue>,
    span: &Span,
    macro_scope: ScopeId,
) -> SExpr {
    match template {
        SExpr::Unsyntax(inner, _) => {
            // #, - evaluate and insert
            match inner.as_ref() {
                SExpr::Sym(name, _) => {
                    if let Some(val) = env.get(name) {
                        match val {
                            CompileTimeValue::Syntax(s) => s.clone(),
                            CompileTimeValue::Int(i) => SExpr::Int {
                                value: *i,
                                ty: Type::S32,
                                span: span.with_scope(macro_scope),
                            },
                            CompileTimeValue::Bool(true) => {
                                SExpr::Sym("#t".to_string(), span.with_scope(macro_scope))
                            }
                            CompileTimeValue::Bool(false) => {
                                SExpr::Sym("#f".to_string(), span.with_scope(macro_scope))
                            }
                            CompileTimeValue::List(_) => {
                                panic!("Cannot unsyntax a list directly, use #,@")
                            }
                        }
                    } else {
                        // Unbound - keep as symbol
                        SExpr::Sym(name.clone(), span.with_scope(macro_scope))
                    }
                }
                other => eval_quasisyntax(other, env, span, macro_scope),
            }
        }
        SExpr::UnsyntaxSplice(_inner, _) => {
            // #,@ should only appear inside lists
            panic!("Unsyntax-splice (#,@) can only appear inside a list");
        }
        SExpr::List(items, list_span) => {
            let mut result = Vec::new();
            for item in items {
                match item {
                    SExpr::UnsyntaxSplice(inner, _) => {
                        // #,@ - splice the list
                        if let SExpr::Sym(name, _) = inner.as_ref() {
                            if let Some(CompileTimeValue::List(vals)) = env.get(name) {
                                for v in vals {
                                    match v {
                                        CompileTimeValue::Syntax(s) => result.push(s.clone()),
                                        _ => panic!("Cannot splice non-syntax value"),
                                    }
                                }
                            } else if let Some(CompileTimeValue::Syntax(s)) = env.get(name) {
                                // Single value - just push it
                                result.push(s.clone());
                            }
                        }
                    }
                    _ => {
                        result.push(eval_quasisyntax(item, env, span, macro_scope));
                    }
                }
            }
            SExpr::List(result, list_span.with_scope(macro_scope))
        }
        SExpr::Sym(s, sym_span) => {
            // Check if this is a pattern variable that should be substituted
            if let Some(val) = env.get(s) {
                match val {
                    CompileTimeValue::Syntax(syntax) => syntax.clone(), // Keep original scopes
                    CompileTimeValue::Int(i) => SExpr::Int {
                        value: *i,
                        ty: Type::S32,
                        span: sym_span.clone(),
                    },
                    CompileTimeValue::Bool(true) => {
                        SExpr::Sym("#t".to_string(), sym_span.with_scope(macro_scope))
                    }
                    CompileTimeValue::Bool(false) => {
                        SExpr::Sym("#f".to_string(), sym_span.with_scope(macro_scope))
                    }
                    CompileTimeValue::List(_) => {
                        panic!("Cannot substitute list as single syntax in quasisyntax")
                    }
                }
            } else {
                // Not a pattern variable - add macro scope for hygiene
                SExpr::Sym(s.clone(), sym_span.with_scope(macro_scope))
            }
        }
        other => add_scope_to_sexpr(other, macro_scope),
    }
}

/// Match a pattern against an S-expression, returning bindings if successful
fn match_pattern(
    pattern: &Pattern,
    input: &SExpr,
    literals: &[String],
) -> Option<HashMap<String, PatternBinding>> {
    let mut bindings = HashMap::new();
    if match_pattern_impl(pattern, input, literals, &mut bindings) {
        Some(bindings)
    } else {
        None
    }
}

#[allow(clippy::only_used_in_recursion)]
fn match_pattern_impl(
    pattern: &Pattern,
    input: &SExpr,
    literals: &[String],
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    match pattern {
        Pattern::Wildcard => true,
        Pattern::Variable(name) => {
            bindings.insert(name.clone(), PatternBinding::Single(input.clone()));
            true
        }
        Pattern::Literal(lit) => {
            // Match against literal symbol
            match input {
                SExpr::Sym(s, _) => s == lit,
                _ => false,
            }
        }
        Pattern::List(patterns) => match input {
            SExpr::List(items, _) => {
                if items.len() != patterns.len() {
                    return false;
                }
                for (pat, item) in patterns.iter().zip(items.iter()) {
                    if !match_pattern_impl(pat, item, literals, bindings) {
                        return false;
                    }
                }
                true
            }
            _ => false,
        },
        Pattern::ListWithEllipsis {
            before,
            repeated,
            after,
        } => {
            match input {
                SExpr::List(items, _) => {
                    let min_len = before.len() + after.len();
                    if items.len() < min_len {
                        return false;
                    }

                    // Match elements before the ellipsis
                    for (pat, item) in before.iter().zip(items.iter()) {
                        if !match_pattern_impl(pat, item, literals, bindings) {
                            return false;
                        }
                    }

                    // Match elements after the ellipsis (from the end)
                    let after_start = items.len() - after.len();
                    for (pat, item) in after.iter().zip(items[after_start..].iter()) {
                        if !match_pattern_impl(pat, item, literals, bindings) {
                            return false;
                        }
                    }

                    // Match the repeated elements in the middle
                    let repeated_items = &items[before.len()..after_start];

                    // Collect bindings from repeated pattern
                    // We need to match each repeated item and collect all the bindings
                    match repeated.as_ref() {
                        Pattern::Variable(var_name) => {
                            // Simple case: pattern variable matches each item
                            let values: Vec<SExpr> = repeated_items.to_vec();
                            bindings.insert(var_name.clone(), PatternBinding::List(values));
                            true
                        }
                        _ => {
                            // Complex pattern - match each item and collect bindings
                            // For now, only support simple variable patterns in ellipsis
                            // A full implementation would need to collect nested bindings
                            for item in repeated_items {
                                if !match_pattern_impl(repeated, item, literals, bindings) {
                                    return false;
                                }
                            }
                            true
                        }
                    }
                }
                _ => false,
            }
        }
    }
}

/// Expand a template with bindings
fn expand_template(
    template: &Template,
    bindings: &HashMap<String, PatternBinding>,
    span: &Span,
    macro_scope: ScopeId,
) -> SExpr {
    match template {
        Template::Variable(name) => {
            match bindings.get(name) {
                Some(PatternBinding::Single(expr)) => {
                    // Keep original scopes (from call site)
                    expr.clone()
                }
                Some(PatternBinding::List(exprs)) => {
                    // This shouldn't happen in non-ellipsis context
                    // Return first element or error
                    if let Some(first) = exprs.first() {
                        first.clone()
                    } else {
                        SExpr::List(vec![], span.clone())
                    }
                }
                None => {
                    // Unbound variable - treat as symbol with macro scope
                    SExpr::Sym(name.clone(), span.with_scope(macro_scope))
                }
            }
        }
        Template::Symbol(name) => {
            // Template-introduced symbol - add macro scope
            SExpr::Sym(name.clone(), span.with_scope(macro_scope))
        }
        Template::Atom(sexpr) => {
            // Keep the atom as-is but add macro scope
            add_scope_to_sexpr(sexpr, macro_scope)
        }
        Template::List(templates) => {
            let mut items = Vec::new();
            for t in templates {
                match t {
                    Template::Ellipsis(inner) => {
                        // Expand the inner template for each value in the ellipsis binding
                        let expanded = expand_ellipsis_template(inner, bindings, span, macro_scope);
                        items.extend(expanded);
                    }
                    _ => {
                        items.push(expand_template(t, bindings, span, macro_scope));
                    }
                }
            }
            SExpr::List(items, span.with_scope(macro_scope))
        }
        Template::Ellipsis(_) => {
            // Ellipsis at top level shouldn't happen
            panic!("Unexpected ellipsis at top level of template");
        }
    }
}

/// Expand an ellipsis template, returning multiple S-expressions
fn expand_ellipsis_template(
    template: &Template,
    bindings: &HashMap<String, PatternBinding>,
    span: &Span,
    macro_scope: ScopeId,
) -> Vec<SExpr> {
    // Find how many iterations we need by checking list bindings
    let count = find_ellipsis_count(template, bindings);

    (0..count)
        .map(|i| expand_template_at_index(template, bindings, span, macro_scope, i))
        .collect()
}

/// Find the number of elements in ellipsis bindings
fn find_ellipsis_count(template: &Template, bindings: &HashMap<String, PatternBinding>) -> usize {
    match template {
        Template::Variable(name) => match bindings.get(name) {
            Some(PatternBinding::List(items)) => items.len(),
            _ => 0,
        },
        Template::List(templates) => {
            // Find the first list binding
            for t in templates {
                let count = find_ellipsis_count(t, bindings);
                if count > 0 {
                    return count;
                }
            }
            0
        }
        _ => 0,
    }
}

/// Expand a template at a specific ellipsis index
fn expand_template_at_index(
    template: &Template,
    bindings: &HashMap<String, PatternBinding>,
    span: &Span,
    macro_scope: ScopeId,
    index: usize,
) -> SExpr {
    match template {
        Template::Variable(name) => match bindings.get(name) {
            Some(PatternBinding::List(items)) => items
                .get(index)
                .cloned()
                .unwrap_or_else(|| SExpr::List(vec![], span.clone())),
            Some(PatternBinding::Single(expr)) => expr.clone(),
            None => SExpr::Sym(name.clone(), span.with_scope(macro_scope)),
        },
        Template::Symbol(name) => SExpr::Sym(name.clone(), span.with_scope(macro_scope)),
        Template::Atom(sexpr) => add_scope_to_sexpr(sexpr, macro_scope),
        Template::List(templates) => {
            let items: Vec<_> = templates
                .iter()
                .map(|t| match t {
                    Template::Ellipsis(inner) => {
                        // Nested ellipsis - expand at this index
                        expand_template_at_index(inner, bindings, span, macro_scope, index)
                    }
                    _ => expand_template_at_index(t, bindings, span, macro_scope, index),
                })
                .collect();
            SExpr::List(items, span.with_scope(macro_scope))
        }
        Template::Ellipsis(inner) => {
            expand_template_at_index(inner, bindings, span, macro_scope, index)
        }
    }
}

/// Add a scope to an S-expression
fn add_scope_to_sexpr(sexpr: &SExpr, scope: ScopeId) -> SExpr {
    match sexpr {
        SExpr::Sym(s, span) => SExpr::Sym(s.clone(), span.with_scope(scope)),
        SExpr::Int { value, ty, span } => SExpr::Int {
            value: *value,
            ty: ty.clone(),
            span: span.with_scope(scope),
        },
        SExpr::Float { value, ty, span } => SExpr::Float {
            value: *value,
            ty: ty.clone(),
            span: span.with_scope(scope),
        },
        SExpr::Str(s, span) => SExpr::Str(s.clone(), span.with_scope(scope)),
        SExpr::List(items, span) => SExpr::List(
            items.iter().map(|i| add_scope_to_sexpr(i, scope)).collect(),
            span.with_scope(scope),
        ),
        SExpr::Quasiquote(inner, span) => SExpr::Quasiquote(
            Box::new(add_scope_to_sexpr(inner, scope)),
            span.with_scope(scope),
        ),
        SExpr::Unquote(inner, span) => SExpr::Unquote(
            Box::new(add_scope_to_sexpr(inner, scope)),
            span.with_scope(scope),
        ),
        SExpr::UnquoteSplice(inner, span) => SExpr::UnquoteSplice(
            Box::new(add_scope_to_sexpr(inner, scope)),
            span.with_scope(scope),
        ),
        SExpr::SyntaxQuote(inner, span) => SExpr::SyntaxQuote(
            Box::new(add_scope_to_sexpr(inner, scope)),
            span.with_scope(scope),
        ),
        SExpr::Quasisyntax(inner, span) => SExpr::Quasisyntax(
            Box::new(add_scope_to_sexpr(inner, scope)),
            span.with_scope(scope),
        ),
        SExpr::Unsyntax(inner, span) => SExpr::Unsyntax(
            Box::new(add_scope_to_sexpr(inner, scope)),
            span.with_scope(scope),
        ),
        SExpr::UnsyntaxSplice(inner, span) => SExpr::UnsyntaxSplice(
            Box::new(add_scope_to_sexpr(inner, scope)),
            span.with_scope(scope),
        ),
    }
}

// Evaluate quasiquoted template with substitutions
// macro_scope: Optional scope to add to template-introduced identifiers (for hygiene)
#[allow(clippy::only_used_in_recursion)]
fn eval_quasiquote(
    template: &SExpr,
    subs: &HashMap<String, SExpr>,
    span: &Span,
    macro_scope: Option<ScopeId>,
) -> SExpr {
    match template {
        SExpr::Quasiquote(inner, inner_span) => {
            // Nested quasiquote - increase depth conceptually
            // For now, just return as-is (proper nesting would need depth tracking)
            SExpr::Quasiquote(
                Box::new(eval_quasiquote(inner, subs, inner_span, macro_scope)),
                add_scope_to_span(inner_span, macro_scope),
            )
        }
        SExpr::Unquote(inner, _) => {
            // Unquote - substitute the inner expression
            // IMPORTANT: Unquoted expressions keep their ORIGINAL scopes (call site scopes)
            // This is key to hygiene - user code is not affected by macro's scope
            match inner.as_ref() {
                SExpr::Sym(name, sym_span) => {
                    if let Some(replacement) = subs.get(name) {
                        // Substitution from call site - keep original scopes
                        replacement.clone()
                    } else {
                        // Not a macro parameter - keep as symbol with original scopes
                        SExpr::Sym(name.clone(), sym_span.clone())
                    }
                }
                // For complex expressions in unquote, evaluate but don't add macro scope
                other => eval_quasiquote(other, subs, span, None),
            }
        }
        SExpr::UnquoteSplice(_, _) => {
            // Unquote-splice should only appear inside lists
            panic!("Unquote-splice (,@) can only appear inside a list");
        }
        SExpr::List(items, list_span) => {
            // Recursively process list, handling unquote-splice
            let mut result = Vec::new();
            for item in items {
                match item {
                    SExpr::UnquoteSplice(inner, _) => {
                        // Splice the contents into the result
                        // Unquote-splice uses call site scopes (no macro scope added)
                        let spliced = eval_quasiquote(inner, subs, span, None);
                        match spliced {
                            SExpr::List(splice_items, _) => {
                                result.extend(splice_items);
                            }
                            other => {
                                // If not a list, just add it (could be an error)
                                result.push(other);
                            }
                        }
                    }
                    _ => {
                        result.push(eval_quasiquote(item, subs, span, macro_scope));
                    }
                }
            }
            // Add macro scope to the list span (template-introduced)
            SExpr::List(result, add_scope_to_span(list_span, macro_scope))
        }
        SExpr::Sym(name, sym_span) => {
            // Template-introduced symbol - add macro scope for hygiene
            // This makes it distinct from same-named symbols at the call site
            SExpr::Sym(name.clone(), add_scope_to_span(sym_span, macro_scope))
        }
        SExpr::Int {
            value,
            ty,
            span: int_span,
        } => SExpr::Int {
            value: *value,
            ty: ty.clone(),
            span: add_scope_to_span(int_span, macro_scope),
        },
        SExpr::Float {
            value,
            ty,
            span: float_span,
        } => SExpr::Float {
            value: *value,
            ty: ty.clone(),
            span: add_scope_to_span(float_span, macro_scope),
        },
        SExpr::Str(s, str_span) => SExpr::Str(s.clone(), add_scope_to_span(str_span, macro_scope)),
        // New syntax forms - pass through with scope
        SExpr::SyntaxQuote(inner, sq_span) => SExpr::SyntaxQuote(
            Box::new(eval_quasiquote(inner, subs, span, macro_scope)),
            add_scope_to_span(sq_span, macro_scope),
        ),
        SExpr::Quasisyntax(inner, qs_span) => SExpr::Quasisyntax(
            Box::new(eval_quasiquote(inner, subs, span, macro_scope)),
            add_scope_to_span(qs_span, macro_scope),
        ),
        SExpr::Unsyntax(inner, us_span) => SExpr::Unsyntax(
            Box::new(eval_quasiquote(inner, subs, span, macro_scope)),
            add_scope_to_span(us_span, macro_scope),
        ),
        SExpr::UnsyntaxSplice(inner, uss_span) => SExpr::UnsyntaxSplice(
            Box::new(eval_quasiquote(inner, subs, span, macro_scope)),
            add_scope_to_span(uss_span, macro_scope),
        ),
    }
}

// Helper to add a scope to a span if provided
fn add_scope_to_span(span: &Span, scope: Option<ScopeId>) -> Span {
    match scope {
        Some(s) => span.with_scope(s),
        None => span.clone(),
    }
}

// ===========================================================================
// Generics + traits pre-pass (monomorphization / dictionary erasure)
//
// Lowers `trait`, `instance`, and generic `fn` (those with a `where` clause)
// into plain monomorphic `fn` forms. Runs after macro expansion and before
// `parse_program`, so the rest of the typed pipeline is untouched.
//   - Each instance method becomes a concrete top-level fn.
//   - Each use of a generic fn at a concrete type is specialized to a copy.
//   - Inside a copy, trait-method calls resolve to the concrete instance fn.
// ===========================================================================

#[derive(Debug, Clone)]
struct GenericFnDef {
    name: String,
    tparams: Vec<String>,               // type parameters (declaration order)
    constraints: Vec<(String, String)>, // (trait, type parameter it constrains)
    func_params: Vec<usize>,            // argument positions of function-typed parameters
    params: SExpr,                      // ((name type) ...) with type params still symbolic
    ret: SExpr,                         // return type expr with type params still symbolic
    body: SExpr,
}

/// A generic variant template: `(variant (Name T ...) (case payload...) ...)`.
/// Monomorphized on demand into a concrete nominal variant with per-instantiation
/// case names, mirroring how generic functions are specialized.
#[derive(Clone)]
struct GenericTypeDef {
    tparams: Vec<String>,
    cases: Vec<SExpr>, // each `(case-name payload-type...)`, type params symbolic
    span: Span,
}

/// A generic record template: `(record (Name T ...) (field T) ...)`. Monomorphized
/// by name into a concrete nominal record, the same way generic variants are.
#[derive(Clone)]
struct GenericRecordDef {
    tparams: Vec<String>,
    fields: Vec<SExpr>, // each `(field-name field-type)`, type params symbolic
    span: Span,
}

/// One monomorphization request: a template specialized at concrete types (one binding
/// per type parameter, in declaration order) and function-name arguments.
#[derive(Debug, Clone)]
struct SpecKey {
    name: String,
    bindings: Vec<(String, String)>, // (type parameter, concrete type)
    func_args: Vec<String>,
}

/// True when a parameter's type expr is a function type `(-> arg... ret)`.
fn is_func_type(ty: &SExpr) -> bool {
    matches!(ty, SExpr::List(items, _) if head_sym(items) == Some("->"))
}

/// Argument positions of the function-typed parameters in a param list.
fn func_param_indices(params: &SExpr) -> Vec<usize> {
    let mut out = Vec::new();
    if let SExpr::List(items, _) = params {
        for (i, p) in items.iter().enumerate() {
            if let Some((_, ty)) = param_name_and_type(p)
                && is_func_type(ty)
            {
                out.push(i);
            }
        }
    }
    out
}

/// The parameter name at a given position in a param list.
fn param_name_at(params: &SExpr, idx: usize) -> Option<String> {
    if let SExpr::List(items, _) = params {
        param_name_and_type(items.get(idx)?).map(|(n, _)| n.to_string())
    } else {
        None
    }
}

/// The mangled name of a specialized template: base, then each function argument,
/// then the concrete type (if any). e.g. `map--double--s32`, `apply-twice--inc`.
fn template_fn_name(base: &str, func_args: &[String], concretes: &[String]) -> String {
    let mut s = base.to_string();
    for fa in func_args {
        s.push_str("--");
        s.push_str(&sanitize_method(fa));
    }
    for c in concretes {
        s.push_str("--");
        s.push_str(c);
    }
    s
}

/// A trait method's declared signature (the type parameter is still symbolic).
#[derive(Debug, Clone)]
struct TraitMethodSig {
    name: String,
    params: SExpr,
    ret: SExpr,
}

/// A declared trait: its type parameters and its method signatures.
#[derive(Debug, Clone)]
struct TraitDef {
    tparams: Vec<String>,
    methods: Vec<TraitMethodSig>,
}

/// Parse a bodyless trait method signature: `(fn name params [:] ret)`.
fn parse_method_sig(mi: &[SExpr]) -> Option<TraitMethodSig> {
    let name = match mi.get(1)? {
        SExpr::Sym(s, _) => s.clone(),
        _ => return None,
    };
    let params = mi.get(2)?.clone();
    let mut idx = 3;
    if mi.get(idx).is_some_and(is_colon) {
        idx += 1;
    }
    let ret = mi.get(idx)?.clone();
    if idx != mi.len() - 1 {
        return None; // a trait method has no body
    }
    Some(TraitMethodSig { name, params, ret })
}

/// Structural equality of two type expressions, ignoring spans.
fn type_expr_eq(a: &SExpr, b: &SExpr) -> bool {
    match (a, b) {
        (SExpr::Sym(x, _), SExpr::Sym(y, _)) => x == y,
        (SExpr::List(xs, _), SExpr::List(ys, _)) => {
            xs.len() == ys.len() && xs.iter().zip(ys).all(|(p, q)| type_expr_eq(p, q))
        }
        _ => false,
    }
}

/// The type expr of each parameter in a param list (colon or bare form).
fn param_type_exprs(params: &SExpr) -> Vec<&SExpr> {
    let mut out = Vec::new();
    if let SExpr::List(items, _) = params {
        for p in items {
            if let Some((_, ty)) = param_name_and_type(p) {
                out.push(ty);
            }
        }
    }
    out
}

/// Context for resolving trait-method calls inside a specialized body.
struct MethodCtx {
    constraints: Vec<(String, String)>, // (trait, type parameter it constrains)
    bindings: Vec<(String, String)>,    // (type parameter, concrete type)
}

struct Lowering<'a> {
    ctx: &'a CompileContext,
    generics: HashMap<String, GenericFnDef>,
    traits: HashMap<String, TraitDef>, // trait name -> declaration (type params + sigs)
    method_to_trait: HashMap<String, String>,
    monofn_returns: HashMap<String, String>, // fn name -> scalar return type string
    fn_params: HashMap<String, Vec<Option<String>>>, // concrete fn name -> per-param type strings
    instances: HashMap<(String, String), HashMap<String, String>>, // (trait, type-key)->method->fn
    instance_defs: HashMap<String, SExpr>,   // instance fn name -> its (renamed) fn form
    used_instances: HashSet<String>,         // instance fn names actually referenced
    instance_worklist: Vec<String>,          // instance fns awaiting emission
    worklist: Vec<SpecKey>,                  // template specializations awaiting emission
    emitted: HashSet<String>,                // mangled names already emitted
    generic_variants: HashMap<String, GenericTypeDef>, // generic variant templates
    case_to_generic: HashMap<String, String>, // variant case name -> generic variant name
    type_instances: HashMap<String, (String, Vec<String>)>, // mangled -> (generic, concretes)
    type_worklist: Vec<(String, Vec<String>)>, // (generic, concretes) awaiting emission
    generic_records: HashMap<String, GenericRecordDef>, // generic record templates
    record_instances: HashMap<String, (String, Vec<String>)>, // mangled -> (generic, concretes)
    record_worklist: Vec<(String, Vec<String>)>, // (generic, concretes) awaiting emission
}

/// The scalar type name for a type-expr that is a bare symbol (e.g. `s32`).
fn type_expr_string(e: &SExpr) -> Option<String> {
    match e {
        SExpr::Sym(s, _) => Some(s.clone()),
        _ => None,
    }
}

/// Map a literal's attached `Type` to its surface name.
fn scalar_type_name(ty: &Type) -> Option<String> {
    Some(
        match ty {
            Type::S32 => "s32",
            Type::S64 => "s64",
            Type::F32 => "f32",
            Type::F64 => "f64",
            Type::U8 => "u8",
            Type::Bool => "bool",
            Type::U16 => "u16",
            Type::U32 => "u32",
            Type::U64 => "u64",
            _ => return None,
        }
        .to_string(),
    )
}

/// A canonical string for a concrete type expr, supporting nesting:
/// `s32`, `(list s32)`, `(option (list s32))`, `(tuple s32 f64)`, ...
fn canonical_type(e: &SExpr) -> Option<String> {
    match e {
        SExpr::Sym(s, _) => Some(s.clone()),
        SExpr::List(items, _) => {
            let parts = items
                .iter()
                .map(canonical_type)
                .collect::<Option<Vec<_>>>()?;
            Some(format!("({})", parts.join(" ")))
        }
        _ => None,
    }
}

/// Parse a canonical type string (e.g. `(list s32)`) back into a type expr.
fn type_str_to_expr(s: &str) -> Option<SExpr> {
    let toks = tokenize(s);
    if toks.is_empty() {
        return None;
    }
    let (e, _) = parse_sexpr(&toks, 0);
    Some(e)
}

/// The element type of a canonical list type string, e.g. `(list s32)` -> `s32`.
fn list_elem_type(s: &str) -> Option<String> {
    match type_str_to_expr(s)? {
        SExpr::List(items, _) if items.len() == 2 && head_sym(&items) == Some("list") => {
            canonical_type(&items[1])
        }
        _ => None,
    }
}

/// Structurally unify a parameter type pattern against a concrete type expr,
/// accumulating a binding for each of `tparams` it can determine (first wins).
/// e.g. pattern `(-> T U)` vs concrete `(-> s32 f64)` binds `T=s32`, `U=f64`.
fn unify_types(pat: &SExpr, concrete: &SExpr, tparams: &[String], out: &mut Vec<(String, String)>) {
    match (pat, concrete) {
        (SExpr::Sym(s, _), _) if tparams.iter().any(|t| t == s) => {
            if !out.iter().any(|(k, _)| k == s)
                && let Some(c) = canonical_type(concrete)
            {
                out.push((s.clone(), c));
            }
        }
        (SExpr::List(ps, _), SExpr::List(cs, _)) if ps.len() == cs.len() => {
            for (p, c) in ps.iter().zip(cs) {
                unify_types(p, c, tparams, out);
            }
        }
        _ => {}
    }
}

/// Apply every `(type parameter, concrete)` substitution to a type expr.
fn subst_types(e: &SExpr, bindings: &[(String, String)]) -> SExpr {
    let mut out = e.clone();
    for (tp, concrete) in bindings {
        out = subst_type(&out, tp, concrete);
    }
    out
}

/// Substitute a type parameter symbol with a concrete type throughout a type expr.
/// The concrete type may itself be compound (e.g. `(list s32)`).
fn subst_type(e: &SExpr, tparam: &str, concrete: &str) -> SExpr {
    match e {
        SExpr::Sym(s, span) if s == tparam => {
            if concrete.contains('(') {
                type_str_to_expr(concrete)
                    .unwrap_or_else(|| SExpr::Sym(concrete.to_string(), span.clone()))
            } else {
                SExpr::Sym(concrete.to_string(), span.clone())
            }
        }
        SExpr::List(items, span) => SExpr::List(
            items
                .iter()
                .map(|i| subst_type(i, tparam, concrete))
                .collect(),
            span.clone(),
        ),
        other => other.clone(),
    }
}

/// Extract the name symbol and type expr from one param, for either
/// `(name type)` or `(name : type)`.
fn param_name_and_type(p: &SExpr) -> Option<(&str, &SExpr)> {
    if let SExpr::List(pp, _) = p {
        let name = match pp.first() {
            Some(SExpr::Sym(n, _)) => n.as_str(),
            _ => return None,
        };
        let ty = match pp.len() {
            2 => &pp[1],
            3 if matches!(&pp[1], SExpr::Sym(s, _) if s == ":") => &pp[2],
            _ => return None,
        };
        return Some((name, ty));
    }
    None
}

/// Strip a `(lin T)` / `(aff T)` multiplicity qualifier, returning the inner type
/// expr. The lowering pass reasons about types structurally and ignores
/// multiplicity (the linearity checker handles it later), so its type lookups
/// see `T`, not `(lin T)`.
fn unwrap_mult(e: &SExpr) -> &SExpr {
    if let SExpr::List(items, _) = e
        && items.len() == 2
        && matches!(head_sym(items), Some("lin") | Some("aff"))
    {
        &items[1]
    } else {
        e
    }
}

/// Build a name->canonical-type environment from a param list SExpr. Compound
/// types (e.g. `(list s32)`) are included, so structural inference can use them.
fn param_env(params: &SExpr) -> Vec<(String, String)> {
    let mut env = Vec::new();
    if let SExpr::List(items, _) = params {
        for p in items {
            if let Some((n, ty)) = param_name_and_type(p)
                && let Some(ts) = canonical_type(unwrap_mult(ty))
            {
                env.push((n.to_string(), ts));
            }
        }
    }
    env
}

/// Per-parameter declared canonical type strings (None where absent).
fn param_type_strings(params: &SExpr) -> Vec<Option<String>> {
    let mut out = Vec::new();
    if let SExpr::List(items, _) = params {
        for p in items {
            match param_name_and_type(p) {
                Some((_, ty)) => out.push(canonical_type(unwrap_mult(ty))),
                None => out.push(None),
            }
        }
    }
    out
}

fn head_sym(items: &[SExpr]) -> Option<&str> {
    match items.first() {
        Some(SExpr::Sym(s, _)) => Some(s.as_str()),
        _ => None,
    }
}

/// True when an SExpr is the bare `:` symbol used for type annotations.
fn is_colon(e: &SExpr) -> bool {
    matches!(e, SExpr::Sym(s, _) if s == ":")
}

/// True for the built-in scalar type names.
fn is_scalar_name(s: &str) -> bool {
    matches!(s, "s32" | "s64" | "f32" | "f64" | "u8")
}

/// Structural view of a `fn` form. Tolerates an optional `:` before the return
/// type and an optional `(where ...)` clause:
///   (fn name (params) [:] ret [(where ...)] body)
struct FnShape<'a> {
    name: &'a SExpr,
    params: &'a SExpr,
    ret: &'a SExpr,
    where_clause: Option<&'a SExpr>,
    body: &'a SExpr,
}

fn fn_shape(items: &[SExpr]) -> Option<FnShape<'_>> {
    // items[0] == "fn" is checked by the caller.
    let name = items.get(1)?;
    let params = items.get(2)?;
    let mut idx = 3;
    if items.get(idx).is_some_and(is_colon) {
        idx += 1;
    }
    let ret = items.get(idx)?;
    idx += 1;
    let where_clause = if idx + 1 < items.len()
        && matches!(items.get(idx), Some(SExpr::List(w, _)) if head_sym(w) == Some("where"))
    {
        let w = items.get(idx);
        idx += 1;
        w
    } else {
        None
    };
    let body = items.get(idx)?;
    if idx != items.len() - 1 {
        return None; // trailing junk after the body
    }
    Some(FnShape {
        name,
        params,
        ret,
        where_clause,
        body,
    })
}

fn sanitize_method(m: &str) -> String {
    m.chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '-' {
                c.to_string()
            } else {
                format!("c{}", c as u32)
            }
        })
        .collect()
}

fn instance_fn_name(trait_name: &str, types: &[String], method: &str) -> String {
    format!(
        "{}--{}--{}",
        trait_name,
        sanitize_method(method),
        types.join("--")
    )
}

/// The internal map key for an instance: the trait's type arguments, joined.
fn instance_key(types: &[String]) -> String {
    types.join(",")
}

impl<'a> Lowering<'a> {
    /// Infer the surface type name of an expression (literals, params, known calls).
    fn infer_type(&self, e: &SExpr, env: &[(String, String)]) -> Option<String> {
        match e {
            SExpr::Int { ty, .. } => scalar_type_name(ty),
            SExpr::Float { ty, .. } => scalar_type_name(ty),
            SExpr::Sym(name, _) => env.iter().find(|(n, _)| n == name).map(|(_, t)| t.clone()),
            SExpr::List(items, _) => {
                let head = head_sym(items)?;
                if let Some(info) = lookup_wasm_instr(head) {
                    return scalar_type_name(&info.result);
                }
                // Built-in list/string operations with a known result type.
                match head {
                    "list-new" => {
                        return Some(format!("(list {})", canonical_type(items.get(1)?)?));
                    }
                    "list-len" | "string-len" | "string-ref" => return Some("s32".to_string()),
                    "list-push" => return self.infer_type(items.get(1)?, env),
                    "list-get" => {
                        let lt = self.infer_type(items.get(1)?, env)?;
                        return list_elem_type(&lt);
                    }
                    _ => {}
                }
                // A call to a template (generic and/or higher-order): its result type
                // is the return type with the type parameters replaced by the inferred
                // type arguments. Function arguments do not affect the return.
                if let Some(genfn) = self.generics.get(head) {
                    let bindings = self.infer_bindings(genfn, &items[1..], env);
                    let ret = subst_types(&genfn.ret, &bindings);
                    return canonical_type(&ret);
                }
                // A generic record construction's type is its mangled instantiation.
                if self.generic_records.contains_key(head) {
                    return self.generic_record_instance(head, &items[1..], env);
                }
                self.monofn_returns.get(head).cloned()
            }
            _ => None,
        }
    }

    /// The signature of a known monomorphic function as a `(-> arg... ret)` type expr,
    /// used to unify a function parameter's declared type against the actual function.
    fn func_signature_expr(&self, fname: &str) -> Option<SExpr> {
        let params = self.fn_params.get(fname)?;
        let ret = self.monofn_returns.get(fname)?;
        let mut parts = vec!["->".to_string()];
        for p in params {
            parts.push(p.clone()?);
        }
        parts.push(ret.clone());
        type_str_to_expr(&format!("({})", parts.join(" ")))
    }

    /// Infer a concrete binding for each type parameter of a template call. A value
    /// parameter's type pattern is unified against the argument's inferred type; a
    /// function parameter's `(-> ...)` is unified against the function argument's
    /// signature. Bindings are returned in type-parameter declaration order.
    fn infer_bindings(
        &self,
        genfn: &GenericFnDef,
        args: &[SExpr],
        env: &[(String, String)],
    ) -> Vec<(String, String)> {
        let mut found: Vec<(String, String)> = Vec::new();
        let ptypes = param_type_exprs(&genfn.params);
        for (i, pexpr) in ptypes.iter().enumerate() {
            let Some(a) = args.get(i) else { continue };
            if genfn.func_params.contains(&i) {
                if let SExpr::Sym(fname, _) = a
                    && let Some(sig) = self.func_signature_expr(fname)
                {
                    unify_types(pexpr, &sig, &genfn.tparams, &mut found);
                }
            } else if let Some(cstr) = self.infer_type(a, env)
                && let Some(cexpr) = type_str_to_expr(&cstr)
            {
                unify_types(pexpr, &cexpr, &genfn.tparams, &mut found);
            }
        }
        // Return in declaration order, keeping only parameters that got a binding.
        genfn
            .tparams
            .iter()
            .filter_map(|tp| {
                found
                    .iter()
                    .find(|(k, _)| k == tp)
                    .map(|(_, c)| (tp.clone(), c.clone()))
            })
            .collect()
    }

    /// Resolve which concrete types a trait-method call uses, one per trait type
    /// parameter (in declaration order). Bindings come from the argument types, the
    /// expected type (return-position dispatch), and the enclosing generic's bindings.
    /// Returns None if any type parameter is left unbound.
    fn resolve_trait_types(
        &self,
        trait_name: &str,
        method: &str,
        args: &[SExpr],
        env: &[(String, String)],
        expected: Option<&str>,
        mrc: Option<&MethodCtx>,
    ) -> Option<Vec<String>> {
        let tdef = self.traits.get(trait_name)?;
        let sig = tdef.methods.iter().find(|m| m.name == method)?;
        let mut bindings: Vec<(String, String)> = Vec::new();

        // From the argument types.
        for (i, pexpr) in param_type_exprs(&sig.params).iter().enumerate() {
            if let Some(a) = args.get(i)
                && let Some(cs) = self.infer_type(a, env)
                && let Some(ce) = type_str_to_expr(&cs)
            {
                unify_types(pexpr, &ce, &tdef.tparams, &mut bindings);
            }
        }
        // From the expected type, against the return type (return-position dispatch).
        if let Some(exp) = expected
            && let Some(ee) = type_str_to_expr(exp)
        {
            unify_types(&sig.ret, &ee, &tdef.tparams, &mut bindings);
        }
        // From the enclosing generic body: a `(where (Trait G) ...)` constraint maps
        // the generic's type parameter `G` to this (single-parameter) trait's own type
        // parameter, so we can use `G`'s binding when arguments don't fix it.
        if let Some(mc) = mrc
            && tdef.tparams.len() == 1
            && !bindings.iter().any(|(k, _)| k == &tdef.tparams[0])
        {
            for (tn, gtp) in &mc.constraints {
                if tn == trait_name
                    && let Some((_, gc)) = mc.bindings.iter().find(|(k, _)| k == gtp)
                {
                    bindings.push((tdef.tparams[0].clone(), gc.clone()));
                    break;
                }
            }
        }

        let types: Vec<String> = tdef
            .tparams
            .iter()
            .filter_map(|tp| {
                bindings
                    .iter()
                    .find(|(k, _)| k == tp)
                    .map(|(_, c)| c.clone())
            })
            .collect();
        (types.len() == tdef.tparams.len()).then_some(types)
    }

    /// Rewrite a body: resolve trait methods (when `mrc` is set) and generic calls.
    ///
    /// `expected` is the type this expression is used at, flowing *down* from
    /// context (a return annotation, an ascription, an `if`/`let` tail, or a
    /// sibling argument). It supplies the missing type for return-type dispatch
    /// (methods like `zero : T` whose type parameter is only in the return).
    fn walk(
        &mut self,
        e: &SExpr,
        env: &[(String, String)],
        mrc: Option<&MethodCtx>,
        expected: Option<String>,
    ) -> Result<SExpr> {
        match e {
            SExpr::List(items, span) => {
                // Colon ascription `(expr : type)`: rewrite a generic ADT annotation
                // to its concrete name and steer the expected type into `expr`.
                if items.len() == 3 && is_colon(&items[1]) {
                    let rewritten_ty = self.rewrite_type_expr(&items[2]);
                    let exp = canonical_type(&rewritten_ty);
                    let inner = self.walk(&items[0], env, mrc, exp)?;
                    return Ok(SExpr::List(
                        vec![inner, items[1].clone(), rewritten_ty],
                        span.clone(),
                    ));
                }

                if let Some(head) = head_sym(items) {
                    // Trait-method call: resolve to a concrete instance function.
                    // Works anywhere, not only inside a generic body — the dispatch
                    // type comes from the arguments, the expected type, or (inside a
                    // generic) the type-parameter binding.
                    if let Some(trait_name) = self.method_to_trait.get(head).cloned() {
                        // Bind the trait's type parameters from the argument types, the
                        // expected type, and (inside a generic) the enclosing bindings.
                        let types = self
                            .resolve_trait_types(
                                &trait_name,
                                head,
                                &items[1..],
                                env,
                                expected.as_deref(),
                                mrc,
                            )
                            .ok_or_else(|| {
                                self.ctx.error(
                                    format!(
                                        "cannot resolve trait method '{}': no instance matches the argument types or the expected type",
                                        head
                                    ),
                                    span,
                                )
                            })?;
                        let fname = self
                            .instances
                            .get(&(trait_name.clone(), instance_key(&types)))
                            .and_then(|m| m.get(head))
                            .cloned()
                            .ok_or_else(|| {
                                self.ctx.error(
                                    format!(
                                        "no instance of trait '{}' for {:?} (method '{}')",
                                        trait_name, types, head
                                    ),
                                    span,
                                )
                            })?;
                        // Mark this instance for emission (instances are emitted on
                        // demand, so an unused stdlib costs nothing).
                        self.mark_instance(&fname);
                        // Each argument is expected at the instance method's param type.
                        let arg_exp = self.fn_params.get(&fname).cloned().unwrap_or_default();
                        let mut new_items = Vec::with_capacity(items.len());
                        new_items.push(SExpr::Sym(fname, items[0].span().clone()));
                        for (i, a) in items[1..].iter().enumerate() {
                            let exp = arg_exp.get(i).cloned().flatten();
                            new_items.push(self.walk(a, env, mrc, exp)?);
                        }
                        return Ok(SExpr::List(new_items, span.clone()));
                    }

                    // Template call (generic and/or higher-order)?
                    if let Some(genfn) = self.generics.get(head).cloned() {
                        let args = &items[1..];
                        // Collect the function-name argument at each function parameter.
                        let mut func_args: Vec<String> = Vec::new();
                        for &fi in &genfn.func_params {
                            match args.get(fi) {
                                Some(SExpr::Sym(fname, _)) => func_args.push(fname.clone()),
                                _ => {
                                    return Err(self.ctx.error(
                                        format!(
                                            "higher-order argument {} to '{}' must be a function name",
                                            fi + 1,
                                            genfn.name
                                        ),
                                        span,
                                    ));
                                }
                            }
                        }
                        // Infer a binding for each type parameter from the arguments.
                        let mut bindings = self.infer_bindings(&genfn, args, env);
                        // Any parameter still unbound (e.g. one appearing only in the
                        // return type) is taken from the expected type, by unifying the
                        // return type against it.
                        if bindings.len() < genfn.tparams.len()
                            && let Some(exp) = expected.as_deref()
                            && let Some(ee) = type_str_to_expr(exp)
                        {
                            unify_types(&genfn.ret, &ee, &genfn.tparams, &mut bindings);
                            bindings = genfn
                                .tparams
                                .iter()
                                .filter_map(|tp| {
                                    bindings
                                        .iter()
                                        .find(|(k, _)| k == tp)
                                        .map(|(_, c)| (tp.clone(), c.clone()))
                                })
                                .collect();
                        }
                        if bindings.len() < genfn.tparams.len() {
                            return Err(self.ctx.error(
                                format!(
                                    "cannot infer type argument(s) for generic '{}'",
                                    genfn.name
                                ),
                                span,
                            ));
                        }
                        let concretes: Vec<String> =
                            bindings.iter().map(|(_, c)| c.clone()).collect();
                        self.worklist.push(SpecKey {
                            name: genfn.name.clone(),
                            bindings: bindings.clone(),
                            func_args: func_args.clone(),
                        });
                        let mname = template_fn_name(&genfn.name, &func_args, &concretes);
                        let gptypes = param_type_strings(&genfn.params);
                        let mut new_items = Vec::with_capacity(items.len());
                        new_items.push(SExpr::Sym(mname, items[0].span().clone()));
                        for (i, a) in args.iter().enumerate() {
                            // Function arguments are compile-time; drop them from the call.
                            if genfn.func_params.contains(&i) {
                                continue;
                            }
                            // A parameter typed as a bare type parameter is expected at
                            // that parameter's concrete binding.
                            let exp = gptypes.get(i).cloned().flatten().map(|s| {
                                bindings
                                    .iter()
                                    .find(|(tp, _)| *tp == s)
                                    .map(|(_, c)| c.clone())
                                    .unwrap_or(s)
                            });
                            new_items.push(self.walk(a, env, mrc, exp)?);
                        }
                        return Ok(SExpr::List(new_items, span.clone()));
                    }

                    // Generic ADT constructor: a case of a generic variant. The
                    // instantiation is inferred from the argument types, falling back
                    // to the expected type for nullary / under-determined cases.
                    if let Some(gen_name) = self.case_to_generic.get(head).cloned() {
                        let gdef = self.generic_variants.get(&gen_name).cloned().unwrap();
                        let args = &items[1..];
                        let payload_pats: Vec<SExpr> = gdef
                            .cases
                            .iter()
                            .find_map(|c| match c {
                                SExpr::List(ci, _) if head_sym(ci) == Some(head) => {
                                    Some(ci[1..].to_vec())
                                }
                                _ => None,
                            })
                            .unwrap_or_default();
                        if args.len() != payload_pats.len() {
                            return Err(self.ctx.error(
                                format!(
                                    "variant case '{}' expects {} argument(s), got {}",
                                    head,
                                    payload_pats.len(),
                                    args.len()
                                ),
                                span,
                            ));
                        }
                        // Infer type-parameter bindings from the argument types.
                        let mut found: Vec<(String, String)> = Vec::new();
                        for (pat, a) in payload_pats.iter().zip(args) {
                            if let Some(cs) = self.infer_type(a, env)
                                && let Some(ce) = type_str_to_expr(&cs)
                            {
                                unify_types(pat, &ce, &gdef.tparams, &mut found);
                            }
                        }
                        // Fallback: an expected type naming this generic supplies any
                        // still-unbound parameter (e.g. `((none) : (opt s32))`).
                        if found.len() < gdef.tparams.len()
                            && let Some(exp) = expected.as_deref()
                            && let Some((g, concretes)) = self.type_instances.get(exp).cloned()
                            && g == gen_name
                        {
                            for (tp, c) in gdef.tparams.iter().zip(concretes) {
                                if !found.iter().any(|(k, _)| k == tp) {
                                    found.push((tp.clone(), c));
                                }
                            }
                        }
                        let mut concretes = Vec::new();
                        for tp in &gdef.tparams {
                            match found.iter().find(|(k, _)| k == tp) {
                                Some((_, c)) => concretes.push(c.clone()),
                                None => {
                                    return Err(self.ctx.error(
                                        format!(
                                            "cannot infer type argument '{}' for constructor '{}'; add a type annotation",
                                            tp, head
                                        ),
                                        span,
                                    ));
                                }
                            }
                        }
                        let mangled_type = self.queue_variant(&gen_name, &concretes);
                        let mangled_case = Self::mangle_case_name(head, &mangled_type);
                        let bindings: Vec<(String, String)> = gdef
                            .tparams
                            .iter()
                            .cloned()
                            .zip(concretes.iter().cloned())
                            .collect();
                        let mut new_items = Vec::with_capacity(items.len());
                        new_items.push(SExpr::Sym(mangled_case, items[0].span().clone()));
                        for (pat, a) in payload_pats.iter().zip(args) {
                            let exp = canonical_type(&subst_types(pat, &bindings));
                            new_items.push(self.walk(a, env, mrc, exp)?);
                        }
                        return Ok(SExpr::List(new_items, span.clone()));
                    }

                    // `match` on a generic-variant value: rewrite each arm's case name
                    // to the scrutinee's concrete instantiation and bind the arm
                    // variables at their substituted payload types.
                    if head == "match"
                        && items.len() >= 2
                        && let Some(sty) = self.infer_type(&items[1], env)
                        && let Some((gen_name, concretes)) = self.type_instances.get(&sty).cloned()
                    {
                        let gdef = self.generic_variants.get(&gen_name).cloned().unwrap();
                        let bindings: Vec<(String, String)> = gdef
                            .tparams
                            .iter()
                            .cloned()
                            .zip(concretes.iter().cloned())
                            .collect();
                        let new_scrut = self.walk(&items[1], env, mrc, None)?;
                        let mut new_items = vec![items[0].clone(), new_scrut];
                        for arm in &items[2..] {
                            if let SExpr::List(ai, aspan) = arm
                                && ai.len() == 2
                                && let SExpr::List(pat, pspan) = &ai[0]
                                && let Some(SExpr::Sym(case, cspan)) = pat.first()
                                && self.case_to_generic.get(case) == Some(&gen_name)
                            {
                                let payload_pats: Vec<SExpr> = gdef
                                    .cases
                                    .iter()
                                    .find_map(|c| match c {
                                        SExpr::List(ci, _)
                                            if head_sym(ci) == Some(case.as_str()) =>
                                        {
                                            Some(ci[1..].to_vec())
                                        }
                                        _ => None,
                                    })
                                    .unwrap_or_default();
                                let mut arm_env = env.to_vec();
                                for (bnd, pty) in pat[1..].iter().zip(&payload_pats) {
                                    if let SExpr::Sym(bn, _) = bnd
                                        && let Some(ts) =
                                            canonical_type(&subst_types(pty, &bindings))
                                    {
                                        arm_env.push((bn.clone(), ts));
                                    }
                                }
                                let new_body =
                                    self.walk(&ai[1], &arm_env, mrc, expected.clone())?;
                                let mut new_pat = pat.clone();
                                new_pat[0] =
                                    SExpr::Sym(Self::mangle_case_name(case, &sty), cspan.clone());
                                new_items.push(SExpr::List(
                                    vec![SExpr::List(new_pat, pspan.clone()), new_body],
                                    aspan.clone(),
                                ));
                            } else if let SExpr::List(ai, aspan) = arm
                                && ai.len() == 2
                            {
                                // A non-generic arm (e.g. the `_` wildcard): keep the
                                // pattern, walk only the body.
                                let new_body = self.walk(&ai[1], env, mrc, expected.clone())?;
                                new_items.push(SExpr::List(
                                    vec![ai[0].clone(), new_body],
                                    aspan.clone(),
                                ));
                            } else {
                                new_items.push(self.walk(arm, env, mrc, None)?);
                            }
                        }
                        return Ok(SExpr::List(new_items, span.clone()));
                    }

                    // Generic record field access `(Name.field expr)`: rewrite the
                    // accessor to the record expression's concrete instantiation.
                    if let Some((rec, field)) = head.split_once('.')
                        && self.generic_records.contains_key(rec)
                        && items.len() == 2
                    {
                        let expr_ty = self.infer_type(&items[1], env);
                        let new_expr = self.walk(&items[1], env, mrc, None)?;
                        let new_head = match expr_ty {
                            Some(t) if self.record_instances.contains_key(&t) => {
                                format!("{}.{}", t, field)
                            }
                            // Unresolved: leave the generic accessor; parse_program
                            // reports the unknown record type with a source span.
                            _ => head.to_string(),
                        };
                        return Ok(SExpr::List(
                            vec![SExpr::Sym(new_head, items[0].span().clone()), new_expr],
                            span.clone(),
                        ));
                    }

                    // Generic record construction `(Name field...)`: infer the
                    // instantiation from the field argument types (expected-type
                    // fallback for under-determined parameters), then rewrite to the
                    // concrete record constructor.
                    if let Some(gdef) = self.generic_records.get(head).cloned() {
                        let args = &items[1..];
                        let field_pats: Vec<SExpr> = gdef
                            .fields
                            .iter()
                            .filter_map(|f| match f {
                                SExpr::List(fd, _) if fd.len() >= 2 => Some(fd[1].clone()),
                                _ => None,
                            })
                            .collect();
                        if args.len() != field_pats.len() {
                            return Err(self.ctx.error(
                                format!(
                                    "record '{}' expects {} field(s), got {}",
                                    head,
                                    field_pats.len(),
                                    args.len()
                                ),
                                span,
                            ));
                        }
                        let mut found: Vec<(String, String)> = Vec::new();
                        for (pat, a) in field_pats.iter().zip(args) {
                            if let Some(cs) = self.infer_type(a, env)
                                && let Some(ce) = type_str_to_expr(&cs)
                            {
                                unify_types(pat, &ce, &gdef.tparams, &mut found);
                            }
                        }
                        if found.len() < gdef.tparams.len()
                            && let Some(exp) = expected.as_deref()
                            && let Some((g, concretes)) = self.record_instances.get(exp).cloned()
                            && g == *head
                        {
                            for (tp, c) in gdef.tparams.iter().zip(concretes) {
                                if !found.iter().any(|(k, _)| k == tp) {
                                    found.push((tp.clone(), c));
                                }
                            }
                        }
                        let mut concretes = Vec::new();
                        for tp in &gdef.tparams {
                            match found.iter().find(|(k, _)| k == tp) {
                                Some((_, c)) => concretes.push(c.clone()),
                                None => {
                                    return Err(self.ctx.error(
                                        format!(
                                            "cannot infer type argument '{}' for record '{}'; add a type annotation",
                                            tp, head
                                        ),
                                        span,
                                    ));
                                }
                            }
                        }
                        let mangled = self.queue_record(head, &concretes);
                        let bindings: Vec<(String, String)> = gdef
                            .tparams
                            .iter()
                            .cloned()
                            .zip(concretes.iter().cloned())
                            .collect();
                        let mut new_items = Vec::with_capacity(items.len());
                        new_items.push(SExpr::Sym(mangled, items[0].span().clone()));
                        for (pat, a) in field_pats.iter().zip(args) {
                            let exp = canonical_type(&subst_types(pat, &bindings));
                            new_items.push(self.walk(a, env, mrc, exp)?);
                        }
                        return Ok(SExpr::List(new_items, span.clone()));
                    }

                    // Forms that carry the expected type into their tail positions.
                    match head {
                        "if" if items.len() == 4 => {
                            let c = self.walk(&items[1], env, mrc, None)?;
                            let t = self.walk(&items[2], env, mrc, expected.clone())?;
                            let f = self.walk(&items[3], env, mrc, expected)?;
                            return Ok(SExpr::List(vec![items[0].clone(), c, t, f], span.clone()));
                        }
                        "let" if items.len() == 3 => {
                            // (let (name value) body) or (let (name : type value) body)
                            if let SExpr::List(bind, bspan) = &items[1] {
                                let (val_idx, val_exp) = if bind.len() == 4 && is_colon(&bind[1]) {
                                    (3, type_expr_string(&bind[2]))
                                } else if bind.len() == 2 {
                                    (1, None)
                                } else {
                                    (usize::MAX, None)
                                };
                                if val_idx != usize::MAX {
                                    let bound_ty = val_exp
                                        .clone()
                                        .or_else(|| self.infer_type(&bind[val_idx], env));
                                    let new_val = self.walk(&bind[val_idx], env, mrc, val_exp)?;
                                    let mut new_bind = bind.clone();
                                    new_bind[val_idx] = new_val;
                                    // The body may reference the bound name; record its type.
                                    let mut body_env = env.to_vec();
                                    if let (Some(SExpr::Sym(n, _)), Some(bt)) =
                                        (bind.first(), bound_ty)
                                    {
                                        body_env.push((n.clone(), bt));
                                    }
                                    let body = self.walk(&items[2], &body_env, mrc, expected)?;
                                    return Ok(SExpr::List(
                                        vec![
                                            items[0].clone(),
                                            SExpr::List(new_bind, bspan.clone()),
                                            body,
                                        ],
                                        span.clone(),
                                    ));
                                }
                            }
                        }
                        s if is_scalar_name(s) && items.len() == 2 => {
                            // scalar cast / ascription `(type expr)`
                            let x = self.walk(&items[1], env, mrc, Some(s.to_string()))?;
                            return Ok(SExpr::List(vec![items[0].clone(), x], span.clone()));
                        }
                        s if lookup_wasm_instr(s).is_some() => {
                            // A raw wasm instruction expects each argument at its
                            // operand type (e.g. `i32.add` wants two `s32`s).
                            let ptypes: Vec<Option<String>> = lookup_wasm_instr(s)
                                .unwrap()
                                .params
                                .iter()
                                .map(scalar_type_name)
                                .collect();
                            let mut new_items = Vec::with_capacity(items.len());
                            new_items.push(items[0].clone());
                            for (i, a) in items[1..].iter().enumerate() {
                                let exp = ptypes.get(i).cloned().flatten();
                                new_items.push(self.walk(a, env, mrc, exp)?);
                            }
                            return Ok(SExpr::List(new_items, span.clone()));
                        }
                        _ => {}
                    }
                }
                // Plain list: recurse into every element (no expected type).
                let mut new_items = Vec::with_capacity(items.len());
                for i in items {
                    new_items.push(self.walk(i, env, mrc, None)?);
                }
                Ok(SExpr::List(new_items, span.clone()))
            }
            // A default integer literal (there is no `s32` suffix, so `ty == S32`
            // is always a default) adopts the expected type: it widens to `s64`, or
            // promotes to a float. An explicit suffix (`s64`, `f32`, `f64`) is left
            // untouched, as is a literal with no expected type.
            SExpr::Int {
                value,
                ty: Type::S32,
                span,
            } if expected.is_some() => Ok(match expected.as_deref().unwrap() {
                "s64" => SExpr::Int {
                    value: *value,
                    ty: Type::S64,
                    span: span.clone(),
                },
                "f32" => SExpr::Float {
                    value: *value as f64,
                    ty: Type::F32,
                    span: span.clone(),
                },
                "f64" => SExpr::Float {
                    value: *value as f64,
                    ty: Type::F64,
                    span: span.clone(),
                },
                _ => e.clone(),
            }),
            other => Ok(other.clone()),
        }
    }

    /// Record that an instance method is used, queuing it for emission once.
    fn mark_instance(&mut self, fname: &str) {
        if self.used_instances.insert(fname.to_string()) {
            self.instance_worklist.push(fname.to_string());
        }
    }

    /// Mangle a generic type instantiation into a concrete nominal name:
    /// `box` @ `[s32]` -> `box$s32`; non-symbol characters in a nested argument
    /// (e.g. the parens of `(list s32)`) become `_`.
    fn mangle_type_name(base: &str, concretes: &[String]) -> String {
        let mut s = base.to_string();
        for c in concretes {
            s.push('$');
            for ch in c.chars() {
                if ch.is_alphanumeric() || ch == '-' {
                    s.push(ch);
                } else {
                    s.push('_');
                }
            }
        }
        s
    }

    /// The per-instantiation case name: `wrap` in `box$s32` -> `wrap$box$s32`.
    /// Unique per instantiation, so the existing by-name constructor/match
    /// resolution needs no change once a generic type is monomorphized.
    fn mangle_case_name(case: &str, type_name: &str) -> String {
        format!("{}${}", case, type_name)
    }

    /// Queue a generic variant instantiation for emission (once) and return its
    /// mangled concrete type name.
    fn queue_variant(&mut self, name: &str, concretes: &[String]) -> String {
        let mangled = Self::mangle_type_name(name, concretes);
        if !self.type_instances.contains_key(&mangled) {
            self.type_instances
                .insert(mangled.clone(), (name.to_string(), concretes.to_vec()));
            self.type_worklist
                .push((name.to_string(), concretes.to_vec()));
        }
        mangled
    }

    /// Queue a generic record instantiation for emission (once) and return its
    /// mangled concrete type name.
    fn queue_record(&mut self, name: &str, concretes: &[String]) -> String {
        let mangled = Self::mangle_type_name(name, concretes);
        if !self.record_instances.contains_key(&mangled) {
            self.record_instances
                .insert(mangled.clone(), (name.to_string(), concretes.to_vec()));
            self.record_worklist
                .push((name.to_string(), concretes.to_vec()));
        }
        mangled
    }

    /// The mangled concrete name of a generic record construction, inferring each
    /// type parameter from the field argument types. Read-only (no queueing) so
    /// `infer_type` can use it. None if `name` is not a generic record or a type
    /// parameter can't be determined.
    fn generic_record_instance(
        &self,
        name: &str,
        args: &[SExpr],
        env: &[(String, String)],
    ) -> Option<String> {
        let gdef = self.generic_records.get(name)?;
        let field_pats: Vec<&SExpr> = gdef
            .fields
            .iter()
            .filter_map(|f| match f {
                SExpr::List(fd, _) if fd.len() >= 2 => Some(&fd[1]),
                _ => None,
            })
            .collect();
        if args.len() != field_pats.len() {
            return None;
        }
        let mut found: Vec<(String, String)> = Vec::new();
        for (pat, a) in field_pats.iter().zip(args) {
            if let Some(cs) = self.infer_type(a, env)
                && let Some(ce) = type_str_to_expr(&cs)
            {
                unify_types(pat, &ce, &gdef.tparams, &mut found);
            }
        }
        let mut concretes = Vec::new();
        for tp in &gdef.tparams {
            concretes.push(
                found
                    .iter()
                    .find(|(k, _)| k == tp)
                    .map(|(_, c)| c.clone())?,
            );
        }
        Some(Self::mangle_type_name(name, &concretes))
    }

    /// Rewrite type-expression occurrences of generic ADTs into their mangled
    /// concrete names, queueing each instantiation. Recurses into builtin
    /// parameterized types (list/option/result/tuple/->) so nested generics like
    /// `(list (box s32))` resolve. Non-generic type exprs pass through unchanged.
    fn rewrite_type_expr(&mut self, e: &SExpr) -> SExpr {
        match e {
            SExpr::List(items, span) if !items.is_empty() => {
                let rewritten: Vec<SExpr> =
                    items.iter().map(|i| self.rewrite_type_expr(i)).collect();
                if let Some(head) = head_sym(items) {
                    let concretes: Vec<String> =
                        rewritten[1..].iter().filter_map(canonical_type).collect();
                    let arity = concretes.len();
                    if let Some(gdef) = self.generic_variants.get(head)
                        && arity == gdef.tparams.len()
                    {
                        let mangled = self.queue_variant(head, &concretes);
                        return SExpr::Sym(mangled, span.clone());
                    }
                    if let Some(gdef) = self.generic_records.get(head)
                        && arity == gdef.tparams.len()
                    {
                        let mangled = self.queue_record(head, &concretes);
                        return SExpr::Sym(mangled, span.clone());
                    }
                }
                SExpr::List(rewritten, span.clone())
            }
            other => other.clone(),
        }
    }

    /// Rewrite the type positions of one parameter (`(name type)` or `(name : type)`).
    fn rewrite_param(&mut self, p: &SExpr) -> SExpr {
        if let SExpr::List(pp, pspan) = p {
            if pp.len() == 2 {
                return SExpr::List(
                    vec![pp[0].clone(), self.rewrite_type_expr(&pp[1])],
                    pspan.clone(),
                );
            } else if pp.len() == 3 && is_colon(&pp[1]) {
                return SExpr::List(
                    vec![pp[0].clone(), pp[1].clone(), self.rewrite_type_expr(&pp[2])],
                    pspan.clone(),
                );
            }
        }
        p.clone()
    }

    /// Rewrite generic ADT references in a function signature (parameter types and
    /// return type), leaving the body for `walk`. Returns the new `fn` items.
    fn rewrite_fn_signature(&mut self, items: &[SExpr]) -> Vec<SExpr> {
        let mut out = items.to_vec();
        if let Some(SExpr::List(ps, pspan)) = out.get(2).cloned() {
            let new_ps: Vec<SExpr> = ps.iter().map(|p| self.rewrite_param(p)).collect();
            out[2] = SExpr::List(new_ps, pspan);
        }
        // The return type is at index 3, or 4 when a `:` precedes it.
        let ret_idx = if out.get(3).is_some_and(is_colon) {
            4
        } else {
            3
        };
        if let Some(r) = out.get(ret_idx).cloned() {
            out[ret_idx] = self.rewrite_type_expr(&r);
        }
        out
    }

    /// Emit a concrete variant form for one generic instantiation: substitute the
    /// type parameters, mangle each case name, and rewrite nested generic payloads
    /// (queueing them in turn).
    fn emit_variant_instance(&mut self, gen_name: &str, concretes: &[String]) -> SExpr {
        let gdef = self.generic_variants.get(gen_name).cloned().unwrap();
        let mangled = Self::mangle_type_name(gen_name, concretes);
        let span = gdef.span.clone();
        let bindings: Vec<(String, String)> = gdef
            .tparams
            .iter()
            .cloned()
            .zip(concretes.iter().cloned())
            .collect();
        let mut out = vec![
            SExpr::Sym("variant".to_string(), span.clone()),
            SExpr::Sym(mangled.clone(), span.clone()),
        ];
        for case in &gdef.cases {
            if let SExpr::List(ci, cspan) = case
                && let Some(SExpr::Sym(cn, cnspan)) = ci.first()
            {
                let mut new_ci = vec![SExpr::Sym(
                    Self::mangle_case_name(cn, &mangled),
                    cnspan.clone(),
                )];
                for pty in &ci[1..] {
                    let subst = subst_types(pty, &bindings);
                    new_ci.push(self.rewrite_type_expr(&subst));
                }
                out.push(SExpr::List(new_ci, cspan.clone()));
            }
        }
        SExpr::List(out, span)
    }

    /// Emit a concrete record form for one generic instantiation: substitute the
    /// type parameters into each field type (rewriting nested generics in turn).
    /// Field names are kept; the record name carries the instantiation.
    fn emit_record_instance(&mut self, gen_name: &str, concretes: &[String]) -> SExpr {
        let gdef = self.generic_records.get(gen_name).cloned().unwrap();
        let mangled = Self::mangle_type_name(gen_name, concretes);
        let span = gdef.span.clone();
        let bindings: Vec<(String, String)> = gdef
            .tparams
            .iter()
            .cloned()
            .zip(concretes.iter().cloned())
            .collect();
        let mut out = vec![
            SExpr::Sym("record".to_string(), span.clone()),
            SExpr::Sym(mangled, span.clone()),
        ];
        for field in &gdef.fields {
            if let SExpr::List(fd, fspan) = field
                && fd.len() >= 2
                && let SExpr::Sym(..) = &fd[0]
            {
                let subst = subst_types(&fd[1], &bindings);
                out.push(SExpr::List(
                    vec![fd[0].clone(), self.rewrite_type_expr(&subst)],
                    fspan.clone(),
                ));
            }
        }
        SExpr::List(out, span)
    }

    /// Produce the specialized `fn` form for one monomorphization request:
    /// the type parameter is substituted, each function parameter's name is replaced
    /// by its function argument (and the parameter is dropped from the signature),
    /// and generic/trait calls in the body are then resolved.
    fn specialize(&mut self, key: &SpecKey) -> Result<SExpr> {
        let genfn = self
            .generics
            .get(&key.name)
            .cloned()
            .expect("generic exists");
        let concretes: Vec<String> = key.bindings.iter().map(|(_, c)| c.clone()).collect();
        let mname = template_fn_name(&key.name, &key.func_args, &concretes);

        // 1. Substitute the type parameters throughout params, return, and body. Type
        //    positions like `(list-new T)` become concrete; safe because a type
        //    parameter is uppercase by convention and never names a value.
        let mut params = subst_types(&genfn.params, &key.bindings);
        let ret = subst_types(&genfn.ret, &key.bindings);
        let mut body = subst_types(&genfn.body, &key.bindings);

        // 2. Substitute each function parameter's name with its function argument, so
        //    `(f x)` becomes `(double x)`. Reuses `subst_type` (a bare symbol swap).
        for (fp_idx, fname) in genfn.func_params.iter().zip(&key.func_args) {
            if let Some(pname) = param_name_at(&genfn.params, *fp_idx) {
                body = subst_type(&body, &pname, fname);
            }
        }

        // 3. Drop the function parameters from the signature — they are compile-time.
        if !genfn.func_params.is_empty()
            && let SExpr::List(pitems, pspan) = &params
        {
            let kept: Vec<SExpr> = pitems
                .iter()
                .enumerate()
                .filter(|(i, _)| !genfn.func_params.contains(i))
                .map(|(_, p)| p.clone())
                .collect();
            params = SExpr::List(kept, pspan.clone());
        }

        let env = param_env(&params);
        self.fn_params
            .insert(mname.clone(), param_type_strings(&params));
        let mc = MethodCtx {
            constraints: genfn.constraints.clone(),
            bindings: key.bindings.clone(),
        };
        // The body is in return position, so it is expected at the return type.
        let ret_exp = canonical_type(&ret);
        let body = self.walk(&body, &env, Some(&mc), ret_exp)?;
        let span = genfn.body.span().clone();
        Ok(SExpr::List(
            vec![
                SExpr::Sym("fn".to_string(), span.clone()),
                SExpr::Sym(mname, span.clone()),
                params,
                ret,
                body,
            ],
            span,
        ))
    }

    /// Rewrite the body of a retained `fn` form (generic calls -> specialized names).
    /// The body is always the last element, whatever the annotation shape.
    fn process_fn_form(&mut self, items: &[SExpr], span: &Span) -> Result<SExpr> {
        // Rewrite generic ADT references in the signature first, so the body's env
        // and expected return type see the mangled concrete names.
        let items = self.rewrite_fn_signature(items);
        let shape = fn_shape(&items)
            .ok_or_else(|| self.ctx.error("malformed function definition", span))?;
        let env = param_env(shape.params);
        // The body is in return position, so it is expected at the return type
        // (ignoring any multiplicity qualifier, which the checker handles).
        let ret_exp = canonical_type(unwrap_mult(shape.ret));
        let new_body = self.walk(shape.body, &env, None, ret_exp)?;
        let mut new_items = items.clone();
        if let Some(last) = new_items.last_mut() {
            *last = new_body;
        }
        Ok(SExpr::List(new_items, span.clone()))
    }

    /// Rewrite any `fn` bodies reachable from a retained top-level form.
    fn process_form(&mut self, form: &SExpr) -> Result<SExpr> {
        if let SExpr::List(items, span) = form {
            match head_sym(items) {
                Some("fn") => return self.process_fn_form(items, span),
                Some("export") => {
                    // Rewrite an inner (fn ...) if present, leaving the wrapper shape intact.
                    let mut new_items = Vec::with_capacity(items.len());
                    for it in items {
                        match it {
                            SExpr::List(inner, ispan) if head_sym(inner) == Some("fn") => {
                                new_items.push(self.process_fn_form(inner, ispan)?);
                            }
                            other => new_items.push(other.clone()),
                        }
                    }
                    return Ok(SExpr::List(new_items, span.clone()));
                }
                _ => {}
            }
        }
        Ok(form.clone())
    }
}

/// Lower traits/instances/generics to plain monomorphic forms.
/// The primitive equality instruction for a scalar type, or None if not scalar.
fn scalar_eq_instr(ty: &str) -> Option<&'static str> {
    Some(match ty {
        "s32" | "u8" => "i32.eq",
        "s64" => "i64.eq",
        "f32" => "f32.eq",
        "f64" => "f64.eq",
        _ => return None,
    })
}

/// Generate an `(instance (Eq Type) ...)` that compares a record field by field.
fn derive_eq_record(
    trait_name: &str,
    type_name: &str,
    fields: &[(String, String)],
    span: &Span,
    ctx: &CompileContext,
) -> Result<SExpr> {
    let sym = |s: &str| SExpr::Sym(s.to_string(), span.clone());
    let list = |v: Vec<SExpr>| SExpr::List(v, span.clone());

    // One comparison per field: (<eq> (Type.field a) (Type.field b)).
    let mut cmps = Vec::new();
    for (fname, fty) in fields {
        let eq = scalar_eq_instr(fty).ok_or_else(|| {
            ctx.error(
                format!(
                    "cannot derive Eq for '{}': field '{}' has non-scalar type '{}'",
                    type_name, fname, fty
                ),
                span,
            )
        })?;
        let accessor = format!("{}.{}", type_name, fname);
        cmps.push(list(vec![
            sym(eq),
            list(vec![sym(&accessor), sym("a")]),
            list(vec![sym(&accessor), sym("b")]),
        ]));
    }
    // Combine with i32.and (an empty record is always equal).
    let body = cmps
        .into_iter()
        .reduce(|acc, c| list(vec![sym("i32.and"), acc, c]))
        .unwrap_or_else(|| SExpr::Int {
            value: 1,
            ty: Type::S32,
            span: span.clone(),
        });

    let param = |n: &str| list(vec![sym(n), sym(":"), sym(type_name)]);
    let method = list(vec![
        sym("fn"),
        sym("="),
        list(vec![param("a"), param("b")]),
        sym(":"),
        sym("s32"),
        body,
    ]);
    Ok(list(vec![
        sym("instance"),
        list(vec![sym(trait_name), sym(type_name)]),
        method,
    ]))
}

/// Generate an `(instance (Eq Variant) ...)` that compares two variant values:
/// equal only when they share a case and that case's (scalar) payloads are equal.
/// Built as a nested match — outer on `a`, inner on `b` with a `_` fallback to 0.
fn derive_eq_variant(
    trait_name: &str,
    type_name: &str,
    cases: &[(String, Vec<String>)],
    span: &Span,
    ctx: &CompileContext,
) -> Result<SExpr> {
    let sym = |s: &str| SExpr::Sym(s.to_string(), span.clone());
    let list = |v: Vec<SExpr>| SExpr::List(v, span.clone());
    let int = |v: i64| SExpr::Int {
        value: v,
        ty: Type::S32,
        span: span.clone(),
    };

    let mut outer_arms = Vec::new();
    for (case, payloads) in cases {
        let a_binds: Vec<String> = (0..payloads.len()).map(|i| format!("a_{i}")).collect();
        let b_binds: Vec<String> = (0..payloads.len()).map(|i| format!("b_{i}")).collect();
        // Compare payloads pairwise (scalar equality, matching the record path).
        let mut cmps = Vec::new();
        for (i, pty) in payloads.iter().enumerate() {
            let eq = scalar_eq_instr(pty).ok_or_else(|| {
                ctx.error(
                    format!(
                        "cannot derive Eq for '{}': case '{}' has non-scalar payload '{}'",
                        type_name, case, pty
                    ),
                    span,
                )
            })?;
            cmps.push(list(vec![sym(eq), sym(&a_binds[i]), sym(&b_binds[i])]));
        }
        let eq_body = cmps
            .into_iter()
            .reduce(|acc, c| list(vec![sym("i32.and"), acc, c]))
            .unwrap_or_else(|| int(1));
        // Inner match on b: the same case compares payloads; anything else is 0.
        let inner_pat = {
            let mut p = vec![sym(case)];
            p.extend(b_binds.iter().map(|n| sym(n)));
            list(p)
        };
        let inner = list(vec![
            sym("match"),
            sym("b"),
            list(vec![inner_pat, eq_body]),
            list(vec![list(vec![sym("_")]), int(0)]),
        ]);
        let outer_pat = {
            let mut p = vec![sym(case)];
            p.extend(a_binds.iter().map(|n| sym(n)));
            list(p)
        };
        outer_arms.push(list(vec![outer_pat, inner]));
    }
    let mut match_expr = vec![sym("match"), sym("a")];
    match_expr.extend(outer_arms);
    let body = list(match_expr);

    let param = |n: &str| list(vec![sym(n), sym(":"), sym(type_name)]);
    let method = list(vec![
        sym("fn"),
        sym("="),
        list(vec![param("a"), param("b")]),
        sym(":"),
        sym("s32"),
        body,
    ]);
    Ok(list(vec![
        sym("instance"),
        list(vec![sym(trait_name), sym(type_name)]),
        method,
    ]))
}

/// Compile-time deriving: `(derive Trait Type)` inspects Type's definition and emits a
/// trait instance. Runs after macro expansion and before the generics pre-pass, so the
/// generated instance flows through the normal trait pipeline. This is the first
/// "type-aware macro": it reflects on a type's structure to generate code.
fn expand_derives(forms: Vec<SExpr>, ctx: &CompileContext) -> Result<Vec<SExpr>> {
    // Collect record shapes: name -> [(field, canonical type)].
    let mut records: HashMap<String, Vec<(String, String)>> = HashMap::new();
    for form in &forms {
        if let SExpr::List(items, _) = form
            && head_sym(items) == Some("record")
            && let Some(SExpr::Sym(name, _)) = items.get(1)
        {
            let mut fields = Vec::new();
            for f in &items[2..] {
                if let SExpr::List(fd, _) = f
                    && let Some(SExpr::Sym(fname, _)) = fd.first()
                    && let Some(fty) = fd.get(1).and_then(canonical_type)
                {
                    fields.push((fname.clone(), fty));
                }
            }
            records.insert(name.clone(), fields);
        }
    }

    // Collect (bare-named) variant shapes: name -> [(case, [payload canonical types])].
    let mut variants: HashMap<String, Vec<(String, Vec<String>)>> = HashMap::new();
    for form in &forms {
        if let SExpr::List(items, _) = form
            && head_sym(items) == Some("variant")
            && let Some(SExpr::Sym(name, _)) = items.get(1)
        {
            let mut cases = Vec::new();
            for c in &items[2..] {
                if let SExpr::List(ci, _) = c
                    && let Some(SExpr::Sym(cname, _)) = ci.first()
                {
                    let payloads: Vec<String> = ci[1..].iter().filter_map(canonical_type).collect();
                    cases.push((cname.clone(), payloads));
                }
            }
            variants.insert(name.clone(), cases);
        }
    }

    let mut out = Vec::new();
    for form in forms {
        let is_derive = matches!(&form, SExpr::List(items, _) if head_sym(items) == Some("derive"));
        if !is_derive {
            out.push(form);
            continue;
        }
        let (items, span) = match &form {
            SExpr::List(i, s) => (i, s),
            _ => unreachable!(),
        };
        let trait_name = match items.get(1) {
            Some(SExpr::Sym(s, _)) => s.clone(),
            _ => return Err(ctx.error("derive expects (derive Trait Type)", span)),
        };
        let type_name = match items.get(2) {
            Some(SExpr::Sym(s, _)) => s.clone(),
            _ => return Err(ctx.error("derive expects (derive Trait Type)", span)),
        };
        match trait_name.as_str() {
            "Eq" => {
                if let Some(fields) = records.get(&type_name) {
                    out.push(derive_eq_record(
                        &trait_name,
                        &type_name,
                        fields,
                        span,
                        ctx,
                    )?);
                } else if let Some(cases) = variants.get(&type_name) {
                    out.push(derive_eq_variant(
                        &trait_name,
                        &type_name,
                        cases,
                        span,
                        ctx,
                    )?);
                } else {
                    return Err(ctx.error(
                        format!(
                            "cannot derive Eq for '{}': not a record or variant",
                            type_name
                        ),
                        span,
                    ));
                }
            }
            other => {
                return Err(ctx.error(format!("cannot derive '{}' (supported: Eq)", other), span));
            }
        }
    }
    Ok(out)
}

fn expand_generics(forms: Vec<SExpr>, ctx: &CompileContext) -> Result<Vec<SExpr>> {
    let mut low = Lowering {
        ctx,
        generics: HashMap::new(),
        traits: HashMap::new(),
        method_to_trait: HashMap::new(),
        monofn_returns: HashMap::new(),
        fn_params: HashMap::new(),
        instances: HashMap::new(),
        instance_defs: HashMap::new(),
        used_instances: HashSet::new(),
        instance_worklist: Vec::new(),
        worklist: Vec::new(),
        emitted: HashSet::new(),
        generic_variants: HashMap::new(),
        case_to_generic: HashMap::new(),
        type_instances: HashMap::new(),
        type_worklist: Vec::new(),
        generic_records: HashMap::new(),
        record_instances: HashMap::new(),
        record_worklist: Vec::new(),
    };

    let mut retained: Vec<SExpr> = Vec::new();

    // Pass 0: collect trait declarations (name, type parameter, method signatures)
    // so instances and `where` clauses can be checked regardless of source order.
    let mut traits: HashMap<String, TraitDef> = HashMap::new();
    let mut seen_instances: HashSet<(String, String)> = HashSet::new();
    for form in &forms {
        let (items, span) = match form {
            SExpr::List(items, span) if !items.is_empty() => (items, span),
            _ => continue,
        };
        if head_sym(items) != Some("trait") {
            continue;
        }
        // (trait (Name T U ...) methods...)
        let (name, tparams) = match items.get(1) {
            Some(SExpr::List(head, _)) if head.len() >= 2 => {
                let name = match &head[0] {
                    SExpr::Sym(n, _) => n.clone(),
                    _ => return Err(ctx.error("trait name must be a symbol", span)),
                };
                let mut tparams = Vec::new();
                for t in &head[1..] {
                    match t {
                        SExpr::Sym(tp, _) => tparams.push(tp.clone()),
                        _ => return Err(ctx.error("trait type parameter must be a symbol", span)),
                    }
                }
                (name, tparams)
            }
            _ => return Err(ctx.error("trait expects (trait (Name T ...) methods...)", span)),
        };
        let mut methods = Vec::new();
        for m in &items[2..] {
            let mi = match m {
                SExpr::List(mi, _) if head_sym(mi) == Some("fn") => mi,
                _ => return Err(ctx.error("trait method must be (fn name params : ret)", span)),
            };
            let sig = parse_method_sig(mi)
                .ok_or_else(|| ctx.error("malformed trait method signature", span))?;
            low.method_to_trait.insert(sig.name.clone(), name.clone());
            methods.push(sig);
        }
        if traits
            .insert(name.clone(), TraitDef { tparams, methods })
            .is_some()
        {
            return Err(ctx.error(format!("duplicate trait '{}'", name), span));
        }
    }
    // Make the trait declarations available during body rewriting (Pass 2/3).
    low.traits = traits.clone();

    // Pass 0.5: collect generic variant templates `(variant (Name T ...) ...)`, so
    // their constructors/matches resolve regardless of source order (Pass 1/2).
    for form in &forms {
        let (items, span) = match form {
            SExpr::List(items, span) if !items.is_empty() => (items, span),
            _ => continue,
        };
        if head_sym(items) != Some("variant") {
            continue;
        }
        let head = match items.get(1) {
            Some(SExpr::List(head, _)) if !head.is_empty() => head,
            _ => continue, // concrete variant (bare-symbol name): handled in Pass 1
        };
        let name = match &head[0] {
            SExpr::Sym(n, _) => n.clone(),
            _ => return Err(ctx.error("variant name must be a symbol", span)),
        };
        let mut tparams = Vec::new();
        for t in &head[1..] {
            match t {
                SExpr::Sym(tp, _) => tparams.push(tp.clone()),
                _ => return Err(ctx.error("variant type parameter must be a symbol", span)),
            }
        }
        if tparams.is_empty() {
            return Err(ctx.error("generic variant needs at least one type parameter", span));
        }
        let cases: Vec<SExpr> = items[2..].to_vec();
        for c in &cases {
            if let SExpr::List(ci, _) = c
                && let Some(SExpr::Sym(cn, _)) = ci.first()
                && let Some(prev) = low.case_to_generic.insert(cn.clone(), name.clone())
                && prev != name
            {
                return Err(ctx.error(
                    format!(
                        "variant case '{}' is declared by both '{}' and '{}'",
                        cn, prev, name
                    ),
                    span,
                ));
            }
        }
        if low
            .generic_variants
            .insert(
                name.clone(),
                GenericTypeDef {
                    tparams,
                    cases,
                    span: span.clone(),
                },
            )
            .is_some()
        {
            return Err(ctx.error(format!("duplicate generic variant '{}'", name), span));
        }
    }

    // Pass 0.6: collect generic record templates `(record (Name T ...) ...)`.
    for form in &forms {
        let (items, span) = match form {
            SExpr::List(items, span) if !items.is_empty() => (items, span),
            _ => continue,
        };
        if head_sym(items) != Some("record") {
            continue;
        }
        let head = match items.get(1) {
            Some(SExpr::List(head, _)) if !head.is_empty() => head,
            _ => continue, // concrete record (bare-symbol name): handled in Pass 1
        };
        let name = match &head[0] {
            SExpr::Sym(n, _) => n.clone(),
            _ => return Err(ctx.error("record name must be a symbol", span)),
        };
        let mut tparams = Vec::new();
        for t in &head[1..] {
            match t {
                SExpr::Sym(tp, _) => tparams.push(tp.clone()),
                _ => return Err(ctx.error("record type parameter must be a symbol", span)),
            }
        }
        if tparams.is_empty() {
            return Err(ctx.error("generic record needs at least one type parameter", span));
        }
        let fields: Vec<SExpr> = items[2..].to_vec();
        if low
            .generic_records
            .insert(
                name.clone(),
                GenericRecordDef {
                    tparams,
                    fields,
                    span: span.clone(),
                },
            )
            .is_some()
        {
            return Err(ctx.error(format!("duplicate generic record '{}'", name), span));
        }
    }

    // Pass 1: classify every top-level form.
    for form in &forms {
        let (items, span) = match form {
            SExpr::List(items, span) if !items.is_empty() => (items, span),
            _ => {
                retained.push(form.clone());
                continue;
            }
        };
        match head_sym(items) {
            Some("trait") => {} // collected in Pass 0
            Some("instance") => {
                // (instance (Trait Type ...) (fn method params [:] ret body) ...)
                let (trait_name, types) = match items.get(1) {
                    Some(SExpr::List(head, _)) if head.len() >= 2 => {
                        let tn = match &head[0] {
                            SExpr::Sym(s, _) => s.clone(),
                            _ => return Err(ctx.error("instance trait must be a symbol", span)),
                        };
                        let mut types = Vec::new();
                        for t in &head[1..] {
                            types.push(canonical_type(t).ok_or_else(|| {
                                ctx.error("instance type must be a type name", span)
                            })?);
                        }
                        (tn, types)
                    }
                    _ => {
                        return Err(
                            ctx.error("instance expects (instance (Trait Type ...) ...)", span)
                        );
                    }
                };
                let trait_def = traits.get(&trait_name).ok_or_else(|| {
                    ctx.error(format!("unknown trait '{}' in instance", trait_name), span)
                })?;
                if trait_def.tparams.len() != types.len() {
                    return Err(ctx.error(
                        format!(
                            "instance of '{}' has {} type argument(s) but the trait declares {}",
                            trait_name,
                            types.len(),
                            trait_def.tparams.len()
                        ),
                        span,
                    ));
                }
                // (type parameter -> instance type) bindings for substitution.
                let tbindings: Vec<(String, String)> = trait_def
                    .tparams
                    .iter()
                    .cloned()
                    .zip(types.iter().cloned())
                    .collect();
                if !seen_instances.insert((trait_name.clone(), instance_key(&types))) {
                    return Err(ctx.error(
                        format!(
                            "duplicate instance for ({} {})",
                            trait_name,
                            types.join(" ")
                        ),
                        span,
                    ));
                }
                let mut provided: HashSet<String> = HashSet::new();
                for m in &items[2..] {
                    let mi = match m {
                        SExpr::List(mi, _) if head_sym(mi) == Some("fn") => mi,
                        _ => return Err(ctx.error("instance method must be (fn ...)", span)),
                    };
                    let shape =
                        fn_shape(mi).ok_or_else(|| ctx.error("malformed instance method", span))?;
                    let method = match shape.name {
                        SExpr::Sym(s, _) => s.clone(),
                        _ => return Err(ctx.error("method name must be a symbol", span)),
                    };
                    // The method must be declared by the trait.
                    let tsig = trait_def
                        .methods
                        .iter()
                        .find(|s| s.name == method)
                        .ok_or_else(|| {
                            ctx.error(
                                format!("trait '{}' has no method '{}'", trait_name, method),
                                span,
                            )
                        })?;
                    // Its signature must match the trait's, with the type parameters
                    // substituted by this instance's types.
                    let want_params = subst_types(&tsig.params, &tbindings);
                    let want_ret = subst_types(&tsig.ret, &tbindings);
                    let want_pt = param_type_exprs(&want_params);
                    let got_pt = param_type_exprs(shape.params);
                    if want_pt.len() != got_pt.len()
                        || !want_pt.iter().zip(&got_pt).all(|(a, b)| type_expr_eq(a, b))
                    {
                        return Err(ctx.error(
                            format!(
                                "instance ({} {}) method '{}' has parameter types that do not match trait '{}'",
                                trait_name, types.join(" "), method, trait_name
                            ),
                            span,
                        ));
                    }
                    if !type_expr_eq(&want_ret, shape.ret) {
                        return Err(ctx.error(
                            format!(
                                "instance ({} {}) method '{}' has a return type that does not match trait '{}'",
                                trait_name, types.join(" "), method, trait_name
                            ),
                            span,
                        ));
                    }
                    if !provided.insert(method.clone()) {
                        return Err(ctx.error(
                            format!(
                                "instance ({} {}) defines method '{}' more than once",
                                trait_name,
                                types.join(" "),
                                method
                            ),
                            span,
                        ));
                    }
                    let fname = instance_fn_name(&trait_name, &types, &method);
                    // Emit the instance method as a plain concrete fn (same shape, renamed).
                    if let Some(rt) = canonical_type(shape.ret) {
                        low.monofn_returns.insert(fname.clone(), rt);
                    }
                    low.fn_params
                        .insert(fname.clone(), param_type_strings(shape.params));
                    let mut new_mi = mi.clone();
                    new_mi[1] = SExpr::Sym(fname.clone(), mi[1].span().clone());
                    // Store the instance fn; it is emitted only if it is referenced.
                    low.instance_defs
                        .insert(fname.clone(), SExpr::List(new_mi, span.clone()));
                    low.instances
                        .entry((trait_name.clone(), instance_key(&types)))
                        .or_default()
                        .insert(method, fname);
                }
                // Every trait method must be implemented.
                for s in &trait_def.methods {
                    if !provided.contains(&s.name) {
                        return Err(ctx.error(
                            format!(
                                "instance ({} {}) is missing method '{}'",
                                trait_name,
                                types.join(" "),
                                s.name
                            ),
                            span,
                        ));
                    }
                }
            }
            Some("fn") => {
                let shape = fn_shape(items)
                    .ok_or_else(|| ctx.error("malformed function definition", span))?;
                let name = match shape.name {
                    SExpr::Sym(s, _) => s.clone(),
                    _ => return Err(ctx.error("function name must be a symbol", span)),
                };
                // A function is a template (specialized on demand) if it has a `where`
                // clause (a type parameter) and/or a function-typed parameter.
                let func_params = func_param_indices(shape.params);
                let is_template = shape.where_clause.is_some() || !func_params.is_empty();
                if is_template {
                    let mut constraints: Vec<(String, String)> = Vec::new();
                    let mut tparams: Vec<String> = Vec::new();
                    let note_tparam = |tp: &str, list: &mut Vec<String>| {
                        if !list.iter().any(|t| t == tp) {
                            list.push(tp.to_string());
                        }
                    };
                    if let Some(SExpr::List(where_items, _)) = shape.where_clause {
                        for c in &where_items[1..] {
                            // A bare type parameter with no constraint: (where T).
                            if let SExpr::Sym(tp, _) = c {
                                note_tparam(tp, &mut tparams);
                            } else if let SExpr::List(cc, _) = c
                                && cc.len() >= 2
                                && let SExpr::Sym(tn, _) = &cc[0]
                                && cc[1..].iter().all(|x| matches!(x, SExpr::Sym(..)))
                            {
                                // A trait bound: (Trait T ...). All named type
                                // parameters are declared; the constraint is recorded
                                // against the first (used for single-parameter dispatch).
                                for x in &cc[1..] {
                                    if let SExpr::Sym(tp, _) = x {
                                        note_tparam(tp, &mut tparams);
                                    }
                                }
                                if let SExpr::Sym(tp, _) = &cc[1] {
                                    constraints.push((tn.clone(), tp.clone()));
                                }
                            } else {
                                return Err(ctx.error(
                                    "where clause entry must be a type parameter or (Trait TypeParam ...)",
                                    span,
                                ));
                            }
                        }
                        if tparams.is_empty() {
                            return Err(ctx.error(
                                "generic fn needs a type parameter in its where clause",
                                span,
                            ));
                        }
                    }
                    for (tn, _) in &constraints {
                        if !traits.contains_key(tn) {
                            return Err(
                                ctx.error(format!("unknown trait '{}' in where clause", tn), span)
                            );
                        }
                    }
                    low.generics.insert(
                        name.clone(),
                        GenericFnDef {
                            name,
                            tparams,
                            constraints,
                            func_params,
                            params: shape.params.clone(),
                            ret: shape.ret.clone(),
                            body: shape.body.clone(),
                        },
                    );
                } else {
                    low.fn_params
                        .insert(name.clone(), param_type_strings(shape.params));
                    if let Some(rt) = canonical_type(shape.ret) {
                        low.monofn_returns.insert(name, rt);
                    }
                    retained.push(form.clone());
                }
            }
            Some("export") => {
                // Record the signature of an inner (fn ...) so calls to it propagate
                // types, then retain the export unchanged for Pass 2.
                for it in &items[1..] {
                    if let SExpr::List(inner, _) = it
                        && head_sym(inner) == Some("fn")
                        && let Some(shape) = fn_shape(inner)
                        && let SExpr::Sym(n, _) = shape.name
                    {
                        low.fn_params
                            .insert(n.clone(), param_type_strings(shape.params));
                        if let Some(rt) = canonical_type(shape.ret) {
                            low.monofn_returns.insert(n.clone(), rt);
                        }
                    }
                }
                retained.push(form.clone());
            }
            Some("variant") => {
                // A generic variant template (list name) is collected in Pass 0.5 and
                // emitted per instantiation; a concrete variant is retained as-is.
                if !matches!(items.get(1), Some(SExpr::List(..))) {
                    retained.push(form.clone());
                }
            }
            Some("record") => {
                // Likewise: a generic record template is emitted per instantiation; a
                // concrete record is retained as-is.
                if !matches!(items.get(1), Some(SExpr::List(..))) {
                    retained.push(form.clone());
                }
            }
            _ => retained.push(form.clone()),
        }
    }

    // Pass 2: rewrite retained forms. This marks the instances they use and seeds
    // the generic worklist.
    let mut output: Vec<SExpr> = Vec::new();
    for form in &retained {
        let rewritten = low.process_form(form)?;
        output.push(rewritten);
    }

    // Pass 3: drain both worklists, emitting one copy per specialized generic and
    // one copy per referenced instance. Each emitted body may reference further
    // generics or instances, so we loop until both are empty. Unused instances
    // (e.g. most of an included stdlib) are never emitted.
    loop {
        if let Some(key) = low.worklist.pop() {
            let concretes: Vec<String> = key.bindings.iter().map(|(_, c)| c.clone()).collect();
            let mname = template_fn_name(&key.name, &key.func_args, &concretes);
            if low.emitted.insert(mname) {
                let fn_form = low.specialize(&key)?;
                output.push(fn_form);
            }
        } else if let Some(fname) = low.instance_worklist.pop() {
            if let Some(SExpr::List(items, span)) = low.instance_defs.get(&fname).cloned() {
                let rewritten = low.process_fn_form(&items, &span)?;
                output.push(rewritten);
            }
        } else if let Some((gen_name, concretes)) = low.type_worklist.pop() {
            // Emit a concrete variant per generic instantiation. Emission may rewrite
            // nested generic payloads, queueing further instantiations.
            let form = low.emit_variant_instance(&gen_name, &concretes);
            output.push(form);
        } else if let Some((gen_name, concretes)) = low.record_worklist.pop() {
            // Emit a concrete record per generic instantiation.
            let form = low.emit_record_instance(&gen_name, &concretes);
            output.push(form);
        } else {
            break;
        }
    }

    Ok(output)
}

fn parse_program(forms: Vec<SExpr>, ctx: &CompileContext) -> Result<Program> {
    let mut pending = Vec::new();
    let mut defined = HashSet::new();
    let mut imports = Vec::new();
    let mut imported = HashSet::new();
    let mut exports = Vec::new();
    let mut export_set = HashSet::new();
    let mut globals = Vec::new();
    let mut global_names = HashSet::new();
    let mut records = Vec::new();
    let mut record_names: HashSet<String> = HashSet::new();
    let mut variants = Vec::new();
    let mut variant_names: HashSet<String> = HashSet::new();
    let mut resources = Vec::new();
    let mut resource_names: HashSet<String> = HashSet::new();
    // Capability type names. A capability is a linear resource: it shares the
    // resource representation (an i32 handle, so it parses as Type::Resource and
    // needs no new Type variant), but every binding of it carries a use-exactly-once
    // obligation and it has no public constructor (minted only by `with-cap`).
    let mut capability_names: HashSet<String> = HashSet::new();
    let mut world_config: Option<WorldConfig> = None;
    let mut data_segments = Vec::new();

    // First pass: collect type names (records, variants, resources) so we can distinguish them
    for form in &forms {
        if let SExpr::List(items, span) = form {
            if items.is_empty() {
                return Err(ctx.error("empty list is not a valid top-level form", span));
            }
            match &items[0] {
                SExpr::Sym(sym, _) if sym == "record" => {
                    if items.len() >= 2
                        && let SExpr::Sym(name, _) = &items[1]
                        && !record_names.insert(name.clone())
                    {
                        return Err(ctx.error(format!("duplicate record type '{}'", name), span));
                    }
                }
                SExpr::Sym(sym, _) if sym == "variant" => {
                    if items.len() >= 2
                        && let SExpr::Sym(name, _) = &items[1]
                        && !variant_names.insert(name.clone())
                    {
                        return Err(ctx.error(format!("duplicate variant type '{}'", name), span));
                    }
                }
                SExpr::Sym(sym, _) if sym == "resource" => {
                    if items.len() >= 2
                        && let SExpr::Sym(name, _) = &items[1]
                        && !resource_names.insert(name.clone())
                    {
                        return Err(ctx.error(format!("duplicate resource type '{}'", name), span));
                    }
                }
                SExpr::Sym(sym, _) if sym == "capability" => {
                    if items.len() >= 2
                        && let SExpr::Sym(name, _) = &items[1]
                    {
                        // Register as a resource name (so it parses as Type::Resource)
                        // and as a capability (so the checker treats it as linear).
                        if !resource_names.insert(name.clone()) {
                            return Err(ctx.error(format!("duplicate type name '{}'", name), span));
                        }
                        capability_names.insert(name.clone());
                    }
                }
                _ => {}
            }
        }
    }

    // Second pass: parse everything with type names available
    for form in forms {
        match form {
            SExpr::List(items, span) => {
                if items.is_empty() {
                    return Err(ctx.error("empty list is not a valid top-level form", &span));
                }
                match &items[0] {
                    SExpr::Sym(sym, _) if sym == "fn" => {
                        let func = parse_fn_form(
                            SExpr::List(items, span.clone()),
                            &variant_names,
                            &resource_names,
                            ctx,
                        )?;
                        if !defined.insert(func.name.clone()) {
                            return Err(
                                ctx.error(format!("duplicate function '{}'", func.name), &span)
                            );
                        }
                        pending.push(func);
                    }
                    SExpr::Sym(sym, _) if sym == "export" => {
                        if items.len() == 3 {
                            // (export "alias" (fn ...)) - aliased export
                            match (&items[1], &items[2]) {
                                (SExpr::Str(alias, _), SExpr::List(_, inner_span)) => {
                                    let func = parse_fn_form(
                                        items[2].clone(),
                                        &variant_names,
                                        &resource_names,
                                        ctx,
                                    )?;
                                    if !defined.insert(func.name.clone()) {
                                        return Err(ctx.error(
                                            format!("duplicate function '{}'", func.name),
                                            inner_span,
                                        ));
                                    }
                                    if export_set.insert(func.name.clone()) {
                                        exports.push(ExportDef::aliased(
                                            alias.clone(),
                                            func.name.clone(),
                                        ));
                                    }
                                    pending.push(func);
                                }
                                _ => {
                                    return Err(ctx.error(
                                        "aliased export expects (export \"name\" (fn ...))",
                                        &span,
                                    ));
                                }
                            }
                        } else if items.len() == 2 {
                            match &items[1] {
                                SExpr::Sym(name, _) => {
                                    if export_set.insert(name.clone()) {
                                        exports.push(ExportDef::simple(name.clone()));
                                    }
                                }
                                SExpr::List(_, inner_span) => {
                                    let func = parse_fn_form(
                                        items[1].clone(),
                                        &variant_names,
                                        &resource_names,
                                        ctx,
                                    )?;
                                    if !defined.insert(func.name.clone()) {
                                        return Err(ctx.error(
                                            format!("duplicate function '{}'", func.name),
                                            inner_span,
                                        ));
                                    }
                                    if export_set.insert(func.name.clone()) {
                                        exports.push(ExportDef::simple(func.name.clone()));
                                    }
                                    pending.push(func);
                                }
                                other => {
                                    return Err(ctx.error(
                                        "export argument must be a symbol or (fn ...)",
                                        other.span(),
                                    ));
                                }
                            }
                        } else {
                            return Err(ctx.error("export expects 1 or 2 arguments", &span));
                        }
                    }
                    SExpr::Sym(sym, _) if sym == "import" => {
                        let import =
                            parse_import_form(&items, &variant_names, &resource_names, ctx)?;
                        if defined.contains(&import.name) {
                            return Err(ctx.error(
                                format!(
                                    "function '{}' is already defined and cannot be imported",
                                    import.name
                                ),
                                &span,
                            ));
                        }
                        if !imported.insert((import.module.clone(), import.name.clone())) {
                            return Err(ctx.error(
                                format!("duplicate import '{}/{}'", import.module, import.name),
                                &span,
                            ));
                        }
                        imports.push(import);
                    }
                    SExpr::Sym(sym, _) if sym == "global" => {
                        let global =
                            parse_global_form(&items, &variant_names, &resource_names, ctx)?;
                        if !global_names.insert(global.name.clone()) {
                            return Err(
                                ctx.error(format!("duplicate global '{}'", global.name), &span)
                            );
                        }
                        globals.push(global);
                    }
                    SExpr::Sym(sym, _) if sym == "record" => {
                        let record =
                            parse_record_form(&items, &variant_names, &resource_names, ctx)?;
                        // Already checked for duplicates in first pass
                        records.push(record);
                    }
                    SExpr::Sym(sym, _) if sym == "variant" => {
                        let variant =
                            parse_variant_form(&items, &variant_names, &resource_names, ctx)?;
                        // Already checked for duplicates in first pass
                        variants.push(variant);
                    }
                    SExpr::Sym(sym, _) if sym == "resource" => {
                        let resource = parse_resource_form(&items, ctx)?;
                        // Already checked for duplicates in first pass
                        resources.push(resource);
                    }
                    SExpr::Sym(sym, _) if sym == "capability" => {
                        // A capability is a pure type declaration (no fields, no
                        // codegen, unforgeable). Validate shape; it was registered in
                        // the first pass. `(capability Name)`.
                        if items.len() != 2 || !matches!(&items[1], SExpr::Sym(..)) {
                            return Err(ctx.error_with_note(
                                "invalid capability declaration",
                                &span,
                                "expected: (capability Name)",
                            ));
                        }
                    }
                    SExpr::Sym(sym, _) if sym == "world" => {
                        if world_config.is_some() {
                            return Err(
                                ctx.error("only one (world ...) declaration is allowed", &span)
                            );
                        }
                        world_config = Some(parse_world_form(&items, ctx)?);
                    }
                    SExpr::Sym(sym, _) if sym == "data" => {
                        if items.len() != 3 {
                            return Err(ctx.error("data expects (data <offset> <string>)", &span));
                        }
                        let offset = match &items[1] {
                            SExpr::Int { value, .. } => *value as i32,
                            _ => {
                                return Err(
                                    ctx.error("data offset must be an integer", items[1].span())
                                );
                            }
                        };
                        let bytes = match &items[2] {
                            SExpr::Str(s, _) => s.bytes().collect::<Vec<u8>>(),
                            _ => {
                                return Err(
                                    ctx.error("data content must be a string", items[2].span())
                                );
                            }
                        };
                        data_segments.push(DataSegment { offset, bytes });
                    }
                    other => {
                        return Err(ctx.error_with_note(
                            "unknown top-level form",
                            other.span(),
                            "expected 'fn', 'export', 'import', 'global', 'record', 'variant', 'resource', 'capability', 'world', or 'data'"
                        ));
                    }
                }
            }
            other => {
                return Err(ctx.error("top-level forms must be lists", other.span()));
            }
        }
    }

    let mut signatures = HashMap::new();
    for func in &pending {
        let params = func.params.iter().map(|p| p.ty.clone()).collect();
        let sig = Signature {
            params,
            result: func.return_type.clone(),
        };
        if signatures.insert(func.name.clone(), sig).is_some() {
            return Err(ctx.error(format!("duplicate function '{}'", func.name), &func.span));
        }
    }

    for import in &imports {
        let params = import.params.iter().map(|p| p.ty.clone()).collect();
        let sig = Signature {
            params,
            result: import.return_type.clone(),
        };
        // Same-named imports across interfaces are allowed (keyed by bare name
        // only for typed-call resolution, which raw-invoked imports don't use);
        // keep the first, matching the typed-wrapper dedup.
        signatures.entry(import.name.clone()).or_insert(sig);
    }

    for export in exports.iter() {
        if !signatures.contains_key(&export.func_name) {
            return Err(ctx.error(
                format!("cannot export undefined function '{}'", export.func_name),
                &Span::dummy(),
            ));
        }
        if imported.iter().any(|(_, name)| name == &export.func_name) {
            return Err(ctx.error(
                format!("cannot export imported function '{}'", export.func_name),
                &Span::dummy(),
            ));
        }
    }

    // Build records and variants maps for parse_expr
    let records_map: HashMap<String, RecordDef> = records
        .iter()
        .map(|r| (r.name.clone(), r.clone()))
        .collect();
    let variants_map: HashMap<String, VariantDef> = variants
        .iter()
        .map(|v| (v.name.clone(), v.clone()))
        .collect();

    let mut functions = Vec::new();
    for func in pending {
        // Create bindings with scopes from parameters for hygienic variable resolution
        let param_bindings = func
            .params
            .iter()
            .map(|p| Binding::new(p.name.clone(), p.scopes.clone()))
            .collect::<Vec<_>>();
        let body_expr = parse_expr(
            &func.body,
            &param_bindings,
            &signatures,
            &records_map,
            &variants_map,
            ctx,
        )?;
        functions.push(Function {
            name: func.name,
            params: func.params,
            return_type: func.return_type,
            body: body_expr,
        });
    }

    Ok(Program {
        functions,
        imports,
        exports,
        globals,
        records,
        variants,
        resources,
        capabilities: capability_names,
        world_config,
        data_segments,
    })
}

/// Parse (world name (wit-deps "path") (import pkg/iface) ... (export pkg/iface) ...)
fn parse_world_form(items: &[SExpr], ctx: &CompileContext) -> Result<WorldConfig> {
    let span = items[0].span().clone();
    if items.len() < 2 {
        return Err(ctx.error_with_note(
            "invalid world declaration",
            &span,
            "expected: (world name (wit-deps \"path\") (import pkg/iface) ... (export pkg/iface) ...)",
        ));
    }

    let name = match &items[1] {
        SExpr::Sym(s, _) => s.clone(),
        other => return Err(ctx.error("world name must be a symbol", other.span())),
    };

    let mut wit_deps = None;
    let mut external_imports = Vec::new();
    let mut external_exports = Vec::new();

    for item in &items[2..] {
        match item {
            SExpr::List(sub_items, sub_span) => {
                if sub_items.is_empty() {
                    return Err(ctx.error("empty world clause", sub_span));
                }
                match &sub_items[0] {
                    SExpr::Sym(sym, _) if sym == "wit-deps" => {
                        if sub_items.len() != 2 {
                            return Err(ctx.error("wit-deps expects a path string", sub_span));
                        }
                        match &sub_items[1] {
                            SExpr::Sym(path, _) => {
                                // Allow unquoted path for convenience
                                wit_deps = Some(PathBuf::from(path));
                            }
                            other => {
                                // Try to extract string literal if parser supports it
                                return Err(ctx.error(
                                    "wit-deps path must be a string or symbol",
                                    other.span(),
                                ));
                            }
                        }
                    }
                    SExpr::Sym(sym, _) if sym == "import" => {
                        if sub_items.len() != 2 {
                            return Err(
                                ctx.error("import expects an interface reference", sub_span)
                            );
                        }
                        match &sub_items[1] {
                            SExpr::Sym(iface_ref, _) => match ExternalInterface::parse(iface_ref) {
                                Some(ext) => external_imports.push(ext),
                                None => return Err(ctx.error(
                                    format!(
                                        "invalid interface reference '{}', expected 'pkg:ns/iface'",
                                        iface_ref
                                    ),
                                    sub_span,
                                )),
                            },
                            other => {
                                return Err(ctx
                                    .error("import expects an interface reference", other.span()));
                            }
                        }
                    }
                    SExpr::Sym(sym, _) if sym == "export" => {
                        if sub_items.len() != 2 {
                            return Err(
                                ctx.error("export expects an interface reference", sub_span)
                            );
                        }
                        match &sub_items[1] {
                            SExpr::Sym(iface_ref, _) => match ExternalInterface::parse(iface_ref) {
                                Some(ext) => external_exports.push(ext),
                                None => return Err(ctx.error(
                                    format!(
                                        "invalid interface reference '{}', expected 'pkg:ns/iface'",
                                        iface_ref
                                    ),
                                    sub_span,
                                )),
                            },
                            other => {
                                return Err(ctx
                                    .error("export expects an interface reference", other.span()));
                            }
                        }
                    }
                    other => {
                        return Err(ctx.error_with_note(
                            "unknown world clause",
                            other.span(),
                            "expected 'wit-deps', 'import', or 'export'",
                        ));
                    }
                }
            }
            other => return Err(ctx.error("world clause must be a list", other.span())),
        }
    }

    Ok(WorldConfig {
        name,
        wit_deps,
        external_imports,
        external_exports,
    })
}

fn parse_fn_form(
    form: SExpr,
    variant_names: &HashSet<String>,
    resource_names: &HashSet<String>,
    ctx: &CompileContext,
) -> Result<PendingFunction> {
    let (items, span) = match form {
        SExpr::List(items, span) => (items, span),
        other => return Err(ctx.error("function definition must be a list", other.span())),
    };
    match items.first() {
        Some(SExpr::Sym(s, _)) if s == "fn" => {}
        _ => return Err(ctx.error("function definition must start with 'fn'", &span)),
    }
    let shape = fn_shape(&items).ok_or_else(|| {
        ctx.error_with_note(
            "invalid function definition",
            &span,
            "expected: (fn name ((param : type) ...) [:] return-type body)",
        )
    })?;
    if shape.where_clause.is_some() {
        return Err(ctx.error(
            "generic functions (with a `where` clause) are not valid here",
            &span,
        ));
    }
    let name = match shape.name {
        SExpr::Sym(name, _) => name.clone(),
        other => return Err(ctx.error("function name must be a symbol", other.span())),
    };
    let params = parse_typed_params(shape.params, variant_names, resource_names, ctx)?;
    let return_type = parse_type_expr(shape.ret, variant_names, resource_names, ctx)?;
    let body = shape.body.clone();
    Ok(PendingFunction {
        name,
        params,
        return_type,
        body,
        span,
    })
}

fn parse_import_form(
    items: &[SExpr],
    variant_names: &HashSet<String>,
    resource_names: &HashSet<String>,
    ctx: &CompileContext,
) -> Result<Import> {
    let span = items[0].span().clone();
    // (import module name (params) [:] return-type)
    let ret_idx = match items.len() {
        5 => 4,
        6 if is_colon(&items[4]) => 5,
        _ => {
            return Err(ctx.error_with_note(
                "invalid import declaration",
                &span,
                "expected: (import module name ((param : type) ...) [:] return-type)",
            ));
        }
    };

    let module = match &items[1] {
        SExpr::Sym(s, _) => s.clone(),
        other => return Err(ctx.error("import module must be a symbol", other.span())),
    };
    let name = match &items[2] {
        SExpr::Sym(s, _) => s.clone(),
        other => return Err(ctx.error("import name must be a symbol", other.span())),
    };
    let params = parse_typed_params(&items[3], variant_names, resource_names, ctx)?;
    let return_type = parse_type_expr(&items[ret_idx], variant_names, resource_names, ctx)?;

    Ok(Import {
        module,
        name,
        params,
        return_type,
    })
}

fn parse_global_form(
    items: &[SExpr],
    variant_names: &HashSet<String>,
    resource_names: &HashSet<String>,
    ctx: &CompileContext,
) -> Result<Global> {
    let span = items[0].span().clone();
    // (global $name [:] type mutability init-value)
    let type_idx = match items.len() {
        5 => 2,
        6 if is_colon(&items[2]) => 3,
        _ => {
            return Err(ctx.error_with_note(
                "invalid global declaration",
                &span,
                "expected: (global $name [:] type mutability init-value)",
            ));
        }
    };
    let mut_idx = type_idx + 1;
    let init_idx = type_idx + 2;

    let name = match &items[1] {
        SExpr::Sym(s, sym_span) => {
            if !s.starts_with('$') {
                return Err(ctx.error_with_note(
                    "global name must start with '$'",
                    sym_span,
                    "e.g., $heap-ptr, $counter",
                ));
            }
            s.clone()
        }
        other => {
            return Err(ctx.error("global name must be a symbol starting with $", other.span()));
        }
    };

    let ty = parse_type_expr(&items[type_idx], variant_names, resource_names, ctx)?;

    let mutable = match &items[mut_idx] {
        SExpr::Sym(s, sym_span) => match s.as_str() {
            "mut" => true,
            "const" => false,
            _ => {
                return Err(ctx.error_with_note(
                    "invalid mutability specifier",
                    sym_span,
                    "expected 'mut' or 'const'",
                ));
            }
        },
        other => return Err(ctx.error("mutability must be 'mut' or 'const'", other.span())),
    };

    let init_value = match &items[init_idx] {
        SExpr::Int { value, .. } => *value,
        other => {
            return Err(ctx.error(
                "global init value must be an integer constant",
                other.span(),
            ));
        }
    };

    Ok(Global {
        name,
        ty,
        mutable,
        init_value,
    })
}

/// Parse a record definition: (record name (field1 type1) (field2 type2) ...)
fn parse_record_form(
    items: &[SExpr],
    variant_names: &HashSet<String>,
    resource_names: &HashSet<String>,
    ctx: &CompileContext,
) -> Result<RecordDef> {
    let span = items[0].span().clone();

    if items.len() < 2 {
        return Err(ctx.error_with_note(
            "invalid record declaration",
            &span,
            "expected: (record name (field type) ...)",
        ));
    }

    let name = match &items[1] {
        SExpr::Sym(s, _) => s.clone(),
        other => return Err(ctx.error("record name must be a symbol", other.span())),
    };

    let mut fields = Vec::new();
    for item in &items[2..] {
        match item {
            SExpr::List(parts, field_span) => {
                if parts.len() != 2 {
                    return Err(ctx.error_with_note(
                        "invalid field declaration",
                        field_span,
                        "expected: (field-name type)",
                    ));
                }
                let field_name = match &parts[0] {
                    SExpr::Sym(s, _) => s.clone(),
                    other => return Err(ctx.error("field name must be a symbol", other.span())),
                };
                let field_ty = parse_type_expr(&parts[1], variant_names, resource_names, ctx)?;
                fields.push(RecordField {
                    name: field_name,
                    ty: field_ty,
                });
            }
            other => return Err(ctx.error("field must be a list (name type)", other.span())),
        }
    }

    if fields.is_empty() {
        return Err(ctx.error_with_note(
            "record must have at least one field",
            &span,
            "add fields like: (record point (x s32) (y s32))",
        ));
    }

    Ok(RecordDef { name, fields })
}

fn parse_variant_form(
    items: &[SExpr],
    variant_names: &HashSet<String>,
    resource_names: &HashSet<String>,
    ctx: &CompileContext,
) -> Result<VariantDef> {
    let span = items[0].span().clone();

    if items.len() < 2 {
        return Err(ctx.error_with_note(
            "invalid variant declaration",
            &span,
            "expected: (variant name (case payload...) ...)",
        ));
    }

    let name = match &items[1] {
        SExpr::Sym(s, _) => s.clone(),
        other => return Err(ctx.error("variant name must be a symbol", other.span())),
    };

    let mut cases = Vec::new();
    for item in &items[2..] {
        match item {
            SExpr::List(parts, case_span) => {
                if parts.is_empty() {
                    return Err(ctx.error("variant case must have a name", case_span));
                }
                let case_name = match &parts[0] {
                    SExpr::Sym(s, _) => s.clone(),
                    other => return Err(ctx.error("case name must be a symbol", other.span())),
                };
                let mut payload = Vec::new();
                let mut payload_mult = Vec::new();
                for ty_expr in &parts[1..] {
                    // A `(lin T)` / `(aff T)` payload makes this slot linear/affine;
                    // the qualifier is erased at the Type level.
                    let mult = match ty_expr {
                        SExpr::List(q, _) if head_sym(q) == Some("lin") => Multiplicity::Lin,
                        SExpr::List(q, _) if head_sym(q) == Some("aff") => Multiplicity::Aff,
                        _ => Multiplicity::Un,
                    };
                    payload.push(parse_type_expr(
                        ty_expr,
                        variant_names,
                        resource_names,
                        ctx,
                    )?);
                    payload_mult.push(mult);
                }
                cases.push(VariantCase {
                    name: case_name,
                    payload,
                    payload_mult,
                });
            }
            other => {
                return Err(ctx.error(
                    "variant case must be a list (case-name type...)",
                    other.span(),
                ));
            }
        }
    }

    if cases.is_empty() {
        return Err(ctx.error_with_note(
            "variant must have at least one case",
            &span,
            "add cases like: (variant shape (circle s32) (rectangle s32 s32) (point))",
        ));
    }

    Ok(VariantDef { name, cases })
}

fn parse_resource_form(items: &[SExpr], ctx: &CompileContext) -> Result<ResourceDef> {
    let span = items[0].span().clone();

    if items.len() != 2 {
        return Err(ctx.error_with_note(
            "invalid resource declaration",
            &span,
            "expected: (resource name)",
        ));
    }

    let name = match &items[1] {
        SExpr::Sym(s, _) => s.clone(),
        other => return Err(ctx.error("resource name must be a symbol", other.span())),
    };

    Ok(ResourceDef { name })
}

fn parse_typed_params(
    expr: &SExpr,
    variant_names: &HashSet<String>,
    resource_names: &HashSet<String>,
    ctx: &CompileContext,
) -> Result<Vec<Parameter>> {
    match expr {
        SExpr::List(params, _) => {
            let mut result = Vec::new();
            for p in params {
                match p {
                    SExpr::List(parts, param_span) => {
                        // Accept both `(name type)` and `(name : type)`.
                        let type_expr = match parts.len() {
                            2 => &parts[1],
                            3 if matches!(&parts[1], SExpr::Sym(s, _) if s == ":") => &parts[2],
                            _ => {
                                return Err(ctx.error_with_note(
                                    "invalid parameter",
                                    param_span,
                                    "expected: (name type) or (name : type)",
                                ));
                            }
                        };
                        let (name, scopes) = match &parts[0] {
                            SExpr::Sym(s, span) => (s.clone(), span.scopes.clone()),
                            other => {
                                return Err(
                                    ctx.error("parameter name must be a symbol", other.span())
                                );
                            }
                        };
                        // A `(lin T)` / `(aff T)` parameter type sets its multiplicity.
                        // The qualifier is erased at the Type level; `parse_type_expr`
                        // yields the underlying `T`.
                        let mult = match type_expr {
                            SExpr::List(q, _) if head_sym(q) == Some("lin") => Multiplicity::Lin,
                            SExpr::List(q, _) if head_sym(q) == Some("aff") => Multiplicity::Aff,
                            _ => Multiplicity::Un,
                        };
                        let ty = parse_type_expr(type_expr, variant_names, resource_names, ctx)?;
                        result.push(Parameter {
                            name,
                            ty,
                            scopes,
                            mult,
                        });
                    }
                    other => {
                        return Err(ctx.error_with_note(
                            "invalid parameter",
                            other.span(),
                            "expected: (name type)",
                        ));
                    }
                }
            }
            Ok(result)
        }
        other => Err(ctx.error("expected parameter list", other.span())),
    }
}

fn parse_type_expr(
    expr: &SExpr,
    variant_names: &HashSet<String>,
    resource_names: &HashSet<String>,
    ctx: &CompileContext,
) -> Result<Type> {
    match expr {
        SExpr::Sym(s, span) => parse_type_symbol(s, variant_names, resource_names, span, ctx),
        SExpr::List(items, span) => {
            if items.is_empty() {
                return Err(ctx.error("empty type expression", span));
            }
            match &items[0] {
                SExpr::Sym(s, _) if s == "option" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid option type",
                            span,
                            "expected: (option T)",
                        ));
                    }
                    let inner = parse_type_expr(&items[1], variant_names, resource_names, ctx)?;
                    Ok(Type::Option(Box::new(inner)))
                }
                SExpr::Sym(s, _) if s == "result" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid result type",
                            span,
                            "expected: (result T E)",
                        ));
                    }
                    let ok_ty = parse_type_expr(&items[1], variant_names, resource_names, ctx)?;
                    let err_ty = parse_type_expr(&items[2], variant_names, resource_names, ctx)?;
                    Ok(Type::Result(Box::new(ok_ty), Box::new(err_ty)))
                }
                SExpr::Sym(s, _) if s == "list" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid list type",
                            span,
                            "expected: (list T)",
                        ));
                    }
                    let inner = parse_type_expr(&items[1], variant_names, resource_names, ctx)?;
                    Ok(Type::List(Box::new(inner)))
                }
                SExpr::Sym(s, _) if s == "borrow" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid borrow type",
                            span,
                            "expected: (borrow T)",
                        ));
                    }
                    let inner = parse_type_expr(&items[1], variant_names, resource_names, ctx)?;
                    Ok(Type::Borrow(Box::new(inner)))
                }
                SExpr::Sym(s, _) if s == "tuple" => {
                    if items.len() < 2 {
                        return Err(ctx.error_with_note(
                            "invalid tuple type",
                            span,
                            "expected: (tuple T1 T2 ...)",
                        ));
                    }
                    let elems: Vec<Type> = items[1..]
                        .iter()
                        .map(|item| parse_type_expr(item, variant_names, resource_names, ctx))
                        .collect::<Result<Vec<_>>>()?;
                    Ok(Type::Tuple(elems))
                }
                // Multiplicity qualifier: `(lin T)` is `T` with a use-exactly-once
                // obligation enforced by the linearity checker; the qualifier is
                // erased at the Type level (same runtime representation as `T`).
                // Multiplicity qualifiers `(lin T)` / `(aff T)`: erased at the Type
                // level (same representation as `T`); the obligation is enforced by
                // the linearity checker from the binding's recorded multiplicity.
                SExpr::Sym(s, _) if s == "lin" || s == "aff" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid multiplicity-qualified type",
                            span,
                            "expected: (lin T) or (aff T)",
                        ));
                    }
                    parse_type_expr(&items[1], variant_names, resource_names, ctx)
                }
                _ => Err(ctx.error("unknown parameterized type", span)),
            }
        }
        other => Err(ctx.error("type must be a symbol or parameterized type", other.span())),
    }
}

fn parse_type_symbol(
    sym: &str,
    variant_names: &HashSet<String>,
    resource_names: &HashSet<String>,
    _span: &Span,
    _ctx: &CompileContext,
) -> Result<Type> {
    match sym {
        "s32" => Ok(Type::S32),
        "s64" => Ok(Type::S64),
        "f32" => Ok(Type::F32),
        "f64" => Ok(Type::F64),
        "u8" => Ok(Type::U8),
        "bool" => Ok(Type::Bool),
        "u16" => Ok(Type::U16),
        "u32" => Ok(Type::U32),
        "u64" => Ok(Type::U64),
        "string" => Ok(Type::Str),
        "any" => Ok(Type::Any),            // Pack dynamic `value`
        "unit" => Ok(Type::Tuple(vec![])), // unit type is empty tuple
        // Check if this is a variant type name
        other if variant_names.contains(other) => Ok(Type::Variant(other.to_string())),
        // Check if this is a resource type name
        other if resource_names.contains(other) => Ok(Type::Resource(other.to_string())),
        // Otherwise treat as a record type name
        // We'll validate that the record actually exists during type checking
        other => Ok(Type::Record(other.to_string())),
    }
}

fn is_type_symbol(sym: &str) -> bool {
    matches!(
        sym,
        "s32"
            | "s64"
            | "f32"
            | "f64"
            | "u8"
            | "bool"
            | "u16"
            | "u32"
            | "u64"
            | "string"
            | "any"
            | "unit"
    )
}

fn parse_expr(
    sexpr: &SExpr,
    vars: &[Binding],
    functions: &HashMap<String, Signature>,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
    ctx: &CompileContext,
) -> Result<Expr> {
    match sexpr {
        SExpr::Int { value, ty, .. } => Ok(Expr::Int {
            value: *value,
            ty: ty.clone(),
        }),
        SExpr::Float { value, ty, .. } => Ok(Expr::Float {
            value: *value,
            ty: ty.clone(),
        }),
        SExpr::Str(s, _) => Ok(Expr::StringLiteral(s.clone())),
        SExpr::Sym(s, span) => {
            // Hygienic variable resolution: find bindings with matching name
            // where the binding's scopes are a subset of the reference's scopes
            let ref_scopes = &span.scopes;

            let matching_bindings: Vec<_> = vars
                .iter()
                .filter(|b| b.name == *s && b.is_visible_from(ref_scopes))
                .collect();

            match matching_bindings.len() {
                0 => Err(ctx.error(format!("unknown variable '{}'", s), span)),
                1 => {
                    // Use mangled name to preserve scope distinction in codegen
                    Ok(Expr::Var(matching_bindings[0].mangled_name()))
                }
                _ => {
                    // Multiple matching bindings - find the most specific one
                    // (the one with the most scopes that is still a subset)
                    let best = matching_bindings
                        .iter()
                        .max_by_key(|b| b.scopes.scopes.len())
                        .unwrap();
                    // Use mangled name to preserve scope distinction in codegen
                    Ok(Expr::Var(best.mangled_name()))
                }
            }
        }
        SExpr::List(items, list_span) => {
            if items.is_empty() {
                return Err(ctx.error("empty list is not a valid expression", list_span));
            }
            // Ascription in colon form: (expr : type), mirroring the head form.
            // Accepts any type (scalar OR compound): a same-type ascription is an
            // identity annotation; a differing scalar ascription is a cast.
            if items.len() == 3 && is_colon(&items[1]) {
                let variant_names: HashSet<String> = variants.keys().cloned().collect();
                let ty = parse_type_expr(&items[2], &variant_names, &HashSet::new(), ctx)?;
                let inner = parse_expr(&items[0], vars, functions, records, variants, ctx)?;
                return Ok(Expr::Ascribe {
                    expr: Box::new(inner),
                    ty,
                });
            }
            // Create variant name set for type parsing
            let variant_names: HashSet<String> = variants.keys().cloned().collect();
            let op = &items[0];
            match op {
                SExpr::Sym(sym, sym_span) if is_type_symbol(sym) && items.len() == 2 => {
                    // Numeric casts emit conversion instructions.
                    let numeric = match sym.as_str() {
                        "s32" => Some(Type::S32),
                        "s64" => Some(Type::S64),
                        "f32" => Some(Type::F32),
                        "f64" => Some(Type::F64),
                        _ => None,
                    };
                    if let Some(ty) = numeric {
                        let inner = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                        return Ok(Expr::Ascribe {
                            expr: Box::new(inner),
                            ty,
                        });
                    }
                    // The first-class integer/bool scalars have no literal syntax of
                    // their own; `(bool 1)` / `(u32 100)` retypes an integer literal.
                    let scalar = match sym.as_str() {
                        "u8" => Some(Type::U8),
                        "u16" => Some(Type::U16),
                        "u32" => Some(Type::U32),
                        "u64" => Some(Type::U64),
                        "bool" => Some(Type::Bool),
                        _ => None,
                    };
                    if let Some(ty) = scalar {
                        // An integer literal retypes in place; a runtime value
                        // casts via Ascribe (masking/extension as needed).
                        if let SExpr::Int { value, .. } = &items[1] {
                            return Ok(Expr::Int { value: *value, ty });
                        }
                        let inner = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                        return Ok(Expr::Ascribe {
                            expr: Box::new(inner),
                            ty,
                        });
                    }
                    Err(ctx.error_with_note(
                        format!("cannot cast to '{sym}'"),
                        sym_span,
                        "casts apply to scalar types (s32/s64/f32/f64/u8/u16/u32/u64/bool)",
                    ))
                }
                SExpr::Sym(sym, sym_span) if sym == "if" => {
                    if items.len() != 4 {
                        return Err(ctx.error_with_note(
                            "invalid 'if' expression",
                            list_span,
                            "expected: (if condition then-expr else-expr)",
                        ));
                    }
                    let cond = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    let then_branch =
                        parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    let else_branch =
                        parse_expr(&items[3], vars, functions, records, variants, ctx)?;
                    Ok(Expr::If {
                        cond: Box::new(cond),
                        then_branch: Box::new(then_branch),
                        else_branch: Box::new(else_branch),
                    })
                }
                SExpr::Sym(sym, sym_span) if sym == "global.get" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'global.get' expression",
                            list_span,
                            "expected: (global.get $name)",
                        ));
                    }
                    let name = match &items[1] {
                        SExpr::Sym(s, s_span) => {
                            if !s.starts_with('$') {
                                return Err(ctx.error("global name must start with '$'", s_span));
                            }
                            s.clone()
                        }
                        other => {
                            return Err(ctx.error(
                                "global.get argument must be a global name starting with $",
                                other.span(),
                            ));
                        }
                    };
                    Ok(Expr::GlobalGet { name })
                }
                SExpr::Sym(sym, sym_span) if sym == "global.set" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'global.set' expression",
                            list_span,
                            "expected: (global.set $name value)",
                        ));
                    }
                    let name = match &items[1] {
                        SExpr::Sym(s, s_span) => {
                            if !s.starts_with('$') {
                                return Err(ctx.error("global name must start with '$'", s_span));
                            }
                            s.clone()
                        }
                        other => {
                            return Err(ctx.error(
                                "global.set first argument must be a global name starting with $",
                                other.span(),
                            ));
                        }
                    };
                    let value = parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    Ok(Expr::GlobalSet {
                        name,
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, sym_span) if sym == "let" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'let' expression",
                            list_span,
                            "expected: (let (name value) body)",
                        ));
                    }
                    let binding = match &items[1] {
                        SExpr::List(parts, _) => parts,
                        other => {
                            return Err(ctx.error_with_note(
                                "let binding must be a list",
                                other.span(),
                                "expected: (name value) or (name : type value)",
                            ));
                        }
                    };
                    // Accept `(name value)`, `(name : scalar value)` (a cast), and
                    // `(name : (lin T) value)` / `(name : (aff T) value)` (a linear/
                    // affine binding — the qualifier marks multiplicity, no cast).
                    let (name_sexpr, value_sexpr, annotation, explicit_mult) = match binding.len() {
                        2 => (&binding[0], &binding[1], None, None),
                        4 if is_colon(&binding[1]) => match &binding[2] {
                            SExpr::List(q, _) if head_sym(q) == Some("lin") => {
                                (&binding[0], &binding[3], None, Some(Multiplicity::Lin))
                            }
                            SExpr::List(q, _) if head_sym(q) == Some("aff") => {
                                (&binding[0], &binding[3], None, Some(Multiplicity::Aff))
                            }
                            SExpr::Sym(t, _) if is_type_symbol(t) => {
                                let ty = match t.as_str() {
                                    "s32" => Type::S32,
                                    "s64" => Type::S64,
                                    "f32" => Type::F32,
                                    "f64" => Type::F64,
                                    _ => {
                                        return Err(ctx.error_with_note(
                                                "let scalar cast supports s32/s64/f32/f64",
                                                binding[2].span(),
                                                "for other types use (name : (lin T) value) or no annotation",
                                            ));
                                    }
                                };
                                (&binding[0], &binding[3], Some(ty), None)
                            }
                            other => {
                                return Err(ctx.error_with_note(
                                    "let type annotation must be a scalar type or (lin T)/(aff T)",
                                    other.span(),
                                    "e.g. (name : s32 value) or (name : (lin T) value)",
                                ));
                            }
                        },
                        _ => {
                            return Err(ctx.error_with_note(
                                "invalid let binding",
                                items[1].span(),
                                "expected: (name value) or (name : type value)",
                            ));
                        }
                    };
                    let (name, name_scopes) = match name_sexpr {
                        SExpr::Sym(s, span) => (s.clone(), span.scopes.clone()),
                        other => {
                            return Err(
                                ctx.error("let binding name must be a symbol", other.span())
                            );
                        }
                    };
                    let mut value_expr =
                        parse_expr(value_sexpr, vars, functions, records, variants, ctx)?;
                    // A colon annotation ascribes the value to the declared type.
                    if let Some(ty) = annotation {
                        value_expr = Expr::Ascribe {
                            expr: Box::new(value_expr),
                            ty,
                        };
                    }
                    // Multiplicity: an explicit `(lin/aff T)` wins; otherwise infer it
                    // from the value (a linear-variant constructor or a function
                    // returning one makes the binding linear — infectious linearity).
                    let mult = explicit_mult
                        .unwrap_or_else(|| value_multiplicity(value_sexpr, functions, variants));
                    // Create a new binding with the name and its scopes for hygienic resolution
                    let new_binding = Binding::new(name, name_scopes);
                    let mangled_name = new_binding.mangled_name();
                    let mut next_vars = vars.to_vec();
                    next_vars.push(new_binding);
                    let body_expr =
                        parse_expr(&items[2], &next_vars, functions, records, variants, ctx)?;
                    Ok(Expr::Let {
                        name: mangled_name, // Use mangled name for codegen
                        value: Box::new(value_expr),
                        body: Box::new(body_expr),
                        mult,
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "with-cap" => {
                    // (with-cap (c Cap) body): bind c to a fresh linear capability
                    // token of type Cap for the scope of body.
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'with-cap' expression",
                            list_span,
                            "expected: (with-cap (name Cap) body)",
                        ));
                    }
                    let binding = match &items[1] {
                        SExpr::List(parts, _) if parts.len() == 2 => parts,
                        other => {
                            return Err(ctx.error_with_note(
                                "with-cap binding must be (name Cap)",
                                other.span(),
                                "expected: (with-cap (name Cap) body)",
                            ));
                        }
                    };
                    let (name, name_scopes) = match &binding[0] {
                        SExpr::Sym(s, span) => (s.clone(), span.scopes.clone()),
                        other => {
                            return Err(ctx.error("capability name must be a symbol", other.span()));
                        }
                    };
                    let cap = match &binding[1] {
                        SExpr::Sym(s, _) => s.clone(),
                        other => {
                            return Err(ctx.error("capability type must be a symbol", other.span()));
                        }
                    };
                    let new_binding = Binding::new(name, name_scopes);
                    let mangled_name = new_binding.mangled_name();
                    let mut next_vars = vars.to_vec();
                    next_vars.push(new_binding);
                    let body_expr =
                        parse_expr(&items[2], &next_vars, functions, records, variants, ctx)?;
                    Ok(Expr::WithCap {
                        name: mangled_name,
                        cap,
                        body: Box::new(body_expr),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "release-cap" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'release-cap' expression",
                            list_span,
                            "expected: (release-cap capability)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::ReleaseCap {
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, sym_span) if sym == "&" => {
                    // (& c): borrow the capability bound to c without consuming it.
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid borrow expression",
                            list_span,
                            "expected: (& capability-variable)",
                        ));
                    }
                    let inner = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    match inner {
                        Expr::Var(name) => Ok(Expr::BorrowCap { name }),
                        _ => Err(ctx.error_with_note(
                            "borrow expects a capability variable",
                            sym_span,
                            "expected: (& c) where c is a capability bound by with-cap",
                        )),
                    }
                }
                SExpr::Sym(sym, _sym_span) if sym == "begin" => {
                    if items.len() < 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'begin' expression",
                            list_span,
                            "expected: (begin expr1 expr2 ...)",
                        ));
                    }
                    let mut exprs = Vec::new();
                    for item in &items[1..] {
                        exprs.push(parse_expr(item, vars, functions, records, variants, ctx)?);
                    }
                    Ok(Expr::Begin { exprs })
                }
                SExpr::Sym(sym, _sym_span) if sym == "match" => {
                    // (match expr ((case var1 var2) body) ...)
                    if items.len() < 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'match' expression",
                            list_span,
                            "expected: (match expr ((case-name bindings...) body) ...)",
                        ));
                    }
                    let match_expr =
                        parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    let mut arms = Vec::new();

                    for case_item in &items[2..] {
                        let case_parts = match case_item {
                            SExpr::List(parts, _) => parts,
                            other => {
                                return Err(ctx.error("match arm must be a list", other.span()));
                            }
                        };

                        if case_parts.len() != 2 {
                            return Err(ctx.error_with_note(
                                "invalid match arm",
                                case_item.span(),
                                "expected: ((case-name bindings...) body)",
                            ));
                        }

                        // Parse pattern: (case-name binding1 binding2 ...)
                        let pattern = match &case_parts[0] {
                            SExpr::List(pat_parts, _) => pat_parts,
                            other => {
                                return Err(ctx.error("match pattern must be a list", other.span()));
                            }
                        };

                        if pattern.is_empty() {
                            return Err(
                                ctx.error("match pattern cannot be empty", case_parts[0].span())
                            );
                        }

                        let case_name = match &pattern[0] {
                            SExpr::Sym(s, _) => s.clone(),
                            other => {
                                return Err(ctx.error("case name must be a symbol", other.span()));
                            }
                        };

                        // Collect bindings
                        let mut bindings = Vec::new();
                        let mut next_vars = vars.to_vec();
                        for binding in &pattern[1..] {
                            let (name, name_scopes) = match binding {
                                SExpr::Sym(s, span) => (s.clone(), span.scopes.clone()),
                                other => {
                                    return Err(ctx.error("binding must be a symbol", other.span()));
                                }
                            };
                            let new_binding = Binding::new(name, name_scopes);
                            let mangled_name = new_binding.mangled_name();
                            bindings.push(mangled_name);
                            next_vars.push(new_binding);
                        }

                        // Parse body with extended bindings
                        let body = parse_expr(
                            &case_parts[1],
                            &next_vars,
                            functions,
                            records,
                            variants,
                            ctx,
                        )?;

                        // The bindings' multiplicities come from the matched case's
                        // payload (a `(lin T)` payload binds a linear value). Unknown
                        // cases (`_`, option/result) carry unrestricted bindings.
                        let binding_mult = find_variant_by_case(&case_name, variants)
                            .and_then(|vd| {
                                vd.find_case(&case_name)
                                    .map(|(_, c)| c.payload_mult.clone())
                            })
                            .filter(|m| m.len() == bindings.len())
                            .unwrap_or_else(|| vec![Multiplicity::Un; bindings.len()]);
                        arms.push(MatchArm {
                            case_name,
                            bindings,
                            binding_mult,
                            body,
                        });
                    }

                    Ok(Expr::Match {
                        expr: Box::new(match_expr),
                        cases: arms,
                    })
                }
                // Option constructors: (some T value) and (none T)
                SExpr::Sym(sym, _sym_span) if sym == "some" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'some' expression",
                            list_span,
                            "expected: (some inner-type value)",
                        ));
                    }
                    let inner_type =
                        parse_type_expr(&items[1], &variant_names, &HashSet::new(), ctx)?;
                    let value = parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    Ok(Expr::Some {
                        inner_type,
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "none" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'none' expression",
                            list_span,
                            "expected: (none inner-type)",
                        ));
                    }
                    let inner_type =
                        parse_type_expr(&items[1], &variant_names, &HashSet::new(), ctx)?;
                    Ok(Expr::None { inner_type })
                }
                // Result constructors: (ok T E value) and (err T E value)
                SExpr::Sym(sym, _sym_span) if sym == "ok" => {
                    if items.len() != 4 {
                        return Err(ctx.error_with_note(
                            "invalid 'ok' expression",
                            list_span,
                            "expected: (ok ok-type err-type value)",
                        ));
                    }
                    let ok_type = parse_type_expr(&items[1], &variant_names, &HashSet::new(), ctx)?;
                    let err_type =
                        parse_type_expr(&items[2], &variant_names, &HashSet::new(), ctx)?;
                    let value = parse_expr(&items[3], vars, functions, records, variants, ctx)?;
                    Ok(Expr::Ok {
                        ok_type,
                        err_type,
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "err" => {
                    if items.len() != 4 {
                        return Err(ctx.error_with_note(
                            "invalid 'err' expression",
                            list_span,
                            "expected: (err ok-type err-type value)",
                        ));
                    }
                    let ok_type = parse_type_expr(&items[1], &variant_names, &HashSet::new(), ctx)?;
                    let err_type =
                        parse_type_expr(&items[2], &variant_names, &HashSet::new(), ctx)?;
                    let value = parse_expr(&items[3], vars, functions, records, variants, ctx)?;
                    Ok(Expr::Err {
                        ok_type,
                        err_type,
                        value: Box::new(value),
                    })
                }
                // Tuple constructor: (tuple expr1 expr2 ...)
                SExpr::Sym(sym, _sym_span) if sym == "tuple" => {
                    if items.len() < 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'tuple' expression",
                            list_span,
                            "expected: (tuple value1 value2 ...)",
                        ));
                    }
                    let values: Vec<Expr> = items[1..]
                        .iter()
                        .map(|item| parse_expr(item, vars, functions, records, variants, ctx))
                        .collect::<Result<Vec<_>>>()?;
                    Ok(Expr::TupleConstruct { values })
                }
                // List operations
                SExpr::Sym(sym, _sym_span) if sym == "list-new" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'list-new' expression",
                            list_span,
                            "expected: (list-new elem-type)",
                        ));
                    }
                    let elem_type =
                        parse_type_expr(&items[1], &variant_names, &HashSet::new(), ctx)?;
                    Ok(Expr::ListNew { elem_type })
                }
                SExpr::Sym(sym, _sym_span) if sym == "list-push" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'list-push' expression",
                            list_span,
                            "expected: (list-push list value)",
                        ));
                    }
                    let list = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    let value = parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    Ok(Expr::ListPush {
                        list: Box::new(list),
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "list-get" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'list-get' expression",
                            list_span,
                            "expected: (list-get list index)",
                        ));
                    }
                    let list = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    let index = parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    Ok(Expr::ListGet {
                        list: Box::new(list),
                        index: Box::new(index),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "list-len" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'list-len' expression",
                            list_span,
                            "expected: (list-len list)",
                        ));
                    }
                    let list = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::ListLen {
                        list: Box::new(list),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "string-len" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'string-len' expression",
                            list_span,
                            "expected: (string-len string)",
                        ));
                    }
                    let string = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::StringLen {
                        string: Box::new(string),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "string-ref" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'string-ref' expression",
                            list_span,
                            "expected: (string-ref string index)",
                        ));
                    }
                    let string = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    let index = parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    Ok(Expr::StringRef {
                        string: Box::new(string),
                        index: Box::new(index),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "substring" => {
                    if items.len() != 4 {
                        return Err(ctx.error_with_note(
                            "invalid 'substring' expression",
                            list_span,
                            "expected: (substring string start end)",
                        ));
                    }
                    let string = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    let start = parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    let end = parse_expr(&items[3], vars, functions, records, variants, ctx)?;
                    Ok(Expr::Substring {
                        string: Box::new(string),
                        start: Box::new(start),
                        end: Box::new(end),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "any-s32" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'any-s32' expression",
                            list_span,
                            "expected: (any-s32 s32-expr)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::AnyFromS32 {
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "any-as-s32" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'any-as-s32' expression",
                            list_span,
                            "expected: (any-as-s32 any-expr)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::AnyToS32 {
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "any-string" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'any-string' expression",
                            list_span,
                            "expected: (any-string string-expr)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::AnyFromString {
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "any-as-string" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'any-as-string' expression",
                            list_span,
                            "expected: (any-as-string any-expr)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::AnyToString {
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "heap-alloc" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'heap-alloc' expression",
                            list_span,
                            "expected: (heap-alloc size-expr)",
                        ));
                    }
                    let size = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::HeapAlloc {
                        size: Box::new(size),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "any-addr" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'any-addr' expression",
                            list_span,
                            "expected: (any-addr any-expr)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::AnyAddr {
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "any-from-addr" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'any-from-addr' expression",
                            list_span,
                            "expected: (any-from-addr addr-expr)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::AnyFromAddr {
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "string-addr" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'string-addr' expression",
                            list_span,
                            "expected: (string-addr string-expr)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::StringAddr {
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "string-from-addr" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'string-from-addr' expression",
                            list_span,
                            "expected: (string-from-addr addr-expr)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::StringFromAddr {
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "call-raw" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'call-raw' expression",
                            list_span,
                            "expected: (call-raw args-any)",
                        ));
                    }
                    let value = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::RawInvoke {
                        module: "theater:simple/rpc".to_string(),
                        import: "call".to_string(),
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "raw-invoke" => {
                    // (raw-invoke "iface" "name" args-any) -> any. Generic raw-CGRF
                    // call to any declared host import; args-any is a marshalled blob.
                    // The interface qualifies the raw symbol (so same-named functions
                    // in different interfaces don't collide).
                    if items.len() != 4 {
                        return Err(ctx.error_with_note(
                            "invalid 'raw-invoke' expression",
                            list_span,
                            "expected: (raw-invoke \"iface\" \"name\" args-any)",
                        ));
                    }
                    let (SExpr::Str(module, _), SExpr::Str(import, _)) = (&items[1], &items[2])
                    else {
                        return Err(ctx.error_with_note(
                            "raw-invoke interface and name must be string literals",
                            list_span,
                            "expected: (raw-invoke \"iface\" \"name\" args-any)",
                        ));
                    };
                    let value = parse_expr(&items[3], vars, functions, records, variants, ctx)?;
                    Ok(Expr::RawInvoke {
                        module: module.clone(),
                        import: import.clone(),
                        value: Box::new(value),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "string-append" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'string-append' expression",
                            list_span,
                            "expected: (string-append string1 string2)",
                        ));
                    }
                    let left = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    let right = parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    Ok(Expr::StringAppend {
                        left: Box::new(left),
                        right: Box::new(right),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "string=?" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'string=?' expression",
                            list_span,
                            "expected: (string=? string1 string2)",
                        ));
                    }
                    let left = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    let right = parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    Ok(Expr::StringEq {
                        left: Box::new(left),
                        right: Box::new(right),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "string-from-bytes" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'string-from-bytes' expression",
                            list_span,
                            "expected: (string-from-bytes bytes)",
                        ));
                    }
                    let bytes = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::StringFromBytes {
                        bytes: Box::new(bytes),
                    })
                }
                SExpr::Sym(sym, _sym_span) if sym == "string-to-bytes" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid 'string-to-bytes' expression",
                            list_span,
                            "expected: (string-to-bytes string)",
                        ));
                    }
                    let string = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::StringToBytes {
                        string: Box::new(string),
                    })
                }
                _ => {
                    if let SExpr::Sym(sym, sym_span) = op {
                        // Check if this is a WASM instruction
                        if lookup_wasm_instr(sym).is_some() {
                            let mut args = Vec::new();
                            for arg in &items[1..] {
                                args.push(parse_expr(
                                    arg, vars, functions, records, variants, ctx,
                                )?);
                            }
                            Ok(Expr::WasmInstr {
                                name: sym.clone(),
                                args,
                            })
                        } else if let Some(expected) = functions.get(sym) {
                            // Function call
                            if items.len() - 1 != expected.params.len() {
                                return Err(ctx.error(
                                    format!(
                                        "function '{}' expects {} arguments, got {}",
                                        sym,
                                        expected.params.len(),
                                        items.len() - 1
                                    ),
                                    list_span,
                                ));
                            }
                            let mut args = Vec::new();
                            for arg in &items[1..] {
                                args.push(parse_expr(
                                    arg, vars, functions, records, variants, ctx,
                                )?);
                            }
                            Ok(Expr::Call {
                                name: sym.clone(),
                                args,
                            })
                        } else if let Some(record_def) = records.get(sym) {
                            // Record construction: (point 10 20)
                            if items.len() - 1 != record_def.fields.len() {
                                return Err(ctx.error(
                                    format!(
                                        "record '{}' expects {} fields, got {}",
                                        sym,
                                        record_def.fields.len(),
                                        items.len() - 1
                                    ),
                                    list_span,
                                ));
                            }
                            let mut fields = Vec::new();
                            for arg in &items[1..] {
                                fields.push(parse_expr(
                                    arg, vars, functions, records, variants, ctx,
                                )?);
                            }
                            Ok(Expr::RecordConstruct {
                                record_name: sym.clone(),
                                fields,
                            })
                        } else if let Some(variant_def) = find_variant_by_case(sym, variants) {
                            // Variant case construction: (circle 5) or (point)
                            let (_, case) = variant_def.find_case(sym).unwrap();
                            if items.len() - 1 != case.payload.len() {
                                return Err(ctx.error(
                                    format!(
                                        "variant case '{}' expects {} payload values, got {}",
                                        sym,
                                        case.payload.len(),
                                        items.len() - 1
                                    ),
                                    list_span,
                                ));
                            }
                            let mut payload = Vec::new();
                            for arg in &items[1..] {
                                payload.push(parse_expr(
                                    arg, vars, functions, records, variants, ctx,
                                )?);
                            }
                            Ok(Expr::VariantConstruct {
                                variant_name: variant_def.name.clone(),
                                case_name: sym.clone(),
                                payload,
                            })
                        } else if sym.contains('.') {
                            // Check for record field access: (point.x expr)
                            let parts: Vec<&str> = sym.splitn(2, '.').collect();
                            if parts.len() == 2 {
                                let record_name = parts[0];
                                let field_name = parts[1];
                                if let Some(_record_def) = records.get(record_name) {
                                    if items.len() != 2 {
                                        return Err(ctx.error_with_note(
                                            "invalid field access",
                                            list_span,
                                            format!(
                                                "expected: ({}.{} record-expr)",
                                                record_name, field_name
                                            ),
                                        ));
                                    }
                                    let expr = parse_expr(
                                        &items[1], vars, functions, records, variants, ctx,
                                    )?;
                                    Ok(Expr::RecordAccess {
                                        record_name: record_name.to_string(),
                                        field_name: field_name.to_string(),
                                        expr: Box::new(expr),
                                    })
                                } else {
                                    Err(ctx.error(
                                        format!("unknown record type '{}'", record_name),
                                        sym_span,
                                    ))
                                }
                            } else {
                                Err(ctx.error(
                                    format!("unknown function or operator '{}'", sym),
                                    sym_span,
                                ))
                            }
                        } else {
                            Err(ctx
                                .error(format!("unknown function or operator '{}'", sym), sym_span))
                        }
                    } else {
                        Err(ctx.error("expression must start with a symbol", op.span()))
                    }
                }
            }
        }
        SExpr::Quasiquote(_, span) | SExpr::Unquote(_, span) | SExpr::UnquoteSplice(_, span) => {
            Err(ctx.error(
                "quasiquote/unquote should have been expanded before parsing",
                span,
            ))
        }
        SExpr::SyntaxQuote(_, span)
        | SExpr::Quasisyntax(_, span)
        | SExpr::Unsyntax(_, span)
        | SExpr::UnsyntaxSplice(_, span) => Err(ctx.error(
            "syntax forms (#', #`, #,, #,@) should have been expanded before parsing",
            span,
        )),
    }
}
