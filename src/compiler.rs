use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, anyhow, bail};
use wat::parse_str;
use wit_component::{ComponentEncoder, StringEncoding, embed_component_metadata};
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
    List { elem_type: Type, items: Vec<InlineValue> },
    Option { inner_type: Type, value: Option<Box<InlineValue>> },
    Result { ok_type: Type, err_type: Type, value: std::result::Result<Box<InlineValue>, Box<InlineValue>> },

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
            InlineValue::Result { ok_type, err_type, .. } => {
                Type::Result(Box::new(ok_type.clone()), Box::new(err_type.clone()))
            }
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
    fn new() -> Self {
        Self {
            scopes: HashSet::new(),
        }
    }

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
    pub wat: PathBuf,
    pub wasm: PathBuf,
}

pub fn compile(source_path: &Path, out_base: &Path) -> Result<CompileArtifacts> {
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

    // Collect macro definitions (both defmacro and define-syntax) and expand macros
    let macros = collect_macros(&forms);
    let expanded_forms = expand_all_macros(forms, &macros);

    let prog = parse_program(expanded_forms, &ctx)?;
    let signatures = collect_signatures(&prog)?;
    type_check(&prog, &signatures, &ctx)?;

    // Generate Pack-compatible WAT (raw module with Pack/Graph ABI)
    let wat = generate_wat_pack(&prog, &signatures);

    let mut wat_path = out_base.to_path_buf();
    wat_path.set_extension("wat");
    let mut wasm_path = out_base.to_path_buf();
    wasm_path.set_extension("wasm");

    fs::write(&wat_path, &wat)
        .with_context(|| format!("failed to write {}", wat_path.display()))?;

    // Convert WAT to raw WASM module (not a component)
    let wasm_bytes = parse_str(&wat).context("failed to convert generated WAT to wasm")?;
    fs::write(&wasm_path, &wasm_bytes)
        .with_context(|| format!("failed to write {}", wasm_path.display()))?;

    Ok(CompileArtifacts {
        wat: wat_path,
        wasm: wasm_path,
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
    if let Some(config) = world_config {
        if let Some(wit_deps) = &config.wit_deps {
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
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    LParen,
    RParen,
    Symbol(String),
    Number(NumericToken),
    String(String), // String literal
    Quasiquote,     // `
    Unquote,        // ,
    UnquoteSplice,  // ,@
    SyntaxQuote,    // #'
    Quasisyntax,    // #`
    Unsyntax,       // #,
    UnsyntaxSplice, // #,@
}

#[derive(Debug, Clone)]
pub enum NumericToken {
    Int { value: i64, ty: Type },
    Float { value: f64, ty: Type },
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
}

/// A single arm in a match expression
#[derive(Debug, Clone)]
struct MatchArm {
    case_name: String,
    bindings: Vec<String>, // Variable names to bind payload values
    body: Expr,
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

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub ty: Type,
    scopes: ScopeSet,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub module: String,
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Type,
    span: Span,
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
        // Records, variants, options, results, lists, and strings are pointer-sized
        Type::Record(_)
        | Type::Variant(_)
        | Type::Option(_)
        | Type::Result(_, _)
        | Type::List(_)
        | Type::Str => 4,
        // Resources and borrows are i32 handles
        Type::Resource(_) | Type::Borrow(_) => 4,
    }
}

/// Check if a type requires heap allocation
fn type_needs_heap(ty: &Type) -> bool {
    match ty {
        Type::S32 | Type::S64 | Type::F32 | Type::F64 => false,
        Type::Record(_)
        | Type::Variant(_)
        | Type::Option(_)
        | Type::Result(_, _)
        | Type::List(_)
        | Type::Str => true,
        // Resources don't need heap - they're opaque handles managed externally
        Type::Resource(_) | Type::Borrow(_) => false,
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
        Expr::WasmInstr { args, .. } => args.iter().any(expr_uses_heap),
        Expr::GlobalSet { value, .. } => expr_uses_heap(value),
        Expr::RecordConstruct { fields, .. } => true, // records need heap
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
        Expr::Substring { .. } | Expr::StringAppend { .. } => true, // allocate new strings
        Expr::StringEq { left, right } => expr_uses_heap(left) || expr_uses_heap(right),
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
    param: String, // The stx parameter name
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
    pub package: String,      // e.g., "theater:simple"
    pub interface: String,    // e.g., "runtime"
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
    pub wit_deps: Option<PathBuf>,           // Path to wit deps directory
    pub external_imports: Vec<ExternalInterface>,  // e.g., theater:simple/runtime
    pub external_exports: Vec<ExternalInterface>,  // e.g., theater:simple/actor
}

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
    pub imports: Vec<Import>,
    pub exports: Vec<String>,
    pub globals: Vec<Global>,
    pub records: Vec<RecordDef>,
    pub variants: Vec<VariantDef>,
    pub resources: Vec<ResourceDef>,
    pub world_config: Option<WorldConfig>,
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
        "i32.and" | "i32.or" | "i32.xor" | "i32.shl" | "i32.shr_s" | "i32.shr_u"
        | "i32.rotl" | "i32.rotr" => Some(WasmInstrInfo {
            params: vec![Type::S32, Type::S32],
            result: Type::S32,
        }),
        // i64 bitwise operations
        "i64.and" | "i64.or" | "i64.xor" | "i64.shl" | "i64.shr_s" | "i64.shr_u"
        | "i64.rotl" | "i64.rotr" => Some(WasmInstrInfo {
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
        if signatures.insert(import.name.clone(), sig).is_some() {
            bail!("Duplicate function '{}'", import.name);
        }
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
            ensure_numeric(&inner_ty, "ascribe requires numeric types")?;
            ensure_numeric(ty, "ascribe requires numeric types")?;
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
        Expr::Let { name, value, body } => {
            let value_ty = check_expr(value, env, signatures, globals, records, variants)?;
            let mut next_env = env.clone();
            next_env.insert(name.clone(), value_ty);
            check_expr(body, &next_env, signatures, globals, records, variants)
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
                if ty != *expected_ty {
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
                    return result_ty
                        .ok_or_else(|| anyhow!("match expression must have at least one case"));
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
                    return result_ty
                        .ok_or_else(|| anyhow!("match expression must have at least one case"));
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
                    return result_ty
                        .ok_or_else(|| anyhow!("match expression must have at least one case"));
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
        // List operations
        Expr::ListNew { elem_type } => Ok(Type::List(Box::new(elem_type.clone()))),
        Expr::ListPush { list, value } => {
            let list_ty = check_expr(list, env, signatures, globals, records, variants)?;
            let elem_type = match &list_ty {
                Type::List(inner) => inner.as_ref().clone(),
                _ => bail!("list-push expects a list, got {:?}", list_ty),
            };
            let value_ty = check_expr(value, env, signatures, globals, records, variants)?;
            if value_ty != elem_type {
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
    }
}

fn ensure_numeric(ty: &Type, msg: &str) -> Result<()> {
    match ty {
        Type::S32 | Type::S64 | Type::F32 | Type::F64 => Ok(()),
        Type::Record(name) => bail!("{}: expected numeric type, got record '{}'", msg, name),
        Type::Variant(name) => bail!("{}: expected numeric type, got variant '{}'", msg, name),
        Type::Option(_) => bail!("{}: expected numeric type, got option", msg),
        Type::Result(_, _) => bail!("{}: expected numeric type, got result", msg),
        Type::List(_) => bail!("{}: expected numeric type, got list", msg),
        Type::Str => bail!("{}: expected numeric type, got string", msg),
        Type::Resource(name) => bail!("{}: expected numeric type, got resource '{}'", msg, name),
        Type::Borrow(_) => bail!("{}: expected numeric type, got borrow", msg),
    }
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    let mut line = 1usize;
    let mut column = 1usize;

    while let Some(&ch) = chars.peek() {
        match ch {
            '(' => {
                tokens.push(Token {
                    kind: TokenKind::LParen,
                    span: Span::new(line, column, 1),
                });
                chars.next();
                column += 1;
            }
            ')' => {
                tokens.push(Token {
                    kind: TokenKind::RParen,
                    span: Span::new(line, column, 1),
                });
                chars.next();
                column += 1;
            }
            '`' => {
                tokens.push(Token {
                    kind: TokenKind::Quasiquote,
                    span: Span::new(line, column, 1),
                });
                chars.next();
                column += 1;
            }
            ',' => {
                let start_col = column;
                chars.next();
                column += 1;
                if chars.peek() == Some(&'@') {
                    chars.next();
                    column += 1;
                    tokens.push(Token {
                        kind: TokenKind::UnquoteSplice,
                        span: Span::new(line, start_col, 2),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Unquote,
                        span: Span::new(line, start_col, 1),
                    });
                }
            }
            '#' => {
                let start_col = column;
                chars.next();
                column += 1;
                match chars.peek() {
                    Some(&'\'') => {
                        chars.next();
                        column += 1;
                        tokens.push(Token {
                            kind: TokenKind::SyntaxQuote,
                            span: Span::new(line, start_col, 2),
                        });
                    }
                    Some(&'`') => {
                        chars.next();
                        column += 1;
                        tokens.push(Token {
                            kind: TokenKind::Quasisyntax,
                            span: Span::new(line, start_col, 2),
                        });
                    }
                    Some(&',') => {
                        chars.next();
                        column += 1;
                        if chars.peek() == Some(&'@') {
                            chars.next();
                            column += 1;
                            tokens.push(Token {
                                kind: TokenKind::UnsyntaxSplice,
                                span: Span::new(line, start_col, 3),
                            });
                        } else {
                            tokens.push(Token {
                                kind: TokenKind::Unsyntax,
                                span: Span::new(line, start_col, 2),
                            });
                        }
                    }
                    _ => {
                        // Treat # as start of a symbol (e.g., #t, #f)
                        let mut lexeme = String::from("#");
                        while let Some(&c2) = chars.peek() {
                            if c2.is_whitespace()
                                || c2 == '('
                                || c2 == ')'
                                || c2 == '`'
                                || c2 == ','
                                || c2 == ';'
                                || c2 == '\''
                            {
                                break;
                            }
                            lexeme.push(c2);
                            chars.next();
                            column += 1;
                        }
                        tokens.push(Token {
                            kind: TokenKind::Symbol(lexeme),
                            span: Span::new(line, start_col, column - start_col),
                        });
                    }
                }
            }
            ';' => {
                // Skip comments (everything until end of line)
                while let Some(&c) = chars.peek() {
                    chars.next();
                    if c == '\n' {
                        line += 1;
                        column = 1;
                        break;
                    } else {
                        column += 1;
                    }
                }
            }
            '"' => {
                // String literal
                let start_col = column;
                chars.next(); // consume opening quote
                column += 1;
                let mut content = String::new();
                while let Some(&c) = chars.peek() {
                    chars.next();
                    column += 1;
                    if c == '"' {
                        break;
                    } else if c == '\\' {
                        // Handle escape sequences
                        if let Some(&escaped) = chars.peek() {
                            chars.next();
                            column += 1;
                            match escaped {
                                'n' => content.push('\n'),
                                't' => content.push('\t'),
                                'r' => content.push('\r'),
                                '"' => content.push('"'),
                                '\\' => content.push('\\'),
                                _ => {
                                    content.push('\\');
                                    content.push(escaped);
                                }
                            }
                        }
                    } else if c == '\n' {
                        content.push(c);
                        line += 1;
                        column = 1;
                    } else {
                        content.push(c);
                    }
                }
                tokens.push(Token {
                    kind: TokenKind::String(content),
                    span: Span::new(line, start_col, column - start_col),
                });
            }
            '\n' => {
                chars.next();
                line += 1;
                column = 1;
            }
            _ => {
                if ch.is_whitespace() {
                    chars.next();
                    column += 1;
                    continue;
                }
                let start_col = column;
                let mut lexeme = String::new();
                while let Some(&c2) = chars.peek() {
                    if c2.is_whitespace()
                        || c2 == '('
                        || c2 == ')'
                        || c2 == '`'
                        || c2 == ','
                        || c2 == ';'
                    {
                        break;
                    }
                    lexeme.push(c2);
                    chars.next();
                    column += 1;
                }
                let span = Span::new(line, start_col, lexeme.len());
                if let Some(num) = parse_numeric_token(&lexeme) {
                    tokens.push(Token {
                        kind: TokenKind::Number(num),
                        span,
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Symbol(lexeme),
                        span,
                    });
                }
            }
        }
    }

    tokens
}

fn parse_numeric_token(raw: &str) -> Option<NumericToken> {
    let (base, explicit_type) = strip_numeric_suffix(raw)?;

    let is_float = base.contains('.') || matches!(explicit_type, Some(Type::F32 | Type::F64));
    if is_float {
        let value: f64 = base.parse().ok()?;
        let ty = explicit_type.unwrap_or(Type::F64);
        match ty {
            Type::F32 | Type::F64 => Some(NumericToken::Float { value, ty }),
            _ => None,
        }
    } else {
        let value: i64 = base.parse().ok()?;
        let ty = explicit_type.unwrap_or(Type::S32);
        match ty {
            Type::S32 | Type::S64 => Some(NumericToken::Int { value, ty }),
            _ => None,
        }
    }
}

fn strip_numeric_suffix(raw: &str) -> Option<(&str, Option<Type>)> {
    if raw.is_empty() {
        return None;
    }
    let suffixes = [("s64", Type::S64), ("f32", Type::F32), ("f64", Type::F64)];
    for (suffix, ty) in suffixes {
        if let Some(base) = raw.strip_suffix(suffix) {
            return Some((base, Some(ty)));
        }
    }
    Some((raw, None))
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
        if let SExpr::List(items, _) = form {
            if let Some(SExpr::Sym(sym, _)) = items.first() {
                if sym == "defmacro" && items.len() >= 4 {
                    let mac = parse_defmacro_form(items);
                    defmacros.insert(mac.name.clone(), mac);
                } else if sym == "define-syntax" && items.len() >= 3 {
                    // Check if it's syntax-rules or syntax-case-lambda
                    if let SExpr::List(body_items, _) = &items[2] {
                        if let Some(SExpr::Sym(body_sym, _)) = body_items.first() {
                            if body_sym == "syntax-rules" {
                                if let Some(mac) = parse_define_syntax_form(items) {
                                    syntax_rules.insert(mac.name.clone(), mac);
                                }
                            } else if body_sym == "syntax-case-lambda" {
                                if let Some(mac) = parse_syntax_case_form(items) {
                                    syntax_case.insert(mac.name.clone(), mac);
                                }
                            }
                        }
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
        param,
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
                        if let SExpr::List(binding, _) = &items[1] {
                            if binding.len() == 2 {
                                if let SExpr::Sym(var_name, _) = &binding[0] {
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
                if i + 1 < items.len() {
                    if let SExpr::Sym(s, _) = &items[i + 1] {
                        if s == "..." {
                            // This element is repeated
                            let inner = parse_template(&items[i], pattern_vars)?;
                            templates.push(Template::Ellipsis(Box::new(inner)));
                            i += 2; // Skip both element and ellipsis
                            continue;
                        }
                    }
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
        SExpr::UnsyntaxSplice(inner, _) => {
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
    let mut world_config: Option<WorldConfig> = None;

    // First pass: collect type names (records, variants, resources) so we can distinguish them
    for form in &forms {
        if let SExpr::List(items, span) = form {
            if items.is_empty() {
                return Err(ctx.error("empty list is not a valid top-level form", span));
            }
            match &items[0] {
                SExpr::Sym(sym, _) if sym == "record" => {
                    if items.len() >= 2 {
                        if let SExpr::Sym(name, _) = &items[1] {
                            if !record_names.insert(name.clone()) {
                                return Err(
                                    ctx.error(format!("duplicate record type '{}'", name), span)
                                );
                            }
                        }
                    }
                }
                SExpr::Sym(sym, _) if sym == "variant" => {
                    if items.len() >= 2 {
                        if let SExpr::Sym(name, _) = &items[1] {
                            if !variant_names.insert(name.clone()) {
                                return Err(
                                    ctx.error(format!("duplicate variant type '{}'", name), span)
                                );
                            }
                        }
                    }
                }
                SExpr::Sym(sym, _) if sym == "resource" => {
                    if items.len() >= 2 {
                        if let SExpr::Sym(name, _) = &items[1] {
                            if !resource_names.insert(name.clone()) {
                                return Err(
                                    ctx.error(format!("duplicate resource type '{}'", name), span)
                                );
                            }
                        }
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
                        if items.len() != 2 {
                            return Err(ctx.error("export expects exactly one argument", &span));
                        }
                        match &items[1] {
                            SExpr::Sym(name, _) => {
                                if export_set.insert(name.clone()) {
                                    exports.push(name.clone());
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
                                    exports.push(func.name.clone());
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
                        if !imported.insert(import.name.clone()) {
                            return Err(
                                ctx.error(format!("duplicate import '{}'", import.name), &span)
                            );
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
                    SExpr::Sym(sym, _) if sym == "world" => {
                        if world_config.is_some() {
                            return Err(ctx.error("only one (world ...) declaration is allowed", &span));
                        }
                        world_config = Some(parse_world_form(&items, ctx)?);
                    }
                    other => {
                        return Err(ctx.error_with_note(
                            "unknown top-level form",
                            other.span(),
                            "expected 'fn', 'export', 'import', 'global', 'record', 'variant', 'resource', or 'world'"
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
        if signatures.insert(import.name.clone(), sig).is_some() {
            return Err(ctx.error(
                format!("duplicate function '{}'", import.name),
                &import.span,
            ));
        }
    }

    for (export, export_span) in exports.iter().zip(export_set.iter()) {
        if !signatures.contains_key(export) {
            return Err(ctx.error(
                format!("cannot export undefined function '{}'", export),
                &Span::dummy(),
            ));
        }
        if imported.contains(export) {
            return Err(ctx.error(
                format!("cannot export imported function '{}'", export),
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
        world_config,
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
                                return Err(ctx.error("wit-deps path must be a string or symbol", other.span()));
                            }
                        }
                    }
                    SExpr::Sym(sym, _) if sym == "import" => {
                        if sub_items.len() != 2 {
                            return Err(ctx.error("import expects an interface reference", sub_span));
                        }
                        match &sub_items[1] {
                            SExpr::Sym(iface_ref, _) => {
                                match ExternalInterface::parse(iface_ref) {
                                    Some(ext) => external_imports.push(ext),
                                    None => return Err(ctx.error(
                                        format!("invalid interface reference '{}', expected 'pkg:ns/iface'", iface_ref),
                                        sub_span,
                                    )),
                                }
                            }
                            other => return Err(ctx.error("import expects an interface reference", other.span())),
                        }
                    }
                    SExpr::Sym(sym, _) if sym == "export" => {
                        if sub_items.len() != 2 {
                            return Err(ctx.error("export expects an interface reference", sub_span));
                        }
                        match &sub_items[1] {
                            SExpr::Sym(iface_ref, _) => {
                                match ExternalInterface::parse(iface_ref) {
                                    Some(ext) => external_exports.push(ext),
                                    None => return Err(ctx.error(
                                        format!("invalid interface reference '{}', expected 'pkg:ns/iface'", iface_ref),
                                        sub_span,
                                    )),
                                }
                            }
                            other => return Err(ctx.error("export expects an interface reference", other.span())),
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
    if items.len() != 5 {
        return Err(ctx.error_with_note(
            "invalid function definition",
            &span,
            "expected: (fn name ((param type) ...) return-type body)",
        ));
    }
    match &items[0] {
        SExpr::Sym(s, _) if s == "fn" => {}
        other => return Err(ctx.error("function definition must start with 'fn'", other.span())),
    }
    let name = match &items[1] {
        SExpr::Sym(name, _) => name.clone(),
        other => return Err(ctx.error("function name must be a symbol", other.span())),
    };
    let params = parse_typed_params(&items[2], variant_names, resource_names, ctx)?;
    let return_type = parse_type_expr(&items[3], variant_names, resource_names, ctx)?;
    Ok(PendingFunction {
        name,
        params,
        return_type,
        body: items[4].clone(),
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
    if items.len() != 5 {
        return Err(ctx.error_with_note(
            "invalid import declaration",
            &span,
            "expected: (import module name ((param type) ...) return-type)",
        ));
    }

    let module = match &items[1] {
        SExpr::Sym(s, _) => s.clone(),
        other => return Err(ctx.error("import module must be a symbol", other.span())),
    };
    let name = match &items[2] {
        SExpr::Sym(s, _) => s.clone(),
        other => return Err(ctx.error("import name must be a symbol", other.span())),
    };
    let params = parse_typed_params(&items[3], variant_names, resource_names, ctx)?;
    let return_type = parse_type_expr(&items[4], variant_names, resource_names, ctx)?;

    Ok(Import {
        module,
        name,
        params,
        return_type,
        span,
    })
}

fn parse_global_form(
    items: &[SExpr],
    variant_names: &HashSet<String>,
    resource_names: &HashSet<String>,
    ctx: &CompileContext,
) -> Result<Global> {
    let span = items[0].span().clone();
    if items.len() != 5 {
        return Err(ctx.error_with_note(
            "invalid global declaration",
            &span,
            "expected: (global $name type mutability init-value)",
        ));
    }

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

    let ty = parse_type_expr(&items[2], variant_names, resource_names, ctx)?;

    let mutable = match &items[3] {
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

    let init_value = match &items[4] {
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
                for ty_expr in &parts[1..] {
                    payload.push(parse_type_expr(
                        ty_expr,
                        variant_names,
                        resource_names,
                        ctx,
                    )?);
                }
                cases.push(VariantCase {
                    name: case_name,
                    payload,
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
                        if parts.len() != 2 {
                            return Err(ctx.error_with_note(
                                "invalid parameter",
                                param_span,
                                "expected: (name type)",
                            ));
                        }
                        let (name, scopes) = match &parts[0] {
                            SExpr::Sym(s, span) => (s.clone(), span.scopes.clone()),
                            other => {
                                return Err(
                                    ctx.error("parameter name must be a symbol", other.span())
                                );
                            }
                        };
                        let ty = parse_type_expr(&parts[1], variant_names, resource_names, ctx)?;
                        result.push(Parameter { name, ty, scopes });
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
        "string" => Ok(Type::Str),
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
    matches!(sym, "s32" | "s64" | "f32" | "f64" | "string")
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
            // Create variant name set for type parsing
            let variant_names: HashSet<String> = variants.keys().cloned().collect();
            let op = &items[0];
            match op {
                SExpr::Sym(sym, sym_span) if is_type_symbol(sym) && items.len() == 2 => {
                    let ty = match sym.as_str() {
                        "s32" => Type::S32,
                        "s64" => Type::S64,
                        "f32" => Type::F32,
                        "f64" => Type::F64,
                        _ => unreachable!(),
                    };
                    let inner = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::Ascribe {
                        expr: Box::new(inner),
                        ty,
                    })
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
                                "expected: (name value)",
                            ));
                        }
                    };
                    if binding.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid let binding",
                            items[1].span(),
                            "expected: (name value)",
                        ));
                    }
                    let (name, name_scopes) = match &binding[0] {
                        SExpr::Sym(s, span) => (s.clone(), span.scopes.clone()),
                        other => {
                            return Err(
                                ctx.error("let binding name must be a symbol", other.span())
                            );
                        }
                    };
                    let value_expr =
                        parse_expr(&binding[1], vars, functions, records, variants, ctx)?;
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
                    })
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

                        arms.push(MatchArm {
                            case_name,
                            bindings,
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
                    let ok_type =
                        parse_type_expr(&items[1], &variant_names, &HashSet::new(), ctx)?;
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
                    let ok_type =
                        parse_type_expr(&items[1], &variant_names, &HashSet::new(), ctx)?;
                    let err_type =
                        parse_type_expr(&items[2], &variant_names, &HashSet::new(), ctx)?;
                    let value = parse_expr(&items[3], vars, functions, records, variants, ctx)?;
                    Ok(Expr::Err {
                        ok_type,
                        err_type,
                        value: Box::new(value),
                    })
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

fn generate_wat(prog: &Program, signatures: &HashMap<String, Signature>) -> String {
    let mut out = String::new();
    out.push_str("(module\n");

    // Imports must come first in WAT
    for import in &prog.imports {
        out.push_str(&format!(
            "  (import \"{}\" \"{}\" (func ${} ",
            import.module, import.name, import.name
        ));
        for param in &import.params {
            out.push_str(&format!("(param ${} {}) ", param.name, wat_type(&param.ty)));
        }
        out.push_str(&format!("(result {})))\n", wat_type(&import.return_type)));
    }

    // Declare memory (500 pages = 32MB, allow growth up to 1000 pages = 64MB)
    // Larger initial size needed for programs that do heavy string/list allocation
    out.push_str("  (memory 4000 4000)\n");

    // Build global type map for codegen
    let mut globals_map = HashMap::new();
    for global in &prog.globals {
        globals_map.insert(global.name.clone(), (global.ty.clone(), global.mutable));
    }

    // Build records and variants maps for codegen
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

    // Add heap pointer global for bump allocation (if we have records, variants, or parameterized types)
    let needs_heap = !prog.records.is_empty()
        || !prog.variants.is_empty()
        || prog.functions.iter().any(|f| {
            type_needs_heap(&f.return_type)
                || f.params.iter().any(|p| type_needs_heap(&p.ty))
                || expr_uses_heap(&f.body)
        });
    if needs_heap {
        // Start heap at byte 0 (first page of memory)
        out.push_str("  (global $__heap_ptr (mut i32) (i32.const 0))\n");
    }

    // Declare user globals
    for global in &prog.globals {
        let mutability = if global.mutable { "(mut " } else { "" };
        let close = if global.mutable { ")" } else { "" };
        out.push_str(&format!(
            "  (global {} {}{}{} ({}.const {}))\n",
            global.name,
            mutability,
            wat_type(&global.ty),
            close,
            wat_type(&global.ty),
            global.init_value
        ));
    }

    // Generate functions
    for func in &prog.functions {
        let is_exported = prog.exports.contains(&func.name);
        let needs_wrapper = is_exported && function_needs_abi_wrapper(func);

        // If this function is exported and needs a wrapper, name the internal function differently
        let internal_name = if needs_wrapper {
            format!("{}__internal", func.name)
        } else {
            func.name.clone()
        };

        let mut body = String::new();
        let mut env = CodegenEnv::new(&func.params);
        gen_expr(
            &func.body,
            &mut body,
            4,
            &mut env,
            signatures,
            &globals_map,
            &records_map,
            &variants_map,
            true, // Function body is in tail position
        );

        out.push_str(&format!("  (func ${} ", internal_name));
        for param in &func.params {
            out.push_str(&format!("(param ${} {}) ", param.name, wat_type(&param.ty)));
        }
        out.push_str(&format!("(result {})\n", wat_type(&func.return_type)));
        for local in &env.locals {
            out.push_str(&format!("    (local {})\n", wat_type(local)));
        }
        out.push_str(&body);
        out.push_str("  )\n");

        // Generate ABI wrapper if needed
        if needs_wrapper {
            generate_abi_wrapper(&mut out, func, &records_map, &variants_map);
        }
    }
    // Generate cabi_realloc for component model (required for strings/lists crossing component boundary)
    // cabi_realloc(old_ptr: i32, old_size: i32, align: i32, new_size: i32) -> i32
    if needs_heap {
        out.push_str("  (func $cabi_realloc (param $old_ptr i32) (param $old_size i32) (param $align i32) (param $new_size i32) (result i32)\n");
        out.push_str("    (local $ptr i32)\n");
        // Simple bump allocation - ignore old_ptr/old_size (no reuse), just allocate new_size bytes
        // Align the heap pointer to the requested alignment
        out.push_str("    global.get $__heap_ptr\n");
        out.push_str("    local.get $align\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 1\n");
        out.push_str("    i32.sub\n");
        out.push_str("    local.get $align\n");
        out.push_str("    i32.const 1\n");
        out.push_str("    i32.sub\n");
        out.push_str("    i32.const -1\n");
        out.push_str("    i32.xor\n");
        out.push_str("    i32.and\n");
        out.push_str("    local.set $ptr\n");
        // Bump the heap pointer
        out.push_str("    local.get $ptr\n");
        out.push_str("    local.get $new_size\n");
        out.push_str("    i32.add\n");
        out.push_str("    global.set $__heap_ptr\n");
        // Return the pointer
        out.push_str("    local.get $ptr\n");
        out.push_str("  )\n");
    }

    for export in &prog.exports {
        out.push_str(&format!("  (export \"{}\" (func ${}))\n", export, export));
    }

    // Export memory for component model
    out.push_str("  (export \"memory\" (memory 0))\n");

    // Export cabi_realloc for component model
    if needs_heap {
        out.push_str("  (export \"cabi_realloc\" (func $cabi_realloc))\n");
    }

    out.push_str(")\n");
    out
}

fn gen_expr(
    expr: &Expr,
    out: &mut String,
    indent: usize,
    env: &mut CodegenEnv,
    signatures: &HashMap<String, Signature>,
    globals: &HashMap<String, (Type, bool)>,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
    is_tail: bool, // True if this expression is in tail position
) -> Type {
    let pad = " ".repeat(indent);
    match expr {
        Expr::Int { value, ty } => {
            let instr = match ty {
                Type::S32 => "i32.const",
                Type::S64 => "i64.const",
                _ => panic!("integer literal not supported for {:?}", ty),
            };
            out.push_str(&format!("{}{} {}\n", pad, instr, *value));
            ty.clone()
        }
        Expr::Float { value, ty } => {
            match ty {
                Type::F32 => out.push_str(&format!("{}f32.const {}\n", pad, *value as f32)),
                Type::F64 => out.push_str(&format!("{}f64.const {}\n", pad, *value)),
                _ => panic!("float literal not supported for {:?}", ty),
            }
            ty.clone()
        }
        Expr::StringLiteral(s) => {
            // String layout in memory: 4 bytes length + UTF-8 bytes
            let bytes = s.as_bytes();
            let len = bytes.len();
            let total_size = 4 + len; // 4 bytes for length + string data

            // Allocate space for the string
            let ptr_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr_local));
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, total_size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store length at ptr
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const {}\n", pad, len));
            out.push_str(&format!("{}i32.store\n", pad));

            // Store each byte of the string
            for (i, byte) in bytes.iter().enumerate() {
                out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
                out.push_str(&format!("{}i32.const {}\n", pad, 4 + i)); // offset past length
                out.push_str(&format!("{}i32.add\n", pad));
                out.push_str(&format!("{}i32.const {}\n", pad, *byte));
                out.push_str(&format!("{}i32.store8\n", pad));
            }

            // Return pointer to string
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            Type::Str
        }
        Expr::Ascribe { expr, ty } => {
            let from_ty = gen_expr(
                expr, out, indent, env, signatures, globals, records, variants, false,
            );
            if from_ty == *ty {
                return from_ty;
            }
            let instr = conversion_instr(&from_ty, ty)
                .unwrap_or_else(|| panic!("unsupported conversion {:?} -> {:?}", from_ty, ty));
            out.push_str(&format!("{}{}\n", pad, instr));
            ty.clone()
        }
        Expr::Var(name) => {
            let (idx, ty) = env.lookup(name);
            out.push_str(&format!("{}local.get {}\n", pad, idx));
            ty
        }
        Expr::Call { name, args } => {
            let sig = signatures
                .get(name)
                .unwrap_or_else(|| panic!("Missing signature for {}", name));
            for arg in args {
                gen_expr(
                    arg, out, indent, env, signatures, globals, records, variants, false,
                );
            }
            // Use return_call for tail position to enable tail call optimization
            if is_tail {
                out.push_str(&format!("{}return_call ${}\n", pad, name));
            } else {
                out.push_str(&format!("{}call ${}\n", pad, name));
            }
            sig.result.clone()
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_ty = gen_expr(
                cond, out, indent, env, signatures, globals, records, variants, false,
            );
            if cond_ty != Type::S32 {
                panic!("if condition must be s32");
            }
            let result_ty = expr_type(then_branch, env, signatures, globals, records, variants);
            out.push_str(&format!("{}(if (result {})\n", pad, wat_type(&result_ty)));
            out.push_str(&format!("{}  (then\n", pad));
            gen_expr(
                then_branch,
                out,
                indent + 4,
                env,
                signatures,
                globals,
                records,
                variants,
                is_tail, // Both branches inherit tail position
            );
            out.push_str(&format!("{}  )\n", pad));
            out.push_str(&format!("{}  (else\n", pad));
            let else_ty = gen_expr(
                else_branch,
                out,
                indent + 4,
                env,
                signatures,
                globals,
                records,
                variants,
                is_tail, // Both branches inherit tail position
            );
            if else_ty != result_ty {
                panic!(
                    "if branches must match types: {:?} vs {:?}",
                    result_ty, else_ty
                );
            }
            out.push_str(&format!("{}  )\n", pad));
            out.push_str(&format!("{})\n", pad));
            result_ty
        }
        Expr::Let { name, value, body } => {
            let value_ty = gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            let idx = env.declare_local(value_ty);
            out.push_str(&format!("{}local.set {}\n", pad, idx));
            env.push_binding(name.clone(), idx);
            let body_ty = gen_expr(
                body, out, indent, env, signatures, globals, records, variants, is_tail,
            );
            env.pop_binding();
            body_ty
        }
        Expr::WasmInstr { name, args } => {
            let instr_info = lookup_wasm_instr(name)
                .unwrap_or_else(|| panic!("Missing WASM instruction info for {}", name));

            // Special handling for const instructions - they take immediates, not stack values
            if name.ends_with(".const") {
                if args.len() != 1 {
                    panic!("{} expects exactly 1 argument", name);
                }
                match &args[0] {
                    Expr::Int { value, .. } => {
                        out.push_str(&format!("{}{} {}\n", pad, name, value));
                    }
                    Expr::Float { value, .. } => {
                        out.push_str(&format!("{}{} {}\n", pad, name, value));
                    }
                    _ => panic!("{} requires a literal value", name),
                }
            } else if name.ends_with(".store")
                || name == "i32.store8"
                || name == "i32.store16"
                || name == "i64.store8"
                || name == "i64.store16"
                || name == "i64.store32"
            {
                // Store instructions: emit address, then value, then store
                // Note: In WASM stores don't return values, but we make them return the stored value
                // We save the value in a local, emit the store, then restore it
                if args.len() != 2 {
                    panic!("{} expects exactly 2 arguments (address, value)", name);
                }

                // Emit and save the value first
                let value_ty = gen_expr(
                    &args[1], out, indent, env, signatures, globals, records, variants, false,
                );
                let value_local = env.declare_local(value_ty);
                out.push_str(&format!("{}local.set {}\n", pad, value_local));

                // Emit the address
                gen_expr(
                    &args[0], out, indent, env, signatures, globals, records, variants, false,
                );

                // Get the value back
                out.push_str(&format!("{}local.get {}\n", pad, value_local));

                // Emit the store
                out.push_str(&format!("{}{}\n", pad, name));

                // Put the value back on the stack as the "return value"
                out.push_str(&format!("{}local.get {}\n", pad, value_local));
            } else {
                // Normal instructions - emit args then instruction
                for arg in args {
                    gen_expr(
                        arg, out, indent, env, signatures, globals, records, variants, false,
                    );
                }
                out.push_str(&format!("{}{}\n", pad, name));
            }
            instr_info.result
        }
        Expr::GlobalGet { name } => {
            out.push_str(&format!("{}global.get {}\n", pad, name));
            let (ty, _) = globals.get(name).expect("global should exist");
            ty.clone()
        }
        Expr::GlobalSet { name, value } => {
            // Global.set consumes the value, so we save it to a local first
            // and restore it after to return the value for composability
            let value_ty = gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            let value_local = env.declare_local(value_ty.clone());
            out.push_str(&format!("{}local.set {}\n", pad, value_local));
            out.push_str(&format!("{}local.get {}\n", pad, value_local));
            out.push_str(&format!("{}global.set {}\n", pad, name));
            out.push_str(&format!("{}local.get {}\n", pad, value_local));
            value_ty
        }
        Expr::RecordConstruct {
            record_name,
            fields,
        } => {
            let record_def = records.get(record_name).expect("record should exist");
            let size = record_def.size();

            // Bump allocate: get current heap_ptr, advance it by record size
            // Save the base pointer to a local
            let ptr_local = env.declare_local(Type::S32);

            // ptr = heap_ptr
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr_local));

            // heap_ptr += size
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store each field at the appropriate offset
            for (i, (field_expr, field_def)) in
                fields.iter().zip(record_def.fields.iter()).enumerate()
            {
                let offset = record_def.field_offset(i);

                // Compute address: ptr + offset
                out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
                if offset > 0 {
                    out.push_str(&format!("{}i32.const {}\n", pad, offset));
                    out.push_str(&format!("{}i32.add\n", pad));
                }

                // Evaluate the field expression
                gen_expr(
                    field_expr, out, indent, env, signatures, globals, records, variants, false,
                );

                // Store based on field type
                let store_instr = match &field_def.ty {
                    Type::S32 => "i32.store",
                    Type::S64 => "i64.store",
                    Type::F32 => "f32.store",
                    Type::F64 => "f64.store",
                    // All compound types are pointers, resources are i32 handles
                    Type::Record(_)
                    | Type::Variant(_)
                    | Type::Option(_)
                    | Type::Result(_, _)
                    | Type::List(_)
                    | Type::Str
                    | Type::Resource(_)
                    | Type::Borrow(_) => "i32.store",
                };
                out.push_str(&format!("{}{}\n", pad, store_instr));
            }

            // Return the pointer to the record
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            Type::Record(record_name.clone())
        }
        Expr::RecordAccess {
            record_name,
            field_name,
            expr,
        } => {
            let record_def = records.get(record_name).expect("record should exist");

            // Find the field and its offset
            let (field_idx, field_def) = record_def
                .fields
                .iter()
                .enumerate()
                .find(|(_, f)| f.name == *field_name)
                .expect("field should exist");
            let offset = record_def.field_offset(field_idx);

            // Evaluate the record expression (gives us the pointer)
            gen_expr(
                expr, out, indent, env, signatures, globals, records, variants, false,
            );

            // Add offset if non-zero
            if offset > 0 {
                out.push_str(&format!("{}i32.const {}\n", pad, offset));
                out.push_str(&format!("{}i32.add\n", pad));
            }

            // Load based on field type
            let load_instr = match &field_def.ty {
                Type::S32 => "i32.load",
                Type::S64 => "i64.load",
                Type::F32 => "f32.load",
                Type::F64 => "f64.load",
                // All compound types are pointers, resources are i32 handles
                Type::Record(_)
                | Type::Variant(_)
                | Type::Option(_)
                | Type::Result(_, _)
                | Type::List(_)
                | Type::Str
                | Type::Resource(_)
                | Type::Borrow(_) => "i32.load",
            };
            out.push_str(&format!("{}{}\n", pad, load_instr));
            field_def.ty.clone()
        }
        Expr::VariantConstruct {
            variant_name,
            case_name,
            payload,
        } => {
            let variant_def = variants.get(variant_name).expect("variant should exist");
            let (case_idx, case) = variant_def.find_case(case_name).expect("case should exist");
            let size = variant_def.size();

            // Bump allocate: get current heap_ptr, advance it by variant size
            let ptr_local = env.declare_local(Type::S32);

            // ptr = heap_ptr
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr_local));

            // heap_ptr += size
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store discriminant (case index) at offset 0
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const {}\n", pad, case_idx));
            out.push_str(&format!("{}i32.store\n", pad));

            // Store payload values starting at offset 4 (after discriminant)
            let mut payload_offset = 4;
            for (payload_expr, payload_ty) in payload.iter().zip(case.payload.iter()) {
                // Compute address: ptr + payload_offset
                out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
                out.push_str(&format!("{}i32.const {}\n", pad, payload_offset));
                out.push_str(&format!("{}i32.add\n", pad));

                // Evaluate the payload expression
                gen_expr(
                    payload_expr,
                    out,
                    indent,
                    env,
                    signatures,
                    globals,
                    records,
                    variants,
                    false,
                );

                // Store based on payload type
                let store_instr = match payload_ty {
                    Type::S32 => "i32.store",
                    Type::S64 => "i64.store",
                    Type::F32 => "f32.store",
                    Type::F64 => "f64.store",
                    Type::Record(_)
                    | Type::Variant(_)
                    | Type::Option(_)
                    | Type::Result(_, _)
                    | Type::List(_)
                    | Type::Str
                    | Type::Resource(_)
                    | Type::Borrow(_) => "i32.store",
                };
                out.push_str(&format!("{}{}\n", pad, store_instr));
                payload_offset += type_size(payload_ty);
            }

            // Return the pointer to the variant
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            Type::Variant(variant_name.clone())
        }
        Expr::Match { expr, cases } => {
            // Evaluate the expression to get the pointer
            let expr_ty = gen_expr(
                expr, out, indent, env, signatures, globals, records, variants, false,
            );

            // Save the pointer to a local
            let value_ptr = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.set {}\n", pad, value_ptr));

            // Handle Option, Result, and Variant types
            match &expr_ty {
                Type::Option(inner_ty) => {
                    // Option: discriminant 0 = none, 1 = some
                    // Load discriminant
                    out.push_str(&format!("{}local.get {}\n", pad, value_ptr));
                    out.push_str(&format!("{}i32.load\n", pad));

                    // Determine result type (simplified)
                    let result_ty = Type::S32; // Will be overridden by actual arm body type

                    let num_cases = cases.len();
                    for (i, arm) in cases.iter().enumerate() {
                        let case_idx = match arm.case_name.as_str() {
                            "none" => 0,
                            "some" => 1,
                            _ => panic!("invalid option case"),
                        };

                        // Compare discriminant with case index
                        if i > 0 {
                            out.push_str(&format!("{}local.get {}\n", pad, value_ptr));
                            out.push_str(&format!("{}i32.load\n", pad));
                        }
                        out.push_str(&format!("{}i32.const {}\n", pad, case_idx));
                        out.push_str(&format!("{}i32.eq\n", pad));

                        let is_last = i == num_cases - 1;
                        if !is_last || (is_last && num_cases > 1) {
                            out.push_str(&format!(
                                "{}(if (result {})\n",
                                pad,
                                wat_type(&result_ty)
                            ));
                            out.push_str(&format!("{}  (then\n", pad));
                        }

                        // Load payload for 'some' case
                        let saved_binding_count = env.bindings.len();
                        if arm.case_name == "some" && !arm.bindings.is_empty() {
                            out.push_str(&format!("{}    local.get {}\n", pad, value_ptr));
                            out.push_str(&format!("{}    i32.const 4\n", pad));
                            out.push_str(&format!("{}    i32.add\n", pad));

                            let load_instr = match **inner_ty {
                                Type::S32 => "i32.load",
                                Type::S64 => "i64.load",
                                Type::F32 => "f32.load",
                                Type::F64 => "f64.load",
                                Type::Record(_)
                                | Type::Variant(_)
                                | Type::Option(_)
                                | Type::Result(_, _)
                                | Type::List(_)
                                | Type::Str
                                | Type::Resource(_)
                                | Type::Borrow(_) => "i32.load",
                            };
                            out.push_str(&format!("{}    {}\n", pad, load_instr));

                            let idx = env.declare_local((**inner_ty).clone());
                            out.push_str(&format!("{}    local.set {}\n", pad, idx));
                            env.push_binding(arm.bindings[0].clone(), idx);
                        }

                        // Generate arm body - inherits tail position
                        gen_expr(
                            &arm.body,
                            out,
                            indent + 4,
                            env,
                            signatures,
                            globals,
                            records,
                            variants,
                            is_tail,
                        );

                        // Pop bindings
                        while env.bindings.len() > saved_binding_count {
                            env.pop_binding();
                        }

                        if !is_last {
                            out.push_str(&format!("{}  )\n", pad));
                            out.push_str(&format!("{}  (else\n", pad));
                        } else if num_cases > 1 {
                            out.push_str(&format!("{}  )\n", pad));
                            out.push_str(&format!("{}  (else\n", pad));
                            out.push_str(&format!("{}    unreachable\n", pad));
                            out.push_str(&format!("{}  )\n", pad));
                            out.push_str(&format!("{})\n", pad));
                        }
                    }

                    // Close all if-else blocks
                    for _ in 0..num_cases.saturating_sub(1) {
                        out.push_str(&format!("{}  )\n", pad));
                        out.push_str(&format!("{})\n", pad));
                    }

                    result_ty
                }
                Type::Result(ok_ty, err_ty) => {
                    // Result: discriminant 0 = ok, 1 = err
                    // Load discriminant
                    out.push_str(&format!("{}local.get {}\n", pad, value_ptr));
                    out.push_str(&format!("{}i32.load\n", pad));

                    // Determine result type (simplified)
                    let result_ty = Type::S32;

                    let num_cases = cases.len();
                    for (i, arm) in cases.iter().enumerate() {
                        let (case_idx, payload_ty) = match arm.case_name.as_str() {
                            "ok" => (0, (**ok_ty).clone()),
                            "err" => (1, (**err_ty).clone()),
                            _ => panic!("invalid result case"),
                        };

                        // Compare discriminant with case index
                        if i > 0 {
                            out.push_str(&format!("{}local.get {}\n", pad, value_ptr));
                            out.push_str(&format!("{}i32.load\n", pad));
                        }
                        out.push_str(&format!("{}i32.const {}\n", pad, case_idx));
                        out.push_str(&format!("{}i32.eq\n", pad));

                        let is_last = i == num_cases - 1;
                        if !is_last || (is_last && num_cases > 1) {
                            out.push_str(&format!(
                                "{}(if (result {})\n",
                                pad,
                                wat_type(&result_ty)
                            ));
                            out.push_str(&format!("{}  (then\n", pad));
                        }

                        // Load payload
                        let saved_binding_count = env.bindings.len();
                        if !arm.bindings.is_empty() {
                            out.push_str(&format!("{}    local.get {}\n", pad, value_ptr));
                            out.push_str(&format!("{}    i32.const 4\n", pad));
                            out.push_str(&format!("{}    i32.add\n", pad));

                            let load_instr = match payload_ty {
                                Type::S32 => "i32.load",
                                Type::S64 => "i64.load",
                                Type::F32 => "f32.load",
                                Type::F64 => "f64.load",
                                Type::Record(_)
                                | Type::Variant(_)
                                | Type::Option(_)
                                | Type::Result(_, _)
                                | Type::List(_)
                                | Type::Str
                                | Type::Resource(_)
                                | Type::Borrow(_) => "i32.load",
                            };
                            out.push_str(&format!("{}    {}\n", pad, load_instr));

                            let idx = env.declare_local(payload_ty.clone());
                            out.push_str(&format!("{}    local.set {}\n", pad, idx));
                            env.push_binding(arm.bindings[0].clone(), idx);
                        }

                        // Generate arm body - inherits tail position
                        gen_expr(
                            &arm.body,
                            out,
                            indent + 4,
                            env,
                            signatures,
                            globals,
                            records,
                            variants,
                            is_tail,
                        );

                        // Pop bindings
                        while env.bindings.len() > saved_binding_count {
                            env.pop_binding();
                        }

                        if !is_last {
                            out.push_str(&format!("{}  )\n", pad));
                            out.push_str(&format!("{}  (else\n", pad));
                        } else if num_cases > 1 {
                            out.push_str(&format!("{}  )\n", pad));
                            out.push_str(&format!("{}  (else\n", pad));
                            out.push_str(&format!("{}    unreachable\n", pad));
                            out.push_str(&format!("{}  )\n", pad));
                            out.push_str(&format!("{})\n", pad));
                        }
                    }

                    // Close all if-else blocks
                    for _ in 0..num_cases.saturating_sub(1) {
                        out.push_str(&format!("{}  )\n", pad));
                        out.push_str(&format!("{})\n", pad));
                    }

                    result_ty
                }
                Type::Variant(variant_name) => {
                    let variant_def = variants.get(variant_name).expect("variant should exist");

                    // Load discriminant
                    out.push_str(&format!("{}local.get {}\n", pad, value_ptr));
                    out.push_str(&format!("{}i32.load\n", pad));

                    // Determine result type from first arm
                    let result_ty = if let Some(first_arm) = cases.first() {
                        // Build environment for first arm to get its type
                        let (_, first_case) = variant_def
                            .find_case(&first_arm.case_name)
                            .expect("case should exist");
                        let mut arm_env = HashMap::new();
                        for (binding, ty) in
                            first_arm.bindings.iter().zip(first_case.payload.iter())
                        {
                            arm_env.insert(binding.clone(), ty.clone());
                        }
                        // Get type from body - simplified, assumes type checking passed
                        match &first_arm.body {
                            Expr::Int { ty, .. } => ty.clone(),
                            Expr::Float { ty, .. } => ty.clone(),
                            Expr::Var(name) => arm_env.get(name).cloned().unwrap_or(Type::S32),
                            _ => Type::S32, // Default fallback
                        }
                    } else {
                        Type::S32
                    };

                    // For simplicity, use nested if-else for now
                    let num_cases = cases.len();
                    for (i, arm) in cases.iter().enumerate() {
                        let (case_idx, case) = variant_def
                            .find_case(&arm.case_name)
                            .expect("case should exist");

                        // Compare discriminant with case index
                        if i > 0 {
                            out.push_str(&format!("{}local.get {}\n", pad, value_ptr));
                            out.push_str(&format!("{}i32.load\n", pad));
                        }
                        out.push_str(&format!("{}i32.const {}\n", pad, case_idx));
                        out.push_str(&format!("{}i32.eq\n", pad));

                        let is_last = i == num_cases - 1;
                        if is_last && num_cases > 1 {
                            out.push_str(&format!(
                                "{}(if (result {})\n",
                                pad,
                                wat_type(&result_ty)
                            ));
                            out.push_str(&format!("{}  (then\n", pad));
                        } else if !is_last {
                            out.push_str(&format!(
                                "{}(if (result {})\n",
                                pad,
                                wat_type(&result_ty)
                            ));
                            out.push_str(&format!("{}  (then\n", pad));
                        }

                        // Load payload values into locals and bind them
                        let saved_binding_count = env.bindings.len();
                        let mut payload_offset = 4;
                        for (binding, payload_ty) in arm.bindings.iter().zip(case.payload.iter()) {
                            // Load payload value
                            out.push_str(&format!("{}    local.get {}\n", pad, value_ptr));
                            out.push_str(&format!("{}    i32.const {}\n", pad, payload_offset));
                            out.push_str(&format!("{}    i32.add\n", pad));

                            let load_instr = match payload_ty {
                                Type::S32 => "i32.load",
                                Type::S64 => "i64.load",
                                Type::F32 => "f32.load",
                                Type::F64 => "f64.load",
                                Type::Record(_)
                                | Type::Variant(_)
                                | Type::Option(_)
                                | Type::Result(_, _)
                                | Type::List(_)
                                | Type::Str
                                | Type::Resource(_)
                                | Type::Borrow(_) => "i32.load",
                            };
                            out.push_str(&format!("{}    {}\n", pad, load_instr));

                            // Save to local
                            let idx = env.declare_local(payload_ty.clone());
                            out.push_str(&format!("{}    local.set {}\n", pad, idx));
                            env.push_binding(binding.clone(), idx);

                            payload_offset += type_size(payload_ty);
                        }

                        // Generate arm body - inherits tail position
                        gen_expr(
                            &arm.body,
                            out,
                            indent + 4,
                            env,
                            signatures,
                            globals,
                            records,
                            variants,
                            is_tail,
                        );

                        // Pop bindings
                        while env.bindings.len() > saved_binding_count {
                            env.pop_binding();
                        }

                        if !is_last {
                            out.push_str(&format!("{}  )\n", pad));
                            out.push_str(&format!("{}  (else\n", pad));
                        } else if num_cases > 1 {
                            out.push_str(&format!("{}  )\n", pad));
                            out.push_str(&format!("{}  (else\n", pad));
                            out.push_str(&format!("{}    unreachable\n", pad));
                            out.push_str(&format!("{}  )\n", pad));
                            out.push_str(&format!("{})\n", pad));
                        }
                    }

                    // Close all the if-else blocks
                    for _ in 0..num_cases.saturating_sub(1) {
                        out.push_str(&format!("{}  )\n", pad));
                        out.push_str(&format!("{})\n", pad));
                    }

                    result_ty
                }
                _ => panic!("match expression must be variant, option, or result"),
            }
        }
        // Option: some - allocate, store discriminant 1, store value
        Expr::Some { inner_type, value } => {
            let size = 4 + type_size(inner_type); // discriminant + payload
            let ptr_local = env.declare_local(Type::S32);

            // ptr = heap_ptr
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr_local));

            // heap_ptr += size
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store discriminant = 1 (some)
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 1\n", pad));
            out.push_str(&format!("{}i32.store\n", pad));

            // Store value at offset 4
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            let store_instr = match inner_type {
                Type::S32 => "i32.store",
                Type::S64 => "i64.store",
                Type::F32 => "f32.store",
                Type::F64 => "f64.store",
                _ => "i32.store",
            };
            out.push_str(&format!("{}{}\n", pad, store_instr));

            // Return pointer
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            Type::Option(Box::new(inner_type.clone()))
        }
        // Option: none - allocate, store discriminant 0
        Expr::None { inner_type } => {
            let size = 4 + type_size(inner_type); // discriminant + payload space
            let ptr_local = env.declare_local(Type::S32);

            // ptr = heap_ptr
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr_local));

            // heap_ptr += size
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store discriminant = 0 (none)
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 0\n", pad));
            out.push_str(&format!("{}i32.store\n", pad));

            // Return pointer
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            Type::Option(Box::new(inner_type.clone()))
        }
        // Result: ok - allocate, store discriminant 0, store value
        Expr::Ok {
            ok_type,
            err_type,
            value,
        } => {
            let max_payload = std::cmp::max(type_size(ok_type), type_size(err_type));
            let size = 4 + max_payload; // discriminant + max payload
            let ptr_local = env.declare_local(Type::S32);

            // ptr = heap_ptr
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr_local));

            // heap_ptr += size
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store discriminant = 0 (ok)
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 0\n", pad));
            out.push_str(&format!("{}i32.store\n", pad));

            // Store ok value at offset 4
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            let store_instr = match ok_type {
                Type::S32 => "i32.store",
                Type::S64 => "i64.store",
                Type::F32 => "f32.store",
                Type::F64 => "f64.store",
                _ => "i32.store",
            };
            out.push_str(&format!("{}{}\n", pad, store_instr));

            // Return pointer
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            Type::Result(Box::new(ok_type.clone()), Box::new(err_type.clone()))
        }
        // Result: err - allocate, store discriminant 1, store value
        Expr::Err {
            ok_type,
            err_type,
            value,
        } => {
            let max_payload = std::cmp::max(type_size(ok_type), type_size(err_type));
            let size = 4 + max_payload; // discriminant + max payload
            let ptr_local = env.declare_local(Type::S32);

            // ptr = heap_ptr
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr_local));

            // heap_ptr += size
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store discriminant = 1 (err)
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 1\n", pad));
            out.push_str(&format!("{}i32.store\n", pad));

            // Store err value at offset 4
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            let store_instr = match err_type {
                Type::S32 => "i32.store",
                Type::S64 => "i64.store",
                Type::F32 => "f32.store",
                Type::F64 => "f64.store",
                _ => "i32.store",
            };
            out.push_str(&format!("{}{}\n", pad, store_instr));

            // Return pointer
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            Type::Result(Box::new(ok_type.clone()), Box::new(err_type.clone()))
        }
        // List: new - allocate header (len=0, cap=0, data=null)
        Expr::ListNew { elem_type } => {
            let header_size = 12; // 4 bytes len + 4 bytes cap + 4 bytes data ptr
            let ptr_local = env.declare_local(Type::S32);

            // ptr = heap_ptr
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, ptr_local));

            // heap_ptr += header_size
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, header_size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store len = 0
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 0\n", pad));
            out.push_str(&format!("{}i32.store\n", pad));

            // Store cap = 0
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.const 0\n", pad));
            out.push_str(&format!("{}i32.store\n", pad));

            // Store data = 0 (null)
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            out.push_str(&format!("{}i32.const 8\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.const 0\n", pad));
            out.push_str(&format!("{}i32.store\n", pad));

            // Return pointer
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            Type::List(Box::new(elem_type.clone()))
        }
        // List: push - simplified version that reallocates every time
        Expr::ListPush { list, value } => {
            let list_ty = gen_expr(
                list, out, indent, env, signatures, globals, records, variants, false,
            );
            let elem_type = match &list_ty {
                Type::List(inner) => inner.as_ref().clone(),
                _ => panic!("list-push expects a list"),
            };
            let elem_size = type_size(&elem_type);
            let list_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.set {}\n", pad, list_local));

            // Get current len
            let len_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, list_local));
            out.push_str(&format!("{}i32.load\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, len_local));

            // Allocate new data array (simple approach: always reallocate)
            let new_data_local = env.declare_local(Type::S32);
            let new_size = env.declare_local(Type::S32);

            // new_size = (len + 1) * elem_size
            out.push_str(&format!("{}local.get {}\n", pad, len_local));
            out.push_str(&format!("{}i32.const 1\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.const {}\n", pad, elem_size));
            out.push_str(&format!("{}i32.mul\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, new_size));

            // new_data = heap_ptr
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, new_data_local));

            // heap_ptr += new_size
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, new_size));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Copy old data if len > 0
            // Get old data pointer from list+8
            let old_data_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, list_local));
            out.push_str(&format!("{}i32.const 8\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.load\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, old_data_local));

            // Copy len * elem_size bytes from old_data to new_data
            // memory.copy(dst, src, len)
            out.push_str(&format!("{}local.get {}\n", pad, new_data_local)); // dst
            out.push_str(&format!("{}local.get {}\n", pad, old_data_local)); // src
            out.push_str(&format!("{}local.get {}\n", pad, len_local)); // len (in elements)
            out.push_str(&format!("{}i32.const {}\n", pad, elem_size));
            out.push_str(&format!("{}i32.mul\n", pad)); // len * elem_size
            out.push_str(&format!("{}memory.copy\n", pad));

            // Store new value at new_data + len * elem_size
            out.push_str(&format!("{}local.get {}\n", pad, new_data_local));
            out.push_str(&format!("{}local.get {}\n", pad, len_local));
            out.push_str(&format!("{}i32.const {}\n", pad, elem_size));
            out.push_str(&format!("{}i32.mul\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            gen_expr(
                value, out, indent, env, signatures, globals, records, variants, false,
            );
            let store_instr = match &elem_type {
                Type::S32 => "i32.store",
                Type::S64 => "i64.store",
                Type::F32 => "f32.store",
                Type::F64 => "f64.store",
                _ => "i32.store",
            };
            out.push_str(&format!("{}{}\n", pad, store_instr));

            // Update list header: len = len + 1
            out.push_str(&format!("{}local.get {}\n", pad, list_local));
            out.push_str(&format!("{}local.get {}\n", pad, len_local));
            out.push_str(&format!("{}i32.const 1\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.store\n", pad));

            // Update list header: data = new_data
            out.push_str(&format!("{}local.get {}\n", pad, list_local));
            out.push_str(&format!("{}i32.const 8\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, new_data_local));
            out.push_str(&format!("{}i32.store\n", pad));

            // Return the list pointer
            out.push_str(&format!("{}local.get {}\n", pad, list_local));
            list_ty
        }
        // List: get - load element at index
        Expr::ListGet { list, index } => {
            let list_ty = gen_expr(
                list, out, indent, env, signatures, globals, records, variants, false,
            );
            let elem_type = match &list_ty {
                Type::List(inner) => inner.as_ref().clone(),
                _ => panic!("list-get expects a list"),
            };
            let elem_size = type_size(&elem_type);
            let list_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.set {}\n", pad, list_local));

            // Evaluate index
            gen_expr(
                index, out, indent, env, signatures, globals, records, variants, false,
            );
            let index_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.set {}\n", pad, index_local));

            // Load data pointer
            out.push_str(&format!("{}local.get {}\n", pad, list_local));
            out.push_str(&format!("{}i32.const 8\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.load\n", pad));

            // Add index * elem_size
            out.push_str(&format!("{}local.get {}\n", pad, index_local));
            out.push_str(&format!("{}i32.const {}\n", pad, elem_size));
            out.push_str(&format!("{}i32.mul\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));

            // Load element
            let load_instr = match &elem_type {
                Type::S32 => "i32.load",
                Type::S64 => "i64.load",
                Type::F32 => "f32.load",
                Type::F64 => "f64.load",
                _ => "i32.load",
            };
            out.push_str(&format!("{}{}\n", pad, load_instr));
            elem_type
        }
        // List: len - return length
        Expr::ListLen { list } => {
            gen_expr(
                list, out, indent, env, signatures, globals, records, variants, false,
            );
            // Load len field at offset 0
            out.push_str(&format!("{}i32.load\n", pad));
            Type::S32
        }
        // String: len - return length
        Expr::StringLen { string } => {
            gen_expr(
                string, out, indent, env, signatures, globals, records, variants, false,
            );
            // Load len field at offset 0 (string layout: 4 bytes len + data)
            out.push_str(&format!("{}i32.load\n", pad));
            Type::S32
        }
        // String: ref - get byte at index
        Expr::StringRef { string, index } => {
            // String layout: 4 bytes len + data bytes
            // Result: byte at (string_ptr + 4 + index)
            gen_expr(
                string, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            gen_expr(
                index, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.load8_u\n", pad));
            Type::S32
        }
        // String: substring - extract portion of string
        Expr::Substring { string, start, end } => {
            // Evaluate string pointer
            let str_local = env.declare_local(Type::S32);
            gen_expr(
                string, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, str_local));

            // Evaluate start index
            let start_local = env.declare_local(Type::S32);
            gen_expr(
                start, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, start_local));

            // Evaluate end index
            let end_local = env.declare_local(Type::S32);
            gen_expr(
                end, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, end_local));

            // Calculate new length: end - start
            let new_len_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, end_local));
            out.push_str(&format!("{}local.get {}\n", pad, start_local));
            out.push_str(&format!("{}i32.sub\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, new_len_local));

            // Allocate space for new string: 4 bytes len + new_len bytes
            let new_ptr_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, new_ptr_local));
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, new_len_local));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store new length at new_ptr
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            out.push_str(&format!("{}local.get {}\n", pad, new_len_local));
            out.push_str(&format!("{}i32.store\n", pad));

            // Copy bytes using memory.copy
            // dst: new_ptr + 4
            // src: str_local + 4 + start
            // len: new_len
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, str_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, start_local));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, new_len_local));
            out.push_str(&format!("{}memory.copy\n", pad));

            // Return new string pointer
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            Type::Str
        }
        // String: append - concatenate two strings
        Expr::StringAppend { left, right } => {
            // Evaluate left string pointer
            let left_local = env.declare_local(Type::S32);
            gen_expr(
                left, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, left_local));

            // Evaluate right string pointer
            let right_local = env.declare_local(Type::S32);
            gen_expr(
                right, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, right_local));

            // Get left length
            let left_len_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, left_local));
            out.push_str(&format!("{}i32.load\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, left_len_local));

            // Get right length
            let right_len_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, right_local));
            out.push_str(&format!("{}i32.load\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, right_len_local));

            // Calculate total length
            let total_len_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, left_len_local));
            out.push_str(&format!("{}local.get {}\n", pad, right_len_local));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, total_len_local));

            // Allocate space for new string: 4 bytes len + total_len bytes
            let new_ptr_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, new_ptr_local));
            out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, total_len_local));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

            // Store total length at new_ptr
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            out.push_str(&format!("{}local.get {}\n", pad, total_len_local));
            out.push_str(&format!("{}i32.store\n", pad));

            // Copy left string bytes
            // dst: new_ptr + 4
            // src: left_local + 4
            // len: left_len
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, left_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, left_len_local));
            out.push_str(&format!("{}memory.copy\n", pad));

            // Copy right string bytes
            // dst: new_ptr + 4 + left_len
            // src: right_local + 4
            // len: right_len
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, left_len_local));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, right_local));
            out.push_str(&format!("{}i32.const 4\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            out.push_str(&format!("{}local.get {}\n", pad, right_len_local));
            out.push_str(&format!("{}memory.copy\n", pad));

            // Return new string pointer
            out.push_str(&format!("{}local.get {}\n", pad, new_ptr_local));
            Type::Str
        }
        // String: equality - compare two strings
        Expr::StringEq { left, right } => {
            // Evaluate left string pointer
            let left_local = env.declare_local(Type::S32);
            gen_expr(
                left, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, left_local));

            // Evaluate right string pointer
            let right_local = env.declare_local(Type::S32);
            gen_expr(
                right, out, indent, env, signatures, globals, records, variants, false,
            );
            out.push_str(&format!("{}local.set {}\n", pad, right_local));

            // Get left length
            let left_len_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, left_local));
            out.push_str(&format!("{}i32.load\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, left_len_local));

            // Get right length
            let right_len_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.get {}\n", pad, right_local));
            out.push_str(&format!("{}i32.load\n", pad));
            out.push_str(&format!("{}local.set {}\n", pad, right_len_local));

            // First check if lengths are equal
            // If lengths differ, strings can't be equal
            // Use block/br structure for early return on length mismatch
            out.push_str(&format!("{}block (result i32) ;; string-eq outer\n", pad));
            out.push_str(&format!("{}  local.get {}\n", pad, left_len_local));
            out.push_str(&format!("{}  local.get {}\n", pad, right_len_local));
            out.push_str(&format!("{}  i32.ne\n", pad));
            out.push_str(&format!("{}  if (result i32)\n", pad));
            out.push_str(&format!("{}    i32.const 0\n", pad)); // lengths differ, not equal
            out.push_str(&format!("{}  else\n", pad));
            // Lengths match, compare byte by byte using loop
            let idx_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}    i32.const 0\n", pad));
            out.push_str(&format!("{}    local.set {}\n", pad, idx_local));
            out.push_str(&format!("{}    block (result i32) ;; comparison result\n", pad));
            out.push_str(&format!("{}      loop ;; compare loop\n", pad));
            // Check if idx >= len (done comparing)
            out.push_str(&format!("{}        local.get {}\n", pad, idx_local));
            out.push_str(&format!("{}        local.get {}\n", pad, left_len_local));
            out.push_str(&format!("{}        i32.ge_u\n", pad));
            out.push_str(&format!("{}        if\n", pad));
            out.push_str(&format!("{}          i32.const 1\n", pad)); // all bytes match
            out.push_str(&format!("{}          br 2 ;; exit with 1\n", pad));
            out.push_str(&format!("{}        end\n", pad));
            // Compare bytes at idx
            out.push_str(&format!("{}        local.get {}\n", pad, left_local));
            out.push_str(&format!("{}        i32.const 4\n", pad));
            out.push_str(&format!("{}        i32.add\n", pad));
            out.push_str(&format!("{}        local.get {}\n", pad, idx_local));
            out.push_str(&format!("{}        i32.add\n", pad));
            out.push_str(&format!("{}        i32.load8_u\n", pad));
            out.push_str(&format!("{}        local.get {}\n", pad, right_local));
            out.push_str(&format!("{}        i32.const 4\n", pad));
            out.push_str(&format!("{}        i32.add\n", pad));
            out.push_str(&format!("{}        local.get {}\n", pad, idx_local));
            out.push_str(&format!("{}        i32.add\n", pad));
            out.push_str(&format!("{}        i32.load8_u\n", pad));
            out.push_str(&format!("{}        i32.ne\n", pad));
            out.push_str(&format!("{}        if\n", pad));
            out.push_str(&format!("{}          i32.const 0\n", pad)); // bytes differ
            out.push_str(&format!("{}          br 3 ;; exit with 0\n", pad));
            out.push_str(&format!("{}        end\n", pad));
            // Increment idx
            out.push_str(&format!("{}        local.get {}\n", pad, idx_local));
            out.push_str(&format!("{}        i32.const 1\n", pad));
            out.push_str(&format!("{}        i32.add\n", pad));
            out.push_str(&format!("{}        local.set {}\n", pad, idx_local));
            out.push_str(&format!("{}        br 0 ;; continue loop\n", pad));
            out.push_str(&format!("{}      end ;; loop\n", pad));
            out.push_str(&format!("{}      i32.const 1 ;; fallback (empty strings)\n", pad));
            out.push_str(&format!("{}    end ;; comparison result block\n", pad));
            out.push_str(&format!("{}  end ;; if\n", pad));
            out.push_str(&format!("{}end ;; string-eq outer\n", pad));
            Type::S32
        }
    }
}

struct CodegenEnv {
    bindings: Vec<(String, u32)>,
    param_count: u32,
    locals: Vec<Type>,
    param_types: Vec<Type>,
}

impl CodegenEnv {
    fn new(params: &[Parameter]) -> Self {
        let mut bindings = Vec::new();
        for (idx, name) in params.iter().enumerate() {
            bindings.push((name.name.clone(), idx as u32));
        }
        Self {
            bindings,
            param_count: params.len() as u32,
            locals: Vec::new(),
            param_types: params.iter().map(|p| p.ty.clone()).collect(),
        }
    }

    fn declare_local(&mut self, ty: Type) -> u32 {
        let idx = self.param_count + self.locals.len() as u32;
        self.locals.push(ty);
        idx
    }

    fn push_binding(&mut self, name: String, idx: u32) {
        self.bindings.push((name, idx));
    }

    fn pop_binding(&mut self) {
        self.bindings.pop();
    }

    fn lookup(&self, name: &str) -> (u32, Type) {
        let (_name, idx) = self
            .bindings
            .iter()
            .rev()
            .find(|(n, _)| n == name)
            .unwrap_or_else(|| panic!("Codegen missing variable {}", name));
        let ty = if (*idx as usize) < self.param_count as usize {
            self.param_types[*idx as usize].clone()
        } else {
            let local_idx = *idx as usize - self.param_count as usize;
            self.locals[local_idx].clone()
        };
        (*idx, ty)
    }
}

fn expr_type(
    expr: &Expr,
    env: &CodegenEnv,
    signatures: &HashMap<String, Signature>,
    globals: &HashMap<String, (Type, bool)>,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
) -> Type {
    let mut vars = HashMap::new();
    for (name, idx) in &env.bindings {
        let ty = if (*idx as usize) < env.param_count as usize {
            env.param_types[*idx as usize].clone()
        } else {
            let local_idx = *idx as usize - env.param_count as usize;
            env.locals[local_idx].clone()
        };
        vars.insert(name.clone(), ty);
    }
    check_expr(expr, &vars, signatures, globals, records, variants)
        .expect("type checking already performed")
}

fn conversion_instr(from: &Type, to: &Type) -> Option<&'static str> {
    match (from, to) {
        (Type::S32, Type::S64) => Some("i64.extend_i32_s"),
        (Type::S64, Type::S32) => Some("i32.wrap_i64"),
        (Type::F32, Type::F64) => Some("f64.promote_f32"),
        (Type::F64, Type::F32) => Some("f32.demote_f64"),
        (Type::S32, Type::F32) => Some("f32.convert_i32_s"),
        (Type::S32, Type::F64) => Some("f64.convert_i32_s"),
        (Type::S64, Type::F32) => Some("f32.convert_i64_s"),
        (Type::S64, Type::F64) => Some("f64.convert_i64_s"),
        (Type::F32, Type::S32) => Some("i32.trunc_f32_s"),
        (Type::F32, Type::S64) => Some("i64.trunc_f32_s"),
        (Type::F64, Type::S32) => Some("i32.trunc_f64_s"),
        (Type::F64, Type::S64) => Some("i64.trunc_f64_s"),
        _ if from == to => None,
        // Records don't have conversion instructions
        (Type::Record(_), _) | (_, Type::Record(_)) => None,
        _ => None,
    }
}

fn wat_type(ty: &Type) -> &'static str {
    match ty {
        Type::S32 => "i32",
        Type::S64 => "i64",
        Type::F32 => "f32",
        Type::F64 => "f64",
        // All compound types are pointer-sized (i32 handles)
        Type::Record(_)
        | Type::Variant(_)
        | Type::Option(_)
        | Type::Result(_, _)
        | Type::List(_)
        | Type::Str => "i32",
        // Resources are i32 handles
        Type::Resource(_) | Type::Borrow(_) => "i32",
    }
}

fn wit_type(ty: &Type) -> String {
    match ty {
        Type::S32 => "s32".to_string(),
        Type::S64 => "s64".to_string(),
        Type::F32 => "f32".to_string(),
        Type::F64 => "f64".to_string(),
        Type::Record(name) | Type::Variant(name) => name.clone(),
        Type::Option(inner) => format!("option<{}>", wit_type(inner)),
        Type::Result(ok, err) => format!("result<{}, {}>", wit_type(ok), wit_type(err)),
        Type::List(inner) => format!("list<{}>", wit_type(inner)),
        Type::Str => "string".to_string(),
        Type::Resource(name) => name.clone(),
        Type::Borrow(inner) => format!("borrow<{}>", wit_type(inner)),
    }
}

/// Returns the flattened canonical ABI types for a given type.
/// Records are flattened into their fields, variants into discriminant + max payload.
fn flatten_type(
    ty: &Type,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
) -> Vec<Type> {
    match ty {
        Type::S32 | Type::S64 | Type::F32 | Type::F64 => vec![ty.clone()],
        Type::Record(name) => {
            let record = records.get(name).expect("record not found");
            let mut result = Vec::new();
            for field in &record.fields {
                result.extend(flatten_type(&field.ty, records, variants));
            }
            result
        }
        Type::Variant(name) => {
            // Variants: discriminant (i32) + flattened max payload
            let variant = variants.get(name).expect("variant not found");
            let mut result = vec![Type::S32]; // discriminant

            // Find max payload size (in number of flattened fields)
            let max_payload_size = variant
                .cases
                .iter()
                .map(|c| {
                    c.payload
                        .iter()
                        .map(|t| flatten_type(t, records, variants).len())
                        .sum::<usize>()
                })
                .max()
                .unwrap_or(0);

            // Add i32 slots for the max payload
            for _ in 0..max_payload_size {
                result.push(Type::S32);
            }
            result
        }
        Type::Option(inner) => {
            // Option: discriminant (i32) + flattened inner type
            let mut result = vec![Type::S32]; // discriminant (0=none, 1=some)
            result.extend(flatten_type(inner, records, variants));
            result
        }
        Type::Result(ok, err) => {
            // Result: discriminant (i32) + max of ok/err flattened
            let ok_flat = flatten_type(ok, records, variants);
            let err_flat = flatten_type(err, records, variants);
            let max_size = ok_flat.len().max(err_flat.len());
            let mut result = vec![Type::S32]; // discriminant (0=ok, 1=err)
            for _ in 0..max_size {
                result.push(Type::S32);
            }
            result
        }
        Type::List(_) => {
            // List is pointer + length
            vec![Type::S32, Type::S32]
        }
        Type::Str => {
            // String is pointer + length (canonical ABI)
            vec![Type::S32, Type::S32]
        }
        Type::Resource(_) | Type::Borrow(_) => {
            // Resources and borrows are i32 handles
            vec![Type::S32]
        }
    }
}

/// Check if a type needs ABI wrapper (is not a simple scalar or handle)
fn needs_abi_wrapper(ty: &Type) -> bool {
    // Scalars and resource handles don't need ABI wrappers - they pass directly as primitives
    !matches!(
        ty,
        Type::S32 | Type::S64 | Type::F32 | Type::F64 | Type::Resource(_) | Type::Borrow(_)
    )
}

/// Check if a function needs an ABI wrapper for export
fn function_needs_abi_wrapper(func: &Function) -> bool {
    needs_abi_wrapper(&func.return_type) || func.params.iter().any(|p| needs_abi_wrapper(&p.ty))
}

/// Generate an ABI wrapper function for exported functions with rich types.
/// The wrapper takes flattened canonical ABI params and calls the internal function.
///
/// Canonical ABI rules:
/// - Record params: flattened into individual scalar fields
/// - Variant params: flattened into discriminant + max payload fields
/// - Record/Variant returns: pointer (MAX_FLAT_RESULTS=1, so complex types stay as pointers)
fn generate_abi_wrapper(
    out: &mut String,
    func: &Function,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
) {
    let pad = "    ";

    // Compute flattened params (but NOT returns - returns stay as pointers for complex types)
    let mut flat_params: Vec<(String, Type)> = Vec::new();

    for param in &func.params {
        let flat = flatten_type(&param.ty, records, variants);
        for (i, ty) in flat.iter().enumerate() {
            flat_params.push((format!("{}_{}", param.name, i), ty.clone()));
        }
    }

    // For returns: scalar types stay scalar, records/variants stay as pointers
    // (Canonical ABI MAX_FLAT_RESULTS = 1, so multi-field records return as pointer)
    let return_wat = wat_type(&func.return_type);

    // Start function definition
    out.push_str(&format!("  (func ${} ", func.name));
    for (name, ty) in &flat_params {
        out.push_str(&format!("(param ${} {}) ", name, wat_type(ty)));
    }
    out.push_str(&format!("(result {})\n", return_wat));

    // For each record/variant parameter, we need a local to store the pointer
    for param in &func.params {
        if matches!(&param.ty, Type::Record(_) | Type::Variant(_)) {
            out.push_str(&format!("{}(local i32)\n", pad)); // pointer to constructed value
        }
    }

    // Construct record/variant parameters from flattened values
    let mut flat_param_idx = 0;
    let mut complex_local_idx = flat_params.len();
    let mut internal_call_args: Vec<String> = Vec::new();

    for param in &func.params {
        match &param.ty {
            Type::Record(name) => {
                let record = records.get(name).expect("record not found");
                let size = type_size(&param.ty);

                // Allocate space for the record
                out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
                out.push_str(&format!("{}local.set {}\n", pad, complex_local_idx));
                out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
                out.push_str(&format!("{}i32.const {}\n", pad, size));
                out.push_str(&format!("{}i32.add\n", pad));
                out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

                // Store each field
                let mut offset = 0;
                for field in &record.fields {
                    out.push_str(&format!("{}local.get {}\n", pad, complex_local_idx));
                    if offset > 0 {
                        out.push_str(&format!("{}i32.const {}\n", pad, offset));
                        out.push_str(&format!("{}i32.add\n", pad));
                    }
                    out.push_str(&format!(
                        "{}local.get ${}\n",
                        pad, flat_params[flat_param_idx].0
                    ));
                    out.push_str(&format!("{}{}\n", pad, store_instr(&field.ty)));
                    offset += type_size(&field.ty);
                    flat_param_idx += 1;
                }

                internal_call_args.push(format!("local.get {}", complex_local_idx));
                complex_local_idx += 1;
            }
            Type::Variant(name) => {
                let variant = variants.get(name).expect("variant not found");
                let size = type_size(&param.ty);

                // Allocate space for the variant
                out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
                out.push_str(&format!("{}local.set {}\n", pad, complex_local_idx));
                out.push_str(&format!("{}global.get $__heap_ptr\n", pad));
                out.push_str(&format!("{}i32.const {}\n", pad, size));
                out.push_str(&format!("{}i32.add\n", pad));
                out.push_str(&format!("{}global.set $__heap_ptr\n", pad));

                // Store discriminant (first flat param)
                out.push_str(&format!("{}local.get {}\n", pad, complex_local_idx));
                out.push_str(&format!(
                    "{}local.get ${}\n",
                    pad, flat_params[flat_param_idx].0
                ));
                out.push_str(&format!("{}i32.store\n", pad));
                flat_param_idx += 1;

                // Find max payload size for this variant
                let max_payload_count = variant
                    .cases
                    .iter()
                    .map(|c| c.payload.len())
                    .max()
                    .unwrap_or(0);

                // Store payload values
                for i in 0..max_payload_count {
                    out.push_str(&format!("{}local.get {}\n", pad, complex_local_idx));
                    out.push_str(&format!("{}i32.const {}\n", pad, 4 + i * 4));
                    out.push_str(&format!("{}i32.add\n", pad));
                    out.push_str(&format!(
                        "{}local.get ${}\n",
                        pad, flat_params[flat_param_idx].0
                    ));
                    out.push_str(&format!("{}i32.store\n", pad));
                    flat_param_idx += 1;
                }

                internal_call_args.push(format!("local.get {}", complex_local_idx));
                complex_local_idx += 1;
            }
            Type::S32 | Type::S64 | Type::F32 | Type::F64 => {
                internal_call_args.push(format!("local.get ${}", flat_params[flat_param_idx].0));
                flat_param_idx += 1;
            }
            _ => {
                // For other types (options, results, etc.), consume their flattened params
                let flat_count = flatten_type(&param.ty, records, variants).len();
                for _ in 0..flat_count {
                    flat_param_idx += 1;
                }
                // TODO: properly handle options/results
                internal_call_args.push(format!("i32.const 0"));
            }
        }
    }

    // Call the internal function - return value stays on stack (pointer or scalar)
    for arg in &internal_call_args {
        out.push_str(&format!("{}{}\n", pad, arg));
    }
    out.push_str(&format!("{}call ${}__internal\n", pad, func.name));

    // Return value is already on stack from internal function call
    // For records, it's a pointer; for scalars, it's the value

    out.push_str("  )\n");
}

/// Get the store instruction for a type
fn store_instr(ty: &Type) -> &'static str {
    match ty {
        Type::S32 => "i32.store",
        Type::S64 => "i64.store",
        Type::F32 => "f32.store",
        Type::F64 => "f64.store",
        // Compound types are pointer-sized, resources are i32 handles
        Type::Record(_)
        | Type::Variant(_)
        | Type::Option(_)
        | Type::Result(_, _)
        | Type::List(_)
        | Type::Str
        | Type::Resource(_)
        | Type::Borrow(_) => "i32.store",
    }
}

/// Get the load instruction for a type
fn load_instr(ty: &Type) -> &'static str {
    match ty {
        Type::S32 => "i32.load",
        Type::S64 => "i64.load",
        Type::F32 => "f32.load",
        Type::F64 => "f64.load",
        // Compound types are pointer-sized, resources are i32 handles
        Type::Record(_)
        | Type::Variant(_)
        | Type::Option(_)
        | Type::Result(_, _)
        | Type::List(_)
        | Type::Str
        | Type::Resource(_)
        | Type::Borrow(_) => "i32.load",
    }
}

fn generate_wit(prog: &Program) -> String {
    let mut out = String::new();

    // If we have a world_config with external interfaces, generate WIT that references them
    if let Some(world_config) = &prog.world_config {
        // Package name derived from world name
        out.push_str(&format!("package package:{};\n\n", world_config.name));
        out.push_str(&format!("world {} {{\n", world_config.name));

        // External imports (e.g., theater:simple/runtime)
        for ext_import in &world_config.external_imports {
            out.push_str(&format!("  import {};\n", ext_import.to_wit_ref()));
        }

        // External exports (e.g., theater:simple/actor)
        for ext_export in &world_config.external_exports {
            out.push_str(&format!("  export {};\n", ext_export.to_wit_ref()));
        }

        // Also include any local exports that aren't part of external interfaces
        for export in &prog.exports {
            let func = find_function(prog, export);
            out.push_str(&format!("  export {}: func(", export));
            for (i, param) in func.params.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(&format!("{}: {}", param.name, wit_type(&param.ty)));
            }
            out.push_str(&format!(") -> {};\n", wit_type(&func.return_type)));
        }

        out.push_str("}\n");
    } else {
        // Original behavior for standalone packages
        out.push_str("package example:wisp;\n\n");
        out.push_str("world wisp {\n");

        // Generate record type declarations
        for record in &prog.records {
            out.push_str(&format!("  record {} {{\n", record.name));
            for field in &record.fields {
                out.push_str(&format!("    {}: {},\n", field.name, wit_type(&field.ty)));
            }
            out.push_str("  }\n\n");
        }

        // Generate variant type declarations
        for variant in &prog.variants {
            out.push_str(&format!("  variant {} {{\n", variant.name));
            for case in &variant.cases {
                if case.payload.is_empty() {
                    // Case with no payload: just the name
                    out.push_str(&format!("    {},\n", case.name));
                } else if case.payload.len() == 1 {
                    // Case with single payload: name(type)
                    out.push_str(&format!(
                        "    {}({}),\n",
                        case.name,
                        wit_type(&case.payload[0])
                    ));
                } else {
                    // Case with multiple payloads: name(tuple<type1, type2, ...>)
                    let types: Vec<String> = case.payload.iter().map(wit_type).collect();
                    out.push_str(&format!(
                        "    {}(tuple<{}>),\n",
                        case.name,
                        types.join(", ")
                    ));
                }
            }
            out.push_str("  }\n\n");
        }

        // Generate resource type declarations
        for resource in &prog.resources {
            out.push_str(&format!("  resource {};\n\n", resource.name));
        }

        let mut imports_by_module: BTreeMap<&str, Vec<&Import>> = BTreeMap::new();
        for import in &prog.imports {
            imports_by_module
                .entry(import.module.as_str())
                .or_default()
                .push(import);
        }
        for (module, imports) in imports_by_module {
            out.push_str(&format!("  import {}: interface {{\n", module));
            for import in imports {
                out.push_str(&format!("    {}: func(", import.name));
                for (i, param) in import.params.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    out.push_str(&format!("{}: {}", param.name, wit_type(&param.ty)));
                }
                out.push_str(&format!(") -> {};\n", wit_type(&import.return_type)));
            }
            out.push_str("  }\n");
        }
        for export in &prog.exports {
            let func = find_function(prog, export);
            out.push_str(&format!("  export {}: func(", export));
            for (i, param) in func.params.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(&format!("{}: {}", param.name, wit_type(&param.ty)));
            }
            out.push_str(&format!(") -> {};\n", wit_type(&func.return_type)));
        }
        out.push_str("}\n");
    }

    out
}

fn find_function<'a>(prog: &'a Program, name: &str) -> &'a Function {
    prog.functions
        .iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("Function '{}' not found during codegen", name))
}

// ============================================================================
// REPL Compilation Support
// ============================================================================

/// Compile an expression for REPL evaluation.
///
/// Takes an expression string, variable bindings to inline, and function
/// definitions to include. Returns WASM package bytes with an exported
/// `eval` function that evaluates the expression.
pub fn compile_repl_expr(
    expr_source: &str,
    bindings: &HashMap<String, InlineValue>,
    functions: &[Function],
) -> Result<Vec<u8>> {
    let ctx = CompileContext::new(expr_source.to_string(), "<repl>".to_string());

    // Parse the expression
    let tokens = tokenize(expr_source);
    if tokens.is_empty() {
        bail!("empty expression");
    }

    let (sexpr, _) = parse_sexpr(&tokens, 0);

    // Inline variable bindings by transforming the SExpr
    let inlined_sexpr = inline_bindings(&sexpr, bindings);

    // Build function signatures from provided functions
    let mut signatures: HashMap<String, Signature> = HashMap::new();
    for func in functions {
        signatures.insert(
            func.name.clone(),
            Signature {
                params: func.params.iter().map(|p| p.ty.clone()).collect(),
                result: func.return_type.clone(),
            },
        );
    }

    // Parse the expression into an Expr AST
    let expr = parse_expr(
        &inlined_sexpr,
        &[], // No local variables initially
        &signatures,
        &HashMap::new(), // No records for now
        &HashMap::new(), // No variants for now
        &ctx,
    )?;

    // Infer the return type by type-checking the expression
    let return_type = check_expr(
        &expr,
        &HashMap::new(), // No local variables
        &signatures,
        &HashMap::new(), // No globals
        &HashMap::new(), // No records
        &HashMap::new(), // No variants
    )?;

    // Create the eval function
    let eval_fn = Function {
        name: "eval".to_string(),
        params: vec![],
        return_type,
        body: expr,
    };

    // Build the program with all functions + eval
    let mut all_functions = functions.to_vec();
    all_functions.push(eval_fn);

    let prog = Program {
        functions: all_functions,
        imports: vec![],
        exports: vec!["eval".to_string()],
        globals: vec![],
        records: vec![],
        variants: vec![],
        resources: vec![],
        world_config: None,
    };

    // Type check the full program
    let full_signatures = collect_signatures(&prog)?;
    type_check(&prog, &full_signatures, &ctx)?;

    // Generate WAT and WIT
    let wat = generate_wat(&prog, &full_signatures);
    let wit = generate_wit(&prog);

    // Encode to WASM component
    let wasm_bytes = parse_str(&wat).context("failed to convert generated WAT to wasm")?;
    let component_bytes = encode_component(&wasm_bytes, &wit, None, Path::new("<repl>"))?;

    Ok(component_bytes)
}

/// Transform an SExpr by inlining variable bindings as literal values
fn inline_bindings(sexpr: &SExpr, bindings: &HashMap<String, InlineValue>) -> SExpr {
    match sexpr {
        SExpr::Sym(name, span) => {
            if let Some(value) = bindings.get(name) {
                value_to_sexpr(value, span)
            } else {
                sexpr.clone()
            }
        }
        SExpr::List(items, span) => {
            let inlined_items: Vec<SExpr> = items
                .iter()
                .map(|item| inline_bindings(item, bindings))
                .collect();
            SExpr::List(inlined_items, span.clone())
        }
        SExpr::Quasiquote(inner, span) => {
            SExpr::Quasiquote(Box::new(inline_bindings(inner, bindings)), span.clone())
        }
        SExpr::Unquote(inner, span) => {
            SExpr::Unquote(Box::new(inline_bindings(inner, bindings)), span.clone())
        }
        SExpr::UnquoteSplice(inner, span) => {
            SExpr::UnquoteSplice(Box::new(inline_bindings(inner, bindings)), span.clone())
        }
        // Literals don't need inlining
        SExpr::Int { .. } | SExpr::Float { .. } | SExpr::Str(..) => sexpr.clone(),
        // Syntax forms - recurse into them
        SExpr::SyntaxQuote(inner, span) => {
            SExpr::SyntaxQuote(Box::new(inline_bindings(inner, bindings)), span.clone())
        }
        SExpr::Quasisyntax(inner, span) => {
            SExpr::Quasisyntax(Box::new(inline_bindings(inner, bindings)), span.clone())
        }
        SExpr::Unsyntax(inner, span) => {
            SExpr::Unsyntax(Box::new(inline_bindings(inner, bindings)), span.clone())
        }
        SExpr::UnsyntaxSplice(inner, span) => {
            SExpr::UnsyntaxSplice(Box::new(inline_bindings(inner, bindings)), span.clone())
        }
    }
}

/// Convert an InlineValue to an SExpr literal
fn value_to_sexpr(value: &InlineValue, span: &Span) -> SExpr {
    match value {
        InlineValue::S32(n) => SExpr::Int {
            value: *n as i64,
            ty: Type::S32,
            span: span.clone(),
        },
        InlineValue::S64(n) => SExpr::Int {
            value: *n,
            ty: Type::S64,
            span: span.clone(),
        },
        InlineValue::F32(n) => SExpr::Float {
            value: *n as f64,
            ty: Type::F32,
            span: span.clone(),
        },
        InlineValue::F64(n) => SExpr::Float {
            value: *n,
            ty: Type::F64,
            span: span.clone(),
        },
        InlineValue::Str(s) => SExpr::Str(s.clone(), span.clone()),
        // Compound types need constructor calls - for now, panic with a clear message
        InlineValue::List { .. } => {
            panic!("TODO: inline list values as constructor calls")
        }
        InlineValue::Option { .. } => {
            panic!("TODO: inline option values as constructor calls")
        }
        InlineValue::Result { .. } => {
            panic!("TODO: inline result values as constructor calls")
        }
        InlineValue::Record { .. } => {
            panic!("TODO: inline record values as constructor calls")
        }
        InlineValue::Variant { .. } => {
            panic!("TODO: inline variant values as constructor calls")
        }
    }
}

// ============================================================================
// Pack Package Generation
// ============================================================================

/// CGRF format constants
const CGRF_MAGIC: u32 = 0x46524743; // "CGRF" in little-endian
const CGRF_VERSION: u16 = 2;

/// CGRF node kinds (also used as type tags for v2 encoding)
const CGRF_BOOL: u8 = 0x01;
const CGRF_S32: u8 = 0x02;
const CGRF_S64: u8 = 0x03;
const CGRF_F32: u8 = 0x04;
const CGRF_F64: u8 = 0x05;
const CGRF_STRING: u8 = 0x06;
const CGRF_LIST: u8 = 0x07;
const CGRF_VARIANT: u8 = 0x08;
const CGRF_RECORD: u8 = 0x09;
const CGRF_OPTION: u8 = 0x0A;
const CGRF_TUPLE: u8 = 0x0B;
const CGRF_U8: u8 = 0x0C;
const CGRF_U16: u8 = 0x0D;
const CGRF_U32: u8 = 0x0E;
const CGRF_U64: u8 = 0x0F;
const CGRF_S8: u8 = 0x10;
const CGRF_S16: u8 = 0x11;
const CGRF_CHAR: u8 = 0x12;
const CGRF_FLAGS: u8 = 0x13;
const CGRF_RESULT: u8 = 0x14;

/// Memory layout for Pack packages
const INPUT_BUFFER_OFFSET: i32 = 0x0000;
const OUTPUT_BUFFER_OFFSET: i32 = 0x4000;
const HEAP_START_OFFSET: i32 = 0xC000;

/// Get the type tag byte for a type (for CGRF v2 encoding)
fn type_to_tag(ty: &Type) -> u8 {
    match ty {
        Type::S32 => CGRF_S32,
        Type::S64 => CGRF_S64,
        Type::F32 => CGRF_F32,
        Type::F64 => CGRF_F64,
        Type::Str => CGRF_STRING,
        Type::List(_) => CGRF_LIST,
        Type::Option(_) => CGRF_OPTION,
        Type::Result(_, _) => CGRF_RESULT,
        Type::Record(_) => CGRF_RECORD,
        Type::Variant(_) => CGRF_VARIANT,
        Type::Resource(_) => CGRF_RECORD, // Resources are treated as records for now
        Type::Borrow(inner) => type_to_tag(inner), // Borrow uses inner type's tag
    }
}

/// Calculate the byte size of a type tag (for CGRF v2 encoding)
/// Simple types are 1 byte, compound types include nested type info
fn type_tag_size(ty: &Type) -> usize {
    match ty {
        Type::S32 | Type::S64 | Type::F32 | Type::F64 | Type::Str => 1,
        Type::List(inner) => 1 + type_tag_size(inner),
        Type::Option(inner) => 1 + type_tag_size(inner),
        Type::Result(ok, err) => 1 + type_tag_size(ok) + type_tag_size(err),
        Type::Record(name) | Type::Variant(name) | Type::Resource(name) => 1 + 4 + name.len(),
        Type::Borrow(inner) => type_tag_size(inner),
    }
}

/// Generate WAT code to write a type tag at the given offset
/// Returns the number of bytes written
fn generate_write_type_tag(out: &mut String, ty: &Type, base_local: &str, offset: i32) -> usize {
    let tag = type_to_tag(ty);
    out.push_str(&format!("    local.get {}\n", base_local));
    if offset != 0 {
        out.push_str(&format!("    i32.const {}\n", offset));
        out.push_str("    i32.add\n");
    }
    out.push_str(&format!("    i32.const {}\n", tag));
    out.push_str("    i32.store8\n");

    match ty {
        Type::S32 | Type::S64 | Type::F32 | Type::F64 | Type::Str => 1,
        Type::List(inner) => {
            1 + generate_write_type_tag(out, inner, base_local, offset + 1)
        }
        Type::Option(inner) => {
            1 + generate_write_type_tag(out, inner, base_local, offset + 1)
        }
        Type::Result(ok, err) => {
            let ok_size = generate_write_type_tag(out, ok, base_local, offset + 1);
            let err_size = generate_write_type_tag(out, err, base_local, offset + 1 + ok_size as i32);
            1 + ok_size + err_size
        }
        Type::Record(name) | Type::Variant(name) | Type::Resource(name) => {
            // Write name length
            out.push_str(&format!("    local.get {}\n", base_local));
            out.push_str(&format!("    i32.const {}\n", offset + 1));
            out.push_str("    i32.add\n");
            out.push_str(&format!("    i32.const {}\n", name.len()));
            out.push_str("    i32.store\n");
            // Write name bytes
            for (i, byte) in name.bytes().enumerate() {
                out.push_str(&format!("    local.get {}\n", base_local));
                out.push_str(&format!("    i32.const {}\n", offset + 5 + i as i32));
                out.push_str("    i32.add\n");
                out.push_str(&format!("    i32.const {}\n", byte));
                out.push_str("    i32.store8\n");
            }
            1 + 4 + name.len()
        }
        Type::Borrow(inner) => generate_write_type_tag(out, inner, base_local, offset),
    }
}

/// Generate WAT for a Pack-compatible package.
///
/// This produces WASM with:
/// - Export functions using Pack/Graph ABI calling convention: (i32, i32, i32, i32) -> i32
/// - CGRF encoding for input/output values
/// - Memory layout with input buffer at 0x0, output at 0x4000
fn generate_wat_pack(prog: &Program, signatures: &HashMap<String, Signature>) -> String {
    let mut out = String::new();
    out.push_str("(module\n");

    // Generate import declarations with Pack/Graph ABI signature
    // Each import is declared as (i32, i32, i32, i32) -> i32
    for import in &prog.imports {
        // Raw import with Pack/Graph ABI calling convention
        out.push_str(&format!(
            "  (import \"{}\" \"{}\" (func $__raw_{} (param i32 i32 i32 i32) (result i32)))\n",
            import.module, import.name, import.name
        ));
    }

    // Memory: 500 pages (32MB) initial, 1000 max (64MB), exported as "memory"
    // Large initial size needed for bootstrap compilation of the 42KB compiler
    out.push_str("  (memory (export \"memory\") 16000 16000)\n");

    // Heap pointer for allocations, starts after output buffer
    out.push_str(&format!(
        "  (global $__heap_ptr (mut i32) (i32.const {}))\n",
        HEAP_START_OFFSET
    ));

    // Emit user-defined globals
    for global in &prog.globals {
        let mutability = if global.mutable { "mut" } else { "" };
        let wasm_type = wat_type(&global.ty);
        if global.mutable {
            out.push_str(&format!(
                "  (global {} ({} {}) ({}.const {}))\n",
                global.name, mutability, wasm_type, wasm_type, global.init_value
            ));
        } else {
            out.push_str(&format!(
                "  (global {} {} ({}.const {}))\n",
                global.name, wasm_type, wasm_type, global.init_value
            ));
        }
    }

    // Build maps for codegen
    let globals_map: HashMap<String, (Type, bool)> = prog
        .globals
        .iter()
        .map(|g| (g.name.clone(), (g.ty.clone(), g.mutable)))
        .collect();
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

    // Allocator helper that grows memory when needed
    // Takes size in bytes, returns pointer to allocated block
    out.push_str(
        r#"  (func $__alloc (param $size i32) (result i32)
    (local $ptr i32)
    (local $end i32)
    (local $pages_needed i32)
    ;; Get current heap pointer
    global.get $__heap_ptr
    local.set $ptr
    ;; Calculate end of allocation
    local.get $ptr
    local.get $size
    i32.add
    local.set $end
    ;; Check if we need to grow memory
    ;; memory.size returns pages, multiply by 64KB to get bytes
    memory.size
    i32.const 65536
    i32.mul
    local.get $end
    i32.lt_u
    (if
      (then
        ;; Calculate pages needed: (end - current_size + 65535) / 65536
        local.get $end
        memory.size
        i32.const 65536
        i32.mul
        i32.sub
        i32.const 65535
        i32.add
        i32.const 65536
        i32.div_u
        local.set $pages_needed
        ;; Grow memory
        local.get $pages_needed
        memory.grow
        ;; Check if grow failed (returns -1)
        i32.const -1
        i32.eq
        (if
          (then
            ;; Out of memory - trap
            unreachable
          )
        )
      )
    )
    ;; Bump heap pointer
    local.get $end
    global.set $__heap_ptr
    ;; Return old pointer
    local.get $ptr
  )
"#,
    );

    // Generate import wrapper functions
    // These have the original wisp signature but internally encode args and call the raw import
    for import in &prog.imports {
        generate_import_wrapper(&mut out, import);
    }

    // Generate internal functions
    // These use their original names ($name) so that gen_expr's function calls work correctly
    for func in &prog.functions {
        let mut body = String::new();
        let mut env = CodegenEnv::new(&func.params);
        gen_expr(
            &func.body,
            &mut body,
            4,
            &mut env,
            signatures,
            &globals_map,
            &records_map,
            &variants_map,
            true, // Function body is in tail position
        );

        out.push_str(&format!("  (func ${} ", func.name));
        for param in &func.params {
            out.push_str(&format!("(param ${} {}) ", param.name, wat_type(&param.ty)));
        }
        out.push_str(&format!("(result {})\n", wat_type(&func.return_type)));
        for local in &env.locals {
            out.push_str(&format!("    (local {})\n", wat_type(local)));
        }
        out.push_str(&body);
        out.push_str("  )\n");
    }

    // Generate Pack wrappers for exported functions
    for export in &prog.exports {
        let func = find_function(prog, export);
        generate_pack_wrapper(&mut out, func, &records_map, &variants_map);
    }

    out.push_str(")\n");
    out
}

/// Generate a Pack-compatible export wrapper for a function.
///
/// The wrapper has signature: (in_ptr, in_len, out_ptr, out_cap) -> bytes_written
/// It decodes input (if any), calls the internal function, encodes the result.
fn generate_pack_wrapper(
    out: &mut String,
    func: &Function,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
) {
    let export_name = &func.name;
    let wrapper_name = format!("{}__export", func.name);
    let internal_name = format!("${}", func.name);

    out.push_str(&format!(
        "  (func ${} (export \"{}\") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)\n",
        wrapper_name, export_name
    ));

    // Declare local for the result value (locals must be at the top)
    // All compound types (strings, records, etc.) are i32 pointers
    match &func.return_type {
        Type::S32 => out.push_str("    (local $value i32)\n"),
        Type::S64 => out.push_str("    (local $value i64)\n"),
        Type::F32 => out.push_str("    (local $value f32)\n"),
        Type::F64 => out.push_str("    (local $value f64)\n"),
        Type::Str | Type::Record(_) | Type::Option(_) => {
            out.push_str("    (local $value i32)\n")
        }
        Type::Variant(_) => {
            out.push_str("    (local $value i32)\n");
            // $tag local needed for v2 case name lookup
            out.push_str("    (local $tag i32)\n");
        }
        Type::Result(_, _) => {
            out.push_str("    (local $value i32)\n");
            // $tag local needed for result encoding (though not currently used)
            out.push_str("    (local $tag i32)\n");
        }
        Type::List(_) => {
            out.push_str("    (local $value i32)\n");
            // Additional locals needed for list encoding loop
            out.push_str("    (local $i i32)\n");
            out.push_str("    (local $len i32)\n");
            out.push_str("    (local $data_ptr i32)\n");
            out.push_str("    (local $node_offset i32)\n");
        }
        _ => {}
    }

    // Declare locals for parameters
    for param in &func.params {
        let local_type = match &param.ty {
            Type::S64 => "i64",
            Type::F32 => "f32",
            Type::F64 => "f64",
            _ => "i32", // s32, string, records, etc. are all i32
        };
        out.push_str(&format!(
            "    (local $param_{} {})\n",
            param.name, local_type
        ));
    }

    // Additional locals needed for string decoding
    let needs_string_decode = func.params.iter().any(|p| matches!(p.ty, Type::Str));
    if needs_string_decode {
        out.push_str("    (local $str_len i32)\n");
        out.push_str("    (local $str_ptr i32)\n");
    }

    // Additional locals needed for record/option/variant/result decoding
    let needs_compound_decode = func.params.iter().any(|p| {
        matches!(p.ty, Type::Record(_) | Type::Option(_) | Type::Variant(_) | Type::Result(_, _))
    });
    if needs_compound_decode {
        out.push_str("    (local $rec_ptr i32)\n");
        out.push_str("    (local $field_val i32)\n");
        // Tree traversal locals needed for v2 format decoding
        out.push_str("    (local $child_idx i32)\n");
        out.push_str("    (local $child_offset i32)\n");
        out.push_str("    (local $scan_i i32)\n");
        out.push_str("    (local $payload_len i32)\n");
    }

    // Local for runtime offset tracking in tuple decoding
    // Needed when any tuple element has variable size (strings, lists, etc.)
    let needs_runtime_offset = func.params.len() > 1
        && func.params
            .iter()
            .any(|p| matches!(p.ty, Type::Str | Type::List(_)));
    if needs_runtime_offset {
        out.push_str("    (local $node_offset i32)\n");
        out.push_str("    (local $data_len i32)\n");
    }

    // Additional locals for list decoding
    let needs_list_decode = func.params.iter().any(|p| matches!(p.ty, Type::List(_)));
    if needs_list_decode {
        out.push_str("    (local $list_ptr i32)\n");
        out.push_str("    (local $list_len i32)\n");
        out.push_str("    (local $list_data i32)\n");
        out.push_str("    (local $list_i i32)\n");
        out.push_str("    (local $elem_offset i32)\n");
    }

    // Additional locals for tree traversal (multi-param with complex types)
    let needs_tree_traversal = func.params.len() > 1
        && func.params.iter().any(|p| {
            matches!(
                p.ty,
                Type::Record(_) | Type::Option(_) | Type::Variant(_) | Type::Result(_, _)
            )
        });
    if needs_tree_traversal {
        out.push_str("    (local $tuple_offset i32)\n");
        out.push_str("    (local $child_idx i32)\n");
        out.push_str("    (local $child_offset i32)\n");
        out.push_str("    (local $scan_i i32)\n");
        out.push_str("    (local $payload_len i32)\n");
        // Additional locals for v2 record decoding (reading type_name_len, field_name_len, etc.)
        out.push_str("    (local $str_len i32)\n");
        out.push_str("    (local $data_len i32)\n");
    }

    // Decode input parameters from CGRF
    if !func.params.is_empty() {
        out.push_str("    ;; Decode input parameters from CGRF\n");

        if func.params.len() == 1 {
            // Single parameter: root node is the value directly
            let param = &func.params[0];
            generate_cgrf_decode_param(out, &param.ty, &param.name, 0, false, records, variants);
        } else {
            // Multiple parameters: root is a tuple, decode each element
            out.push_str("    ;; Multiple params - expecting tuple root\n");

            if needs_runtime_offset {
                // Initialize node offset to 16 (start of first child node)
                out.push_str("    i32.const 16\n");
                out.push_str("    local.set $node_offset\n");
            }

            // Child nodes are encoded first (depth-first), so they start at offset 16
            // and are laid out sequentially by node index
            for (idx, param) in func.params.iter().enumerate() {
                generate_cgrf_decode_tuple_param(
                    out,
                    &param.ty,
                    &param.name,
                    idx,
                    &func.params,
                    needs_runtime_offset,
                    records,
                    variants,
                );
            }
        }
    }

    // Push parameters and call the internal function
    for param in &func.params {
        out.push_str(&format!("    local.get $param_{}\n", param.name));
    }
    out.push_str(&format!("    call {}\n", internal_name));
    out.push_str("    local.set $value\n");

    // Encode result based on return type
    match &func.return_type {
        Type::S32 => {
            generate_cgrf_encode_s32(out);
        }
        Type::S64 => {
            generate_cgrf_encode_s64(out);
        }
        Type::F32 => {
            generate_cgrf_encode_f32(out);
        }
        Type::F64 => {
            generate_cgrf_encode_f64(out);
        }
        Type::Str => {
            generate_cgrf_encode_string(out);
        }
        Type::Option(inner_ty) => {
            generate_cgrf_encode_option(out, inner_ty);
        }
        Type::List(elem_ty) => {
            generate_cgrf_encode_list(out, elem_ty, records, variants);
        }
        Type::Record(name) => {
            generate_cgrf_encode_record(out, name, records, variants);
        }
        Type::Variant(name) => {
            generate_cgrf_encode_variant(out, name, records, variants);
        }
        Type::Result(ok_ty, err_ty) => {
            generate_cgrf_encode_result(out, ok_ty, err_ty, records, variants);
        }
        _ => {
            out.push_str("    ;; TODO: encode non-scalar return types\n");
            out.push_str("    i32.const -1\n");
        }
    }

    out.push_str("  )\n");
}

/// Generate an import wrapper function.
///
/// The wrapper has the original wisp signature but internally:
/// 1. Encodes arguments to CGRF in a buffer
/// 2. Calls the raw import (which has Pack/Graph ABI signature)
/// 3. Decodes the result (if any)
fn generate_import_wrapper(out: &mut String, import: &Import) {
    let wrapper_name = &import.name;
    let raw_name = format!("$__raw_{}", import.name);

    // Start function with original signature
    out.push_str(&format!("  (func ${} ", wrapper_name));
    for param in &import.params {
        out.push_str(&format!("(param ${} {}) ", param.name, wat_type(&param.ty)));
    }

    // For now, imports that return "nothing" return s32 (0 for success)
    // This matches Theater's log which returns unit
    let result_type = wat_type(&import.return_type);
    out.push_str(&format!("(result {})\n", result_type));

    // Local variables for encoding
    out.push_str("    (local $in_buf i32)\n");
    out.push_str("    (local $in_len i32)\n");
    out.push_str("    (local $out_buf i32)\n");
    out.push_str("    (local $result i32)\n");

    // Use fixed buffer locations for import calls
    // Import input buffer at 0x8000, output at 0x9000
    let import_in_buf = 0x8000;
    let import_out_buf = 0x9000;
    let buf_cap = 0x1000; // 4KB

    out.push_str(&format!("    i32.const {}\n", import_in_buf));
    out.push_str("    local.set $in_buf\n");
    out.push_str(&format!("    i32.const {}\n", import_out_buf));
    out.push_str("    local.set $out_buf\n");

    // Encode arguments to CGRF
    // For single string argument (like log), encode as a string node
    if import.params.len() == 1 && matches!(import.params[0].ty, Type::Str) {
        // String is passed as (ptr, len) in WASM
        // We need to encode it as CGRF string node
        let param_name = &import.params[0].name;

        // Write CGRF header
        out.push_str("    ;; Write CGRF header for string argument\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC)); // "CGRF"
        out.push_str("    i32.store\n");

        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_VERSION)); // version
        out.push_str("    i32.store16\n");

        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 6\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n"); // flags
        out.push_str("    i32.store16\n");

        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 8\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 1\n"); // node_count
        out.push_str("    i32.store\n");

        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 12\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n"); // root_index
        out.push_str("    i32.store\n");

        // Write string node (kind=0x06 for String)
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 16\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 6\n"); // kind = String
        out.push_str("    i32.store8\n");

        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 17\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n"); // flags
        out.push_str("    i32.store8\n");

        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 18\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n"); // reserved
        out.push_str("    i32.store16\n");

        // Payload length = 4 (length prefix) + string length
        // CGRF string format: payload_len includes a 4-byte length prefix
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 20\n");
        out.push_str("    i32.add\n");
        // String in wisp is a pointer to (len: i32, data: bytes...)
        // Payload length = 4 + string_len
        out.push_str(&format!("    local.get ${}\n", param_name));
        out.push_str("    i32.load\n"); // load string length
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n"); // payload_len = 4 + string_len
        out.push_str("    i32.store\n");

        // Write string length at offset 24 (part of payload)
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 24\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    local.get ${}\n", param_name));
        out.push_str("    i32.load\n"); // string length
        out.push_str("    i32.store\n");

        // Copy string data to offset 28
        out.push_str("    ;; Copy string data to CGRF buffer\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 28\n");
        out.push_str("    i32.add\n"); // destination (after length prefix)
        out.push_str(&format!("    local.get ${}\n", param_name)); // source ptr (string data location)
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n"); // skip wisp string length prefix
        out.push_str(&format!("    local.get ${}\n", param_name));
        out.push_str("    i32.load\n"); // load length
        out.push_str("    memory.copy\n");

        // Calculate total buffer length: 16 (header) + 8 (node header) + 4 (string len) + string_len
        // = 28 + string_len
        out.push_str("    i32.const 28\n"); // header + node header + length prefix
        out.push_str(&format!("    local.get ${}\n", param_name));
        out.push_str("    i32.load\n"); // string length
        out.push_str("    i32.add\n");
        out.push_str("    local.set $in_len\n");
    } else if import.params.is_empty() {
        // No arguments - encode empty tuple
        out.push_str("    ;; No arguments - encode empty tuple\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
        out.push_str("    i32.store\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_VERSION));
        out.push_str("    i32.store16\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 6\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store16\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 8\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 1\n"); // one node (empty tuple)
        out.push_str("    i32.store\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 12\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store\n");
        // Tuple node with 0 children
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 16\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 8\n"); // kind = Tuple
        out.push_str("    i32.store8\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 17\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store8\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 18\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store16\n");
        out.push_str("    local.get $in_buf\n");
        out.push_str("    i32.const 20\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n"); // payload_len = 0
        out.push_str("    i32.store\n");
        out.push_str("    i32.const 24\n"); // total length
        out.push_str("    local.set $in_len\n");
    } else {
        // TODO: support other argument patterns
        out.push_str("    ;; TODO: encode arguments for this import\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    local.set $in_len\n");
    }

    // Call the raw import
    out.push_str("    ;; Call raw import\n");
    out.push_str("    local.get $in_buf\n");
    out.push_str("    local.get $in_len\n");
    out.push_str("    local.get $out_buf\n");
    out.push_str(&format!("    i32.const {}\n", buf_cap));
    out.push_str(&format!("    call {}\n", raw_name));
    out.push_str("    local.set $result\n");

    // For now, just return 0 (success) for void-returning imports
    // TODO: decode result for non-void imports
    out.push_str("    ;; Return result\n");
    match &import.return_type {
        Type::S32 => out.push_str("    i32.const 0\n"),
        Type::S64 => out.push_str("    i64.const 0\n"),
        Type::F32 => out.push_str("    f32.const 0\n"),
        Type::F64 => out.push_str("    f64.const 0\n"),
        _ => out.push_str("    i32.const 0\n"),
    }

    out.push_str("  )\n");
}

/// Generate WAT code to encode an i32 value on the stack to CGRF at $out_ptr.
/// Leaves bytes_written (i32) on the stack.
fn generate_cgrf_encode_s32(out: &mut String) {
    // Assumes $value local is already declared and contains the value to encode
    // Assumes $out_ptr contains the output buffer pointer

    // Write CGRF header (16 bytes)
    // Magic: "CGRF" = 0x46524743
    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
    out.push_str("    i32.store\n");

    // Version (u16) + Flags (u16) = 0x00000001
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_VERSION as i32));
    out.push_str("    i32.store16\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 6\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n"); // flags
    out.push_str("    i32.store16\n");

    // Node count: 1
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 1\n");
    out.push_str("    i32.store\n");

    // Root index: 0
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store\n");

    // Write node (8 bytes header + 4 bytes payload = 12 bytes)
    // Kind: S32 = 0x02, flags: 0, reserved: 0
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 16\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_S32 as i32));
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 17\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n"); // flags
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 18\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n"); // reserved
    out.push_str("    i32.store16\n");

    // Payload length: 4
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 20\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.store\n");

    // Payload: the i32 value
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 24\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $value\n");
    out.push_str("    i32.store\n");

    // Return bytes written: 16 (header) + 8 (node header) + 4 (payload) = 28
    out.push_str("    i32.const 28\n");
}

/// Generate WAT code to encode an i64 value on the stack to CGRF at $out_ptr.
fn generate_cgrf_encode_s64(out: &mut String) {
    // Assumes $value local is already declared and contains the value to encode
    // Assumes $out_ptr contains the output buffer pointer

    // Write CGRF header (16 bytes)
    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
    out.push_str("    i32.store\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_VERSION as i32));
    out.push_str("    i32.store16\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 6\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 1\n");
    out.push_str("    i32.store\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store\n");

    // Write node - kind S64 = 0x03
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 16\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_S64 as i32));
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 17\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 18\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");

    // Payload length: 8
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 20\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.store\n");

    // Payload: the i64 value
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 24\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $value\n");
    out.push_str("    i64.store\n");

    // Return bytes written: 16 + 8 + 8 = 32
    out.push_str("    i32.const 32\n");
}

/// Generate WAT code to encode an f32 value on the stack to CGRF at $out_ptr.
fn generate_cgrf_encode_f32(out: &mut String) {
    // Assumes $value local is already declared and contains the value to encode
    // Assumes $out_ptr contains the output buffer pointer

    // Write CGRF header
    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
    out.push_str("    i32.store\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_VERSION as i32));
    out.push_str("    i32.store16\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 6\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 1\n");
    out.push_str("    i32.store\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store\n");

    // Write node - kind F32 = 0x04
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 16\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_F32 as i32));
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 17\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 18\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");

    // Payload length: 4
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 20\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.store\n");

    // Payload: the f32 value
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 24\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $value\n");
    out.push_str("    f32.store\n");

    // Return bytes written: 16 + 8 + 4 = 28
    out.push_str("    i32.const 28\n");
}

/// Generate WAT code to encode an f64 value on the stack to CGRF at $out_ptr.
fn generate_cgrf_encode_f64(out: &mut String) {
    // Assumes $value local is already declared and contains the value to encode
    // Assumes $out_ptr contains the output buffer pointer

    // Write CGRF header
    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
    out.push_str("    i32.store\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_VERSION as i32));
    out.push_str("    i32.store16\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 6\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 1\n");
    out.push_str("    i32.store\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store\n");

    // Write node - kind F64 = 0x05
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 16\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_F64 as i32));
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 17\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 18\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");

    // Payload length: 8
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 20\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.store\n");

    // Payload: the f64 value
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 24\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $value\n");
    out.push_str("    f64.store\n");

    // Return bytes written: 16 + 8 + 8 = 32
    out.push_str("    i32.const 32\n");
}

/// Generate WAT code to encode a string value to CGRF at $out_ptr.
/// Input: $value contains i32 pointer to (len: i32, data: bytes)
/// Output: bytes written left on stack
///
/// CGRF String format:
/// - Header: 16 bytes (magic, version, flags, node_count=1, root=0)
/// - Node header: 8 bytes (kind=0x06, flags=0, reserved=0, payload_len)
/// - Payload: 4 bytes (string length) + string bytes
fn generate_cgrf_encode_string(out: &mut String) {
    // Write CGRF header (16 bytes)
    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
    out.push_str("    i32.store\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_VERSION as i32));
    out.push_str("    i32.store16\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 6\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 1\n"); // node_count
    out.push_str("    i32.store\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n"); // root_index
    out.push_str("    i32.store\n");

    // Write node header - kind String = 0x06
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 16\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_STRING as i32));
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 17\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n"); // flags
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 18\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n"); // reserved
    out.push_str("    i32.store16\n");

    // Payload length = 4 (string length field) + string length
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 20\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $value\n");
    out.push_str("    i32.load\n"); // load string length
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n"); // payload_len = 4 + str_len
    out.push_str("    i32.store\n");

    // Write string length in payload (offset 24)
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 24\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $value\n");
    out.push_str("    i32.load\n"); // string length
    out.push_str("    i32.store\n");

    // Copy string data to payload (offset 28)
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 28\n");
    out.push_str("    i32.add\n"); // destination
    out.push_str("    local.get $value\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n"); // source = string ptr + 4 (skip length prefix)
    out.push_str("    local.get $value\n");
    out.push_str("    i32.load\n"); // length
    out.push_str("    memory.copy\n");

    // Return bytes written: 16 (header) + 8 (node header) + 4 (str_len) + string_length
    // = 28 + string_length
    out.push_str("    i32.const 28\n");
    out.push_str("    local.get $value\n");
    out.push_str("    i32.load\n");
    out.push_str("    i32.add\n");
}

/// Generate WAT code to encode an option value to CGRF at $out_ptr.
/// Input: $value contains i32 pointer to option (tag: u8, value: T if some)
/// Wisp option layout: byte 0 = tag (0=none, 1=some), bytes 4+ = payload if some
/// CGRF v2 option payload: [inner_type:type_tag*, presence:u8, child_index?:u32]
fn generate_cgrf_encode_option(out: &mut String, inner_ty: &Type) {
    let type_tag_sz = type_tag_size(inner_ty);
    // v2 payload: type_tag + presence(1) + optional child_index(4)
    let payload_none = type_tag_sz + 1;
    let payload_some = type_tag_sz + 1 + 4;

    out.push_str("    ;; Encode option value (CGRF v2)\n");

    // Write CGRF header (16 bytes)
    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
    out.push_str("    i32.store\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_VERSION as i32));
    out.push_str("    i32.store16\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 6\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");

    // Check if we have some or none to determine node count
    // For none: 1 node (just option)
    // For some: 2 nodes (option + inner value)
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $value\n");
    out.push_str("    i32.load8_u\n"); // load tag
    out.push_str("    i32.const 1\n");
    out.push_str("    i32.add\n"); // node_count = 1 + tag (1 for none, 2 for some)
    out.push_str("    i32.store\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n"); // root_index = 0 (the option node)
    out.push_str("    i32.store\n");

    // Write option node at offset 16
    // Node header: kind(1) + flags(1) + reserved(2) + payload_len(4) = 8 bytes
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 16\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_OPTION as i32));
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 17\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 18\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");

    // Payload length: type_tag + 1 (has_value) + 4 (child index) if some
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 20\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $value\n");
    out.push_str("    i32.load8_u\n");
    out.push_str("    if (result i32)\n");
    out.push_str(&format!("      i32.const {}\n", payload_some)); // some
    out.push_str("    else\n");
    out.push_str(&format!("      i32.const {}\n", payload_none)); // none
    out.push_str("    end\n");
    out.push_str("    i32.store\n");

    // Write inner_type tag at offset 24 (start of payload)
    generate_write_type_tag(out, inner_ty, "$out_ptr", 24);

    // Write has_value byte after type tag
    let has_value_offset = 24 + type_tag_sz as i32;
    out.push_str(&format!("    local.get $out_ptr\n"));
    out.push_str(&format!("    i32.const {}\n", has_value_offset));
    out.push_str("    i32.add\n");
    out.push_str("    local.get $value\n");
    out.push_str("    i32.load8_u\n");
    out.push_str("    i32.store8\n");

    // If some, write child index (1) and the inner value node
    let child_index_offset = has_value_offset + 1;
    let inner_node_offset = 24 + payload_some as i32; // right after option payload

    out.push_str("    local.get $value\n");
    out.push_str("    i32.load8_u\n");
    out.push_str("    if\n");
    // Write child index
    out.push_str(&format!("      local.get $out_ptr\n"));
    out.push_str(&format!("      i32.const {}\n", child_index_offset));
    out.push_str("      i32.add\n");
    out.push_str("      i32.const 1\n"); // child index = 1
    out.push_str("      i32.store\n");

    // Write inner value node
    match inner_ty {
        Type::S32 => {
            out.push_str("      ;; Write s32 inner node\n");
            // Node kind
            out.push_str(&format!("      local.get $out_ptr\n"));
            out.push_str(&format!("      i32.const {}\n", inner_node_offset));
            out.push_str("      i32.add\n");
            out.push_str(&format!("      i32.const {}\n", CGRF_S32 as i32));
            out.push_str("      i32.store8\n");
            // Node flags
            out.push_str(&format!("      local.get $out_ptr\n"));
            out.push_str(&format!("      i32.const {}\n", inner_node_offset + 1));
            out.push_str("      i32.add\n");
            out.push_str("      i32.const 0\n");
            out.push_str("      i32.store8\n");
            // Reserved
            out.push_str(&format!("      local.get $out_ptr\n"));
            out.push_str(&format!("      i32.const {}\n", inner_node_offset + 2));
            out.push_str("      i32.add\n");
            out.push_str("      i32.const 0\n");
            out.push_str("      i32.store16\n");
            // Payload length
            out.push_str(&format!("      local.get $out_ptr\n"));
            out.push_str(&format!("      i32.const {}\n", inner_node_offset + 4));
            out.push_str("      i32.add\n");
            out.push_str("      i32.const 4\n"); // payload_len for s32
            out.push_str("      i32.store\n");
            // Payload (the s32 value)
            out.push_str(&format!("      local.get $out_ptr\n"));
            out.push_str(&format!("      i32.const {}\n", inner_node_offset + 8));
            out.push_str("      i32.add\n");
            out.push_str("      local.get $value\n");
            out.push_str("      i32.const 4\n");
            out.push_str("      i32.add\n");
            out.push_str("      i32.load\n"); // load inner s32 value
            out.push_str("      i32.store\n");
        }
        _ => {
            out.push_str("      ;; TODO: handle other inner types\n");
        }
    }
    out.push_str("    end\n");

    // Return bytes written
    // For s32: header(16) + option_node(8 + payload_some) + inner_node(8 + 4)
    let inner_node_size = match inner_ty {
        Type::S32 => 8 + 4, // node header + s32 payload
        _ => 8, // just node header as placeholder
    };
    let total_some = 16 + 8 + payload_some + inner_node_size;
    let total_none = 16 + 8 + payload_none;

    out.push_str("    local.get $value\n");
    out.push_str("    i32.load8_u\n");
    out.push_str("    if (result i32)\n");
    out.push_str(&format!("      i32.const {}\n", total_some));
    out.push_str("    else\n");
    out.push_str(&format!("      i32.const {}\n", total_none));
    out.push_str("    end\n");
}

/// Generate WAT code to encode a list value to CGRF at $out_ptr.
/// List layout in wisp: [len: i32, cap: i32, data_ptr: i32]
/// CGRF v2 List payload: [elem_type:type_tag*, count:u32, child_indices:u32*]
fn generate_cgrf_encode_list(
    out: &mut String,
    elem_ty: &Type,
    _records: &HashMap<String, RecordDef>,
    _variants: &HashMap<String, VariantDef>,
) {
    // For now, only support list<s32>
    if !matches!(elem_ty, Type::S32) {
        out.push_str("    ;; TODO: encode list of non-s32 elements\n");
        out.push_str("    i32.const -1\n");
        return;
    }

    let type_tag_sz = type_tag_size(elem_ty);
    // v2 payload: type_tag + count(4) + child_indices(4 * len)
    let payload_base = type_tag_sz + 4; // type_tag + count

    out.push_str("    ;; Encode list<s32> value (CGRF v2)\n");

    // Write CGRF header
    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
    out.push_str("    i32.store\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_VERSION as i32));
    out.push_str("    i32.store16\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 6\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");

    // Node count = 1 (list node) + len (element nodes)
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $value\n");
    out.push_str("    i32.load\n"); // list.len
    out.push_str("    i32.const 1\n");
    out.push_str("    i32.add\n"); // 1 + len
    out.push_str("    i32.store\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n"); // root = list node
    out.push_str("    i32.store\n");

    // Write list node at offset 16
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 16\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_LIST as i32));
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 17\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 18\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");

    // Payload length = type_tag + count(4) + child_indices(4*len)
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 20\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $value\n");
    out.push_str("    i32.load\n"); // len
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.mul\n");
    out.push_str(&format!("    i32.const {}\n", payload_base)); // type_tag + count
    out.push_str("    i32.add\n");
    out.push_str("    i32.store\n");

    // Write elem_type tag at offset 24 (start of payload)
    generate_write_type_tag(out, elem_ty, "$out_ptr", 24);

    // Write element count after type tag
    let count_offset = 24 + type_tag_sz as i32;
    out.push_str(&format!("    local.get $out_ptr\n"));
    out.push_str(&format!("    i32.const {}\n", count_offset));
    out.push_str("    i32.add\n");
    out.push_str("    local.get $value\n");
    out.push_str("    i32.load\n");
    out.push_str("    i32.store\n");

    // Write child indices after count
    let child_indices_offset = count_offset + 4;
    // Use a loop to write each child index
    out.push_str("    local.get $value\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $len\n");
    out.push_str("    local.get $value\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n"); // data_ptr
    out.push_str("    local.set $data_ptr\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    local.set $i\n");

    // Calculate where element nodes start: header(16) + node_header(8) + payload_base + 4*len
    // = 24 + type_tag_sz + 4 + 4*len
    out.push_str(&format!("    i32.const {}\n", child_indices_offset));
    out.push_str("    local.get $len\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.mul\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.set $node_offset\n");

    out.push_str("    block $break\n");
    out.push_str("      loop $loop\n");
    out.push_str("        local.get $i\n");
    out.push_str("        local.get $len\n");
    out.push_str("        i32.ge_u\n");
    out.push_str("        br_if $break\n");

    // Write child index (1 + i) at child_indices_offset + 4*i
    out.push_str("        local.get $out_ptr\n");
    out.push_str(&format!("        i32.const {}\n", child_indices_offset));
    out.push_str("        local.get $i\n");
    out.push_str("        i32.const 4\n");
    out.push_str("        i32.mul\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.get $i\n");
    out.push_str("        i32.const 1\n");
    out.push_str("        i32.add\n"); // child index = 1 + i
    out.push_str("        i32.store\n");

    // Write s32 node at node_offset + 12*i
    out.push_str("        local.get $out_ptr\n");
    out.push_str("        local.get $node_offset\n");
    out.push_str("        local.get $i\n");
    out.push_str("        i32.const 12\n");
    out.push_str("        i32.mul\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.add\n");
    out.push_str(&format!("        i32.const {}\n", CGRF_S32 as i32));
    out.push_str("        i32.store8\n");

    // Write node flags, reserved, payload_len
    out.push_str("        local.get $out_ptr\n");
    out.push_str("        local.get $node_offset\n");
    out.push_str("        local.get $i\n");
    out.push_str("        i32.const 12\n");
    out.push_str("        i32.mul\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.const 1\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.const 0\n");
    out.push_str("        i32.store8\n");

    out.push_str("        local.get $out_ptr\n");
    out.push_str("        local.get $node_offset\n");
    out.push_str("        local.get $i\n");
    out.push_str("        i32.const 12\n");
    out.push_str("        i32.mul\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.const 2\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.const 0\n");
    out.push_str("        i32.store16\n");

    out.push_str("        local.get $out_ptr\n");
    out.push_str("        local.get $node_offset\n");
    out.push_str("        local.get $i\n");
    out.push_str("        i32.const 12\n");
    out.push_str("        i32.mul\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.const 4\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.const 4\n"); // payload_len = 4
    out.push_str("        i32.store\n");

    // Write s32 value
    out.push_str("        local.get $out_ptr\n");
    out.push_str("        local.get $node_offset\n");
    out.push_str("        local.get $i\n");
    out.push_str("        i32.const 12\n");
    out.push_str("        i32.mul\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.const 8\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.get $data_ptr\n");
    out.push_str("        local.get $i\n");
    out.push_str("        i32.const 4\n");
    out.push_str("        i32.mul\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n"); // load element value
    out.push_str("        i32.store\n");

    out.push_str("        local.get $i\n");
    out.push_str("        i32.const 1\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.set $i\n");
    out.push_str("        br $loop\n");
    out.push_str("      end\n");
    out.push_str("    end\n");

    // Return bytes written: header(16) + node_header(8) + payload_base + 4*len + element_nodes(12*len)
    // = 24 + type_tag_sz + 4 + 4*len + 12*len = child_indices_offset + 16*len
    out.push_str(&format!("    i32.const {}\n", child_indices_offset));
    out.push_str("    local.get $len\n");
    out.push_str("    i32.const 16\n");
    out.push_str("    i32.mul\n");
    out.push_str("    i32.add\n");
}

/// Generate WAT code to encode a record value to CGRF at $out_ptr.
/// Record layout in wisp: fields stored sequentially at known offsets
/// CGRF v2 Record payload: [type_name_len:u32, type_name:utf8, field_count:u32,
///                          field_names:(len:u32, name:utf8)*, child_indices:u32*]
fn generate_cgrf_encode_record(
    out: &mut String,
    name: &str,
    records: &HashMap<String, RecordDef>,
    _variants: &HashMap<String, VariantDef>,
) {
    let record_def = match records.get(name) {
        Some(r) => r,
        None => {
            out.push_str(&format!("    ;; ERROR: unknown record '{}'\n", name));
            out.push_str("    i32.const -1\n");
            return;
        }
    };

    // For simplicity, only support records with scalar fields for now
    for field in &record_def.fields {
        if !matches!(field.ty, Type::S32 | Type::S64 | Type::F32 | Type::F64) {
            out.push_str("    ;; TODO: encode record with non-scalar fields\n");
            out.push_str("    i32.const -1\n");
            return;
        }
    }

    let field_count = record_def.fields.len();

    // Calculate field names size
    let field_names_size: usize = record_def
        .fields
        .iter()
        .map(|f| 4 + f.name.len())
        .sum();

    // CGRF v2 Record payload layout:
    // - type_name_len: 4 bytes
    // - type_name: N bytes
    // - field_count: 4 bytes
    // - field_names: (len:u32 + name:utf8) for each field
    // - child_indices: 4 * field_count bytes
    let payload_len = 4 + name.len() + 4 + field_names_size + 4 * field_count;

    out.push_str(&format!(
        "    ;; Encode record '{}' with {} fields (CGRF v2)\n",
        name, field_count
    ));

    // Write CGRF header
    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
    out.push_str("    i32.store\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_VERSION as i32));
    out.push_str("    i32.store16\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 6\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");

    // Node count = 1 (record) + field_count
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", 1 + field_count));
    out.push_str("    i32.store\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n"); // root = record node
    out.push_str("    i32.store\n");

    // Write record node at offset 16
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 16\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_RECORD as i32));
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 17\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 18\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");

    // Payload length
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 20\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", payload_len));
    out.push_str("    i32.store\n");

    // Payload starts at offset 24
    let mut payload_offset = 24;

    // Write type_name_len
    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", payload_offset));
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", name.len()));
    out.push_str("    i32.store\n");
    payload_offset += 4;

    // Write type_name bytes
    for (i, byte) in name.bytes().enumerate() {
        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", payload_offset + i));
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", byte));
        out.push_str("    i32.store8\n");
    }
    payload_offset += name.len();

    // Write field_count
    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", payload_offset));
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", field_count));
    out.push_str("    i32.store\n");
    payload_offset += 4;

    // Write field names
    for field in &record_def.fields {
        // Write field name length
        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", payload_offset));
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", field.name.len()));
        out.push_str("    i32.store\n");
        payload_offset += 4;

        // Write field name bytes
        for (i, byte) in field.name.bytes().enumerate() {
            out.push_str("    local.get $out_ptr\n");
            out.push_str(&format!("    i32.const {}\n", payload_offset + i));
            out.push_str("    i32.add\n");
            out.push_str(&format!("    i32.const {}\n", byte));
            out.push_str("    i32.store8\n");
        }
        payload_offset += field.name.len();
    }

    // Write child indices (1, 2, 3, ...)
    for i in 0..field_count {
        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", payload_offset + 4 * i));
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", 1 + i)); // child index
        out.push_str("    i32.store\n");
    }
    payload_offset += 4 * field_count;

    // Write field nodes starting after the record node payload
    let mut node_offset = payload_offset;
    for (i, field) in record_def.fields.iter().enumerate() {
        let field_offset = record_def.field_offset(i);
        let (cgrf_kind, field_payload_size) = match field.ty {
            Type::S32 => (CGRF_S32, 4),
            Type::S64 => (CGRF_S64, 8),
            Type::F32 => (CGRF_F32, 4),
            Type::F64 => (CGRF_F64, 8),
            _ => continue,
        };

        // Write node kind
        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", node_offset));
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", cgrf_kind as i32));
        out.push_str("    i32.store8\n");

        // Flags
        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", node_offset + 1));
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store8\n");

        // Reserved
        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", node_offset + 2));
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store16\n");

        // Payload length
        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", node_offset + 4));
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", field_payload_size));
        out.push_str("    i32.store\n");

        // Load field value from record and store in payload
        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", node_offset + 8));
        out.push_str("    i32.add\n");
        out.push_str("    local.get $value\n");
        if field_offset > 0 {
            out.push_str(&format!("    i32.const {}\n", field_offset));
            out.push_str("    i32.add\n");
        }
        let load_instr = match field.ty {
            Type::S32 => "i32.load",
            Type::S64 => "i64.load",
            Type::F32 => "f32.load",
            Type::F64 => "f64.load",
            _ => "i32.load",
        };
        out.push_str(&format!("    {}\n", load_instr));
        let store_instr = match field.ty {
            Type::S32 => "i32.store",
            Type::S64 => "i64.store",
            Type::F32 => "f32.store",
            Type::F64 => "f64.store",
            _ => "i32.store",
        };
        out.push_str(&format!("    {}\n", store_instr));

        node_offset += 8 + field_payload_size;
    }

    // Return total bytes written
    out.push_str(&format!("    i32.const {}\n", node_offset));
}

/// Generate WAT code to encode a variant value to CGRF at $out_ptr.
/// Variant layout in wisp: [tag: i32, payload...]
/// CGRF v2 Variant payload: [type_name_len:u32, type_name:utf8, case_name_len:u32, case_name:utf8,
///                          tag:u32, payload_count:u32, child_indices:u32*]
fn generate_cgrf_encode_variant(
    out: &mut String,
    name: &str,
    _records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
) {
    let variant_def = match variants.get(name) {
        Some(v) => v,
        None => {
            out.push_str(&format!("    ;; ERROR: unknown variant '{}'\n", name));
            out.push_str("    i32.const -1\n");
            return;
        }
    };

    // Analyze variant cases
    let all_no_payload = variant_def.cases.iter().all(|c| c.payload.is_empty());
    let all_have_payload = variant_def.cases.iter().all(|c| !c.payload.is_empty());

    // For simplicity, only support variants with single scalar payload or no payload
    let mut all_simple = true;
    for case in &variant_def.cases {
        if case.payload.len() > 1 {
            all_simple = false;
            break;
        }
        if case.payload.len() == 1 {
            if !matches!(case.payload[0], Type::S32 | Type::S64 | Type::F32 | Type::F64) {
                all_simple = false;
                break;
            }
        }
    }

    if !all_simple {
        out.push_str("    ;; TODO: encode variant with complex payloads\n");
        out.push_str("    i32.const -1\n");
        return;
    }

    // Find the max case name length for buffer sizing
    let max_case_name_len = variant_def.cases.iter().map(|c| c.name.len()).max().unwrap_or(0);

    out.push_str(&format!("    ;; Encode variant '{}' (CGRF v2)\n", name));

    if all_no_payload {
        // Simple case: no cases have payloads
        // CGRF v2 Variant payload: type_name_len + type_name + case_name_len + case_name + tag + payload_count
        // Payload size varies based on which case is active (case_name has different lengths)

        // Write CGRF header
        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
        out.push_str("    i32.store\n");

        out.push_str("    local.get $out_ptr\n");
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_VERSION as i32));
        out.push_str("    i32.store16\n");

        out.push_str("    local.get $out_ptr\n");
        out.push_str("    i32.const 6\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store16\n");

        // node_count = 1 (just the variant node)
        out.push_str("    local.get $out_ptr\n");
        out.push_str("    i32.const 8\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 1\n");
        out.push_str("    i32.store\n");

        // root_index = 0
        out.push_str("    local.get $out_ptr\n");
        out.push_str("    i32.const 12\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store\n");

        // Write variant node at offset 16
        out.push_str("    local.get $out_ptr\n");
        out.push_str("    i32.const 16\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_VARIANT as i32));
        out.push_str("    i32.store8\n");

        out.push_str("    local.get $out_ptr\n");
        out.push_str("    i32.const 17\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store8\n");

        out.push_str("    local.get $out_ptr\n");
        out.push_str("    i32.const 18\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store16\n");

        // Payload length is written inside each case branch (varies by case_name length)
        // Payload starts at offset 24
        let mut payload_offset = 24;

        // Write type_name_len
        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", payload_offset));
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", name.len()));
        out.push_str("    i32.store\n");
        payload_offset += 4;

        // Write type_name bytes
        for (i, byte) in name.bytes().enumerate() {
            out.push_str("    local.get $out_ptr\n");
            out.push_str(&format!("    i32.const {}\n", payload_offset + i));
            out.push_str("    i32.add\n");
            out.push_str(&format!("    i32.const {}\n", byte));
            out.push_str("    i32.store8\n");
        }
        payload_offset += name.len();

        // Read tag from value to determine case_name
        out.push_str("    local.get $value\n");
        out.push_str("    i32.load\n");
        out.push_str("    local.set $tag\n");

        // Write case_name based on tag using if/else chain
        // Each branch writes: case_name_len, case_name, tag, payload_count
        // And sets payload_len and returns the correct total size
        let case_name_len_offset = payload_offset;

        // Generate if/else chain for case names
        // Each branch is a complete expression that returns (result i32)
        for (i, case) in variant_def.cases.iter().enumerate() {
            // Calculate this case's payload_len and total size
            // payload = type_name_len(4) + type_name + case_name_len(4) + case_name + tag(4) + payload_count(4)
            let case_payload_len = 4 + name.len() + 4 + case.name.len() + 4 + 4;
            let case_total_size = 16 + 8 + case_payload_len;
            let case_name_start = case_name_len_offset + 4;
            let tag_offset = case_name_start + case.name.len();
            let payload_count_offset = tag_offset + 4;

            if i == 0 {
                out.push_str("    local.get $tag\n");
                out.push_str("    i32.const 0\n");
                out.push_str("    i32.eq\n");
                out.push_str("    if (result i32)\n");
            } else {
                out.push_str("    else\n");
                if i < variant_def.cases.len() - 1 {
                    out.push_str("      local.get $tag\n");
                    out.push_str(&format!("      i32.const {}\n", i));
                    out.push_str("      i32.eq\n");
                    out.push_str("      if (result i32)\n");
                }
            }

            // Write payload_len for this case (at offset 20)
            out.push_str("      local.get $out_ptr\n");
            out.push_str("      i32.const 20\n");
            out.push_str("      i32.add\n");
            out.push_str(&format!("      i32.const {}\n", case_payload_len));
            out.push_str("      i32.store\n");

            // Write case_name_len
            out.push_str("      local.get $out_ptr\n");
            out.push_str(&format!("      i32.const {}\n", case_name_len_offset));
            out.push_str("      i32.add\n");
            out.push_str(&format!("      i32.const {}\n", case.name.len()));
            out.push_str("      i32.store\n");

            // Write case_name bytes
            for (j, byte) in case.name.bytes().enumerate() {
                out.push_str("      local.get $out_ptr\n");
                out.push_str(&format!("      i32.const {}\n", case_name_start + j));
                out.push_str("      i32.add\n");
                out.push_str(&format!("      i32.const {}\n", byte));
                out.push_str("      i32.store8\n");
            }

            // Write tag immediately after case_name
            out.push_str("      local.get $out_ptr\n");
            out.push_str(&format!("      i32.const {}\n", tag_offset));
            out.push_str("      i32.add\n");
            out.push_str(&format!("      i32.const {}\n", i)); // tag value
            out.push_str("      i32.store\n");

            // Write payload_count = 0
            out.push_str("      local.get $out_ptr\n");
            out.push_str(&format!("      i32.const {}\n", payload_count_offset));
            out.push_str("      i32.add\n");
            out.push_str("      i32.const 0\n");
            out.push_str("      i32.store\n");

            // Return this case's total size
            out.push_str(&format!("      i32.const {}\n", case_total_size));
        }

        // Close all the if/else blocks
        // We have (cases.len() - 1) nested if statements
        for _ in 0..(variant_def.cases.len() - 1) {
            out.push_str("    end\n");
        }
    } else if all_have_payload {
        // All cases have payloads
        // CGRF v2: payload node first (depth-first), then variant node

        // Variant payload size: 4 + name.len() + 4 + max_case_name_len + 4 + 4 + 4 (one child index)
        let variant_payload_len = 4 + name.len() + 4 + max_case_name_len + 4 + 4 + 4;

        // Write CGRF header
        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
        out.push_str("    i32.store\n");

        out.push_str("    local.get $out_ptr\n");
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_VERSION as i32));
        out.push_str("    i32.store16\n");

        out.push_str("    local.get $out_ptr\n");
        out.push_str("    i32.const 6\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store16\n");

        // node_count = 2 (payload + variant)
        out.push_str("    local.get $out_ptr\n");
        out.push_str("    i32.const 8\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 2\n");
        out.push_str("    i32.store\n");

        // root_index = 1 (variant node is after payload, depth-first)
        out.push_str("    local.get $out_ptr\n");
        out.push_str("    i32.const 12\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 1\n");
        out.push_str("    i32.store\n");

        // Write payload node at offset 16 (depth-first: children first)
        out.push_str("    local.get $out_ptr\n");
        out.push_str("    i32.const 16\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_S32 as i32));
        out.push_str("    i32.store8\n");

        out.push_str("    local.get $out_ptr\n");
        out.push_str("    i32.const 17\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store8\n");

        out.push_str("    local.get $out_ptr\n");
        out.push_str("    i32.const 18\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store16\n");

        out.push_str("    local.get $out_ptr\n");
        out.push_str("    i32.const 20\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 4\n"); // payload_len for s32
        out.push_str("    i32.store\n");

        // Write payload value at offset 24
        out.push_str("    local.get $out_ptr\n");
        out.push_str("    i32.const 24\n");
        out.push_str("    i32.add\n");
        out.push_str("    local.get $value\n");
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.load\n");
        out.push_str("    i32.store\n");

        // Write variant node at offset 28 (16 + 12)
        let variant_node_offset = 28;
        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", variant_node_offset));
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", CGRF_VARIANT as i32));
        out.push_str("    i32.store8\n");

        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", variant_node_offset + 1));
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store8\n");

        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", variant_node_offset + 2));
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store16\n");

        // Variant payload length
        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", variant_node_offset + 4));
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", variant_payload_len));
        out.push_str("    i32.store\n");

        // Variant payload starts at variant_node_offset + 8
        let mut payload_offset = variant_node_offset + 8;

        // Write type_name_len
        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", payload_offset));
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", name.len()));
        out.push_str("    i32.store\n");
        payload_offset += 4;

        // Write type_name bytes
        for (i, byte) in name.bytes().enumerate() {
            out.push_str("    local.get $out_ptr\n");
            out.push_str(&format!("    i32.const {}\n", payload_offset + i));
            out.push_str("    i32.add\n");
            out.push_str(&format!("    i32.const {}\n", byte));
            out.push_str("    i32.store8\n");
        }
        payload_offset += name.len();

        // Read tag from value
        out.push_str("    local.get $value\n");
        out.push_str("    i32.load\n");
        out.push_str("    local.set $tag\n");

        // Write case_name based on tag using if/else chain
        let case_name_len_offset = payload_offset;
        payload_offset += 4;
        let case_name_start = payload_offset;

        // Generate if/else chain for case names
        for (i, case) in variant_def.cases.iter().enumerate() {
            if i == 0 {
                out.push_str("    local.get $tag\n");
                out.push_str("    i32.const 0\n");
                out.push_str("    i32.eq\n");
                out.push_str("    if\n");
            } else {
                out.push_str("    else\n");
                if i < variant_def.cases.len() - 1 {
                    out.push_str(&format!("      local.get $tag\n"));
                    out.push_str(&format!("      i32.const {}\n", i));
                    out.push_str("      i32.eq\n");
                    out.push_str("      if\n");
                }
            }

            // Write case_name_len
            out.push_str("      local.get $out_ptr\n");
            out.push_str(&format!("      i32.const {}\n", case_name_len_offset));
            out.push_str("      i32.add\n");
            out.push_str(&format!("      i32.const {}\n", case.name.len()));
            out.push_str("      i32.store\n");

            // Write case_name bytes
            for (j, byte) in case.name.bytes().enumerate() {
                out.push_str("      local.get $out_ptr\n");
                out.push_str(&format!("      i32.const {}\n", case_name_start + j));
                out.push_str("      i32.add\n");
                out.push_str(&format!("      i32.const {}\n", byte));
                out.push_str("      i32.store8\n");
            }
        }

        // Close all the if/else blocks
        // We have (cases.len() - 1) nested if statements
        // (the last case has no 'if' because it's in the final 'else')
        for _ in 0..(variant_def.cases.len() - 1) {
            out.push_str("    end\n");
        }

        payload_offset += max_case_name_len;

        // Write tag
        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", payload_offset));
        out.push_str("    i32.add\n");
        out.push_str("    local.get $tag\n");
        out.push_str("    i32.store\n");
        payload_offset += 4;

        // Write payload_count = 1
        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", payload_offset));
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 1\n");
        out.push_str("    i32.store\n");
        payload_offset += 4;

        // Write child_index = 0
        out.push_str("    local.get $out_ptr\n");
        out.push_str(&format!("    i32.const {}\n", payload_offset));
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store\n");
        payload_offset += 4;

        // Return bytes written
        out.push_str(&format!("    i32.const {}\n", payload_offset));
    } else {
        // Mixed case: need runtime check
        // For now, use a simplified approach - TODO: implement proper runtime branching
        out.push_str("    ;; TODO: encode variant with mixed payload cases\n");
        out.push_str("    i32.const -1\n");
    }
}

/// Generate WAT code to encode a result value to CGRF at $out_ptr.
/// CGRF v2 Result payload: [ok_type:type_tag*, err_type:type_tag*, tag:u32, has_payload:u8, child_index?:u32]
fn generate_cgrf_encode_result(
    out: &mut String,
    ok_ty: &Type,
    err_ty: &Type,
    _records: &HashMap<String, RecordDef>,
    _variants: &HashMap<String, VariantDef>,
) {
    // Result is encoded as CGRF_RESULT (0x14) with type tags
    // Memory layout: [tag: i32 (0 or 1), payload...]

    // For simplicity, only support scalar ok/err types for now
    if !matches!(ok_ty, Type::S32 | Type::S64 | Type::F32 | Type::F64)
        || !matches!(err_ty, Type::S32 | Type::S64 | Type::F32 | Type::F64)
    {
        out.push_str("    ;; TODO: encode result with non-scalar types\n");
        out.push_str("    i32.const -1\n");
        return;
    }

    // Calculate type tag sizes
    let ok_type_tag_size = type_tag_size(ok_ty);
    let err_type_tag_size = type_tag_size(err_ty);

    // Result payload: ok_type_tag + err_type_tag + tag(4) + has_payload(1) + child_index(4)
    let result_payload_len = ok_type_tag_size + err_type_tag_size + 4 + 1 + 4;

    out.push_str("    ;; Encode result value (CGRF v2)\n");

    // Write CGRF header
    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
    out.push_str("    i32.store\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_VERSION as i32));
    out.push_str("    i32.store16\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 6\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");

    // Node count = 2 (result + payload)
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 2\n");
    out.push_str("    i32.store\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n"); // root = result node
    out.push_str("    i32.store\n");

    // Write result node at offset 16
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 16\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", CGRF_RESULT as i32));
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 17\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 18\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");

    // Payload length
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    i32.const 20\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", result_payload_len));
    out.push_str("    i32.store\n");

    // Payload starts at offset 24
    let mut payload_offset: i32 = 24;

    // Write ok_type tag
    let ok_tag_written = generate_write_type_tag(out, ok_ty, "$out_ptr", payload_offset);
    payload_offset += ok_tag_written as i32;

    // Write err_type tag
    let err_tag_written = generate_write_type_tag(out, err_ty, "$out_ptr", payload_offset);
    payload_offset += err_tag_written as i32;

    // Write tag (read from value's discriminant: 0=ok, 1=err)
    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", payload_offset));
    out.push_str("    i32.add\n");
    out.push_str("    local.get $value\n");
    out.push_str("    i32.load\n");
    out.push_str("    i32.store\n");
    payload_offset += 4;

    // Write has_payload = 1
    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", payload_offset));
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 1\n");
    out.push_str("    i32.store8\n");
    payload_offset += 1;

    // Write child index = 1
    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", payload_offset));
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 1\n");
    out.push_str("    i32.store\n");
    payload_offset += 4;

    // Write payload node
    // Determine type based on tag (ok=0 uses ok_ty, err=1 uses err_ty)
    // For simplicity, assume both are same size (s32 for now)
    let (cgrf_kind, value_payload_size) = match ok_ty {
        Type::S32 => (CGRF_S32, 4),
        Type::S64 => (CGRF_S64, 8),
        Type::F32 => (CGRF_F32, 4),
        Type::F64 => (CGRF_F64, 8),
        _ => (CGRF_S32, 4),
    };

    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", payload_offset));
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", cgrf_kind as i32));
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", payload_offset + 1));
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store8\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", payload_offset + 2));
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");

    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", payload_offset + 4));
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", value_payload_size));
    out.push_str("    i32.store\n");

    // Write payload value
    out.push_str("    local.get $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", payload_offset + 8));
    out.push_str("    i32.add\n");
    out.push_str("    local.get $value\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    let load_instr = match ok_ty {
        Type::S32 => "i32.load",
        Type::S64 => "i64.load",
        Type::F32 => "f32.load",
        Type::F64 => "f64.load",
        _ => "i32.load",
    };
    out.push_str(&format!("    {}\n", load_instr));
    let store_instr = match ok_ty {
        Type::S32 => "i32.store",
        Type::S64 => "i64.store",
        Type::F32 => "f32.store",
        Type::F64 => "f64.store",
        _ => "i32.store",
    };
    out.push_str(&format!("    {}\n", store_instr));

    // Return bytes written
    let total_bytes = payload_offset + 8 + value_payload_size as i32;
    out.push_str(&format!("    i32.const {}\n", total_bytes));
}

// =============================================================================
// CGRF Decoding functions (for export parameter decoding)
// =============================================================================

/// Generate WAT code to decode an s32 from CGRF input buffer.
/// Assumes $in_ptr contains the input buffer pointer.
/// Result is left on the stack.
fn generate_cgrf_decode_s32(out: &mut String) {
    // CGRF layout:
    // - Header: 16 bytes (magic, version, flags, node_count, root_index)
    // - Root node at offset 16: 8 bytes header (kind, flags, reserved, payload_len)
    // - Payload at offset 24: 4 bytes (s32 value)
    out.push_str("    ;; Decode s32 from CGRF\n");
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 24\n"); // 16 (header) + 8 (node header) = 24
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
}

/// Generate WAT code to decode an s64 from CGRF input buffer.
fn generate_cgrf_decode_s64(out: &mut String) {
    out.push_str("    ;; Decode s64 from CGRF\n");
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 24\n");
    out.push_str("    i32.add\n");
    out.push_str("    i64.load\n");
}

/// Generate WAT code to decode an f32 from CGRF input buffer.
fn generate_cgrf_decode_f32(out: &mut String) {
    out.push_str("    ;; Decode f32 from CGRF\n");
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 24\n");
    out.push_str("    i32.add\n");
    out.push_str("    f32.load\n");
}

/// Generate WAT code to decode an f64 from CGRF input buffer.
fn generate_cgrf_decode_f64(out: &mut String) {
    out.push_str("    ;; Decode f64 from CGRF\n");
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 24\n");
    out.push_str("    i32.add\n");
    out.push_str("    f64.load\n");
}

/// Generate WAT code to decode a string from CGRF input buffer.
/// Returns a pointer to a wisp string (len: i32, data: bytes) on the stack.
/// Allocates memory for the string on the heap.
fn generate_cgrf_decode_string(out: &mut String) {
    // CGRF string node:
    // - Header at offset 16: kind=0x06, flags, reserved, payload_len
    // - Payload at offset 24: length (u32), then UTF-8 bytes
    //
    // Wisp string layout: (len: i32, data: bytes...)
    // We need to allocate heap space and copy the string data

    out.push_str("    ;; Decode string from CGRF\n");

    // Read string length from CGRF payload (offset 24)
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 24\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $str_len\n");

    // Allocate space for wisp string: 4 bytes for length + string data
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $str_ptr\n");

    // Update heap pointer: heap_ptr += 4 + str_len
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $str_len\n");
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // Write length to wisp string
    out.push_str("    local.get $str_ptr\n");
    out.push_str("    local.get $str_len\n");
    out.push_str("    i32.store\n");

    // Copy string data from CGRF to wisp string
    // Source: $in_ptr + 28 (24 for header+node + 4 for length prefix in payload)
    // Dest: $str_ptr + 4
    out.push_str("    local.get $str_ptr\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n"); // dest
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 28\n");
    out.push_str("    i32.add\n"); // src
    out.push_str("    local.get $str_len\n"); // len
    out.push_str("    memory.copy\n");

    // Return the wisp string pointer
    out.push_str("    local.get $str_ptr\n");
}

/// Generate WAT code to decode a record from CGRF input buffer.
/// CGRF record layout (depth-first encoding):
/// - Field nodes come first (node 0, 1, 2, ...)
/// - Record node comes last (root)
///
/// For v2 Record([S32(10), S32(20)]):
/// - Record node at offset 16 with v2 payload
///   - Payload: [type_name_len, type_name, field_count, field_names, child_indices]
/// - Field nodes start after record node payload
///
/// We allocate heap space for the wisp record and copy field values into it.
fn generate_cgrf_decode_record(
    out: &mut String,
    rec_name: &str,
    param_name: &str,
    records: &HashMap<String, RecordDef>,
) {
    let record_def = match records.get(rec_name) {
        Some(r) => r,
        None => {
            out.push_str(&format!(
                "    ;; ERROR: unknown record type '{}'\n",
                rec_name
            ));
            out.push_str("    i32.const 0\n");
            out.push_str(&format!("    local.set $param_{}\n", param_name));
            return;
        }
    };

    out.push_str(&format!("    ;; Decode record '{}' (CGRF v2)\n", rec_name));

    // Calculate total size needed for wisp record
    let record_size: usize = record_def
        .fields
        .iter()
        .map(|f| type_size(&f.ty))
        .sum();

    // Allocate heap space
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $rec_ptr\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str(&format!("    i32.const {}\n", record_size));
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // In CGRF, children are encoded BEFORE their parent.
    // For a record with N scalar fields, the layout is:
    // - Header: 16 bytes (magic, version, flags, node_count, root)
    // - Node 0: first child value
    // - Node 1: second child value
    // - ...
    // - Node N-1: last child value
    // - Node N: the record node (root)
    //
    // So children are at the BEGINNING of the node array, not after the record.

    // Child nodes start at offset 16 (right after CGRF header)
    let mut child_node_offset = 16;

    for (i, field) in record_def.fields.iter().enumerate() {
        let wisp_field_offset = record_def.field_offset(i);

        out.push_str(&format!("    ;; Field {} '{}' at wisp offset {}\n", i, field.name, wisp_field_offset));

        // Calculate CGRF node payload offset (skip 8-byte node header)
        let payload_offset = child_node_offset + 8;

        match &field.ty {
            Type::S32 => {
                // Load from CGRF child node
                out.push_str("    local.get $in_ptr\n");
                out.push_str(&format!("    i32.const {}\n", payload_offset));
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
                out.push_str("    local.set $field_val\n");

                // Store to wisp record
                out.push_str("    local.get $rec_ptr\n");
                if wisp_field_offset > 0 {
                    out.push_str(&format!("    i32.const {}\n", wisp_field_offset));
                    out.push_str("    i32.add\n");
                }
                out.push_str("    local.get $field_val\n");
                out.push_str("    i32.store\n");

                child_node_offset += 12; // 8 header + 4 payload for S32
            }
            Type::S64 => {
                out.push_str("    local.get $rec_ptr\n");
                if wisp_field_offset > 0 {
                    out.push_str(&format!("    i32.const {}\n", wisp_field_offset));
                    out.push_str("    i32.add\n");
                }
                out.push_str("    local.get $in_ptr\n");
                out.push_str(&format!("    i32.const {}\n", payload_offset));
                out.push_str("    i32.add\n");
                out.push_str("    i64.load\n");
                out.push_str("    i64.store\n");

                child_node_offset += 16; // 8 header + 8 payload for S64
            }
            Type::F32 => {
                out.push_str("    local.get $rec_ptr\n");
                if wisp_field_offset > 0 {
                    out.push_str(&format!("    i32.const {}\n", wisp_field_offset));
                    out.push_str("    i32.add\n");
                }
                out.push_str("    local.get $in_ptr\n");
                out.push_str(&format!("    i32.const {}\n", payload_offset));
                out.push_str("    i32.add\n");
                out.push_str("    f32.load\n");
                out.push_str("    f32.store\n");

                child_node_offset += 12; // 8 header + 4 payload for F32
            }
            Type::F64 => {
                out.push_str("    local.get $rec_ptr\n");
                if wisp_field_offset > 0 {
                    out.push_str(&format!("    i32.const {}\n", wisp_field_offset));
                    out.push_str("    i32.add\n");
                }
                out.push_str("    local.get $in_ptr\n");
                out.push_str(&format!("    i32.const {}\n", payload_offset));
                out.push_str("    i32.add\n");
                out.push_str("    f64.load\n");
                out.push_str("    f64.store\n");

                child_node_offset += 16; // 8 header + 8 payload for F64
            }
            _ => {
                out.push_str(&format!("    ;; TODO: decode non-scalar field '{}'\n", field.name));
                child_node_offset += 12; // Assume 12 as fallback
            }
        }
    }

    // Return the record pointer
    out.push_str("    local.get $rec_ptr\n");
    out.push_str(&format!("    local.set $param_{}\n", param_name));
}

/// Generate WAT code to decode an option from CGRF input buffer.
/// CGRF option layout:
/// - For Some: child node first, then option node
/// - For None: just option node
///
/// Option node payload: [has_value: u8, child_index: u32 (if has_value)]
fn generate_cgrf_decode_option(
    out: &mut String,
    inner_ty: &Type,
    param_name: &str,
) {
    out.push_str("    ;; Decode option\n");

    // Wisp option layout: [tag: i32 (0=none, 1=some), payload if some]
    let inner_size = type_size(inner_ty);
    let option_size = 4 + inner_size; // tag + optional payload

    // Allocate heap space
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $rec_ptr\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str(&format!("    i32.const {}\n", option_size));
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // For a single option param:
    // - If Some: node 0 is the inner value, node 1 is the option (root)
    // - If None: node 0 is the option (root)
    //
    // We need to check the option node's payload to determine which case.
    // The option node is the root, at variable offset depending on whether there's a child.
    //
    // Actually, we can read the root_index from header (offset 12) to find the option node.
    // Then read its has_value byte from the payload.

    // Read root_index
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $field_val\n"); // reuse as root_index

    // Calculate option node offset: 16 + root_index * node_size
    // For option node, we need to scan to find it. For simplicity, assume:
    // - If root_index == 0, option is at offset 16 (None case, or Some with 0-sized inner)
    // - If root_index == 1, there's one child node first

    // Read has_value from option node payload
    // Option node: header(8) + payload(1 byte has_value + optional 4 byte child_index)
    // If Some(scalar): child at node 0, option at node 1
    //   - Node 0 at offset 16 (inner value)
    //   - Node 1 at offset 16 + inner_node_size (option)

    // For simplicity, check node count to determine Some vs None
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n"); // node_count

    // If node_count == 1, it's None (only option node)
    // If node_count == 2, it's Some (child + option node)
    out.push_str("    i32.const 1\n");
    out.push_str("    i32.eq\n");
    out.push_str("    (if\n");
    out.push_str("      (then\n");
    out.push_str("        ;; None case: store tag = 0\n");
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        i32.const 0\n");
    out.push_str("        i32.store\n");
    out.push_str("      )\n");
    out.push_str("      (else\n");
    out.push_str("        ;; Some case: store tag = 1 and decode inner value\n");
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        i32.const 1\n");
    out.push_str("        i32.store\n");

    // Inner value is at node 0, offset 16, payload at 24
    match inner_ty {
        Type::S32 => {
            out.push_str("        local.get $rec_ptr\n");
            out.push_str("        i32.const 4\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        i32.const 24\n");
            out.push_str("        i32.add\n");
            out.push_str("        i32.load\n");
            out.push_str("        i32.store\n");
        }
        Type::S64 => {
            out.push_str("        local.get $rec_ptr\n");
            out.push_str("        i32.const 4\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        i32.const 24\n");
            out.push_str("        i32.add\n");
            out.push_str("        i64.load\n");
            out.push_str("        i64.store\n");
        }
        Type::F32 => {
            out.push_str("        local.get $rec_ptr\n");
            out.push_str("        i32.const 4\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        i32.const 24\n");
            out.push_str("        i32.add\n");
            out.push_str("        f32.load\n");
            out.push_str("        f32.store\n");
        }
        Type::F64 => {
            out.push_str("        local.get $rec_ptr\n");
            out.push_str("        i32.const 4\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        i32.const 24\n");
            out.push_str("        i32.add\n");
            out.push_str("        f64.load\n");
            out.push_str("        f64.store\n");
        }
        _ => {
            out.push_str("        ;; TODO: decode non-scalar option inner\n");
        }
    }

    out.push_str("      )\n");
    out.push_str("    )\n");

    // Return the option pointer
    out.push_str("    local.get $rec_ptr\n");
    out.push_str(&format!("    local.set $param_{}\n", param_name));
}

/// Generate WAT code to decode a variant parameter from CGRF.
///
/// CGRF v2 variant encoding (depth-first):
/// - No payload: variant node is at index 0 (offset 16)
///   - Payload: [type_name_len:u32, type_name:utf8, case_name_len:u32, case_name:utf8,
///              tag:u32, payload_count:u32]
/// - With payload: child node first, then variant node
///   - Child node at offset 16
///   - Variant node at offset 16 + child_size
///   - Payload: [type_name_len:u32, type_name:utf8, case_name_len:u32, case_name:utf8,
///              tag:u32, payload_count:u32, child_indices:u32*]
///
/// Wisp variant layout: [discriminant: i32, payload...]
fn generate_cgrf_decode_variant(
    out: &mut String,
    variant_name: &str,
    param_name: &str,
    variants: &HashMap<String, VariantDef>,
) {
    let variant_def = match variants.get(variant_name) {
        Some(v) => v,
        None => {
            out.push_str(&format!(
                "    ;; ERROR: unknown variant '{}'\n",
                variant_name
            ));
            out.push_str("    i32.const 0\n");
            out.push_str(&format!("    local.set $param_{}\n", param_name));
            return;
        }
    };

    out.push_str(&format!(
        "    ;; Decode variant '{}' from CGRF v2\n",
        variant_name
    ));

    // Calculate variant size for allocation
    let variant_size = variant_def.size();

    // Allocate wisp variant on heap
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $rec_ptr\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str(&format!("    i32.const {}\n", variant_size));
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // Use node_count to determine if there's a payload
    // node_count == 1: no payload (just variant node at offset 16)
    // node_count == 2: has payload (child at offset 16, variant after)

    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n"); // node_count
    out.push_str("    local.set $field_val\n");

    out.push_str("    local.get $field_val\n");
    out.push_str("    i32.const 1\n");
    out.push_str("    i32.eq\n");
    out.push_str("    (if\n");
    out.push_str("      (then\n");
    out.push_str("        ;; No payload case: variant node at offset 16\n");
    // For v2, we need to skip type_name and case_name to find tag
    // Payload starts at offset 24 (16 + 8 header)
    // Read type_name_len at 24, skip type_name, read case_name_len, skip case_name, then read tag
    out.push_str("        ;; Read type_name_len\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        i32.const 24\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        local.set $child_idx\n"); // reuse as type_name_len
    out.push_str("        ;; Calculate case_name_len offset: 24 + 4 + type_name_len\n");
    out.push_str("        i32.const 28\n");
    out.push_str("        local.get $child_idx\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.set $child_offset\n"); // case_name_len offset
    out.push_str("        ;; Read case_name_len\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        local.set $scan_i\n"); // reuse as case_name_len
    out.push_str("        ;; Calculate tag offset: case_name_len_offset + 4 + case_name_len\n");
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.const 4\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.get $scan_i\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.set $payload_len\n"); // reuse as tag_offset
    out.push_str("        ;; Read tag\n");
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $payload_len\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        i32.store\n"); // store tag as discriminant
    out.push_str("      )\n");
    out.push_str("      (else\n");
    out.push_str("        ;; Has payload case: child at offset 16, variant node after\n");
    // Child node (s32) is at offset 16, size 12 (8 header + 4 payload)
    // Variant node is at offset 28
    // For v2, variant payload starts at offset 36 (28 + 8)
    out.push_str("        ;; Read type_name_len from variant node\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        i32.const 36\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        local.set $child_idx\n"); // reuse as type_name_len
    out.push_str("        ;; Calculate case_name_len offset: 36 + 4 + type_name_len\n");
    out.push_str("        i32.const 40\n");
    out.push_str("        local.get $child_idx\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.set $child_offset\n"); // case_name_len offset
    out.push_str("        ;; Read case_name_len\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        local.set $scan_i\n"); // reuse as case_name_len
    out.push_str("        ;; Calculate tag offset: case_name_len_offset + 4 + case_name_len\n");
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.const 4\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.get $scan_i\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.set $payload_len\n"); // reuse as tag_offset
    out.push_str("        ;; Read tag\n");
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $payload_len\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        i32.store\n"); // store tag as discriminant
    // Read payload value from child node (offset 16 + 8 = 24)
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        i32.const 4\n");
    out.push_str("        i32.add\n"); // payload at offset 4
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        i32.const 24\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        i32.store\n"); // store payload value
    out.push_str("      )\n");
    out.push_str("    )\n");

    // Return the variant pointer
    out.push_str("    local.get $rec_ptr\n");
    out.push_str(&format!("    local.set $param_{}\n", param_name));
}

/// Generate WAT code to decode a result<T, E> parameter from CGRF.
/// For now, supports result<s32, s32>.
///
/// CGRF v2 result encoding:
/// - Result node at offset 16 (root = 0)
///   - Payload: [ok_type:type_tag*, err_type:type_tag*, tag:u32, has_payload:u8, child_index:u32]
/// - Payload value node after result node
///
/// Wisp result layout: [tag: i32 (0=ok, 1=err), payload]
fn generate_cgrf_decode_result(
    out: &mut String,
    ok_ty: &Type,
    err_ty: &Type,
    param_name: &str,
) {
    out.push_str("    ;; Decode result from CGRF v2 (depth-first encoded)\n");

    // Calculate result size for allocation
    let payload_size = match (ok_ty, err_ty) {
        (Type::S64, _) | (_, Type::S64) | (Type::F64, _) | (_, Type::F64) => 8,
        _ => 4,
    };
    let result_size = 4 + payload_size; // tag + payload

    // In CGRF depth-first encoding:
    // - Node 0: payload value (if present) at offset 16
    // - Node 1: Result node at offset 16 + payload_node_size (or 16 if no payload)
    //
    // Result payload v2: [ok_type:type_tag*, err_type:type_tag*, tag:u32, has_payload:u8, child_index?:u32]

    // Calculate payload node size based on type
    let payload_node_size = match (ok_ty, err_ty) {
        (Type::S64, _) | (_, Type::S64) | (Type::F64, _) | (_, Type::F64) => 16, // 8 header + 8 payload
        _ => 12, // 8 header + 4 payload
    };

    // Calculate offsets for v2 format
    let ok_tag_size = type_tag_size(ok_ty);
    let err_tag_size = type_tag_size(err_ty);

    // Allocate wisp result on heap
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $rec_ptr\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str(&format!("    i32.const {}\n", result_size));
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // Use node_count to determine if there's a payload
    // node_count == 1: just result node (at offset 16)
    // node_count == 2: payload node first (at offset 16), then result node
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n"); // node_count
    out.push_str("    local.set $child_idx\n"); // reuse as node_count

    out.push_str("    local.get $child_idx\n");
    out.push_str("    i32.const 1\n");
    out.push_str("    i32.eq\n");
    out.push_str("    (if\n");
    out.push_str("      (then\n");
    out.push_str("        ;; No payload: Result node at offset 16\n");
    // tag is at: 16 (node start) + 8 (header) + ok_tag_size + err_tag_size
    let tag_offset_no_payload = 24 + ok_tag_size + err_tag_size;
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str(&format!("        i32.const {}\n", tag_offset_no_payload));
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        i32.store\n"); // store tag
    // No payload value to store
    out.push_str("      )\n");
    out.push_str("      (else\n");
    out.push_str("        ;; Has payload: payload at offset 16, Result node after\n");
    // Result node is at: 16 + payload_node_size
    let result_node_offset = 16 + payload_node_size;
    // tag is at: result_node_offset + 8 (header) + ok_tag_size + err_tag_size
    let tag_offset_with_payload = result_node_offset + 8 + ok_tag_size + err_tag_size;
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str(&format!("        i32.const {}\n", tag_offset_with_payload));
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        i32.store\n"); // store tag
    // Read payload value from payload node (at offset 16 + 8 = 24)
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        i32.const 4\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        i32.const 24\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        i32.store\n");
    out.push_str("      )\n");
    out.push_str("    )\n");

    // Return the result pointer
    out.push_str("    local.get $rec_ptr\n");
    out.push_str(&format!("    local.set $param_{}\n", param_name));
}

/// Generate WAT code to decode a list<T> parameter from CGRF.
/// For now, only supports list<s32>.
///
/// CGRF v2 uses depth-first encoding:
/// - Child nodes (elements) are encoded FIRST at nodes 0, 1, 2, ...
/// - List node is encoded LAST (root)
/// - List payload: [elem_type:type_tag*, count:u32, child_indices:u32*]
///
/// For list [1, 2, 3]:
/// - Node 0: S32(1) at offset 16
/// - Node 1: S32(2) at offset 28
/// - Node 2: S32(3) at offset 40
/// - Node 3: List at offset 52 (root, contains type tag + count + indices)
///
/// Wisp list layout: { len: i32, cap: i32, data_ptr: i32 }
fn generate_cgrf_decode_list(
    out: &mut String,
    elem_ty: &Type,
    param_name: &str,
) {
    out.push_str("    ;; Decode list from CGRF v2 (depth-first encoded)\n");

    // Element nodes are encoded first, starting at offset 16
    // The list node is at the end (root)

    // Read root index from header (offset 12)
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $elem_offset\n"); // reuse as root_index

    // Calculate root node offset: 16 + root_index * node_size
    // For list<s32>, element nodes are 12 bytes each
    let elem_node_size = match elem_ty {
        Type::S64 | Type::F64 => 16, // 8 header + 8 payload
        _ => 12,                      // 8 header + 4 payload
    };

    // Root offset = 16 + root_index * elem_node_size
    // (This assumes uniform node sizes, which works for homogeneous lists)
    out.push_str("    i32.const 16\n");
    out.push_str("    local.get $elem_offset\n");
    out.push_str(&format!("    i32.const {}\n", elem_node_size));
    out.push_str("    i32.mul\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.set $elem_offset\n"); // now holds root node offset

    // For v2, list payload is: [elem_type:type_tag*, count:u32, child_indices:u32*]
    // The count is at offset: 8 (node header) + type_tag_size(elem_ty)
    let elem_type_tag_size = type_tag_size(elem_ty);
    let count_offset = 8 + elem_type_tag_size;

    // Read element count from list node payload
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    local.get $elem_offset\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", count_offset));
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $list_len\n");

    // Allocate wisp list struct (12 bytes: len, cap, data_ptr)
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $list_ptr\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // Store len
    out.push_str("    local.get $list_ptr\n");
    out.push_str("    local.get $list_len\n");
    out.push_str("    i32.store\n");

    // Store cap = len
    out.push_str("    local.get $list_ptr\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $list_len\n");
    out.push_str("    i32.store\n");

    // Calculate element size in wisp data
    let elem_size = match elem_ty {
        Type::S64 | Type::F64 => 8,
        _ => 4, // s32, f32, pointers
    };

    // Allocate element data: elem_size * len bytes
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $list_data\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str(&format!("    i32.const {}\n", elem_size));
    out.push_str("    local.get $list_len\n");
    out.push_str("    i32.mul\n");
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // Store data_ptr
    out.push_str("    local.get $list_ptr\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $list_data\n");
    out.push_str("    i32.store\n");

    // Element nodes are at offsets 16, 16+node_size, 16+2*node_size, ...
    // Loop to copy elements from CGRF to wisp list data
    out.push_str("    i32.const 0\n");
    out.push_str("    local.set $list_i\n");

    out.push_str("    block $break\n");
    out.push_str("      loop $loop\n");
    out.push_str("        local.get $list_i\n");
    out.push_str("        local.get $list_len\n");
    out.push_str("        i32.ge_u\n");
    out.push_str("        br_if $break\n");

    // Copy element i from CGRF node to wisp data
    // CGRF element node i: at offset 16 + i * node_size, value at +8
    // Wisp data: at $list_data + i * elem_size
    match elem_ty {
        Type::S32 => {
            // Dest: $list_data + $list_i * 4
            out.push_str("        local.get $list_data\n");
            out.push_str("        local.get $list_i\n");
            out.push_str("        i32.const 4\n");
            out.push_str("        i32.mul\n");
            out.push_str("        i32.add\n");
            // Src: load from $in_ptr + 16 + $list_i * 12 + 8
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        i32.const 16\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $list_i\n");
            out.push_str(&format!("        i32.const {}\n", elem_node_size));
            out.push_str("        i32.mul\n");
            out.push_str("        i32.add\n");
            out.push_str("        i32.const 8\n");
            out.push_str("        i32.add\n");
            out.push_str("        i32.load\n");
            out.push_str("        i32.store\n");
        }
        Type::S64 => {
            out.push_str("        local.get $list_data\n");
            out.push_str("        local.get $list_i\n");
            out.push_str("        i32.const 8\n");
            out.push_str("        i32.mul\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        i32.const 16\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $list_i\n");
            out.push_str(&format!("        i32.const {}\n", elem_node_size));
            out.push_str("        i32.mul\n");
            out.push_str("        i32.add\n");
            out.push_str("        i32.const 8\n");
            out.push_str("        i32.add\n");
            out.push_str("        i64.load\n");
            out.push_str("        i64.store\n");
        }
        Type::F32 => {
            out.push_str("        local.get $list_data\n");
            out.push_str("        local.get $list_i\n");
            out.push_str("        i32.const 4\n");
            out.push_str("        i32.mul\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        i32.const 16\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $list_i\n");
            out.push_str(&format!("        i32.const {}\n", elem_node_size));
            out.push_str("        i32.mul\n");
            out.push_str("        i32.add\n");
            out.push_str("        i32.const 8\n");
            out.push_str("        i32.add\n");
            out.push_str("        f32.load\n");
            out.push_str("        f32.store\n");
        }
        Type::F64 => {
            out.push_str("        local.get $list_data\n");
            out.push_str("        local.get $list_i\n");
            out.push_str("        i32.const 8\n");
            out.push_str("        i32.mul\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $in_ptr\n");
            out.push_str("        i32.const 16\n");
            out.push_str("        i32.add\n");
            out.push_str("        local.get $list_i\n");
            out.push_str(&format!("        i32.const {}\n", elem_node_size));
            out.push_str("        i32.mul\n");
            out.push_str("        i32.add\n");
            out.push_str("        i32.const 8\n");
            out.push_str("        i32.add\n");
            out.push_str("        f64.load\n");
            out.push_str("        f64.store\n");
        }
        _ => {
            out.push_str("        ;; TODO: decode non-scalar list element\n");
        }
    }

    // Increment loop counter
    out.push_str("        local.get $list_i\n");
    out.push_str("        i32.const 1\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.set $list_i\n");
    out.push_str("        br $loop\n");
    out.push_str("      end\n");
    out.push_str("    end\n");

    // Return the list pointer
    out.push_str("    local.get $list_ptr\n");
    out.push_str(&format!("    local.set $param_{}\n", param_name));
}

/// Generate WAT code to decode a tuple element from CGRF.
/// `element_idx` is the 0-based index of the tuple element.
/// `node_offset` is the local variable holding the current node offset in the buffer.
/// The tuple structure in CGRF:
/// - Header at offset 16: kind=0x0B, flags, reserved, payload_len
/// - Payload: element_count (u32), then element_count node indices (u32 each)
/// - Child nodes follow the tuple node
fn generate_cgrf_decode_tuple_element_offset(out: &mut String, element_idx: usize) {
    // For now, we calculate the offset to the element node.
    // Tuple payload structure: element_count (4 bytes) + indices (4 bytes each)
    // First element index is at offset 24 + 4 = 28
    // Second element index at offset 32, etc.
    let index_offset = 28 + element_idx * 4;
    out.push_str(&format!(
        "    ;; Get tuple element {} node index\n",
        element_idx
    ));
    out.push_str("    local.get $in_ptr\n");
    out.push_str(&format!("    i32.const {}\n", index_offset));
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n"); // node index on stack
}

/// Generate WAT code to decode a single parameter from CGRF.
/// For single-param functions, the root node is the value.
/// `param_name` is the parameter name (for the local variable).
/// `param_idx` is unused for single params but kept for consistency.
/// `is_tuple_element` indicates if we're reading from a tuple (affects offset calculation).
fn generate_cgrf_decode_param(
    out: &mut String,
    param_ty: &Type,
    param_name: &str,
    _param_idx: usize,
    _is_tuple_element: bool,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
) {
    // For single param, root node is at offset 16, payload at offset 24
    match param_ty {
        Type::S32 => {
            generate_cgrf_decode_s32(out);
            out.push_str(&format!("    local.set $param_{}\n", param_name));
        }
        Type::S64 => {
            generate_cgrf_decode_s64(out);
            out.push_str(&format!("    local.set $param_{}\n", param_name));
        }
        Type::F32 => {
            generate_cgrf_decode_f32(out);
            out.push_str(&format!("    local.set $param_{}\n", param_name));
        }
        Type::F64 => {
            generate_cgrf_decode_f64(out);
            out.push_str(&format!("    local.set $param_{}\n", param_name));
        }
        Type::Str => {
            generate_cgrf_decode_string(out);
            out.push_str(&format!("    local.set $param_{}\n", param_name));
        }
        Type::Record(rec_name) => {
            generate_cgrf_decode_record(out, rec_name, param_name, records);
        }
        Type::Option(inner_ty) => {
            generate_cgrf_decode_option(out, inner_ty, param_name);
        }
        Type::List(elem_ty) => {
            generate_cgrf_decode_list(out, elem_ty, param_name);
        }
        Type::Variant(variant_name) => {
            generate_cgrf_decode_variant(out, variant_name, param_name, variants);
        }
        Type::Result(ok_ty, err_ty) => {
            generate_cgrf_decode_result(out, ok_ty, err_ty, param_name);
        }
        _ => {
            // For complex types, we'd need more sophisticated decoding
            out.push_str(&format!(
                "    ;; TODO: decode complex param type for {}\n",
                param_name
            ));
            out.push_str("    i32.const 0\n");
            out.push_str(&format!("    local.set $param_{}\n", param_name));
        }
    }
}

/// Generate WAT code to find a node by index in CGRF.
/// Scans from offset 16, counting nodes until reaching the target index.
/// Result is stored in $child_offset.
///
/// Requires locals: $scan_i, $child_offset, $payload_len
/// Input: target index in $child_idx
fn generate_find_node_by_index(out: &mut String) {
    out.push_str("    ;; Find node at index $child_idx\n");

    // Start at offset 16 (after CGRF header)
    out.push_str("    i32.const 16\n");
    out.push_str("    local.set $child_offset\n");

    // Loop counter = 0
    out.push_str("    i32.const 0\n");
    out.push_str("    local.set $scan_i\n");

    // Loop: while scan_i < child_idx
    out.push_str("    (block $break\n");
    out.push_str("      (loop $continue\n");

    // Check if scan_i >= child_idx, break if so
    out.push_str("        local.get $scan_i\n");
    out.push_str("        local.get $child_idx\n");
    out.push_str("        i32.ge_u\n");
    out.push_str("        br_if $break\n");

    // Read payload_len from current node header (at child_offset + 4)
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.const 4\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        local.set $payload_len\n");

    // Advance child_offset by 8 + payload_len
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.const 8\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.get $payload_len\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.set $child_offset\n");

    // Increment scan_i
    out.push_str("        local.get $scan_i\n");
    out.push_str("        i32.const 1\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.set $scan_i\n");

    // Continue loop
    out.push_str("        br $continue\n");
    out.push_str("      )\n");
    out.push_str("    )\n");

    // Now $child_offset points to node at index $child_idx
}

/// Decode a record from the node at $child_offset.
/// The record node contains child indices for each field.
fn generate_decode_record_at_offset(
    out: &mut String,
    rec_name: &str,
    param_name: &str,
    records: &HashMap<String, RecordDef>,
) {
    let rec_def = match records.get(rec_name) {
        Some(r) => r,
        None => {
            out.push_str(&format!("    ;; ERROR: unknown record '{}'\n", rec_name));
            out.push_str("    i32.const 0\n");
            out.push_str(&format!("    local.set $param_{}\n", param_name));
            return;
        }
    };

    out.push_str(&format!("    ;; Decode record '{}' at $child_offset (CGRF v2)\n", rec_name));

    // Allocate wisp record on heap
    let rec_size = rec_def.size();
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $rec_ptr\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str(&format!("    i32.const {}\n", rec_size));
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // v2 record payload: [type_name_len:u32, type_name:utf8, field_count:u32,
    //                     field_names:(len:u32, name:utf8)*, child_indices:u32*]
    //
    // We need to calculate offset to child_indices:
    // - 8 bytes: node header
    // - 4 bytes: type_name_len
    // - N bytes: type_name (use rec_name.len() since we know the type)
    // - 4 bytes: field_count
    // - For each field: 4 bytes len + field_name.len() bytes
    // Then child_indices start
    //
    // Note: The encoded field names may differ from rec_def field names (e.g., "field0" vs "x"),
    // but the field count and number of child_indices should match.

    // Calculate offset to child_indices from start of node
    // We read the actual type_name_len and field_name_lens from the payload at runtime
    // because the encoder may use different names than the wisp source

    // First, read type_name_len to know where field_count is
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    local.get $child_offset\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 8\n"); // skip node header
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $str_len\n"); // type_name_len

    // field_count is at: header(8) + type_name_len(4) + type_name($str_len) + 0
    // = 12 + $str_len
    // We'll read field_count to verify, but we know it from rec_def

    // Calculate offset to field_names: 8 + 4 + $str_len + 4 = 16 + $str_len
    // Then for each field, skip 4 + field_name_len bytes
    // After all field names, child_indices start

    // For simplicity with variable-length field names, read them at runtime
    // Calculate base offset to payload data
    // child_indices_offset = 8 + 4 + type_name_len + 4 + sum(4 + field_name_len for each field)

    // Store base offset for child_indices calculation
    // We need to scan through field names to find child_indices

    // Start at offset for first field name: 8 + 4 + type_name_len + 4
    out.push_str("    i32.const 16\n"); // 8 header + 4 type_name_len + 4 field_count
    out.push_str("    local.get $str_len\n"); // type_name_len
    out.push_str("    i32.add\n");
    out.push_str("    local.set $data_len\n"); // offset to first field name

    // Skip all field names
    let field_count = rec_def.fields.len();
    for _i in 0..field_count {
        // Read field_name_len at current offset
        out.push_str("    local.get $in_ptr\n");
        out.push_str("    local.get $child_offset\n");
        out.push_str("    i32.add\n");
        out.push_str("    local.get $data_len\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.load\n");
        out.push_str("    local.set $str_len\n"); // field_name_len

        // Advance: data_len += 4 + field_name_len
        out.push_str("    local.get $data_len\n");
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n");
        out.push_str("    local.get $str_len\n");
        out.push_str("    i32.add\n");
        out.push_str("    local.set $data_len\n");
    }

    // Now $data_len is the offset to child_indices from node start
    // For each field, read child_index, find that node, read value
    for (field_idx, field) in rec_def.fields.iter().enumerate() {
        // Read child_indices[field_idx] from record node
        out.push_str("    local.get $in_ptr\n");
        out.push_str("    local.get $child_offset\n");
        out.push_str("    i32.add\n");
        out.push_str("    local.get $data_len\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    i32.const {}\n", field_idx * 4));
        out.push_str("    i32.add\n");
        out.push_str("    i32.load\n");
        out.push_str("    local.set $child_idx\n");

        // Save current child_offset (we'll need it for other fields)
        out.push_str("    local.get $child_offset\n");
        out.push_str("    local.set $tuple_offset\n"); // reuse as temp

        // Find the field's node
        generate_find_node_by_index(out);

        // Read field value from that node (at child_offset + 8)
        out.push_str("    local.get $rec_ptr\n");
        let field_offset = rec_def.field_offset(field_idx);
        if field_offset > 0 {
            out.push_str(&format!("    i32.const {}\n", field_offset));
            out.push_str("    i32.add\n");
        }
        out.push_str("    local.get $in_ptr\n");
        out.push_str("    local.get $child_offset\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.const 8\n");
        out.push_str("    i32.add\n");
        match &field.ty {
            Type::S32 | Type::F32 => out.push_str("    i32.load\n"),
            Type::S64 | Type::F64 => out.push_str("    i64.load\n"),
            _ => out.push_str("    i32.load\n"), // default to i32 for now
        }
        out.push_str("    i32.store\n");

        // Restore child_offset for next field
        out.push_str("    local.get $tuple_offset\n");
        out.push_str("    local.set $child_offset\n");
    }

    out.push_str("    local.get $rec_ptr\n");
    out.push_str(&format!("    local.set $param_{}\n", param_name));
}

/// Decode an option from the node at $child_offset.
fn generate_decode_option_at_offset(
    out: &mut String,
    inner_ty: &Type,
    param_name: &str,
) {
    out.push_str("    ;; Decode option at $child_offset (CGRF v2)\n");

    let inner_size = type_size(inner_ty);
    let option_size = 4 + inner_size;

    // v2 option payload: [inner_type:type_tag*, presence:u8, child_index?:u32]
    // Calculate offset to presence byte (after node header + type tag)
    let inner_type_tag_size = type_tag_size(inner_ty);
    let presence_offset = 8 + inner_type_tag_size; // 8 = node header
    let child_index_offset = presence_offset + 1;

    // Allocate wisp option on heap
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $rec_ptr\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str(&format!("    i32.const {}\n", option_size));
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // Read presence byte from option node
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    local.get $child_offset\n");
    out.push_str("    i32.add\n");
    out.push_str(&format!("    i32.const {}\n", presence_offset));
    out.push_str("    i32.add\n");
    out.push_str("    i32.load8_u\n");
    out.push_str("    local.set $field_val\n");

    out.push_str("    local.get $field_val\n");
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.eq\n");
    out.push_str("    (if\n");
    out.push_str("      (then\n");
    out.push_str("        ;; None case: store tag = 0\n");
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        i32.const 0\n");
    out.push_str("        i32.store\n");
    out.push_str("      )\n");
    out.push_str("      (else\n");
    out.push_str("        ;; Some case: store tag = 1 and decode inner value\n");
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        i32.const 1\n");
    out.push_str("        i32.store\n");

    // Read child_index from option node
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.add\n");
    out.push_str(&format!("        i32.const {}\n", child_index_offset));
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        local.set $child_idx\n");

    // Save option node offset
    out.push_str("        local.get $child_offset\n");
    out.push_str("        local.set $tuple_offset\n");

    // Find the inner value node
    generate_find_node_by_index(out);

    // Read inner value and store in option payload
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        i32.const 4\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.const 8\n");
    out.push_str("        i32.add\n");
    match inner_ty {
        Type::S32 | Type::F32 => out.push_str("        i32.load\n"),
        Type::S64 | Type::F64 => out.push_str("        i64.load\n"),
        _ => out.push_str("        i32.load\n"),
    }
    out.push_str("        i32.store\n");

    out.push_str("      )\n");
    out.push_str("    )\n");

    out.push_str("    local.get $rec_ptr\n");
    out.push_str(&format!("    local.set $param_{}\n", param_name));
}

/// Decode a variant from the node at $child_offset.
fn generate_decode_variant_at_offset(
    out: &mut String,
    var_name: &str,
    param_name: &str,
    variants: &HashMap<String, VariantDef>,
) {
    let var_def = match variants.get(var_name) {
        Some(v) => v,
        None => {
            out.push_str(&format!("    ;; ERROR: unknown variant '{}'\n", var_name));
            out.push_str("    i32.const 0\n");
            out.push_str(&format!("    local.set $param_{}\n", param_name));
            return;
        }
    };

    out.push_str(&format!("    ;; Decode variant '{}' at $child_offset\n", var_name));

    let var_size = var_def.size();
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $rec_ptr\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str(&format!("    i32.const {}\n", var_size));
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // Variant node payload: [tag: u32, has_payload: u8, child_index?: u32]
    // Read tag from variant node (at child_offset + 8)
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    local.get $child_offset\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $field_val\n"); // tag

    // Store tag as discriminant
    out.push_str("    local.get $rec_ptr\n");
    out.push_str("    local.get $field_val\n");
    out.push_str("    i32.store\n");

    // Read has_payload from variant node (at child_offset + 12)
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    local.get $child_offset\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 12\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load8_u\n");

    out.push_str("    i32.const 1\n");
    out.push_str("    i32.eq\n");
    out.push_str("    (if\n");
    out.push_str("      (then\n");
    out.push_str("        ;; Has payload - read child_index and decode\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.const 13\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        local.set $child_idx\n");

    out.push_str("        local.get $child_offset\n");
    out.push_str("        local.set $tuple_offset\n");

    generate_find_node_by_index(out);

    // Read payload value
    out.push_str("        local.get $rec_ptr\n");
    out.push_str("        i32.const 4\n");
    out.push_str("        i32.add\n");
    out.push_str("        local.get $in_ptr\n");
    out.push_str("        local.get $child_offset\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.const 8\n");
    out.push_str("        i32.add\n");
    out.push_str("        i32.load\n");
    out.push_str("        i32.store\n");

    out.push_str("      )\n");
    out.push_str("    )\n");

    out.push_str("    local.get $rec_ptr\n");
    out.push_str(&format!("    local.set $param_{}\n", param_name));
}

/// Decode a result from the node at $child_offset.
fn generate_decode_result_at_offset(
    out: &mut String,
    ok_ty: &Type,
    _err_ty: &Type,
    param_name: &str,
) {
    out.push_str("    ;; Decode result at $child_offset\n");

    let payload_size = match ok_ty {
        Type::S64 | Type::F64 => 8,
        _ => 4,
    };
    let result_size = 4 + payload_size;

    out.push_str("    global.get $__heap_ptr\n");
    out.push_str("    local.set $rec_ptr\n");
    out.push_str("    global.get $__heap_ptr\n");
    out.push_str(&format!("    i32.const {}\n", result_size));
    out.push_str("    i32.add\n");
    out.push_str("    global.set $__heap_ptr\n");

    // Result is encoded like a variant: [tag: u32, has_payload: u8, child_index: u32]
    // Read tag from result node (at child_offset + 8)
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    local.get $child_offset\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $field_val\n");

    // Store tag
    out.push_str("    local.get $rec_ptr\n");
    out.push_str("    local.get $field_val\n");
    out.push_str("    i32.store\n");

    // Read child_index (at child_offset + 13)
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    local.get $child_offset\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 13\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    local.set $child_idx\n");

    out.push_str("    local.get $child_offset\n");
    out.push_str("    local.set $tuple_offset\n");

    generate_find_node_by_index(out);

    // Read payload value
    out.push_str("    local.get $rec_ptr\n");
    out.push_str("    i32.const 4\n");
    out.push_str("    i32.add\n");
    out.push_str("    local.get $in_ptr\n");
    out.push_str("    local.get $child_offset\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.const 8\n");
    out.push_str("    i32.add\n");
    out.push_str("    i32.load\n");
    out.push_str("    i32.store\n");

    out.push_str("    local.get $rec_ptr\n");
    out.push_str(&format!("    local.set $param_{}\n", param_name));
}

/// Generate WAT code to decode a parameter from a tuple in CGRF.
/// For multi-param functions, the root is a tuple with child nodes.
///
/// CGRF encoding order (depth-first):
/// - Child nodes are encoded FIRST (node 0, 1, 2, ...)
/// - Tuple node is encoded LAST (root)
///
/// For Tuple([S32(3), S32(5)]):
/// - Node 0: S32(3) at offset 16
/// - Node 1: S32(5) at offset 28
/// - Node 2: Tuple (root) at offset 40
///
/// So child i is at offset: 16 + sum(sizes of nodes 0..i-1)
/// For uniform scalars: 16 + i * node_size
fn generate_cgrf_decode_tuple_param(
    out: &mut String,
    param_ty: &Type,
    param_name: &str,
    param_idx: usize,
    all_params: &[Parameter],
    use_runtime_offset: bool,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
) {
    out.push_str(&format!(
        "    ;; Decode tuple param {} ({})\n",
        param_idx, param_name
    ));

    // When using runtime offsets, $node_offset holds the current position
    // Otherwise, calculate compile-time offsets for fixed-size types
    if use_runtime_offset {
        // Runtime offset mode: use $node_offset local
        match param_ty {
            Type::S32 => {
                // Load payload from $in_ptr + $node_offset + 8
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 8\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
                // Advance offset: node_offset += 12 (8 header + 4 payload)
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.const 12\n");
                out.push_str("    i32.add\n");
                out.push_str("    local.set $node_offset\n");
            }
            Type::S64 => {
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 8\n");
                out.push_str("    i32.add\n");
                out.push_str("    i64.load\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
                // Advance offset: node_offset += 16 (8 header + 8 payload)
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.const 16\n");
                out.push_str("    i32.add\n");
                out.push_str("    local.set $node_offset\n");
            }
            Type::F32 => {
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 8\n");
                out.push_str("    i32.add\n");
                out.push_str("    f32.load\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.const 12\n");
                out.push_str("    i32.add\n");
                out.push_str("    local.set $node_offset\n");
            }
            Type::F64 => {
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 8\n");
                out.push_str("    i32.add\n");
                out.push_str("    f64.load\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.const 16\n");
                out.push_str("    i32.add\n");
                out.push_str("    local.set $node_offset\n");
            }
            Type::Str => {
                // String node: 8 byte header + 4 byte length + string bytes
                // Read data_len from header (at offset + 4)
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 4\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
                out.push_str("    local.set $data_len\n");

                // Read string length from payload (at offset + 8)
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 8\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
                out.push_str("    local.set $str_len\n");

                // Allocate wisp string: 4 bytes for length + string data
                out.push_str("    global.get $__heap_ptr\n");
                out.push_str("    local.set $str_ptr\n");
                out.push_str("    global.get $__heap_ptr\n");
                out.push_str("    i32.const 4\n");
                out.push_str("    i32.add\n");
                out.push_str("    local.get $str_len\n");
                out.push_str("    i32.add\n");
                out.push_str("    global.set $__heap_ptr\n");

                // Write length to wisp string
                out.push_str("    local.get $str_ptr\n");
                out.push_str("    local.get $str_len\n");
                out.push_str("    i32.store\n");

                // Copy string data from CGRF (at offset + 12) to wisp string (at str_ptr + 4)
                out.push_str("    local.get $str_ptr\n");
                out.push_str("    i32.const 4\n");
                out.push_str("    i32.add\n"); // dest
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 12\n");
                out.push_str("    i32.add\n"); // src
                out.push_str("    local.get $str_len\n"); // len
                out.push_str("    memory.copy\n");

                // Set param to wisp string pointer
                out.push_str("    local.get $str_ptr\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));

                // Advance offset: node_offset += 8 + data_len
                out.push_str("    local.get $node_offset\n");
                out.push_str("    i32.const 8\n");
                out.push_str("    i32.add\n");
                out.push_str("    local.get $data_len\n");
                out.push_str("    i32.add\n");
                out.push_str("    local.set $node_offset\n");
            }
            _ => {
                out.push_str(&format!(
                    "    ;; TODO: decode tuple element of complex type for {}\n",
                    param_name
                ));
                out.push_str("    i32.const 0\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
            }
        }
    } else {
        // Compile-time offset mode for fixed-size types only
        let mut node_offset = 16; // Start after header
        for i in 0..param_idx {
            node_offset += match &all_params[i].ty {
                Type::S64 | Type::F64 => 16, // 8 header + 8 payload
                _ => 12,                      // 8 header + 4 payload (s32, f32, etc.)
            };
        }

        // Payload is at node_offset + 8 (skip node header)
        let payload_offset = node_offset + 8;

        match param_ty {
            Type::S32 => {
                out.push_str("    local.get $in_ptr\n");
                out.push_str(&format!("    i32.const {}\n", payload_offset));
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
            }
            Type::S64 => {
                out.push_str("    local.get $in_ptr\n");
                out.push_str(&format!("    i32.const {}\n", payload_offset));
                out.push_str("    i32.add\n");
                out.push_str("    i64.load\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
            }
            Type::F32 => {
                out.push_str("    local.get $in_ptr\n");
                out.push_str(&format!("    i32.const {}\n", payload_offset));
                out.push_str("    i32.add\n");
                out.push_str("    f32.load\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
            }
            Type::F64 => {
                out.push_str("    local.get $in_ptr\n");
                out.push_str(&format!("    i32.const {}\n", payload_offset));
                out.push_str("    i32.add\n");
                out.push_str("    f64.load\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
            }
            Type::Record(_) | Type::Option(_) | Type::Variant(_) | Type::Result(_, _) => {
                // Complex types need tree traversal
                out.push_str(&format!(
                    "    ;; Decode tuple element {} ({}) via tree traversal\n",
                    param_idx, param_name
                ));

                // Step 1: Find the tuple node (root)
                // Read root_index from header (offset 12)
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    i32.const 12\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
                out.push_str("    local.set $child_idx\n"); // temporarily store root_index

                // Find root node offset
                generate_find_node_by_index(out);
                // Now $child_offset points to the tuple node

                // Step 2: Read child_indices[param_idx] from tuple payload
                // Tuple payload: [child_count: u32, child_indices: [u32; child_count]]
                // child_indices[param_idx] is at tuple_offset + 8 (header) + 4 (count) + param_idx * 4
                out.push_str("    local.get $in_ptr\n");
                out.push_str("    local.get $child_offset\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 12\n"); // 8 header + 4 for child_count
                out.push_str("    i32.add\n");
                out.push_str(&format!("    i32.const {}\n", param_idx * 4));
                out.push_str("    i32.add\n");
                out.push_str("    i32.load\n");
                out.push_str("    local.set $child_idx\n");

                // Step 3: Find that child node
                generate_find_node_by_index(out);
                // Now $child_offset points to the child node for this param

                // Step 4: Decode based on type
                match param_ty {
                    Type::Record(rec_name) => {
                        generate_decode_record_at_offset(out, rec_name, param_name, records);
                    }
                    Type::Option(inner_ty) => {
                        generate_decode_option_at_offset(out, inner_ty, param_name);
                    }
                    Type::Variant(var_name) => {
                        generate_decode_variant_at_offset(out, var_name, param_name, variants);
                    }
                    Type::Result(ok_ty, err_ty) => {
                        generate_decode_result_at_offset(out, ok_ty, err_ty, param_name);
                    }
                    _ => unreachable!(),
                }
            }
            _ => {
                out.push_str(&format!(
                    "    ;; TODO: decode tuple element of complex type for {}\n",
                    param_name
                ));
                out.push_str("    i32.const 0\n");
                out.push_str(&format!("    local.set $param_{}\n", param_name));
            }
        }
    }
}

/// Compile an expression for REPL evaluation, producing a Pack package.
///
/// This generates a WASM module (not a full package) with Pack/Graph ABI calling convention.
/// The module exports an `eval` function with signature (i32, i32, i32, i32) -> i32.
pub fn compile_repl_expr_pack(
    expr_source: &str,
    bindings: &HashMap<String, InlineValue>,
    functions: &[Function],
) -> Result<Vec<u8>> {
    let ctx = CompileContext::new(expr_source.to_string(), "<repl>".to_string());

    // Parse the expression
    let tokens = tokenize(expr_source);
    if tokens.is_empty() {
        bail!("empty expression");
    }

    let (sexpr, _) = parse_sexpr(&tokens, 0);

    // Inline variable bindings by transforming the SExpr
    let inlined_sexpr = inline_bindings(&sexpr, bindings);

    // Build function signatures from provided functions
    let mut signatures: HashMap<String, Signature> = HashMap::new();
    for func in functions {
        signatures.insert(
            func.name.clone(),
            Signature {
                params: func.params.iter().map(|p| p.ty.clone()).collect(),
                result: func.return_type.clone(),
            },
        );
    }

    // Parse the expression into an Expr AST
    let expr = parse_expr(
        &inlined_sexpr,
        &[],
        &signatures,
        &HashMap::new(),
        &HashMap::new(),
        &ctx,
    )?;

    // Infer the return type
    let return_type = check_expr(
        &expr,
        &HashMap::new(),
        &signatures,
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
    )?;

    // Create the eval function
    let eval_fn = Function {
        name: "eval".to_string(),
        params: vec![],
        return_type,
        body: expr,
    };

    // Build the program
    let mut all_functions = functions.to_vec();
    all_functions.push(eval_fn);

    let prog = Program {
        functions: all_functions,
        imports: vec![],
        exports: vec!["eval".to_string()],
        globals: vec![],
        records: vec![],
        variants: vec![],
        resources: vec![],
        world_config: None,
    };

    // Type check
    let full_signatures = collect_signatures(&prog)?;
    type_check(&prog, &full_signatures, &ctx)?;

    // Generate Pack/Graph ABI WAT
    let wat = generate_wat_pack(&prog, &full_signatures);

    // Convert WAT to WASM bytes (raw module, not component)
    let wasm_bytes = parse_str(&wat).context("failed to convert generated WAT to wasm")?;

    Ok(wasm_bytes)
}

/// Like compile_repl_expr_pack but returns WAT string instead of WASM bytes.
/// Useful for debugging and testing.
pub fn compile_repl_expr_pack_wat(
    expr_source: &str,
    bindings: &HashMap<String, InlineValue>,
    functions: &[Function],
) -> Result<String> {
    let ctx = CompileContext::new(expr_source.to_string(), "<repl>".to_string());

    let tokens = tokenize(expr_source);
    if tokens.is_empty() {
        bail!("empty expression");
    }

    let (sexpr, _) = parse_sexpr(&tokens, 0);
    let inlined_sexpr = inline_bindings(&sexpr, bindings);

    let mut signatures: HashMap<String, Signature> = HashMap::new();
    for func in functions {
        signatures.insert(
            func.name.clone(),
            Signature {
                params: func.params.iter().map(|p| p.ty.clone()).collect(),
                result: func.return_type.clone(),
            },
        );
    }

    let expr = parse_expr(
        &inlined_sexpr,
        &[],
        &signatures,
        &HashMap::new(),
        &HashMap::new(),
        &ctx,
    )?;

    let return_type = check_expr(
        &expr,
        &HashMap::new(),
        &signatures,
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
    )?;

    let eval_fn = Function {
        name: "eval".to_string(),
        params: vec![],
        return_type,
        body: expr,
    };

    let mut all_functions = functions.to_vec();
    all_functions.push(eval_fn);

    let prog = Program {
        functions: all_functions,
        imports: vec![],
        exports: vec!["eval".to_string()],
        globals: vec![],
        records: vec![],
        variants: vec![],
        resources: vec![],
        world_config: None,
    };

    let full_signatures = collect_signatures(&prog)?;
    type_check(&prog, &full_signatures, &ctx)?;

    Ok(generate_wat_pack(&prog, &full_signatures))
}
