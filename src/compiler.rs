use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, anyhow, bail};
use wat::parse_str;
use wit_component::{ComponentEncoder, StringEncoding, embed_component_metadata};
use wit_parser::Resolve;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Type {
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
        Self { scopes: HashSet::new() }
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
struct Span {
    line: usize,
    column: usize,
    length: usize,
    scopes: ScopeSet,
}

impl Span {
    fn new(line: usize, column: usize, length: usize) -> Self {
        Self { line, column, length, scopes: ScopeSet::base() }
    }

    /// Create a dummy span for generated code
    fn dummy() -> Self {
        Self { line: 0, column: 0, length: 0, scopes: ScopeSet::base() }
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
        write!(f, "{} at line {}, column {}", self.message, self.span.line, self.span.column)
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

    fn error_with_note(&self, message: impl Into<String>, span: &Span, note: impl Into<String>) -> anyhow::Error {
        let err = CompileError::new(message, span.clone()).with_note(note);
        anyhow::anyhow!("{}", err.format(&self.source, &self.file_path))
    }
}

#[derive(Debug)]
pub struct CompileArtifacts {
    pub wat: PathBuf,
    pub wit: PathBuf,
    pub component: PathBuf,
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
    let wat = generate_wat(&prog, &signatures);
    let wit = generate_wit(&prog);
    let mut wat_path = out_base.to_path_buf();
    wat_path.set_extension("wat");
    let mut component_path = out_base.to_path_buf();
    component_path.set_extension("wasm");
    let mut wit_path = out_base.to_path_buf();
    wit_path.set_extension("wit");

    fs::write(&wat_path, &wat)
        .with_context(|| format!("failed to write {}", wat_path.display()))?;
    fs::write(&wit_path, &wit)
        .with_context(|| format!("failed to write {}", wit_path.display()))?;

    let wasm_bytes = parse_str(&wat).context("failed to convert generated WAT to wasm")?;
    let component_bytes = encode_component(&wasm_bytes, &wit)?;
    fs::write(&component_path, component_bytes)
        .with_context(|| format!("failed to write {}", component_path.display()))?;

    Ok(CompileArtifacts {
        wat: wat_path,
        wit: wit_path,
        component: component_path,
    })
}

fn encode_component(module: &[u8], wit_source: &str) -> Result<Vec<u8>> {
    let mut resolve = Resolve::new();
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
struct Token {
    kind: TokenKind,
    span: Span,
}

#[derive(Debug, Clone)]
enum TokenKind {
    LParen,
    RParen,
    Symbol(String),
    Number(NumericToken),
    String(String),   // String literal
    Quasiquote,       // `
    Unquote,          // ,
    UnquoteSplice,    // ,@
    SyntaxQuote,      // #'
    Quasisyntax,      // #`
    Unsyntax,         // #,
    UnsyntaxSplice,   // #,@
}

#[derive(Debug, Clone)]
enum NumericToken {
    Int { value: i64, ty: Type },
    Float { value: f64, ty: Type },
}

#[derive(Debug, Clone)]
enum SExpr {
    Sym(String, Span),
    Int { value: i64, ty: Type, span: Span },
    Float { value: f64, ty: Type, span: Span },
    Str(String, Span),  // String literal
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

#[derive(Debug)]
enum Expr {
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
}

/// A single arm in a match expression
#[derive(Debug)]
struct MatchArm {
    case_name: String,
    bindings: Vec<String>, // Variable names to bind payload values
    body: Expr,
}

#[derive(Debug)]
struct Function {
    name: String,
    params: Vec<Parameter>,
    return_type: Type,
    body: Expr,
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
            let mut scope_ids: Vec<_> = self.scopes.scopes.iter()
                .filter(|&&s| s != 0)
                .cloned()
                .collect();
            scope_ids.sort();
            format!("{}__hyg{}", self.name, scope_ids.iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join("_"))
        }
    }
}

#[derive(Debug, Clone)]
struct Parameter {
    name: String,
    ty: Type,
    scopes: ScopeSet,
}

#[derive(Debug, Clone)]
struct Import {
    module: String,
    name: String,
    params: Vec<Parameter>,
    return_type: Type,
    span: Span,
}

#[derive(Debug, Clone)]
struct Global {
    name: String,
    ty: Type,
    mutable: bool,
    init_value: i64, // For simplicity, we'll only support integer constants initially
}

/// A field in a record type
#[derive(Debug, Clone)]
struct RecordField {
    name: String,
    ty: Type,
}

/// A record type definition
#[derive(Debug, Clone)]
struct RecordDef {
    name: String,
    fields: Vec<RecordField>,
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
struct VariantCase {
    name: String,
    payload: Vec<Type>, // Can have 0, 1, or more payload types
}

/// A variant type definition (sum type)
#[derive(Debug, Clone)]
struct VariantDef {
    name: String,
    cases: Vec<VariantCase>,
}

impl VariantDef {
    /// Calculate the size of this variant in bytes (discriminant + max payload)
    fn size(&self) -> usize {
        let discriminant_size = 4; // i32 discriminant
        let max_payload_size = self.cases.iter()
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
fn find_variant_by_case<'a>(case_name: &str, variants: &'a HashMap<String, VariantDef>) -> Option<&'a VariantDef> {
    variants.values().find(|v| v.find_case(case_name).is_some())
}

/// Get the size of a type in bytes (for memory layout)
fn type_size(ty: &Type) -> usize {
    match ty {
        Type::S32 | Type::F32 => 4,
        Type::S64 | Type::F64 => 8,
        // Records, variants, options, results, lists, and strings are pointer-sized
        Type::Record(_) | Type::Variant(_) | Type::Option(_) | Type::Result(_, _) | Type::List(_) | Type::Str => 4,
    }
}

/// Check if a type requires heap allocation
fn type_needs_heap(ty: &Type) -> bool {
    match ty {
        Type::S32 | Type::S64 | Type::F32 | Type::F64 => false,
        Type::Record(_) | Type::Variant(_) | Type::Option(_) | Type::Result(_, _) | Type::List(_) | Type::Str => true,
    }
}

/// Check if an expression uses heap allocation
fn expr_uses_heap(expr: &Expr) -> bool {
    match expr {
        Expr::Int { .. } | Expr::Float { .. } | Expr::Var(_) | Expr::GlobalGet { .. } => false,
        Expr::StringLiteral(_) => true,
        Expr::Ascribe { expr, .. } => expr_uses_heap(expr),
        Expr::Call { args, .. } => args.iter().any(expr_uses_heap),
        Expr::If { cond, then_branch, else_branch } => {
            expr_uses_heap(cond) || expr_uses_heap(then_branch) || expr_uses_heap(else_branch)
        }
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
    param: String,       // The stx parameter name
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

#[derive(Debug)]
struct Program {
    functions: Vec<Function>,
    imports: Vec<Import>,
    exports: Vec<String>,
    globals: Vec<Global>,
    records: Vec<RecordDef>,
    variants: Vec<VariantDef>,
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

fn type_check(prog: &Program, signatures: &HashMap<String, Signature>, _ctx: &CompileContext) -> Result<()> {
    // Build global type map
    let mut globals_map = HashMap::new();
    for global in &prog.globals {
        globals_map.insert(global.name.clone(), (global.ty.clone(), global.mutable));
    }

    // Build records and variants maps for type checking
    let records_map: HashMap<String, RecordDef> = prog.records
        .iter()
        .map(|r| (r.name.clone(), r.clone()))
        .collect();
    let variants_map: HashMap<String, VariantDef> = prog.variants
        .iter()
        .map(|v| (v.name.clone(), v.clone()))
        .collect();

    for func in &prog.functions {
        let mut env = HashMap::new();
        for param in &func.params {
            env.insert(param.name.clone(), param.ty.clone());
        }
        let body_ty = check_expr(&func.body, &env, signatures, &globals_map, &records_map, &variants_map)?;
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
        Expr::RecordConstruct { record_name, fields } => {
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
        Expr::RecordAccess { record_name, field_name, expr } => {
            let record_def = records
                .get(record_name)
                .ok_or_else(|| anyhow!("unknown record type '{}'", record_name))?;
            let field = record_def.fields
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
        Expr::VariantConstruct { variant_name, case_name, payload } => {
            let variant_def = variants
                .get(variant_name)
                .ok_or_else(|| anyhow!("unknown variant type '{}'", variant_name))?;
            let (_, case) = variant_def.find_case(case_name)
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
                            other => bail!("option match: unknown case '{}', expected 'some' or 'none'", other),
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

                        let arm_ty = check_expr(&arm.body, &arm_env, signatures, globals, records, variants)?;
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
                    return result_ty.ok_or_else(|| anyhow!("match expression must have at least one case"));
                }
                Type::Result(ok_ty, err_ty) => {
                    // Result can match on 'ok' and 'err'
                    let mut result_ty: Option<Type> = None;
                    for arm in cases {
                        // Validate case names and bindings for result
                        let (expected_bindings, payload_ty) = match arm.case_name.as_str() {
                            "ok" => (1, (**ok_ty).clone()),
                            "err" => (1, (**err_ty).clone()),
                            other => bail!("result match: unknown case '{}', expected 'ok' or 'err'", other),
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

                        let arm_ty = check_expr(&arm.body, &arm_env, signatures, globals, records, variants)?;
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
                    return result_ty.ok_or_else(|| anyhow!("match expression must have at least one case"));
                }
                Type::Variant(variant_name) => {
                    // User-defined variant
                    let variant_def = variants
                        .get(variant_name)
                        .ok_or_else(|| anyhow!("unknown variant type '{}'", variant_name))?;

                    // Check that all cases exist and have correct bindings
                    let mut result_ty: Option<Type> = None;
                    for arm in cases {
                        let (_, case) = variant_def.find_case(&arm.case_name)
                            .ok_or_else(|| anyhow!("variant '{}' has no case '{}'", variant_name, arm.case_name))?;
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

                        let arm_ty = check_expr(&arm.body, &arm_env, signatures, globals, records, variants)?;
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
                    return result_ty.ok_or_else(|| anyhow!("match expression must have at least one case"));
                }
                _ => bail!("match expression must be a variant, option, or result type, got {:?}", expr_ty),
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
        Expr::None { inner_type } => {
            Ok(Type::Option(Box::new(inner_type.clone())))
        }
        // Result constructors
        Expr::Ok { ok_type, err_type, value } => {
            let value_ty = check_expr(value, env, signatures, globals, records, variants)?;
            if value_ty != *ok_type {
                bail!(
                    "ok value type mismatch: expected {:?}, got {:?}",
                    ok_type,
                    value_ty
                );
            }
            Ok(Type::Result(Box::new(ok_type.clone()), Box::new(err_type.clone())))
        }
        Expr::Err { ok_type, err_type, value } => {
            let value_ty = check_expr(value, env, signatures, globals, records, variants)?;
            if value_ty != *err_type {
                bail!(
                    "err value type mismatch: expected {:?}, got {:?}",
                    err_type,
                    value_ty
                );
            }
            Ok(Type::Result(Box::new(ok_type.clone()), Box::new(err_type.clone())))
        }
        // List operations
        Expr::ListNew { elem_type } => {
            Ok(Type::List(Box::new(elem_type.clone())))
        }
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
    }
}

fn tokenize(input: &str) -> Vec<Token> {
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
                            if c2.is_whitespace() || c2 == '(' || c2 == ')' || c2 == '`' || c2 == ',' || c2 == ';' || c2 == '\'' {
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

fn parse_sexpr(tokens: &[Token], pos: usize) -> (SExpr, usize) {
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
                        panic!("Unclosed parenthesis at line {}, column {}", start_span.line, start_span.column);
                    }
                }
            }
        }
        Some((TokenKind::RParen, span)) => {
            panic!("Unexpected closing parenthesis at line {}, column {}", span.line, span.column);
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

    CollectedMacros { defmacros, syntax_rules, syntax_case }
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
        SExpr::List(lits, _) => {
            lits.iter()
                .filter_map(|l| match l {
                    SExpr::Sym(s, _) => Some(s.clone()),
                    _ => None,
                })
                .collect()
        }
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

    Some(SyntaxRulesMacro { name, literals, rules })
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
        SExpr::List(params, _) if params.len() == 1 => {
            match &params[0] {
                SExpr::Sym(s, _) => s.clone(),
                _ => {
                    eprintln!("syntax-case-lambda parameter must be a symbol");
                    return None;
                }
            }
        }
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
                        SExpr::List(lits, _) => {
                            lits.iter()
                                .filter_map(|l| match l {
                                    SExpr::Sym(s, _) => Some(s.clone()),
                                    _ => None,
                                })
                                .collect()
                        }
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

    Some(SyntaxCaseMacro { name, param, literals, clauses })
}

/// Parse a syntax-case clause: (pattern template) or (pattern guard template)
fn parse_syntax_case_clause(form: &SExpr, macro_name: &str, literals: &[String]) -> Option<SyntaxCaseClause> {
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
        Some(SyntaxCaseClause { pattern, guard: None, template })
    } else {
        // With guard: (pattern guard template)
        let guard = parse_compile_time_expr(&items[1], &pattern_vars)?;
        let template = parse_compile_time_expr(&items[2], &pattern_vars)?;
        Some(SyntaxCaseClause { pattern, guard: Some(guard), template })
    }
}

/// Parse a compile-time expression from an S-expression
fn parse_compile_time_expr(sexpr: &SExpr, pattern_vars: &HashSet<String>) -> Option<CompileTimeExpr> {
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
        SExpr::Int { .. } | SExpr::Float { .. } => {
            Some(CompileTimeExpr::Literal(sexpr.clone()))
        }
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
            let ellipsis_pos = items.iter().position(|item| {
                matches!(item, SExpr::Sym(s, _) if s == "...")
            });

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
        SExpr::Int { .. } | SExpr::Float { .. } => {
            Some(Template::Atom(sexpr.clone()))
        }
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
                && let Some(SExpr::Sym(sym, _)) = items.first() {
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
                    let expanded = eval_quasiquote(template_inner, &substitutions, &span, Some(macro_scope));

                    // Recursively expand the result
                    return expand_macros(expanded, macros, depth + 1);
                }

                // Then check syntax-rules
                if let Some(sr_mac) = macros.syntax_rules.get(name) {
                    // Try to match against each rule in order
                    let input = SExpr::List(items.clone(), span.clone());
                    for rule in &sr_mac.rules {
                        if let Some(bindings) = match_pattern(&rule.pattern, &input, &sr_mac.literals) {
                            // Generate fresh scope for hygiene
                            let macro_scope = fresh_scope();

                            // Expand the template with bindings
                            let expanded = expand_template(&rule.template, &bindings, &span, macro_scope);

                            // Recursively expand the result
                            return expand_macros(expanded, macros, depth + 1);
                        }
                    }
                    // No rule matched
                    panic!("No matching rule for macro '{}' with input {:?}", name, items);
                }

                // Then check syntax-case
                if let Some(sc_mac) = macros.syntax_case.get(name) {
                    let input = SExpr::List(items.clone(), span.clone());
                    for clause in &sc_mac.clauses {
                        if let Some(bindings) = match_pattern(&clause.pattern, &input, &sc_mac.literals) {
                            // Generate fresh scope for hygiene
                            let macro_scope = fresh_scope();

                            // Create compile-time environment with pattern bindings
                            let mut ct_env: HashMap<String, CompileTimeValue> = HashMap::new();
                            for (name, binding) in &bindings {
                                match binding {
                                    PatternBinding::Single(sexpr) => {
                                        ct_env.insert(name.clone(), CompileTimeValue::Syntax(sexpr.clone()));
                                    }
                                    PatternBinding::List(sexprs) => {
                                        let vals = sexprs.iter()
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
                                let result = eval_compile_time_expr(&clause.template, &ct_env, &span, macro_scope);

                                // Convert result to SExpr
                                let expanded = match result {
                                    CompileTimeValue::Syntax(sexpr) => sexpr,
                                    other => panic!("syntax-case template must return syntax, got {:?}", other),
                                };

                                // Recursively expand the result
                                return expand_macros(expanded, macros, depth + 1);
                            }
                        }
                    }
                    // No clause matched
                    panic!("No matching clause for syntax-case macro '{}' with input {:?}", name, items);
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
                    CompileTimeValue::Syntax(s) => s.clone(),  // Keep original scopes (from call site)
                    CompileTimeValue::Int(i) => SExpr::Int {
                        value: *i,
                        ty: Type::S32,
                        span: sym_span.clone(),
                    },
                    CompileTimeValue::Bool(true) => SExpr::Sym("#t".to_string(), sym_span.with_scope(macro_scope)),
                    CompileTimeValue::Bool(false) => SExpr::Sym("#f".to_string(), sym_span.with_scope(macro_scope)),
                    CompileTimeValue::List(_) => panic!("Cannot substitute list value as single syntax"),
                }
            } else {
                // Not a pattern variable - add macro scope for hygiene
                SExpr::Sym(name.clone(), sym_span.with_scope(macro_scope))
            }
        }
        SExpr::List(items, list_span) => {
            let substituted: Vec<_> = items.iter()
                .map(|item| substitute_pattern_vars_in_syntax(item, env, span, macro_scope))
                .collect();
            SExpr::List(substituted, list_span.with_scope(macro_scope))
        }
        SExpr::Int { value, ty, span: int_span } => SExpr::Int {
            value: *value,
            ty: ty.clone(),
            span: int_span.with_scope(macro_scope),
        },
        SExpr::Float { value, ty, span: float_span } => SExpr::Float {
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
        CompileTimeExpr::Literal(sexpr) => {
            match sexpr {
                SExpr::Int { value, .. } => CompileTimeValue::Int(*value),
                SExpr::Sym(s, _) if s == "#t" || s == "true" => CompileTimeValue::Bool(true),
                SExpr::Sym(s, _) if s == "#f" || s == "false" => CompileTimeValue::Bool(false),
                other => CompileTimeValue::Syntax(other.clone()),
            }
        }
        CompileTimeExpr::If { cond, then_branch, else_branch } => {
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
            let arg_vals: Vec<_> = args.iter()
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
                        Some(CompileTimeValue::Syntax(SExpr::Int { .. })) => CompileTimeValue::Bool(true),
                        Some(CompileTimeValue::Syntax(SExpr::Float { .. })) => CompileTimeValue::Bool(true),
                        Some(CompileTimeValue::Int(_)) => CompileTimeValue::Bool(true),
                        _ => CompileTimeValue::Bool(false),
                    }
                }
                "syntax->datum" => {
                    // Extract the datum from syntax
                    match arg_vals.first() {
                        Some(CompileTimeValue::Syntax(SExpr::Int { value, .. })) => CompileTimeValue::Int(*value),
                        Some(CompileTimeValue::Syntax(SExpr::Sym(s, _))) => {
                            CompileTimeValue::Syntax(SExpr::Sym(s.clone(), Span::dummy()))
                        }
                        Some(v) => v.clone(),
                        None => panic!("syntax->datum requires an argument"),
                    }
                }
                "not" => {
                    match arg_vals.first() {
                        Some(CompileTimeValue::Bool(b)) => CompileTimeValue::Bool(!b),
                        Some(CompileTimeValue::Int(0)) => CompileTimeValue::Bool(true),
                        _ => CompileTimeValue::Bool(false),
                    }
                }
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
                    let sum: i64 = arg_vals.iter().map(|v| match v {
                        CompileTimeValue::Int(i) => *i,
                        CompileTimeValue::Syntax(SExpr::Int { value, .. }) => *value,
                        _ => 0,
                    }).sum();
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
                        let rest: i64 = arg_vals[1..].iter().map(|v| match v {
                            CompileTimeValue::Int(i) => *i,
                            CompileTimeValue::Syntax(SExpr::Int { value, .. }) => *value,
                            _ => 0,
                        }).sum();
                        CompileTimeValue::Int(first - rest)
                    } else {
                        CompileTimeValue::Int(0)
                    }
                }
                "integer?" => {
                    match arg_vals.first() {
                        Some(CompileTimeValue::Int(_)) => CompileTimeValue::Bool(true),
                        Some(CompileTimeValue::Syntax(SExpr::Int { .. })) => CompileTimeValue::Bool(true),
                        _ => CompileTimeValue::Bool(false),
                    }
                }
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
                    let arg_sexprs: Vec<_> = arg_vals.iter().map(|v| match v {
                        CompileTimeValue::Syntax(s) => s.clone(),
                        CompileTimeValue::Bool(true) => SExpr::Sym("#t".to_string(), span.with_scope(macro_scope)),
                        CompileTimeValue::Bool(false) => SExpr::Sym("#f".to_string(), span.with_scope(macro_scope)),
                        CompileTimeValue::Int(i) => SExpr::Int { value: *i, ty: Type::S32, span: span.with_scope(macro_scope) },
                        CompileTimeValue::List(items) => {
                            let sexprs: Vec<_> = items.iter().map(|item| match item {
                                CompileTimeValue::Syntax(s) => s.clone(),
                                _ => SExpr::Sym("?".to_string(), span.with_scope(macro_scope)),
                            }).collect();
                            SExpr::List(sexprs, span.with_scope(macro_scope))
                        }
                    }).collect();
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
                            CompileTimeValue::Int(i) => SExpr::Int { value: *i, ty: Type::S32, span: span.with_scope(macro_scope) },
                            CompileTimeValue::Bool(true) => SExpr::Sym("#t".to_string(), span.with_scope(macro_scope)),
                            CompileTimeValue::Bool(false) => SExpr::Sym("#f".to_string(), span.with_scope(macro_scope)),
                            CompileTimeValue::List(_) => panic!("Cannot unsyntax a list directly, use #,@"),
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
                    CompileTimeValue::Syntax(syntax) => syntax.clone(),  // Keep original scopes
                    CompileTimeValue::Int(i) => SExpr::Int {
                        value: *i,
                        ty: Type::S32,
                        span: sym_span.clone(),
                    },
                    CompileTimeValue::Bool(true) => SExpr::Sym("#t".to_string(), sym_span.with_scope(macro_scope)),
                    CompileTimeValue::Bool(false) => SExpr::Sym("#f".to_string(), sym_span.with_scope(macro_scope)),
                    CompileTimeValue::List(_) => panic!("Cannot substitute list as single syntax in quasisyntax"),
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
        Pattern::List(patterns) => {
            match input {
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
            }
        }
        Pattern::ListWithEllipsis { before, repeated, after } => {
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
        Template::Variable(name) => {
            match bindings.get(name) {
                Some(PatternBinding::List(items)) => items.len(),
                _ => 0,
            }
        }
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
        Template::Variable(name) => {
            match bindings.get(name) {
                Some(PatternBinding::List(items)) => {
                    items.get(index).cloned().unwrap_or_else(|| {
                        SExpr::List(vec![], span.clone())
                    })
                }
                Some(PatternBinding::Single(expr)) => expr.clone(),
                None => SExpr::Sym(name.clone(), span.with_scope(macro_scope)),
            }
        }
        Template::Symbol(name) => {
            SExpr::Sym(name.clone(), span.with_scope(macro_scope))
        }
        Template::Atom(sexpr) => {
            add_scope_to_sexpr(sexpr, macro_scope)
        }
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
        SExpr::Int { value, ty, span: int_span } => SExpr::Int {
            value: *value,
            ty: ty.clone(),
            span: add_scope_to_span(int_span, macro_scope),
        },
        SExpr::Float { value, ty, span: float_span } => SExpr::Float {
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

    // First pass: collect type names (records and variants) so we can distinguish them
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
                                return Err(ctx.error(format!("duplicate record type '{}'", name), span));
                            }
                        }
                    }
                }
                SExpr::Sym(sym, _) if sym == "variant" => {
                    if items.len() >= 2 {
                        if let SExpr::Sym(name, _) = &items[1] {
                            if !variant_names.insert(name.clone()) {
                                return Err(ctx.error(format!("duplicate variant type '{}'", name), span));
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
                        let func = parse_fn_form(SExpr::List(items, span.clone()), &variant_names, ctx)?;
                        if !defined.insert(func.name.clone()) {
                            return Err(ctx.error(format!("duplicate function '{}'", func.name), &span));
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
                                let func = parse_fn_form(items[1].clone(), &variant_names, ctx)?;
                                if !defined.insert(func.name.clone()) {
                                    return Err(ctx.error(format!("duplicate function '{}'", func.name), inner_span));
                                }
                                if export_set.insert(func.name.clone()) {
                                    exports.push(func.name.clone());
                                }
                                pending.push(func);
                            }
                            other => return Err(ctx.error("export argument must be a symbol or (fn ...)", other.span())),
                        }
                    }
                    SExpr::Sym(sym, _) if sym == "import" => {
                        let import = parse_import_form(&items, &variant_names, ctx)?;
                        if defined.contains(&import.name) {
                            return Err(ctx.error(
                                format!("function '{}' is already defined and cannot be imported", import.name),
                                &span
                            ));
                        }
                        if !imported.insert(import.name.clone()) {
                            return Err(ctx.error(format!("duplicate import '{}'", import.name), &span));
                        }
                        imports.push(import);
                    }
                    SExpr::Sym(sym, _) if sym == "global" => {
                        let global = parse_global_form(&items, &variant_names, ctx)?;
                        if !global_names.insert(global.name.clone()) {
                            return Err(ctx.error(format!("duplicate global '{}'", global.name), &span));
                        }
                        globals.push(global);
                    }
                    SExpr::Sym(sym, _) if sym == "record" => {
                        let record = parse_record_form(&items, &variant_names, ctx)?;
                        // Already checked for duplicates in first pass
                        records.push(record);
                    }
                    SExpr::Sym(sym, _) if sym == "variant" => {
                        let variant = parse_variant_form(&items, &variant_names, ctx)?;
                        // Already checked for duplicates in first pass
                        variants.push(variant);
                    }
                    other => {
                        return Err(ctx.error_with_note(
                            "unknown top-level form",
                            other.span(),
                            "expected 'fn', 'export', 'import', 'global', 'record', or 'variant'"
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
            return Err(ctx.error(format!("duplicate function '{}'", import.name), &import.span));
        }
    }

    for (export, export_span) in exports.iter().zip(export_set.iter()) {
        if !signatures.contains_key(export) {
            return Err(ctx.error(format!("cannot export undefined function '{}'", export), &Span::dummy()));
        }
        if imported.contains(export) {
            return Err(ctx.error(format!("cannot export imported function '{}'", export), &Span::dummy()));
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
        let body_expr = parse_expr(&func.body, &param_bindings, &signatures, &records_map, &variants_map, ctx)?;
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
    })
}

fn parse_fn_form(form: SExpr, variant_names: &HashSet<String>, ctx: &CompileContext) -> Result<PendingFunction> {
    let (items, span) = match form {
        SExpr::List(items, span) => (items, span),
        other => return Err(ctx.error("function definition must be a list", other.span())),
    };
    if items.len() != 5 {
        return Err(ctx.error_with_note(
            "invalid function definition",
            &span,
            "expected: (fn name ((param type) ...) return-type body)"
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
    let params = parse_typed_params(&items[2], variant_names, ctx)?;
    let return_type = parse_type_expr(&items[3], variant_names, ctx)?;
    Ok(PendingFunction {
        name,
        params,
        return_type,
        body: items[4].clone(),
        span,
    })
}

fn parse_import_form(items: &[SExpr], variant_names: &HashSet<String>, ctx: &CompileContext) -> Result<Import> {
    let span = items[0].span().clone();
    if items.len() != 5 {
        return Err(ctx.error_with_note(
            "invalid import declaration",
            &span,
            "expected: (import module name ((param type) ...) return-type)"
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
    let params = parse_typed_params(&items[3], variant_names, ctx)?;
    let return_type = parse_type_expr(&items[4], variant_names, ctx)?;

    Ok(Import {
        module,
        name,
        params,
        return_type,
        span,
    })
}

fn parse_global_form(items: &[SExpr], variant_names: &HashSet<String>, ctx: &CompileContext) -> Result<Global> {
    let span = items[0].span().clone();
    if items.len() != 5 {
        return Err(ctx.error_with_note(
            "invalid global declaration",
            &span,
            "expected: (global $name type mutability init-value)"
        ));
    }

    let name = match &items[1] {
        SExpr::Sym(s, sym_span) => {
            if !s.starts_with('$') {
                return Err(ctx.error_with_note(
                    "global name must start with '$'",
                    sym_span,
                    "e.g., $heap-ptr, $counter"
                ));
            }
            s.clone()
        }
        other => return Err(ctx.error("global name must be a symbol starting with $", other.span())),
    };

    let ty = parse_type_expr(&items[2], variant_names, ctx)?;

    let mutable = match &items[3] {
        SExpr::Sym(s, sym_span) => match s.as_str() {
            "mut" => true,
            "const" => false,
            _ => return Err(ctx.error_with_note(
                "invalid mutability specifier",
                sym_span,
                "expected 'mut' or 'const'"
            )),
        },
        other => return Err(ctx.error("mutability must be 'mut' or 'const'", other.span())),
    };

    let init_value = match &items[4] {
        SExpr::Int { value, .. } => *value,
        other => return Err(ctx.error("global init value must be an integer constant", other.span())),
    };

    Ok(Global {
        name,
        ty,
        mutable,
        init_value,
    })
}

/// Parse a record definition: (record name (field1 type1) (field2 type2) ...)
fn parse_record_form(items: &[SExpr], variant_names: &HashSet<String>, ctx: &CompileContext) -> Result<RecordDef> {
    let span = items[0].span().clone();

    if items.len() < 2 {
        return Err(ctx.error_with_note(
            "invalid record declaration",
            &span,
            "expected: (record name (field type) ...)"
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
                        "expected: (field-name type)"
                    ));
                }
                let field_name = match &parts[0] {
                    SExpr::Sym(s, _) => s.clone(),
                    other => return Err(ctx.error("field name must be a symbol", other.span())),
                };
                let field_ty = parse_type_expr(&parts[1], variant_names, ctx)?;
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
            "add fields like: (record point (x s32) (y s32))"
        ));
    }

    Ok(RecordDef { name, fields })
}

fn parse_variant_form(items: &[SExpr], variant_names: &HashSet<String>, ctx: &CompileContext) -> Result<VariantDef> {
    let span = items[0].span().clone();

    if items.len() < 2 {
        return Err(ctx.error_with_note(
            "invalid variant declaration",
            &span,
            "expected: (variant name (case payload...) ...)"
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
                    payload.push(parse_type_expr(ty_expr, variant_names, ctx)?);
                }
                cases.push(VariantCase {
                    name: case_name,
                    payload,
                });
            }
            other => return Err(ctx.error("variant case must be a list (case-name type...)", other.span())),
        }
    }

    if cases.is_empty() {
        return Err(ctx.error_with_note(
            "variant must have at least one case",
            &span,
            "add cases like: (variant shape (circle s32) (rectangle s32 s32) (point))"
        ));
    }

    Ok(VariantDef { name, cases })
}

fn parse_typed_params(expr: &SExpr, variant_names: &HashSet<String>, ctx: &CompileContext) -> Result<Vec<Parameter>> {
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
                                "expected: (name type)"
                            ));
                        }
                        let (name, scopes) = match &parts[0] {
                            SExpr::Sym(s, span) => (s.clone(), span.scopes.clone()),
                            other => return Err(ctx.error("parameter name must be a symbol", other.span())),
                        };
                        let ty = parse_type_expr(&parts[1], variant_names, ctx)?;
                        result.push(Parameter { name, ty, scopes });
                    }
                    other => return Err(ctx.error_with_note(
                        "invalid parameter",
                        other.span(),
                        "expected: (name type)"
                    )),
                }
            }
            Ok(result)
        }
        other => Err(ctx.error("expected parameter list", other.span())),
    }
}

fn parse_type_expr(expr: &SExpr, variant_names: &HashSet<String>, ctx: &CompileContext) -> Result<Type> {
    match expr {
        SExpr::Sym(s, span) => parse_type_symbol(s, variant_names, span, ctx),
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
                            "expected: (option T)"
                        ));
                    }
                    let inner = parse_type_expr(&items[1], variant_names, ctx)?;
                    Ok(Type::Option(Box::new(inner)))
                }
                SExpr::Sym(s, _) if s == "result" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid result type",
                            span,
                            "expected: (result T E)"
                        ));
                    }
                    let ok_ty = parse_type_expr(&items[1], variant_names, ctx)?;
                    let err_ty = parse_type_expr(&items[2], variant_names, ctx)?;
                    Ok(Type::Result(Box::new(ok_ty), Box::new(err_ty)))
                }
                SExpr::Sym(s, _) if s == "list" => {
                    if items.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid list type",
                            span,
                            "expected: (list T)"
                        ));
                    }
                    let inner = parse_type_expr(&items[1], variant_names, ctx)?;
                    Ok(Type::List(Box::new(inner)))
                }
                _ => Err(ctx.error("unknown parameterized type", span)),
            }
        }
        other => Err(ctx.error("type must be a symbol or parameterized type", other.span())),
    }
}

fn parse_type_symbol(sym: &str, variant_names: &HashSet<String>, _span: &Span, _ctx: &CompileContext) -> Result<Type> {
    match sym {
        "s32" => Ok(Type::S32),
        "s64" => Ok(Type::S64),
        "f32" => Ok(Type::F32),
        "f64" => Ok(Type::F64),
        "string" => Ok(Type::Str),
        // Check if this is a variant type name
        other if variant_names.contains(other) => Ok(Type::Variant(other.to_string())),
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
                            "expected: (if condition then-expr else-expr)"
                        ));
                    }
                    let cond = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    let then_branch = parse_expr(&items[2], vars, functions, records, variants, ctx)?;
                    let else_branch = parse_expr(&items[3], vars, functions, records, variants, ctx)?;
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
                            "expected: (global.get $name)"
                        ));
                    }
                    let name = match &items[1] {
                        SExpr::Sym(s, s_span) => {
                            if !s.starts_with('$') {
                                return Err(ctx.error("global name must start with '$'", s_span));
                            }
                            s.clone()
                        }
                        other => return Err(ctx.error("global.get argument must be a global name starting with $", other.span())),
                    };
                    Ok(Expr::GlobalGet { name })
                }
                SExpr::Sym(sym, sym_span) if sym == "global.set" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'global.set' expression",
                            list_span,
                            "expected: (global.set $name value)"
                        ));
                    }
                    let name = match &items[1] {
                        SExpr::Sym(s, s_span) => {
                            if !s.starts_with('$') {
                                return Err(ctx.error("global name must start with '$'", s_span));
                            }
                            s.clone()
                        }
                        other => return Err(ctx.error("global.set first argument must be a global name starting with $", other.span())),
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
                            "expected: (let (name value) body)"
                        ));
                    }
                    let binding = match &items[1] {
                        SExpr::List(parts, _) => parts,
                        other => return Err(ctx.error_with_note(
                            "let binding must be a list",
                            other.span(),
                            "expected: (name value)"
                        )),
                    };
                    if binding.len() != 2 {
                        return Err(ctx.error_with_note(
                            "invalid let binding",
                            items[1].span(),
                            "expected: (name value)"
                        ));
                    }
                    let (name, name_scopes) = match &binding[0] {
                        SExpr::Sym(s, span) => (s.clone(), span.scopes.clone()),
                        other => return Err(ctx.error("let binding name must be a symbol", other.span())),
                    };
                    let value_expr = parse_expr(&binding[1], vars, functions, records, variants, ctx)?;
                    // Create a new binding with the name and its scopes for hygienic resolution
                    let new_binding = Binding::new(name, name_scopes);
                    let mangled_name = new_binding.mangled_name();
                    let mut next_vars = vars.to_vec();
                    next_vars.push(new_binding);
                    let body_expr = parse_expr(&items[2], &next_vars, functions, records, variants, ctx)?;
                    Ok(Expr::Let {
                        name: mangled_name,  // Use mangled name for codegen
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
                            "expected: (match expr ((case-name bindings...) body) ...)"
                        ));
                    }
                    let match_expr = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    let mut arms = Vec::new();

                    for case_item in &items[2..] {
                        let case_parts = match case_item {
                            SExpr::List(parts, _) => parts,
                            other => return Err(ctx.error("match arm must be a list", other.span())),
                        };

                        if case_parts.len() != 2 {
                            return Err(ctx.error_with_note(
                                "invalid match arm",
                                case_item.span(),
                                "expected: ((case-name bindings...) body)"
                            ));
                        }

                        // Parse pattern: (case-name binding1 binding2 ...)
                        let pattern = match &case_parts[0] {
                            SExpr::List(pat_parts, _) => pat_parts,
                            other => return Err(ctx.error("match pattern must be a list", other.span())),
                        };

                        if pattern.is_empty() {
                            return Err(ctx.error("match pattern cannot be empty", case_parts[0].span()));
                        }

                        let case_name = match &pattern[0] {
                            SExpr::Sym(s, _) => s.clone(),
                            other => return Err(ctx.error("case name must be a symbol", other.span())),
                        };

                        // Collect bindings
                        let mut bindings = Vec::new();
                        let mut next_vars = vars.to_vec();
                        for binding in &pattern[1..] {
                            let (name, name_scopes) = match binding {
                                SExpr::Sym(s, span) => (s.clone(), span.scopes.clone()),
                                other => return Err(ctx.error("binding must be a symbol", other.span())),
                            };
                            let new_binding = Binding::new(name, name_scopes);
                            let mangled_name = new_binding.mangled_name();
                            bindings.push(mangled_name);
                            next_vars.push(new_binding);
                        }

                        // Parse body with extended bindings
                        let body = parse_expr(&case_parts[1], &next_vars, functions, records, variants, ctx)?;

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
                            "expected: (some inner-type value)"
                        ));
                    }
                    let inner_type = parse_type_expr(&items[1], &HashSet::new(), ctx)?;
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
                            "expected: (none inner-type)"
                        ));
                    }
                    let inner_type = parse_type_expr(&items[1], &HashSet::new(), ctx)?;
                    Ok(Expr::None { inner_type })
                }
                // Result constructors: (ok T E value) and (err T E value)
                SExpr::Sym(sym, _sym_span) if sym == "ok" => {
                    if items.len() != 4 {
                        return Err(ctx.error_with_note(
                            "invalid 'ok' expression",
                            list_span,
                            "expected: (ok ok-type err-type value)"
                        ));
                    }
                    let ok_type = parse_type_expr(&items[1], &HashSet::new(), ctx)?;
                    let err_type = parse_type_expr(&items[2], &HashSet::new(), ctx)?;
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
                            "expected: (err ok-type err-type value)"
                        ));
                    }
                    let ok_type = parse_type_expr(&items[1], &HashSet::new(), ctx)?;
                    let err_type = parse_type_expr(&items[2], &HashSet::new(), ctx)?;
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
                            "expected: (list-new elem-type)"
                        ));
                    }
                    let elem_type = parse_type_expr(&items[1], &HashSet::new(), ctx)?;
                    Ok(Expr::ListNew { elem_type })
                }
                SExpr::Sym(sym, _sym_span) if sym == "list-push" => {
                    if items.len() != 3 {
                        return Err(ctx.error_with_note(
                            "invalid 'list-push' expression",
                            list_span,
                            "expected: (list-push list value)"
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
                            "expected: (list-get list index)"
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
                            "expected: (list-len list)"
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
                            "expected: (string-len string)"
                        ));
                    }
                    let string = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                    Ok(Expr::StringLen {
                        string: Box::new(string),
                    })
                }
                _ => {
                    if let SExpr::Sym(sym, sym_span) = op {
                        // Check if this is a WASM instruction
                        if lookup_wasm_instr(sym).is_some() {
                            let mut args = Vec::new();
                            for arg in &items[1..] {
                                args.push(parse_expr(arg, vars, functions, records, variants, ctx)?);
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
                                    list_span
                                ));
                            }
                            let mut args = Vec::new();
                            for arg in &items[1..] {
                                args.push(parse_expr(arg, vars, functions, records, variants, ctx)?);
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
                                    list_span
                                ));
                            }
                            let mut fields = Vec::new();
                            for arg in &items[1..] {
                                fields.push(parse_expr(arg, vars, functions, records, variants, ctx)?);
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
                                    list_span
                                ));
                            }
                            let mut payload = Vec::new();
                            for arg in &items[1..] {
                                payload.push(parse_expr(arg, vars, functions, records, variants, ctx)?);
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
                                            format!("expected: ({}.{} record-expr)", record_name, field_name)
                                        ));
                                    }
                                    let expr = parse_expr(&items[1], vars, functions, records, variants, ctx)?;
                                    Ok(Expr::RecordAccess {
                                        record_name: record_name.to_string(),
                                        field_name: field_name.to_string(),
                                        expr: Box::new(expr),
                                    })
                                } else {
                                    Err(ctx.error(format!("unknown record type '{}'", record_name), sym_span))
                                }
                            } else {
                                Err(ctx.error(format!("unknown function or operator '{}'", sym), sym_span))
                            }
                        } else {
                            Err(ctx.error(format!("unknown function or operator '{}'", sym), sym_span))
                        }
                    } else {
                        Err(ctx.error("expression must start with a symbol", op.span()))
                    }
                }
            }
        }
        SExpr::Quasiquote(_, span) | SExpr::Unquote(_, span) | SExpr::UnquoteSplice(_, span) => {
            Err(ctx.error("quasiquote/unquote should have been expanded before parsing", span))
        }
        SExpr::SyntaxQuote(_, span) | SExpr::Quasisyntax(_, span) |
        SExpr::Unsyntax(_, span) | SExpr::UnsyntaxSplice(_, span) => {
            Err(ctx.error("syntax forms (#', #`, #,, #,@) should have been expanded before parsing", span))
        }
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

    // Declare memory (1 page = 64KB, allow growth up to 100 pages)
    out.push_str("  (memory 1 100)\n");

    // Build global type map for codegen
    let mut globals_map = HashMap::new();
    for global in &prog.globals {
        globals_map.insert(global.name.clone(), (global.ty.clone(), global.mutable));
    }

    // Build records and variants maps for codegen
    let records_map: HashMap<String, RecordDef> = prog.records
        .iter()
        .map(|r| (r.name.clone(), r.clone()))
        .collect();
    let variants_map: HashMap<String, VariantDef> = prog.variants
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

    for func in &prog.functions {
        let mut body = String::new();
        let mut env = CodegenEnv::new(&func.params);
        gen_expr(&func.body, &mut body, 4, &mut env, signatures, &globals_map, &records_map, &variants_map);

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
    for export in &prog.exports {
        out.push_str(&format!("  (export \"{}\" (func ${}))\n", export, export));
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
            let from_ty = gen_expr(expr, out, indent, env, signatures, globals, records, variants);
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
                gen_expr(arg, out, indent, env, signatures, globals, records, variants);
            }
            out.push_str(&format!("{}call ${}\n", pad, name));
            sig.result.clone()
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_ty = gen_expr(cond, out, indent, env, signatures, globals, records, variants);
            if cond_ty != Type::S32 {
                panic!("if condition must be s32");
            }
            let result_ty = expr_type(then_branch, env, signatures, globals, records, variants);
            out.push_str(&format!("{}(if (result {})\n", pad, wat_type(&result_ty)));
            out.push_str(&format!("{}  (then\n", pad));
            gen_expr(then_branch, out, indent + 4, env, signatures, globals, records, variants);
            out.push_str(&format!("{}  )\n", pad));
            out.push_str(&format!("{}  (else\n", pad));
            let else_ty = gen_expr(else_branch, out, indent + 4, env, signatures, globals, records, variants);
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
            let value_ty = gen_expr(value, out, indent, env, signatures, globals, records, variants);
            let idx = env.declare_local(value_ty);
            out.push_str(&format!("{}local.set {}\n", pad, idx));
            env.push_binding(name.clone(), idx);
            let body_ty = gen_expr(body, out, indent, env, signatures, globals, records, variants);
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
                let value_ty = gen_expr(&args[1], out, indent, env, signatures, globals, records, variants);
                let value_local = env.declare_local(value_ty);
                out.push_str(&format!("{}local.set {}\n", pad, value_local));

                // Emit the address
                gen_expr(&args[0], out, indent, env, signatures, globals, records, variants);

                // Get the value back
                out.push_str(&format!("{}local.get {}\n", pad, value_local));

                // Emit the store
                out.push_str(&format!("{}{}\n", pad, name));

                // Put the value back on the stack as the "return value"
                out.push_str(&format!("{}local.get {}\n", pad, value_local));
            } else {
                // Normal instructions - emit args then instruction
                for arg in args {
                    gen_expr(arg, out, indent, env, signatures, globals, records, variants);
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
            let value_ty = gen_expr(value, out, indent, env, signatures, globals, records, variants);
            let value_local = env.declare_local(value_ty.clone());
            out.push_str(&format!("{}local.set {}\n", pad, value_local));
            out.push_str(&format!("{}local.get {}\n", pad, value_local));
            out.push_str(&format!("{}global.set {}\n", pad, name));
            out.push_str(&format!("{}local.get {}\n", pad, value_local));
            value_ty
        }
        Expr::RecordConstruct { record_name, fields } => {
            let record_def = records
                .get(record_name)
                .expect("record should exist");
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
            for (i, (field_expr, field_def)) in fields.iter().zip(record_def.fields.iter()).enumerate() {
                let offset = record_def.field_offset(i);

                // Compute address: ptr + offset
                out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
                if offset > 0 {
                    out.push_str(&format!("{}i32.const {}\n", pad, offset));
                    out.push_str(&format!("{}i32.add\n", pad));
                }

                // Evaluate the field expression
                gen_expr(field_expr, out, indent, env, signatures, globals, records, variants);

                // Store based on field type
                let store_instr = match &field_def.ty {
                    Type::S32 => "i32.store",
                    Type::S64 => "i64.store",
                    Type::F32 => "f32.store",
                    Type::F64 => "f64.store",
                    // All compound types are pointers
                    Type::Record(_) | Type::Variant(_) | Type::Option(_) | Type::Result(_, _) | Type::List(_) | Type::Str => "i32.store",
                };
                out.push_str(&format!("{}{}\n", pad, store_instr));
            }

            // Return the pointer to the record
            out.push_str(&format!("{}local.get {}\n", pad, ptr_local));
            Type::Record(record_name.clone())
        }
        Expr::RecordAccess { record_name, field_name, expr } => {
            let record_def = records
                .get(record_name)
                .expect("record should exist");

            // Find the field and its offset
            let (field_idx, field_def) = record_def.fields
                .iter()
                .enumerate()
                .find(|(_, f)| f.name == *field_name)
                .expect("field should exist");
            let offset = record_def.field_offset(field_idx);

            // Evaluate the record expression (gives us the pointer)
            gen_expr(expr, out, indent, env, signatures, globals, records, variants);

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
                // All compound types are pointers
                Type::Record(_) | Type::Variant(_) | Type::Option(_) | Type::Result(_, _) | Type::List(_) | Type::Str => "i32.load",
            };
            out.push_str(&format!("{}{}\n", pad, load_instr));
            field_def.ty.clone()
        }
        Expr::VariantConstruct { variant_name, case_name, payload } => {
            let variant_def = variants
                .get(variant_name)
                .expect("variant should exist");
            let (case_idx, case) = variant_def.find_case(case_name)
                .expect("case should exist");
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
                gen_expr(payload_expr, out, indent, env, signatures, globals, records, variants);

                // Store based on payload type
                let store_instr = match payload_ty {
                    Type::S32 => "i32.store",
                    Type::S64 => "i64.store",
                    Type::F32 => "f32.store",
                    Type::F64 => "f64.store",
                    Type::Record(_) | Type::Variant(_) | Type::Option(_) | Type::Result(_, _) | Type::List(_) | Type::Str => "i32.store",
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
            let expr_ty = gen_expr(expr, out, indent, env, signatures, globals, records, variants);

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
                            out.push_str(&format!("{}(if (result {})\n", pad, wat_type(&result_ty)));
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
                                Type::Record(_) | Type::Variant(_) | Type::Option(_) | Type::Result(_, _) | Type::List(_) | Type::Str => "i32.load",
                            };
                            out.push_str(&format!("{}    {}\n", pad, load_instr));

                            let idx = env.declare_local((**inner_ty).clone());
                            out.push_str(&format!("{}    local.set {}\n", pad, idx));
                            env.push_binding(arm.bindings[0].clone(), idx);
                        }

                        // Generate arm body
                        gen_expr(&arm.body, out, indent + 4, env, signatures, globals, records, variants);

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
                            out.push_str(&format!("{}(if (result {})\n", pad, wat_type(&result_ty)));
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
                                Type::Record(_) | Type::Variant(_) | Type::Option(_) | Type::Result(_, _) | Type::List(_) | Type::Str => "i32.load",
                            };
                            out.push_str(&format!("{}    {}\n", pad, load_instr));

                            let idx = env.declare_local(payload_ty.clone());
                            out.push_str(&format!("{}    local.set {}\n", pad, idx));
                            env.push_binding(arm.bindings[0].clone(), idx);
                        }

                        // Generate arm body
                        gen_expr(&arm.body, out, indent + 4, env, signatures, globals, records, variants);

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
                    let variant_def = variants
                        .get(variant_name)
                        .expect("variant should exist");

                    // Load discriminant
                    out.push_str(&format!("{}local.get {}\n", pad, value_ptr));
                    out.push_str(&format!("{}i32.load\n", pad));

                    // Determine result type from first arm
                    let result_ty = if let Some(first_arm) = cases.first() {
                        // Build environment for first arm to get its type
                        let (_, first_case) = variant_def.find_case(&first_arm.case_name).expect("case should exist");
                        let mut arm_env = HashMap::new();
                        for (binding, ty) in first_arm.bindings.iter().zip(first_case.payload.iter()) {
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
                        let (case_idx, case) = variant_def.find_case(&arm.case_name)
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
                            out.push_str(&format!("{}(if (result {})\n", pad, wat_type(&result_ty)));
                            out.push_str(&format!("{}  (then\n", pad));
                        } else if !is_last {
                            out.push_str(&format!("{}(if (result {})\n", pad, wat_type(&result_ty)));
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
                                Type::Record(_) | Type::Variant(_) | Type::Option(_) | Type::Result(_, _) | Type::List(_) | Type::Str => "i32.load",
                            };
                            out.push_str(&format!("{}    {}\n", pad, load_instr));

                            // Save to local
                            let idx = env.declare_local(payload_ty.clone());
                            out.push_str(&format!("{}    local.set {}\n", pad, idx));
                            env.push_binding(binding.clone(), idx);

                            payload_offset += type_size(payload_ty);
                        }

                        // Generate arm body
                        gen_expr(&arm.body, out, indent + 4, env, signatures, globals, records, variants);

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
            gen_expr(value, out, indent, env, signatures, globals, records, variants);
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
        Expr::Ok { ok_type, err_type, value } => {
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
            gen_expr(value, out, indent, env, signatures, globals, records, variants);
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
        Expr::Err { ok_type, err_type, value } => {
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
            gen_expr(value, out, indent, env, signatures, globals, records, variants);
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
            let list_ty = gen_expr(list, out, indent, env, signatures, globals, records, variants);
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

            // Copy old data if len > 0 (simplified: use memory.copy if available, else skip for now)
            // For simplicity, we'll just update the list - old data is orphaned (no GC)

            // Store new value at new_data + len * elem_size
            out.push_str(&format!("{}local.get {}\n", pad, new_data_local));
            out.push_str(&format!("{}local.get {}\n", pad, len_local));
            out.push_str(&format!("{}i32.const {}\n", pad, elem_size));
            out.push_str(&format!("{}i32.mul\n", pad));
            out.push_str(&format!("{}i32.add\n", pad));
            gen_expr(value, out, indent, env, signatures, globals, records, variants);
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
            let list_ty = gen_expr(list, out, indent, env, signatures, globals, records, variants);
            let elem_type = match &list_ty {
                Type::List(inner) => inner.as_ref().clone(),
                _ => panic!("list-get expects a list"),
            };
            let elem_size = type_size(&elem_type);
            let list_local = env.declare_local(Type::S32);
            out.push_str(&format!("{}local.set {}\n", pad, list_local));

            // Evaluate index
            gen_expr(index, out, indent, env, signatures, globals, records, variants);
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
            gen_expr(list, out, indent, env, signatures, globals, records, variants);
            // Load len field at offset 0
            out.push_str(&format!("{}i32.load\n", pad));
            Type::S32
        }
        // String: len - return length
        Expr::StringLen { string } => {
            gen_expr(string, out, indent, env, signatures, globals, records, variants);
            // Load len field at offset 0 (string layout: 4 bytes len + data)
            out.push_str(&format!("{}i32.load\n", pad));
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
    check_expr(expr, &vars, signatures, globals, records, variants).expect("type checking already performed")
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
        // All compound types are pointer-sized
        Type::Record(_) | Type::Variant(_) | Type::Option(_) | Type::Result(_, _) | Type::List(_) | Type::Str => "i32",
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
    }
}

fn generate_wit(prog: &Program) -> String {
    let mut out = String::new();
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
                out.push_str(&format!("    {}({}),\n", case.name, wit_type(&case.payload[0])));
            } else {
                // Case with multiple payloads: name(tuple<type1, type2, ...>)
                let types: Vec<String> = case.payload.iter().map(wit_type).collect();
                out.push_str(&format!("    {}(tuple<{}>),\n", case.name, types.join(", ")));
            }
        }
        out.push_str("  }\n\n");
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
    out
}

fn find_function<'a>(prog: &'a Program, name: &str) -> &'a Function {
    prog.functions
        .iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("Function '{}' not found during codegen", name))
}
