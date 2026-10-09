//! The tree-walking back-end: evaluate a typed `Expr` directly, as the dual of
//! codegen. Both consume the same `Program` from `analyze`, so interpreted code
//! goes through the exact front/middle (and type system) the compiler uses.
//!
//! This covers the pure functional core — scalars, ADTs, `match`, functions,
//! `let`/`if`, lists, strings, and the wasm arithmetic/comparison/conversion
//! instructions. Raw-memory ops (`heap-alloc`, loads/stores, `*-addr`), the `any`
//! dynamic bridge, and host calls (`raw-invoke`) are codegen-only and are reported
//! as unsupported here; they belong to the REPL-migration step, not pure eval.

use super::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;

/// A runtime value produced by `eval`. Integer-family scalars (incl. bool/u*) are
/// `Int`; `f32`/`f64` are `Float`.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Str(String),
    Unit,
    /// A variant case: the case name and its payload values.
    Variant {
        case: String,
        payload: Vec<Value>,
    },
    /// A record: its type name and field values (in declaration order).
    Record {
        name: String,
        fields: Vec<Value>,
    },
    Tuple(Vec<Value>),
    List(Vec<Value>),
    Opt(Option<Box<Value>>),
    Res(std::result::Result<Box<Value>, Box<Value>>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{n}"),
            Value::Float(x) => write!(f, "{x}"),
            Value::Str(s) => write!(f, "{s:?}"),
            Value::Unit => write!(f, "()"),
            Value::Variant { case, payload } if payload.is_empty() => write!(f, "({case})"),
            Value::Variant { case, payload } => {
                write!(f, "({case}")?;
                for v in payload {
                    write!(f, " {v}")?;
                }
                write!(f, ")")
            }
            Value::Record { name, fields } => {
                write!(f, "({name}")?;
                for v in fields {
                    write!(f, " {v}")?;
                }
                write!(f, ")")
            }
            Value::Tuple(vs) | Value::List(vs) => {
                write!(f, "(")?;
                for (i, v) in vs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{v}")?;
                }
                write!(f, ")")
            }
            Value::Opt(None) => write!(f, "(none)"),
            Value::Opt(Some(v)) => write!(f, "(some {v})"),
            Value::Res(Ok(v)) => write!(f, "(ok {v})"),
            Value::Res(Err(v)) => write!(f, "(err {v})"),
        }
    }
}

/// A sink for host-interface calls encountered during eval. A call to an imported
/// function — `(write-line msg)` for an `(import … write-line …)` — dispatches here
/// with the interface, name, and argument values. The REPL daemon implements this
/// to reach the live Theater runtime; tests implement it to capture effects. This
/// is the eval dual of codegen's host-import ABI, at the `Value` level (no CGRF).
pub trait Host {
    fn call(&mut self, module: &str, name: &str, args: &[Value]) -> Result<Value>;
}

/// The default host: there is none, so any import call errors.
pub struct NullHost;
impl Host for NullHost {
    fn call(&mut self, module: &str, name: &str, _args: &[Value]) -> Result<Value> {
        bail!("eval: no host available for import {}/{}", module, name)
    }
}

struct EvalCtx<'a> {
    funcs: HashMap<&'a str, &'a Function>,
    records: HashMap<&'a str, &'a RecordDef>,
    imports: HashMap<&'a str, &'a Import>,
    globals: RefCell<HashMap<String, Value>>,
    depth: RefCell<usize>,
    host: RefCell<&'a mut dyn Host>,
}

const MAX_DEPTH: usize = 50_000;

/// Evaluate a nullary entry function of an already-analyzed program. The dual of
/// codegen: the same typed `Program` either back-end consumes. Import calls
/// dispatch to `host`.
pub(crate) fn eval_program(prog: &Program, entry: &str, host: &mut dyn Host) -> Result<Value> {
    let entry = prog
        .functions
        .iter()
        .find(|f| f.name == entry)
        .ok_or_else(|| anyhow!("eval: no entry function '{}'", entry))?;
    let ectx = EvalCtx {
        funcs: prog
            .functions
            .iter()
            .map(|f| (f.name.as_str(), f))
            .collect(),
        records: prog.records.iter().map(|r| (r.name.as_str(), r)).collect(),
        imports: prog.imports.iter().map(|i| (i.name.as_str(), i)).collect(),
        globals: RefCell::new(
            prog.globals
                .iter()
                .map(|g| (g.name.clone(), Value::Int(g.init_value)))
                .collect(),
        ),
        depth: RefCell::new(0),
        host: RefCell::new(host),
    };
    eval_expr(&ectx, &entry.body, &HashMap::new())
}

/// Run the shared pipeline on `src` and evaluate its nullary `test-func`, with no
/// host (import calls error).
pub fn eval_source(src: &str) -> Result<Value> {
    eval_source_with_host(src, &mut NullHost)
}

/// Like `eval_source`, but import calls dispatch to `host` — the seam where eval'd
/// code reaches the live runtime (store/tcp/print/...).
pub fn eval_source_with_host(src: &str, host: &mut dyn Host) -> Result<Value> {
    eval_source_entry_with_host(src, Path::new("."), "test-func", host)
}

/// Analyze `src` (resolving `(include …)` relative to `base_dir`) and evaluate its
/// nullary entry function `entry`, dispatching import calls to `host`. This is the
/// general entry point `wisp eval <file>` runs on — pick the file's directory as
/// `base_dir` so its includes resolve, and the function to run as `entry`.
pub fn eval_source_entry_with_host(
    src: &str,
    base_dir: &Path,
    entry: &str,
    host: &mut dyn Host,
) -> Result<Value> {
    let ctx = CompileContext::new(src.to_string(), "<eval>".to_string());
    let mut visited = HashSet::new();
    let (prog, _sigs) = analyze(src, base_dir, &mut visited, &ctx)?;
    eval_program(&prog, entry, host)
}

/// Evaluate a single REPL expression against accumulated value bindings and
/// function definitions — the eval dual of `compile_repl_expr`. Bindings are
/// inlined as literals; functions are in scope; the expression is type-checked and
/// then evaluated. Returns the resulting `Value` and its inferred `Type` (the REPL
/// needs the type to render/store the result). This is the typed evaluation
/// primitive the REPL runs on (replacing the Wisp interpreter's dynamic eval).
pub fn eval_repl_expr(
    expr_source: &str,
    bindings: &HashMap<String, InlineValue>,
    functions: &[Function],
) -> Result<(Value, Type)> {
    eval_repl_expr_with_host(expr_source, bindings, functions, &[], &mut NullHost)
}

/// Like `eval_repl_expr`, but the expression may call `imports`, and those calls
/// dispatch to `host`. This is the seam that lets a REPL *session* reach a live
/// runtime (the Theater REPL) while staying the same interpreter the local REPL
/// runs on — the imports and host are the only difference. Import signatures join
/// the function signatures for parse + type-check, and the imports go into the
/// evaluated program so eval routes their calls to `host`.
pub fn eval_repl_expr_with_host(
    expr_source: &str,
    bindings: &HashMap<String, InlineValue>,
    functions: &[Function],
    imports: &[Import],
    host: &mut dyn Host,
) -> Result<(Value, Type)> {
    let ctx = CompileContext::new(expr_source.to_string(), "<repl>".to_string());
    let tokens = tokenize(expr_source);
    if tokens.is_empty() {
        bail!("empty expression");
    }
    let (sexpr, _) = parse_sexpr(&tokens, 0);
    let inlined = inline_bindings(&sexpr, bindings);

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
    for import in imports {
        signatures.insert(
            import.name.clone(),
            Signature {
                params: import.params.iter().map(|p| p.ty.clone()).collect(),
                result: import.return_type.clone(),
            },
        );
    }
    let expr = parse_expr(
        &inlined,
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
        return_type: return_type.clone(),
        body: expr,
    };
    let mut all_functions = functions.to_vec();
    all_functions.push(eval_fn);
    let prog = Program {
        functions: all_functions,
        imports: imports.to_vec(),
        exports: vec![ExportDef::simple("eval".to_string())],
        globals: vec![],
        records: vec![],
        variants: vec![],
        resources: vec![],
        capabilities: HashSet::new(),
        world_config: None,
        data_segments: vec![],
    };
    let full_signatures = collect_signatures(&prog)?;
    type_check(&prog, &full_signatures, &ctx)?;
    Ok((eval_program(&prog, "eval", host)?, return_type))
}

type Env = HashMap<String, Value>;

fn eval_expr(ctx: &EvalCtx, expr: &Expr, env: &Env) -> Result<Value> {
    {
        let mut d = ctx.depth.borrow_mut();
        *d += 1;
        if *d > MAX_DEPTH {
            bail!("evaluation recursion limit exceeded");
        }
    }
    let result = eval_inner(ctx, expr, env);
    *ctx.depth.borrow_mut() -= 1;
    result
}

fn eval_inner(ctx: &EvalCtx, expr: &Expr, env: &Env) -> Result<Value> {
    match expr {
        Expr::Int { value, .. } => Ok(Value::Int(*value)),
        Expr::Float { value, .. } => Ok(Value::Float(*value)),
        Expr::StringLiteral(s) => Ok(Value::Str(s.clone())),
        Expr::Var(name) => env
            .get(name)
            .cloned()
            .ok_or_else(|| anyhow!("eval: unbound variable '{}'", name)),
        Expr::Ascribe { expr, ty } => {
            let v = eval_expr(ctx, expr, env)?;
            Ok(coerce(v, ty))
        }
        Expr::Let {
            name, value, body, ..
        } => {
            let v = eval_expr(ctx, value, env)?;
            let mut inner = env.clone();
            inner.insert(name.clone(), v);
            eval_expr(ctx, body, &inner)
        }
        Expr::Begin { exprs } => {
            let mut last = Value::Unit;
            for e in exprs {
                last = eval_expr(ctx, e, env)?;
            }
            Ok(last)
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            if as_int(&eval_expr(ctx, cond, env)?)? != 0 {
                eval_expr(ctx, then_branch, env)
            } else {
                eval_expr(ctx, else_branch, env)
            }
        }
        Expr::Call { name, args } => {
            let argv = eval_args(ctx, args, env)?;
            if let Some(func) = ctx.funcs.get(name.as_str()) {
                let mut call_env = Env::new();
                for (param, v) in func.params.iter().zip(argv) {
                    call_env.insert(param.name.clone(), v);
                }
                eval_expr(ctx, &func.body, &call_env)
            } else if let Some(import) = ctx.imports.get(name.as_str()) {
                // A call to an imported function is a host call — dispatch to the host.
                ctx.host
                    .borrow_mut()
                    .call(&import.module, &import.name, &argv)
            } else {
                bail!("eval: call to unknown function '{}'", name)
            }
        }
        Expr::WasmInstr { name, args } => {
            let argv = eval_args(ctx, args, env)?;
            eval_wasm_instr(name, &argv)
        }
        Expr::GlobalGet { name } => ctx
            .globals
            .borrow()
            .get(name)
            .cloned()
            .ok_or_else(|| anyhow!("eval: unknown global '{}'", name)),
        Expr::GlobalSet { name, value } => {
            let v = eval_expr(ctx, value, env)?;
            ctx.globals.borrow_mut().insert(name.clone(), v);
            Ok(Value::Unit)
        }
        Expr::RecordConstruct {
            record_name,
            fields,
        } => Ok(Value::Record {
            name: record_name.clone(),
            fields: eval_args(ctx, fields, env)?,
        }),
        Expr::RecordAccess {
            record_name,
            field_name,
            expr,
        } => {
            let v = eval_expr(ctx, expr, env)?;
            let Value::Record { fields, .. } = v else {
                bail!("eval: field access on non-record");
            };
            let def = ctx
                .records
                .get(record_name.as_str())
                .ok_or_else(|| anyhow!("eval: unknown record '{}'", record_name))?;
            let idx = def
                .fields
                .iter()
                .position(|fd| fd.name == *field_name)
                .ok_or_else(|| {
                    anyhow!(
                        "eval: record '{}' has no field '{}'",
                        record_name,
                        field_name
                    )
                })?;
            Ok(fields[idx].clone())
        }
        Expr::VariantConstruct {
            case_name, payload, ..
        } => Ok(Value::Variant {
            case: case_name.clone(),
            payload: eval_args(ctx, payload, env)?,
        }),
        Expr::Match { expr, cases } => {
            let scrut = eval_expr(ctx, expr, env)?;
            eval_match(ctx, &scrut, cases, env)
        }
        Expr::Some { value, .. } => Ok(Value::Opt(Some(Box::new(eval_expr(ctx, value, env)?)))),
        Expr::None { .. } => Ok(Value::Opt(None)),
        Expr::Ok { value, .. } => Ok(Value::Res(Ok(Box::new(eval_expr(ctx, value, env)?)))),
        Expr::Err { value, .. } => Ok(Value::Res(Err(Box::new(eval_expr(ctx, value, env)?)))),
        Expr::ListNew { .. } => Ok(Value::List(Vec::new())),
        Expr::ListPush { list, value } => {
            let mut v = as_list(eval_expr(ctx, list, env)?)?;
            v.push(eval_expr(ctx, value, env)?);
            Ok(Value::List(v))
        }
        Expr::ListGet { list, index } => {
            let v = as_list(eval_expr(ctx, list, env)?)?;
            let i = as_int(&eval_expr(ctx, index, env)?)? as usize;
            v.get(i)
                .cloned()
                .ok_or_else(|| anyhow!("eval: list index {} out of bounds", i))
        }
        Expr::ListLen { list } => Ok(Value::Int(as_list(eval_expr(ctx, list, env)?)?.len() as i64)),
        Expr::StringLen { string } => Ok(Value::Int(
            as_str(eval_expr(ctx, string, env)?)?.len() as i64
        )),
        Expr::StringRef { string, index } => {
            let s = as_str(eval_expr(ctx, string, env)?)?;
            let i = as_int(&eval_expr(ctx, index, env)?)? as usize;
            s.as_bytes()
                .get(i)
                .map(|b| Value::Int(*b as i64))
                .ok_or_else(|| anyhow!("eval: string index {} out of bounds", i))
        }
        Expr::Substring { string, start, end } => {
            let s = as_str(eval_expr(ctx, string, env)?)?;
            let a = as_int(&eval_expr(ctx, start, env)?)? as usize;
            let b = as_int(&eval_expr(ctx, end, env)?)? as usize;
            let bytes = s.as_bytes();
            if a > b || b > bytes.len() {
                bail!("eval: substring bounds out of range");
            }
            Ok(Value::Str(
                String::from_utf8_lossy(&bytes[a..b]).into_owned(),
            ))
        }
        Expr::StringAppend { left, right } => {
            let mut s = as_str(eval_expr(ctx, left, env)?)?;
            s.push_str(&as_str(eval_expr(ctx, right, env)?)?);
            Ok(Value::Str(s))
        }
        Expr::StringEq { left, right } => {
            let a = as_str(eval_expr(ctx, left, env)?)?;
            let b = as_str(eval_expr(ctx, right, env)?)?;
            Ok(Value::Int((a == b) as i64))
        }
        Expr::TupleConstruct { values } => Ok(Value::Tuple(eval_args(ctx, values, env)?)),
        // Capabilities are static witnesses at runtime: an i32 that carries no data.
        Expr::WithCap { name, body, .. } => {
            let mut inner = env.clone();
            inner.insert(name.clone(), Value::Int(0));
            eval_expr(ctx, body, &inner)
        }
        Expr::ReleaseCap { value } => {
            eval_expr(ctx, value, env)?;
            Ok(Value::Int(0))
        }
        Expr::BorrowCap { name } => env
            .get(name)
            .cloned()
            .ok_or_else(|| anyhow!("eval: borrow of unbound capability '{}'", name)),
        // Raw-memory / `any` / host operations are codegen-only.
        Expr::HeapAlloc { .. }
        | Expr::AnyFromS32 { .. }
        | Expr::AnyToS32 { .. }
        | Expr::AnyFromString { .. }
        | Expr::AnyToString { .. }
        | Expr::AnyAddr { .. }
        | Expr::AnyFromAddr { .. }
        | Expr::StringAddr { .. }
        | Expr::StringFromAddr { .. }
        | Expr::StringFromBytes { .. }
        | Expr::StringToBytes { .. }
        | Expr::RawInvoke { .. } => {
            bail!(
                "eval: raw-memory / any / host operations are not supported by the tree-walking back-end"
            )
        }
    }
}

fn eval_args(ctx: &EvalCtx, args: &[Expr], env: &Env) -> Result<Vec<Value>> {
    args.iter().map(|a| eval_expr(ctx, a, env)).collect()
}

fn eval_match(ctx: &EvalCtx, scrut: &Value, cases: &[MatchArm], env: &Env) -> Result<Value> {
    // The scrutinee's case name, and the payload values to bind.
    let (case, payload): (String, Vec<Value>) = match scrut {
        Value::Variant { case, payload } => (case.clone(), payload.clone()),
        Value::Opt(Some(v)) => ("some".to_string(), vec![(**v).clone()]),
        Value::Opt(None) => ("none".to_string(), vec![]),
        Value::Res(Ok(v)) => ("ok".to_string(), vec![(**v).clone()]),
        Value::Res(Err(v)) => ("err".to_string(), vec![(**v).clone()]),
        other => bail!("eval: match on non-variant value {:?}", other),
    };
    // First an exact case arm, else a `_` wildcard arm.
    let arm = cases
        .iter()
        .find(|a| a.case_name == case)
        .or_else(|| cases.iter().find(|a| a.case_name == "_"))
        .ok_or_else(|| anyhow!("eval: no match arm for case '{}'", case))?;
    let mut inner = env.clone();
    for (binding, v) in arm.bindings.iter().zip(payload) {
        inner.insert(binding.clone(), v);
    }
    eval_expr(ctx, &arm.body, &inner)
}

fn as_int(v: &Value) -> Result<i64> {
    match v {
        Value::Int(n) => Ok(*n),
        other => bail!("eval: expected an integer, got {:?}", other),
    }
}

fn as_float(v: &Value) -> Result<f64> {
    match v {
        Value::Float(x) => Ok(*x),
        other => bail!("eval: expected a float, got {:?}", other),
    }
}

fn as_list(v: Value) -> Result<Vec<Value>> {
    match v {
        Value::List(xs) => Ok(xs),
        other => bail!("eval: expected a list, got {:?}", other),
    }
}

fn as_str(v: Value) -> Result<String> {
    match v {
        Value::Str(s) => Ok(s),
        other => bail!("eval: expected a string, got {:?}", other),
    }
}

/// Numeric cast/ascription on a value (mirrors `conversion_instr`'s intent).
fn coerce(v: Value, to: &Type) -> Value {
    match (&v, to) {
        (Value::Int(n), Type::F32 | Type::F64) => Value::Float(*n as f64),
        (Value::Float(x), Type::S32 | Type::S64) => Value::Int(*x as i64),
        (Value::Int(n), Type::U8) => Value::Int(*n & 0xFF),
        (Value::Int(n), Type::U16) => Value::Int(*n & 0xFFFF),
        (Value::Int(n), Type::Bool) => Value::Int((*n != 0) as i64),
        (Value::Int(n), Type::S32 | Type::U32) => Value::Int(*n as i32 as i64),
        (Value::Float(x), Type::F32) => Value::Float(*x as f32 as f64),
        _ => v,
    }
}

/// Evaluate a wasm instruction over already-evaluated operands.
fn eval_wasm_instr(name: &str, args: &[Value]) -> Result<Value> {
    // `*.const` carries its value as the single operand.
    if let Some(rest) = name.strip_suffix(".const") {
        return match rest {
            "i32" | "i64" => Ok(Value::Int(as_int(&args[0])?)),
            "f32" | "f64" => Ok(args[0].clone()),
            _ => bail!("eval: unknown const '{}'", name),
        };
    }
    // Unary.
    match name {
        "i32.eqz" | "i64.eqz" => return Ok(Value::Int((as_int(&args[0])? == 0) as i64)),
        "i64.extend_i32_s" => return Ok(Value::Int(as_int(&args[0])? as i32 as i64)),
        "i64.extend_i32_u" => return Ok(Value::Int((as_int(&args[0])? as i32 as u32) as i64)),
        "i32.wrap_i64" => return Ok(Value::Int(as_int(&args[0])? as i32 as i64)),
        "f64.promote_f32" | "f32.demote_f64" => return Ok(Value::Float(as_float(&args[0])?)),
        "f32.convert_i32_s" | "f64.convert_i32_s" | "f32.convert_i64_s" | "f64.convert_i64_s" => {
            return Ok(Value::Float(as_int(&args[0])? as f64));
        }
        "i32.trunc_f32_s" | "i32.trunc_f64_s" | "i64.trunc_f32_s" | "i64.trunc_f64_s" => {
            return Ok(Value::Int(as_float(&args[0])? as i64));
        }
        _ => {}
    }
    // Binary: dispatch float vs integer, 32- vs 64-bit, by the instruction name.
    let (is_i64, is_float) = (name.starts_with("i64."), name.starts_with('f'));
    if is_float {
        let (a, b) = (as_float(&args[0])?, as_float(&args[1])?);
        let op = name.split_once('.').map(|(_, o)| o).unwrap_or("");
        return Ok(match op {
            "add" => Value::Float(a + b),
            "sub" => Value::Float(a - b),
            "mul" => Value::Float(a * b),
            "div" => Value::Float(a / b),
            "eq" => Value::Int((a == b) as i64),
            "ne" => Value::Int((a != b) as i64),
            "lt" => Value::Int((a < b) as i64),
            "le" => Value::Int((a <= b) as i64),
            "gt" => Value::Int((a > b) as i64),
            "ge" => Value::Int((a >= b) as i64),
            _ => bail!("eval: unsupported float instruction '{}'", name),
        });
    }
    let (a, b) = (as_int(&args[0])?, as_int(&args[1])?);
    let op = name.split_once('.').map(|(_, o)| o).unwrap_or("");
    // Normalize operands to the instruction width, compute, re-widen for storage.
    let wrap = |x: i64| if is_i64 { x } else { x as i32 as i64 };
    let r = match op {
        "add" => wrap(a.wrapping_add(b)),
        "sub" => wrap(a.wrapping_sub(b)),
        "mul" => wrap(a.wrapping_mul(b)),
        "div_s" => {
            if b == 0 {
                bail!("eval: division by zero");
            }
            wrap(a.wrapping_div(b))
        }
        "rem_s" => {
            if b == 0 {
                bail!("eval: remainder by zero");
            }
            wrap(a.wrapping_rem(b))
        }
        "div_u" => {
            if b == 0 {
                bail!("eval: division by zero");
            }
            wrap(((a as u64).wrapping_div(b as u64)) as i64)
        }
        "and" => wrap(a & b),
        "or" => wrap(a | b),
        "xor" => wrap(a ^ b),
        "shl" => wrap(a.wrapping_shl(b as u32)),
        "shr_s" => wrap(a.wrapping_shr(b as u32)),
        "shr_u" => {
            if is_i64 {
                ((a as u64).wrapping_shr(b as u32)) as i64
            } else {
                ((a as u32).wrapping_shr(b as u32)) as i64
            }
        }
        "eq" => (a == b) as i64,
        "ne" => (a != b) as i64,
        "lt_s" => (a < b) as i64,
        "le_s" => (a <= b) as i64,
        "gt_s" => (a > b) as i64,
        "ge_s" => (a >= b) as i64,
        "lt_u" => ((a as u64) < (b as u64)) as i64,
        "le_u" => ((a as u64) <= (b as u64)) as i64,
        "gt_u" => ((a as u64) > (b as u64)) as i64,
        "ge_u" => ((a as u64) >= (b as u64)) as i64,
        _ => bail!("eval: unsupported integer instruction '{}'", name),
    };
    Ok(Value::Int(r))
}
