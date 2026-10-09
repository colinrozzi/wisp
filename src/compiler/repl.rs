//! A REPL session on the shared interpreter: accumulate definitions, evaluate
//! expressions. One session abstraction for every REPL surface — the local REPL and
//! the live Theater REPL differ only in the `Host` they feed and the imports they
//! pre-declare, not in how a session works.
//!
//! Definitions accumulate as source and the scope is rebuilt by `analyze` on each
//! expression, so a later `fn` can call an earlier one exactly as a file would, and
//! the full front/middle (macros, type-check) governs them. Expressions evaluate
//! against that analyzed context through `eval_repl_sexpr`; `(define …)` bindings are
//! inlined.

use super::eval::eval_repl_sexpr;
use super::*;

/// What feeding one form to a session did.
#[derive(Debug)]
pub enum Outcome {
    /// A `(fn …)` definition was accepted; carries its name.
    Defined(String),
    /// A `(define name expr)` bound a value.
    Bound {
        name: String,
        value: Value,
        ty: Type,
    },
    /// A bare expression evaluated to a value.
    Evaluated { value: Value, ty: Type },
}

/// An interactive session: accumulated definitions + value bindings, evaluated by
/// the shared back-end. Host-agnostic — callers pass the `Host` per `feed`, so the
/// same session type serves the local REPL (a trivial host) and the Theater REPL (a
/// runtime-backed host).
pub struct ReplSession {
    /// Source of every accepted definition (imports + fns), re-analyzed to build the
    /// scope each expression evaluates in.
    preamble: String,
    /// `(define …)` value bindings, inlined into expressions.
    bindings: HashMap<String, InlineValue>,
}

impl Default for ReplSession {
    fn default() -> Self {
        Self::new()
    }
}

impl ReplSession {
    pub fn new() -> Self {
        Self {
            preamble: String::new(),
            bindings: HashMap::new(),
        }
    }

    /// Start with a fixed preamble of definitions — e.g. the host-interface
    /// `(import …)` declarations whose calls the session's `Host` will serve.
    pub fn with_preamble(preamble: impl Into<String>) -> Self {
        Self {
            preamble: preamble.into(),
            bindings: HashMap::new(),
        }
    }

    /// Feed one source form. A `(fn …)` is accumulated into scope; a
    /// `(define name expr)` evaluates `expr` and binds the result; anything else is
    /// evaluated as an expression. Import calls dispatch to `host`.
    pub fn feed(&mut self, input: &str, host: &mut dyn Host) -> Result<Outcome> {
        let tokens = tokenize(input);
        if tokens.is_empty() {
            bail!("empty input");
        }
        let (form, _) = parse_sexpr(&tokens, 0);
        if let SExpr::List(items, _) = &form
            && let Some(SExpr::Sym(head, _)) = items.first()
        {
            match head.as_str() {
                "fn" => return self.define_fn(&form, input),
                "define" => return self.define_value(items, input, host),
                _ => {}
            }
        }
        let (value, ty) = self.eval(&form, input, host)?;
        Ok(Outcome::Evaluated { value, ty })
    }

    /// Analyze the accumulated preamble to recover the functions + imports in scope.
    fn context(&self) -> Result<(Vec<Function>, Vec<Import>)> {
        if self.preamble.trim().is_empty() {
            return Ok((Vec::new(), Vec::new()));
        }
        let ctx = CompileContext::new(self.preamble.clone(), "<repl>".to_string());
        let mut visited = HashSet::new();
        let (prog, _sigs) = analyze(&self.preamble, Path::new("."), &mut visited, &ctx)?;
        Ok((prog.functions, prog.imports))
    }

    fn eval(&self, form: &SExpr, source: &str, host: &mut dyn Host) -> Result<(Value, Type)> {
        let (functions, imports) = self.context()?;
        eval_repl_sexpr(form, source, &self.bindings, &functions, &imports, host)
    }

    fn define_fn(&mut self, form: &SExpr, input: &str) -> Result<Outcome> {
        let name = fn_name(form)?;
        // Tentatively add the definition and confirm the whole preamble still
        // analyzes; roll back if not, so a rejected definition leaves scope unchanged.
        let saved = self.preamble.clone();
        if !self.preamble.is_empty() && !self.preamble.ends_with('\n') {
            self.preamble.push('\n');
        }
        self.preamble.push_str(input.trim());
        self.preamble.push('\n');
        if let Err(e) = self.context() {
            self.preamble = saved;
            return Err(e);
        }
        Ok(Outcome::Defined(name))
    }

    fn define_value(
        &mut self,
        items: &[SExpr],
        input: &str,
        host: &mut dyn Host,
    ) -> Result<Outcome> {
        let (name, expr) = match items {
            [_, SExpr::Sym(name, _), expr] => (name.clone(), expr),
            _ => bail!("(define name expr): expected a name and one expression"),
        };
        let (value, ty) = self.eval(expr, input, host)?;
        self.bindings
            .insert(name.clone(), value_to_inline(&value, &ty)?);
        Ok(Outcome::Bound { name, value, ty })
    }
}

/// The name in a `(fn name …)` form.
fn fn_name(form: &SExpr) -> Result<String> {
    if let SExpr::List(items, _) = form
        && let Some(SExpr::Sym(name, _)) = items.get(1)
    {
        return Ok(name.clone());
    }
    bail!("malformed function definition")
}

/// Convert an evaluated `Value` + its static `Type` into an `InlineValue` for
/// binding. Covers scalars, strings, and the built-in compound types; user record
/// and variant bindings are a later brick (the eval `Value` doesn't carry the type
/// name a record/variant `InlineValue` needs).
fn value_to_inline(value: &Value, ty: &Type) -> Result<InlineValue> {
    Ok(match (value, ty) {
        (Value::Int(n), Type::S64 | Type::U64) => InlineValue::S64(*n),
        (Value::Int(n), _) => InlineValue::S32(*n as i32),
        (Value::Float(x), Type::F32) => InlineValue::F32(*x as f32),
        (Value::Float(x), _) => InlineValue::F64(*x),
        (Value::Str(s), _) => InlineValue::Str(s.clone()),
        (Value::List(items), Type::List(elem)) => InlineValue::List {
            elem_type: (**elem).clone(),
            items: items
                .iter()
                .map(|i| value_to_inline(i, elem))
                .collect::<Result<_>>()?,
        },
        (Value::Opt(inner), Type::Option(elem)) => InlineValue::Option {
            inner_type: (**elem).clone(),
            value: inner
                .as_ref()
                .map(|b| value_to_inline(b, elem).map(Box::new))
                .transpose()?,
        },
        (Value::Res(r), Type::Result(ok, err)) => InlineValue::Result {
            ok_type: (**ok).clone(),
            err_type: (**err).clone(),
            value: match r {
                Ok(v) => Ok(Box::new(value_to_inline(v, ok)?)),
                Err(v) => Err(Box::new(value_to_inline(v, err)?)),
            },
        },
        _ => bail!("cannot bind a value of type {:?} at the REPL yet", ty),
    })
}
