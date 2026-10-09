// ReplSession is the one session abstraction every REPL surface shares: accumulate
// definitions, bind values, evaluate expressions — all through the shared front/
// middle + eval back-end. The local REPL and the live Theater REPL differ only in
// the Host they feed and the imports they pre-declare.

use wisp::compiler::{Host, Outcome, ReplSession, Type, Value};

/// A host for pure sessions: any import call is an error.
struct NoHost;
impl Host for NoHost {
    fn call(&mut self, module: &str, name: &str, _args: &[Value]) -> anyhow::Result<Value> {
        anyhow::bail!("no host: {module}/{name}")
    }
}

#[test]
fn session_accumulates_functions_and_bindings() {
    let mut s = ReplSession::new();

    // A function definition enters scope.
    assert!(
        matches!(s.feed("(fn dbl ((x s32)) s32 (i32.mul x 2))", &mut NoHost).unwrap(),
        Outcome::Defined(n) if n == "dbl")
    );
    // A later function can call the earlier one (preamble accumulation).
    assert!(matches!(
        s.feed("(fn quad ((x s32)) s32 (dbl (dbl x)))", &mut NoHost)
            .unwrap(),
        Outcome::Defined(_)
    ));
    // A (define …) binds the result of an expression that uses those functions.
    match s.feed("(define n (quad 5))", &mut NoHost).unwrap() {
        Outcome::Bound { name, value, .. } => {
            assert_eq!(name, "n");
            assert_eq!(value, Value::Int(20));
        }
        other => panic!("expected a binding, got {other:?}"),
    }
    // A bare expression sees both the binding and the functions.
    match s.feed("(i32.add n (dbl 1))", &mut NoHost).unwrap() {
        Outcome::Evaluated { value, ty } => {
            assert_eq!(value, Value::Int(22));
            assert_eq!(ty, Type::S32);
        }
        other => panic!("expected an evaluation, got {other:?}"),
    }
}

#[test]
fn session_reaches_a_host_through_preamble_imports() {
    // The Theater-REPL shape in miniature: a preamble declares a host import, and a
    // host serves its calls — the only difference from a pure session.
    struct Fixed;
    impl Host for Fixed {
        fn call(&mut self, _module: &str, name: &str, _args: &[Value]) -> anyhow::Result<Value> {
            assert_eq!(name, "now");
            Ok(Value::Int(123))
        }
    }
    let mut s = ReplSession::with_preamble("(import host now () s32)\n");
    match s.feed("(i32.add (now) 1)", &mut Fixed).unwrap() {
        Outcome::Evaluated { value, .. } => assert_eq!(value, Value::Int(124)),
        other => panic!("expected an evaluation, got {other:?}"),
    }
}

#[test]
fn session_rolls_back_a_rejected_definition() {
    let mut s = ReplSession::new();
    // A type-wrong definition (body f64, declared s32) is rejected...
    assert!(
        s.feed("(fn bad ((x s32)) s32 (f64.const 1.0))", &mut NoHost)
            .is_err()
    );
    // ...and leaves scope unchanged, so a good definition + expression still work.
    s.feed("(fn ident ((x s32)) s32 x)", &mut NoHost).unwrap();
    match s.feed("(ident 7)", &mut NoHost).unwrap() {
        Outcome::Evaluated { value, .. } => assert_eq!(value, Value::Int(7)),
        other => panic!("expected an evaluation, got {other:?}"),
    }
}
