use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{Engine, Instance, Module, Store};
use wisp::compiler;

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn compile_and_run(source: &str) -> i32 {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_codegen_{}.lisp", test_id));
    let out_base = temp_dir.join(format!("test_codegen_{}", test_id));

    std::fs::write(&source_path, source).expect("failed to write temp source");
    compiler::compile(&source_path, &out_base).expect("failed to compile");

    let wasm_path = out_base.with_extension("wasm");
    let wasm_bytes = std::fs::read(&wasm_path).expect("failed to read wasm");

    let engine = Engine::default();
    let module = Module::new(&engine, &wasm_bytes).expect("failed to create module");
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[]).expect("failed to instantiate");

    let func = instance
        .get_func(&mut store, "test-func")
        .expect("function 'test-func' not found");

    let memory = instance
        .get_memory(&mut store, "memory")
        .expect("memory not found");

    let in_ptr: i32 = 0x1000;
    let in_len: i32 = 0;
    let out_ptr: i32 = 0x2000;
    let out_cap: i32 = 256;

    let mut results = [wasmtime::Val::I32(0)];
    func.call(
        &mut store,
        &[
            wasmtime::Val::I32(in_ptr),
            wasmtime::Val::I32(in_len),
            wasmtime::Val::I32(out_ptr),
            wasmtime::Val::I32(out_cap),
        ],
        &mut results,
    )
    .expect("call failed");

    let mut buf = [0u8; 4];
    memory
        .read(&store, (out_ptr + 24) as usize, &mut buf)
        .expect("failed to read result");
    i32::from_le_bytes(buf)
}

// Codegen preamble - includes sexpr type and codegen functions
const CODEGEN_PREAMBLE: &str = r#"
(variant sexpr
  (sym string)
  (num s32)
  (str string)
  (lst (list sexpr)))

(fn digit-to-string ((d s32)) string
  (if (i32.eq d (i32.const 0)) "0"
    (if (i32.eq d (i32.const 1)) "1"
      (if (i32.eq d (i32.const 2)) "2"
        (if (i32.eq d (i32.const 3)) "3"
          (if (i32.eq d (i32.const 4)) "4"
            (if (i32.eq d (i32.const 5)) "5"
              (if (i32.eq d (i32.const 6)) "6"
                (if (i32.eq d (i32.const 7)) "7"
                  (if (i32.eq d (i32.const 8)) "8"
                    "9"))))))))))

(fn i32-to-string-pos ((n s32) (acc string)) string
  (if (i32.eq n (i32.const 0))
    acc
    (let (digit (i32.rem_s n (i32.const 10)))
      (let (rest (i32.div_s n (i32.const 10)))
        (i32-to-string-pos rest (string-append (digit-to-string digit) acc))))))

(fn i32-to-string ((n s32)) string
  (if (i32.eq n (i32.const 0))
    "0"
    (if (i32.lt_s n (i32.const 0))
      (string-append "-" (i32-to-string-pos (i32.sub (i32.const 0) n) ""))
      (i32-to-string-pos n ""))))

(fn is-sym ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 1))
    ((num n) (i32.const 0))
    ((str s) (i32.const 0))
    ((lst l) (i32.const 0))))

(fn is-num ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) (i32.const 1))
    ((str s) (i32.const 0))
    ((lst l) (i32.const 0))))

(fn is-lst ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) (i32.const 0))
    ((str s) (i32.const 0))
    ((lst l) (i32.const 1))))

(fn get-sym ((e sexpr)) string
  (match e
    ((sym s) s)
    ((num n) "")
    ((str s) "")
    ((lst l) "")))

(fn get-num ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) n)
    ((str s) (i32.const 0))
    ((lst l) (i32.const 0))))

(fn get-lst ((e sexpr)) (list sexpr)
  (match e
    ((sym s) (list-new sexpr))
    ((num n) (list-new sexpr))
    ((str s) (list-new sexpr))
    ((lst l) l)))

(fn is-wasm-instr ((s string)) s32
  (if (i32.lt_s (string-len s) (i32.const 4))
    (i32.const 0)
    (let (prefix (substring s (i32.const 0) (i32.const 4)))
      (if (string=? prefix "i32.")
        (i32.const 1)
        (if (string=? prefix "i64.")
          (i32.const 1)
          (if (string=? prefix "f32.")
            (i32.const 1)
            (if (string=? prefix "f64.")
              (i32.const 1)
              (i32.const 0))))))))

(fn compile-number ((n s32)) string
  (string-append "(i32.const " (string-append (i32-to-string n) ")")))

(fn compile-var ((name string)) string
  (string-append "(local.get $" (string-append name ")")))

(fn compile-args ((args (list sexpr)) (idx s32) (len s32) (acc string)) string
  (if (i32.ge_s idx len)
    acc
    (let (arg (list-get args idx))
      (let (compiled (compile-expr arg))
        (let (new-acc (if (i32.eq idx (i32.const 0))
                        compiled
                        (string-append acc (string-append " " compiled))))
          (compile-args args (i32.add idx (i32.const 1)) len new-acc))))))

(fn compile-wasm-call ((instr string) (args (list sexpr))) string
  (let (compiled-args (compile-args args (i32.const 0) (list-len args) ""))
    (string-append "(" (string-append instr (string-append " " (string-append compiled-args ")"))))))

(fn compile-fn-call ((name string) (args (list sexpr))) string
  (let (compiled-args (compile-args args (i32.const 0) (list-len args) ""))
    (string-append "(call $" (string-append name (string-append " " (string-append compiled-args ")"))))))

(fn build-args-list ((items (list sexpr)) (start s32) (len s32) (acc (list sexpr))) (list sexpr)
  (if (i32.le_s len (i32.const 0))
    acc
    (let (item (list-get items start))
      (build-args-list items (i32.add start (i32.const 1)) (i32.sub len (i32.const 1)) (list-push acc item)))))

(fn compile-expr ((expr sexpr)) string
  (match expr
    ((num n) (compile-number n))
    ((sym s) (compile-var s))
    ((str s) "(error: strings not supported)")
    ((lst items) (compile-list items))))

(fn compile-list ((items (list sexpr))) string
  (if (i32.eq (list-len items) (i32.const 0))
    "()"
    (let (head (list-get items (i32.const 0)))
      (if (is-sym head)
        (let (name (get-sym head))
          (if (string=? name "if")
            (if (i32.lt_s (list-len items) (i32.const 4))
              "(error: if needs 3 arguments)"
              (let (cond-expr (list-get items (i32.const 1)))
                (let (then-expr (list-get items (i32.const 2)))
                  (let (else-expr (list-get items (i32.const 3)))
                    (let (cond-wat (compile-expr cond-expr))
                      (let (then-wat (compile-expr then-expr))
                        (let (else-wat (compile-expr else-expr))
                          (string-append "(if (result i32) "
                            (string-append cond-wat
                              (string-append " (then "
                                (string-append then-wat
                                  (string-append ") (else "
                                    (string-append else-wat "))")))))))))))))
            (if (string=? name "let")
              (if (i32.lt_s (list-len items) (i32.const 3))
                "(error: let needs binding and body)"
                (let (binding (list-get items (i32.const 1)))
                  (let (body (list-get items (i32.const 2)))
                    (if (is-lst binding)
                      (let (binding-items (get-lst binding))
                        (if (i32.lt_s (list-len binding-items) (i32.const 2))
                          "(error: let binding needs name and value)"
                          (let (name-expr (list-get binding-items (i32.const 0)))
                            (let (value-expr (list-get binding-items (i32.const 1)))
                              (if (is-sym name-expr)
                                (let (var-name (get-sym name-expr))
                                  (let (value-wat (compile-expr value-expr))
                                    (let (body-wat (compile-expr body))
                                      (string-append "(local.tee $"
                                        (string-append var-name
                                          (string-append " "
                                            (string-append value-wat
                                              (string-append ") "
                                                body-wat))))))))
                                "(error: let binding name must be symbol)")))))
                      "(error: let binding must be a list)"))))
              (let (rest-start (i32.const 1))
                (let (rest-len (i32.sub (list-len items) (i32.const 1)))
                  (let (args (build-args-list items rest-start rest-len (list-new sexpr)))
                    (if (is-wasm-instr name)
                      (compile-wasm-call name args)
                      (compile-fn-call name args))))))))
        "(error: list head not symbol)"))))

(fn type-to-wat ((t string)) string
  (if (string=? t "s32")
    "i32"
    (if (string=? t "s64")
      "i64"
      (if (string=? t "f32")
        "f32"
        (if (string=? t "f64")
          "f64"
          "i32")))))

(fn compile-param ((param sexpr)) string
  (if (is-lst param)
    (let (items (get-lst param))
      (if (i32.ge_s (list-len items) (i32.const 2))
        (let (name-expr (list-get items (i32.const 0)))
          (let (type-expr (list-get items (i32.const 1)))
            (if (is-sym name-expr)
              (if (is-sym type-expr)
                (let (name (get-sym name-expr))
                  (let (type-name (get-sym type-expr))
                    (string-append "(param $"
                      (string-append name
                        (string-append " "
                          (string-append (type-to-wat type-name) ")"))))))
                "(error: param type not symbol)")
              "(error: param name not symbol)")))
        "(error: param needs name and type)"))
    "(error: param must be list)"))

(fn compile-params ((params (list sexpr)) (idx s32) (len s32) (acc string)) string
  (if (i32.ge_s idx len)
    acc
    (let (param (list-get params idx))
      (let (compiled (compile-param param))
        (let (new-acc (if (i32.eq idx (i32.const 0))
                        compiled
                        (string-append acc (string-append " " compiled))))
          (compile-params params (i32.add idx (i32.const 1)) len new-acc))))))

(fn compile-fn ((items (list sexpr))) string
  (if (i32.lt_s (list-len items) (i32.const 5))
    "(error: fn needs name, params, return type, and body)"
    (let (name-expr (list-get items (i32.const 1)))
      (let (params-expr (list-get items (i32.const 2)))
        (let (ret-type-expr (list-get items (i32.const 3)))
          (let (body-expr (list-get items (i32.const 4)))
            (if (is-sym name-expr)
              (if (is-lst params-expr)
                (if (is-sym ret-type-expr)
                  (let (name (get-sym name-expr))
                    (let (params (get-lst params-expr))
                      (let (ret-type (get-sym ret-type-expr))
                        (let (params-wat (compile-params params (i32.const 0) (list-len params) ""))
                          (let (body-wat (compile-expr body-expr))
                            (let (result-wat (string-append "(result " (string-append (type-to-wat ret-type) ")")))
                              (string-append "(func $"
                                (string-append name
                                  (string-append " "
                                    (string-append params-wat
                                      (string-append " "
                                        (string-append result-wat
                                          (string-append " "
                                            (string-append body-wat ")"))))))))))))))
                  "(error: return type not symbol)")
                "(error: params must be list)")
              "(error: fn name not symbol)")))))))
"#;

// ============================================================
// i32-to-string tests
// ============================================================

#[test]
fn test_i32_to_string_zero() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (if (string=? (i32-to-string (i32.const 0)) "0")
    (i32.const 1)
    (i32.const 0))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_i32_to_string_positive() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (if (string=? (i32-to-string (i32.const 42)) "42")
    (i32.const 1)
    (i32.const 0))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_i32_to_string_large() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (if (string=? (i32-to-string (i32.const 12345)) "12345")
    (i32.const 1)
    (i32.const 0))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_i32_to_string_negative() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (if (string=? (i32-to-string (i32.const -123)) "-123")
    (i32.const 1)
    (i32.const 0))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

// ============================================================
// compile-number tests
// ============================================================

#[test]
fn test_compile_number_positive() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (if (string=? (compile-number (i32.const 42)) "(i32.const 42)")
    (i32.const 1)
    (i32.const 0))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_compile_number_zero() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (if (string=? (compile-number (i32.const 0)) "(i32.const 0)")
    (i32.const 1)
    (i32.const 0))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_compile_number_negative() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (if (string=? (compile-number (i32.const -5)) "(i32.const -5)")
    (i32.const 1)
    (i32.const 0))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

// ============================================================
// compile-var tests
// ============================================================

#[test]
fn test_compile_var() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (if (string=? (compile-var "x") "(local.get $x)")
    (i32.const 1)
    (i32.const 0))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_compile_var_longer_name() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (if (string=? (compile-var "my-variable") "(local.get $my-variable)")
    (i32.const 1)
    (i32.const 0))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

// ============================================================
// is-wasm-instr tests
// ============================================================

#[test]
fn test_is_wasm_instr_i32() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (is-wasm-instr "i32.add")))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_is_wasm_instr_i64() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (is-wasm-instr "i64.mul")))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_is_wasm_instr_f32() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (is-wasm-instr "f32.div")))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_is_wasm_instr_f64() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (is-wasm-instr "f64.const")))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_is_wasm_instr_no() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (if (is-wasm-instr "foo")
    (i32.const 0)
    (i32.const 1))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_is_wasm_instr_short() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (if (is-wasm-instr "i32")
    (i32.const 0)
    (i32.const 1))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

// ============================================================
// compile-expr tests
// ============================================================

#[test]
fn test_compile_expr_number() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (expr (num (i32.const 42)))
    (if (string=? (compile-expr expr) "(i32.const 42)")
      (i32.const 1)
      (i32.const 0)))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_compile_expr_var() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (expr (sym "x"))
    (if (string=? (compile-expr expr) "(local.get $x)")
      (i32.const 1)
      (i32.const 0)))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_compile_expr_wasm_const() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  ; Build (i32.const 42) as sexpr
  (let (items (list-push (list-push (list-new sexpr) (sym "i32.const")) (num (i32.const 42))))
    (let (expr (lst items))
      (if (string=? (compile-expr expr) "(i32.const (i32.const 42))")
        (i32.const 1)
        (i32.const 0))))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_compile_expr_wasm_add() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  ; Build (i32.add 1 2) as sexpr
  (let (items (list-push (list-push (list-push (list-new sexpr)
                  (sym "i32.add"))
                  (num (i32.const 1)))
                  (num (i32.const 2))))
    (let (expr (lst items))
      (if (string=? (compile-expr expr) "(i32.add (i32.const 1) (i32.const 2))")
        (i32.const 1)
        (i32.const 0))))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_compile_expr_fn_call() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  ; Build (foo 1 2) as sexpr
  (let (items (list-push (list-push (list-push (list-new sexpr)
                  (sym "foo"))
                  (num (i32.const 1)))
                  (num (i32.const 2))))
    (let (expr (lst items))
      (if (string=? (compile-expr expr) "(call $foo (i32.const 1) (i32.const 2))")
        (i32.const 1)
        (i32.const 0))))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_compile_expr_nested() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  ; Build (i32.add x (i32.const 1)) as sexpr
  (let (inner-items (list-push (list-push (list-new sexpr) (sym "i32.const")) (num (i32.const 1))))
    (let (inner (lst inner-items))
      (let (outer-items (list-push (list-push (list-push (list-new sexpr) (sym "i32.add")) (sym "x")) inner))
        (let (expr (lst outer-items))
          (if (string=? (compile-expr expr) "(i32.add (local.get $x) (i32.const (i32.const 1)))")
            (i32.const 1)
            (i32.const 0))))))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

// ============================================================
// if expression tests
// ============================================================

#[test]
fn test_compile_if() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  ; Build (if 1 2 3) as sexpr
  (let (items (list-push (list-push (list-push (list-push (list-new sexpr)
                  (sym "if"))
                  (num (i32.const 1)))
                  (num (i32.const 2)))
                  (num (i32.const 3))))
    (let (expr (lst items))
      (if (string=? (compile-expr expr) "(if (result i32) (i32.const 1) (then (i32.const 2)) (else (i32.const 3)))")
        (i32.const 1)
        (i32.const 0))))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_compile_if_with_vars() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  ; Build (if x y z) as sexpr
  (let (items (list-push (list-push (list-push (list-push (list-new sexpr)
                  (sym "if"))
                  (sym "x"))
                  (sym "y"))
                  (sym "z")))
    (let (expr (lst items))
      (if (string=? (compile-expr expr) "(if (result i32) (local.get $x) (then (local.get $y)) (else (local.get $z)))")
        (i32.const 1)
        (i32.const 0))))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

// ============================================================
// let expression tests
// ============================================================

#[test]
fn test_compile_let() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  ; Build (let (x 42) x) as sexpr
  ; binding = (x 42)
  (let (binding (lst (list-push (list-push (list-new sexpr) (sym "x")) (num (i32.const 42)))))
    (let (body (sym "x"))
      (let (items (list-push (list-push (list-push (list-new sexpr) (sym "let")) binding) body))
        (let (expr (lst items))
          (if (string=? (compile-expr expr) "(local.tee $x (i32.const 42)) (local.get $x)")
            (i32.const 1)
            (i32.const 0))))))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

// ============================================================
// function compilation tests
// ============================================================

#[test]
fn test_type_to_wat() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (if (string=? (type-to-wat "s32") "i32")
    (i32.const 1)
    (i32.const 0))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_type_to_wat_s64() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (if (string=? (type-to-wat "s64") "i64")
    (i32.const 1)
    (i32.const 0))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_compile_param() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  ; Build (x s32) as sexpr
  (let (param (lst (list-push (list-push (list-new sexpr) (sym "x")) (sym "s32"))))
    (if (string=? (compile-param param) "(param $x i32)")
      (i32.const 1)
      (i32.const 0)))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_compile_fn_simple() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  ; Build (fn add ((x s32)) s32 x) as sexpr
  ; params = ((x s32))
  (let (param (lst (list-push (list-push (list-new sexpr) (sym "x")) (sym "s32"))))
    (let (params (lst (list-push (list-new sexpr) param)))
      (let (items (list-push (list-push (list-push (list-push (list-push (list-new sexpr)
                      (sym "fn"))
                      (sym "identity"))
                      params)
                      (sym "s32"))
                      (sym "x")))
        (let (fn-expr (lst items))
          (if (string=? (compile-fn items) "(func $identity (param $x i32) (result i32) (local.get $x))")
            (i32.const 1)
            (i32.const 0))))))))
"#,
        CODEGEN_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}
