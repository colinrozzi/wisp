use std::sync::atomic::{AtomicUsize, Ordering};
use wasmtime::{Config, Engine, Instance, Module, Store};
use wisp::compiler;

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn compile_and_run(source: &str) -> i32 {
    let test_id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_parser_{}.lisp", test_id));
    let out_base = temp_dir.join(format!("test_parser_{}", test_id));

    std::fs::write(&source_path, source).expect("failed to write temp source");
    compiler::compile(&source_path, &out_base).expect("failed to compile");

    let wasm_path = out_base.with_extension("wasm");
    let wasm_bytes = std::fs::read(&wasm_path).expect("failed to read wasm");

    let mut config = Config::new();
    config.wasm_tail_call(true);
    let engine = Engine::new(&config).expect("failed to create engine");
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

// Full parser preamble - includes tokenizer and parser
const PARSER_PREAMBLE: &str = r#"
(variant token
  (lparen)
  (rparen)
  (number s32)
  (symbol string)
  (str-lit string))

(variant sexpr
  (sym string)
  (num s32)
  (str string)
  (lst (list sexpr)))

(fn is-whitespace ((c s32)) s32
  (if (i32.eq c (i32.const 32))
    (i32.const 1)
    (if (i32.eq c (i32.const 9))
      (i32.const 1)
      (if (i32.eq c (i32.const 10))
        (i32.const 1)
        (if (i32.eq c (i32.const 13))
          (i32.const 1)
          (i32.const 0))))))

(fn is-digit ((c s32)) s32
  (if (i32.ge_s c (i32.const 48))
    (if (i32.le_s c (i32.const 57))
      (i32.const 1)
      (i32.const 0))
    (i32.const 0)))

(fn is-delimiter ((c s32)) s32
  (if (is-whitespace c)
    (i32.const 1)
    (if (i32.eq c (i32.const 40))
      (i32.const 1)
      (if (i32.eq c (i32.const 41))
        (i32.const 1)
        (if (i32.eq c (i32.const 59))
          (i32.const 1)
          (if (i32.eq c (i32.const 34))
            (i32.const 1)
            (i32.const 0)))))))

(fn digit-value ((c s32)) s32
  (i32.sub c (i32.const 48)))

(record token-result
  (tok token)
  (new-pos s32))

(fn parse-number-value ((src string) (pos s32) (len s32) (acc s32)) token-result
  (if (i32.ge_s pos len)
    (token-result (number acc) pos)
    (let (c (string-ref src pos))
      (if (is-digit c)
        (parse-number-value src
          (i32.add pos (i32.const 1))
          len
          (i32.add (i32.mul acc (i32.const 10)) (digit-value c)))
        (token-result (number acc) pos)))))

(fn read-number ((src string) (pos s32) (len s32)) token-result
  (parse-number-value src pos len (i32.const 0)))

(fn find-symbol-end-at ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (is-delimiter c)
        pos
        (find-symbol-end-at src (i32.add pos (i32.const 1)) len)))))

(fn read-symbol ((src string) (pos s32) (len s32)) token-result
  (let (start pos)
    (let (end (find-symbol-end-at src pos len))
      (let (sym-str (substring src start end))
        (token-result (symbol sym-str) end)))))

(fn find-string-end-at ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (i32.eq c (i32.const 34))
        pos
        (if (i32.eq c (i32.const 92))
          (find-string-end-at src (i32.add pos (i32.const 2)) len)
          (find-string-end-at src (i32.add pos (i32.const 1)) len))))))

(fn read-string-lit ((src string) (pos s32) (len s32)) token-result
  (let (start (i32.add pos (i32.const 1)))
    (let (end (find-string-end-at src start len))
      (let (str-content (substring src start end))
        (token-result (str-lit str-content) (i32.add end (i32.const 1)))))))

(fn read-token ((src string) (pos s32) (len s32)) token-result
  (let (c (string-ref src pos))
    (if (i32.eq c (i32.const 40))
      (token-result (lparen) (i32.add pos (i32.const 1)))
      (if (i32.eq c (i32.const 41))
        (token-result (rparen) (i32.add pos (i32.const 1)))
        (if (i32.eq c (i32.const 34))
          (read-string-lit src pos len)
          (if (is-digit c)
            (read-number src pos len)
            (if (i32.eq c (i32.const 45))
              (if (i32.lt_s (i32.add pos (i32.const 1)) len)
                (let (next-c (string-ref src (i32.add pos (i32.const 1))))
                  (if (is-digit next-c)
                    (let (result (read-number src (i32.add pos (i32.const 1)) len))
                      (let (tok (token-result.tok result))
                        (let (new-pos (token-result.new-pos result))
                          (match tok
                            ((number n) (token-result (number (i32.sub (i32.const 0) n)) new-pos))
                            ((lparen) result)
                            ((rparen) result)
                            ((symbol s) result)
                            ((str-lit s) result)))))
                    (read-symbol src pos len)))
                (read-symbol src pos len))
              (read-symbol src pos len))))))))

(fn skip-ws ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (is-whitespace c)
        (skip-ws src (i32.add pos (i32.const 1)) len)
        pos))))

(fn skip-eol ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (i32.eq c (i32.const 10))
        (i32.add pos (i32.const 1))
        (skip-eol src (i32.add pos (i32.const 1)) len)))))

(fn skip-ignored-at ((src string) (pos s32) (len s32)) s32
  (let (pos2 (skip-ws src pos len))
    (if (i32.ge_s pos2 len)
      pos2
      (let (c (string-ref src pos2))
        (if (i32.eq c (i32.const 59))
          (skip-ignored-at src (skip-eol src pos2 len) len)
          pos2)))))

(fn tokenize-acc ((src string) (pos s32) (len s32) (tokens (list token))) (list token)
  (let (pos2 (skip-ignored-at src pos len))
    (if (i32.ge_s pos2 len)
      tokens
      (let (result (read-token src pos2 len))
        (let (new-tokens (list-push tokens (token-result.tok result)))
          (tokenize-acc src (token-result.new-pos result) len new-tokens))))))

(fn tokenize ((src string)) (list token)
  (tokenize-acc src (i32.const 0) (string-len src) (list-new token)))

; Parser
(record parse-result
  (expr sexpr)
  (new-pos s32))

(fn is-lparen ((t token)) s32
  (match t
    ((lparen) (i32.const 1))
    ((rparen) (i32.const 0))
    ((number n) (i32.const 0))
    ((symbol s) (i32.const 0))
    ((str-lit s) (i32.const 0))))

(fn is-rparen ((t token)) s32
  (match t
    ((lparen) (i32.const 0))
    ((rparen) (i32.const 1))
    ((number n) (i32.const 0))
    ((symbol s) (i32.const 0))
    ((str-lit s) (i32.const 0))))

(fn token-to-sexpr ((t token)) sexpr
  (match t
    ((lparen) (sym "error-lparen"))
    ((rparen) (sym "error-rparen"))
    ((number n) (num n))
    ((symbol s) (sym s))
    ((str-lit s) (str s))))

(fn parse-atom ((tokens (list token)) (pos s32)) parse-result
  (let (tok (list-get tokens pos))
    (parse-result (token-to-sexpr tok) (i32.add pos (i32.const 1)))))

; Helper: parse list items only
(fn parse-list-items ((tokens (list token)) (pos s32) (len s32) (items (list sexpr))) parse-result
  (if (i32.ge_s pos len)
    (parse-result (lst items) pos)
    (let (tok (list-get tokens pos))
      (if (is-rparen tok)
        (parse-result (lst items) (i32.add pos (i32.const 1)))
        (if (is-lparen tok)
          (let (nested (parse-list-items tokens (i32.add pos (i32.const 1)) len (list-new sexpr)))
            (let (new-items (list-push items (parse-result.expr nested)))
              (parse-list-items tokens (parse-result.new-pos nested) len new-items)))
          (let (atom (parse-atom tokens pos))
            (let (new-items (list-push items (parse-result.expr atom)))
              (parse-list-items tokens (parse-result.new-pos atom) len new-items))))))))

(fn parse-at ((tokens (list token)) (pos s32) (len s32) (mode s32) (items (list sexpr))) parse-result
  (if (i32.ge_s pos len)
    (if (i32.eq mode (i32.const 0))
      (parse-result (sym "error-eof") pos)
      (parse-result (lst items) pos))
    (let (tok (list-get tokens pos))
      (if (i32.eq mode (i32.const 0))
        (if (is-lparen tok)
          (parse-list-items tokens (i32.add pos (i32.const 1)) len (list-new sexpr))
          (parse-atom tokens pos))
        (parse-list-items tokens pos len items)))))

(fn parse-one ((tokens (list token)) (pos s32) (len s32)) parse-result
  (parse-at tokens pos len (i32.const 0) (list-new sexpr)))

(fn parse-all-acc ((tokens (list token)) (pos s32) (len s32) (exprs (list sexpr))) (list sexpr)
  (if (i32.ge_s pos len)
    exprs
    (let (result (parse-one tokens pos len))
      (let (new-exprs (list-push exprs (parse-result.expr result)))
        (parse-all-acc tokens (parse-result.new-pos result) len new-exprs)))))

(fn parse-all ((tokens (list token))) (list sexpr)
  (parse-all-acc tokens (i32.const 0) (list-len tokens) (list-new sexpr)))

(fn parse ((tokens (list token))) sexpr
  (let (result (parse-one tokens (i32.const 0) (list-len tokens)))
    (parse-result.expr result)))

(fn read-sexpr ((src string)) sexpr
  (parse (tokenize src)))

(fn read-all ((src string)) (list sexpr)
  (parse-all (tokenize src)))

; Utilities
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

(fn is-str ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) (i32.const 0))
    ((str s) (i32.const 1))
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

(fn get-str ((e sexpr)) string
  (match e
    ((sym s) "")
    ((num n) "")
    ((str s) s)
    ((lst l) "")))

(fn get-lst ((e sexpr)) (list sexpr)
  (match e
    ((sym s) (list-new sexpr))
    ((num n) (list-new sexpr))
    ((str s) (list-new sexpr))
    ((lst l) l)))

(fn sexpr-list-len ((e sexpr)) s32
  (match e
    ((sym s) (i32.const 0))
    ((num n) (i32.const 0))
    ((str s) (i32.const 0))
    ((lst l) (list-len l))))
"#;

// === Parse tests ===

// Debug test: verify tokenization works in parser context
#[test]
fn test_debug_token_count() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (tokens (tokenize "(a b c)"))
    (list-len tokens))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 5); // ( a b c ) = 5 tokens
}

// Debug test: check if parse-at mode 1 works for a single item
#[test]
fn test_debug_parse_at_single() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (tokens (tokenize "(a)"))
    (let (result (parse-at tokens (i32.const 1) (i32.const 3) (i32.const 1) (list-new sexpr)))
      (sexpr-list-len (parse-result.expr result))))))
"#,
        PARSER_PREAMBLE
    );
    // tokens: ( a ) at indices 0, 1, 2
    // parse-at starting at pos 1 (symbol a), mode 1, should parse until rparen
    // Result should be a list with 1 element
    assert_eq!(compile_and_run(&source), 1);
}

// Debug test: check parse-at for two items
#[test]
fn test_debug_parse_at_two() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (tokens (tokenize "(a b)"))
    (let (result (parse-at tokens (i32.const 1) (i32.const 4) (i32.const 1) (list-new sexpr)))
      (sexpr-list-len (parse-result.expr result))))))
"#,
        PARSER_PREAMBLE
    );
    // tokens: ( a b ) at indices 0, 1, 2, 3
    // parse-at starting at pos 1 (symbol a), mode 1, should parse a and b, stop at )
    // Result should be a list with 2 elements
    assert_eq!(compile_and_run(&source), 2);
}

// Debug test: check if list-push works with sexpr
#[test]
fn test_debug_list_push_sexpr() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (items (list-new sexpr))
    (let (items2 (list-push items (sym "a")))
      (let (items3 (list-push items2 (sym "b")))
        (list-len items3))))))
"#,
        PARSER_PREAMBLE
    );
    // Should be 2
    assert_eq!(compile_and_run(&source), 2);
}

// Debug test: what does parse-result.expr return for (a)?
#[test]
fn test_debug_parse_result_type() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (tokens (tokenize "(a)"))
    (let (result (parse-at tokens (i32.const 1) (i32.const 3) (i32.const 1) (list-new sexpr)))
      (is-lst (parse-result.expr result))))))
"#,
        PARSER_PREAMBLE
    );
    // Should be 1 (true) - it's a list
    assert_eq!(compile_and_run(&source), 1);
}

// Debug test: check the returned position for (a b)
#[test]
fn test_debug_parse_at_two_pos() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (tokens (tokenize "(a b)"))
    (let (result (parse-at tokens (i32.const 1) (i32.const 4) (i32.const 1) (list-new sexpr)))
      (parse-result.new-pos result)))))
"#,
        PARSER_PREAMBLE
    );
    // tokens: ( a b ) at indices 0, 1, 2, 3
    // After parsing all, new-pos should be 4 (after rparen at index 3)
    // If it's 2, we stopped after just "a"
    // If it's 3, we stopped at rparen without consuming it
    assert_eq!(compile_and_run(&source), 4);
}

// Debug test: manually step through - first get position after parsing first atom
#[test]
fn test_debug_first_atom_pos() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (tokens (tokenize "(a b)"))
    (let (result (parse-atom tokens (i32.const 1)))
      (parse-result.new-pos result)))))
"#,
        PARSER_PREAMBLE
    );
    // parse-atom at pos 1 (symbol "a") should return new-pos 2
    assert_eq!(compile_and_run(&source), 2);
}

// Debug test: create sexpr lst and check length
#[test]
fn test_debug_lst_construction() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (items (list-new sexpr))
    (let (items2 (list-push items (sym "a")))
      (let (items3 (list-push items2 (sym "b")))
        (let (e (lst items3))
          (sexpr-list-len e)))))))
"#,
        PARSER_PREAMBLE
    );
    // Should be 2
    assert_eq!(compile_and_run(&source), 2);
}

// Debug test: simulate what parse-at does step by step
#[test]
fn test_debug_manual_parse_steps() {
    let source = format!(
        r#"{}
(fn test-parse-two-atoms ((tokens (list token))) s32
  (let (items0 (list-new sexpr))
    ; Parse first atom at pos 1
    (let (r1 (parse-atom tokens (i32.const 1)))
      (let (items1 (list-push items0 (parse-result.expr r1)))
        ; Parse second atom at pos 2
        (let (r2 (parse-atom tokens (i32.const 2)))
          (let (items2 (list-push items1 (parse-result.expr r2)))
            (list-len items2)))))))

(export (fn test-func () s32
  (let (tokens (tokenize "(a b)"))
    (test-parse-two-atoms tokens))))
"#,
        PARSER_PREAMBLE
    );
    // Should be 2
    assert_eq!(compile_and_run(&source), 2);
}

// Debug test: call parse-list-items with pre-filled items
#[test]
fn test_debug_parse_list_items_with_prefilled() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (tokens (tokenize "(a b)"))
    ; Create pre-filled items with one element
    (let (items (list-push (list-new sexpr) (sym "pre")))
      ; Call parse-list-items starting at pos 1 with pre-filled items
      ; Should add "a" and "b" to the existing "pre"
      (let (result (parse-list-items tokens (i32.const 1) (i32.const 4) items))
        (sexpr-list-len (parse-result.expr result)))))))
"#,
        PARSER_PREAMBLE
    );
    // Should be 3: pre + a + b
    assert_eq!(compile_and_run(&source), 3);
}

// Debug test: parse-list-items with just rparen
#[test]
fn test_debug_parse_list_items_just_rparen() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (tokens (tokenize "()"))
    ; tokens: ( ) at indices 0, 1
    ; Call parse-list-items at pos 1 (the rparen) with empty items
    (let (result (parse-list-items tokens (i32.const 1) (i32.const 2) (list-new sexpr)))
      (sexpr-list-len (parse-result.expr result))))))
"#,
        PARSER_PREAMBLE
    );
    // Should be 0 (empty list)
    assert_eq!(compile_and_run(&source), 0);
}

// Debug test: simple recursive accumulation without parsing
#[test]
fn test_debug_simple_recursive_accumulation() {
    let source = format!(
        r#"{}
; Simple recursive function that accumulates sexprs
(fn accumulate-n ((n s32) (items (list sexpr))) (list sexpr)
  (if (i32.le_s n (i32.const 0))
    items
    (let (new-items (list-push items (sym "x")))
      (accumulate-n (i32.sub n (i32.const 1)) new-items))))

(export (fn test-func () s32
  (let (result (accumulate-n (i32.const 3) (list-new sexpr)))
    (list-len result))))
"#,
        PARSER_PREAMBLE
    );
    // Should be 3: accumulate 3 items
    assert_eq!(compile_and_run(&source), 3);
}

// Debug test: accumulate with parse-result wrapper
#[test]
fn test_debug_accumulate_returning_parse_result() {
    let source = format!(
        r#"{}
; Recursive accumulation that returns parse-result
(fn accumulate-pr ((n s32) (items (list sexpr))) parse-result
  (if (i32.le_s n (i32.const 0))
    (parse-result (lst items) n)
    (let (new-items (list-push items (sym "x")))
      (accumulate-pr (i32.sub n (i32.const 1)) new-items))))

(export (fn test-func () s32
  (let (result (accumulate-pr (i32.const 3) (list-new sexpr)))
    (sexpr-list-len (parse-result.expr result)))))
"#,
        PARSER_PREAMBLE
    );
    // Should be 3
    assert_eq!(compile_and_run(&source), 3);
}

// Debug test: simplified parse-list with only atoms (no nesting)
#[test]
fn test_debug_parse_atoms_only() {
    let source = format!(
        r#"{}
; Simplified list parser - atoms only, no nested lists
(fn parse-atoms ((tokens (list token)) (pos s32) (len s32) (items (list sexpr))) parse-result
  (if (i32.ge_s pos len)
    (parse-result (lst items) pos)
    (let (tok (list-get tokens pos))
      (if (is-rparen tok)
        (parse-result (lst items) (i32.add pos (i32.const 1)))
        ; Assume all non-rparen tokens are atoms
        (let (atom (parse-atom tokens pos))
          (let (new-items (list-push items (parse-result.expr atom)))
            (parse-atoms tokens (parse-result.new-pos atom) len new-items)))))))

(export (fn test-func () s32
  (let (tokens (tokenize "(a b)"))
    ; tokens: ( a b ) at indices 0, 1, 2, 3
    (let (result (parse-atoms tokens (i32.const 1) (i32.const 4) (list-new sexpr)))
      (sexpr-list-len (parse-result.expr result))))))
"#,
        PARSER_PREAMBLE
    );
    // Should be 2: a and b
    assert_eq!(compile_and_run(&source), 2);
}

// Debug test: parse-atoms with extra if layer (mimics parse-list-items structure)
#[test]
fn test_debug_parse_atoms_with_extra_if() {
    let source = format!(
        r#"{}
; Like parse-atoms but with extra if layer for lparen (which does same thing)
(fn parse-atoms-extra-if ((tokens (list token)) (pos s32) (len s32) (items (list sexpr))) parse-result
  (if (i32.ge_s pos len)
    (parse-result (lst items) pos)
    (let (tok (list-get tokens pos))
      (if (is-rparen tok)
        (parse-result (lst items) (i32.add pos (i32.const 1)))
        ; Add extra if like parse-list-items has
        (if (is-lparen tok)
          ; For lparen, do same as atoms (no special handling)
          (let (atom (parse-atom tokens pos))
            (let (new-items (list-push items (parse-result.expr atom)))
              (parse-atoms-extra-if tokens (parse-result.new-pos atom) len new-items)))
          ; For non-lparen, do atoms
          (let (atom (parse-atom tokens pos))
            (let (new-items (list-push items (parse-result.expr atom)))
              (parse-atoms-extra-if tokens (parse-result.new-pos atom) len new-items))))))))

(export (fn test-func () s32
  (let (tokens (tokenize "(a b)"))
    (let (result (parse-atoms-extra-if tokens (i32.const 1) (i32.const 4) (list-new sexpr)))
      (sexpr-list-len (parse-result.expr result))))))
"#,
        PARSER_PREAMBLE
    );
    // Should be 2: a and b
    assert_eq!(compile_and_run(&source), 2);
}

// Debug test: full parse-list-items with nested list support
#[test]
fn test_debug_parse_full_with_nested() {
    let source = format!(
        r#"{}
; Full parse-list-items that handles nested lists
(fn parse-full ((tokens (list token)) (pos s32) (len s32) (items (list sexpr))) parse-result
  (if (i32.ge_s pos len)
    (parse-result (lst items) pos)
    (let (tok (list-get tokens pos))
      (if (is-rparen tok)
        (parse-result (lst items) (i32.add pos (i32.const 1)))
        (if (is-lparen tok)
          ; Nested list - call self recursively with new empty list
          (let (nested (parse-full tokens (i32.add pos (i32.const 1)) len (list-new sexpr)))
            (let (new-items (list-push items (parse-result.expr nested)))
              (parse-full tokens (parse-result.new-pos nested) len new-items)))
          ; Atom
          (let (atom (parse-atom tokens pos))
            (let (new-items (list-push items (parse-result.expr atom)))
              (parse-full tokens (parse-result.new-pos atom) len new-items))))))))

(export (fn test-func () s32
  (let (tokens (tokenize "(a b)"))
    (let (result (parse-full tokens (i32.const 1) (i32.const 4) (list-new sexpr)))
      (sexpr-list-len (parse-result.expr result))))))
"#,
        PARSER_PREAMBLE
    );
    // Should be 2: a and b
    assert_eq!(compile_and_run(&source), 2);
}

// Debug test: bind empty list before recursive call
#[test]
fn test_debug_parse_full_with_bound_empty_list() {
    let source = format!(
        r#"{}
; Full parser but bind empty list before recursive call
(fn parse-full-bound ((tokens (list token)) (pos s32) (len s32) (items (list sexpr))) parse-result
  (if (i32.ge_s pos len)
    (parse-result (lst items) pos)
    (let (tok (list-get tokens pos))
      (if (is-rparen tok)
        (parse-result (lst items) (i32.add pos (i32.const 1)))
        (if (is-lparen tok)
          ; Bind empty list first
          (let (empty-list (list-new sexpr))
            (let (nested (parse-full-bound tokens (i32.add pos (i32.const 1)) len empty-list))
              (let (new-items (list-push items (parse-result.expr nested)))
                (parse-full-bound tokens (parse-result.new-pos nested) len new-items))))
          ; Atom
          (let (atom (parse-atom tokens pos))
            (let (new-items (list-push items (parse-result.expr atom)))
              (parse-full-bound tokens (parse-result.new-pos atom) len new-items))))))))

(export (fn test-func () s32
  (let (tokens (tokenize "(a b)"))
    (let (result (parse-full-bound tokens (i32.const 1) (i32.const 4) (list-new sexpr)))
      (sexpr-list-len (parse-result.expr result))))))
"#,
        PARSER_PREAMBLE
    );
    // Should be 2: a and b
    assert_eq!(compile_and_run(&source), 2);
}

// Debug test: just call list-new in branch but don't use it for recursion
#[test]
fn test_debug_list_new_in_branch_unused() {
    let source = format!(
        r#"{}
; Like parse-atoms-extra-if but with a list-new in lparen branch that's ignored
(fn parse-with-unused-list-new ((tokens (list token)) (pos s32) (len s32) (items (list sexpr))) parse-result
  (if (i32.ge_s pos len)
    (parse-result (lst items) pos)
    (let (tok (list-get tokens pos))
      (if (is-rparen tok)
        (parse-result (lst items) (i32.add pos (i32.const 1)))
        (if (is-lparen tok)
          ; Call list-new but don't use it
          (let (dummy (list-new sexpr))
            (let (atom (parse-atom tokens pos))
              (let (new-items (list-push items (parse-result.expr atom)))
                (parse-with-unused-list-new tokens (parse-result.new-pos atom) len new-items))))
          (let (atom (parse-atom tokens pos))
            (let (new-items (list-push items (parse-result.expr atom)))
              (parse-with-unused-list-new tokens (parse-result.new-pos atom) len new-items))))))))

(export (fn test-func () s32
  (let (tokens (tokenize "(a b)"))
    (let (result (parse-with-unused-list-new tokens (i32.const 1) (i32.const 4) (list-new sexpr)))
      (sexpr-list-len (parse-result.expr result))))))
"#,
        PARSER_PREAMBLE
    );
    // Should be 2: a and b
    assert_eq!(compile_and_run(&source), 2);
}

// Debug test: use get-symbol-len on index 1 of "(abc)"
#[test]
fn test_debug_symbol_len_index1() {
    let source = r#"
(variant token
  (lparen)
  (rparen)
  (number s32)
  (symbol string)
  (str-lit string))

(fn is-whitespace ((c s32)) s32
  (if (i32.eq c (i32.const 32))
    (i32.const 1)
    (if (i32.eq c (i32.const 9))
      (i32.const 1)
      (if (i32.eq c (i32.const 10))
        (i32.const 1)
        (if (i32.eq c (i32.const 13))
          (i32.const 1)
          (i32.const 0))))))

(fn is-digit ((c s32)) s32
  (if (i32.ge_s c (i32.const 48))
    (if (i32.le_s c (i32.const 57))
      (i32.const 1)
      (i32.const 0))
    (i32.const 0)))

(fn is-delimiter ((c s32)) s32
  (if (is-whitespace c)
    (i32.const 1)
    (if (i32.eq c (i32.const 40))
      (i32.const 1)
      (if (i32.eq c (i32.const 41))
        (i32.const 1)
        (if (i32.eq c (i32.const 59))
          (i32.const 1)
          (if (i32.eq c (i32.const 34))
            (i32.const 1)
            (i32.const 0)))))))

(fn digit-value ((c s32)) s32
  (i32.sub c (i32.const 48)))

(record token-result
  (tok token)
  (new-pos s32))

(fn parse-number-value ((src string) (pos s32) (len s32) (acc s32)) token-result
  (if (i32.ge_s pos len)
    (token-result (number acc) pos)
    (let (c (string-ref src pos))
      (if (is-digit c)
        (parse-number-value src
          (i32.add pos (i32.const 1))
          len
          (i32.add (i32.mul acc (i32.const 10)) (digit-value c)))
        (token-result (number acc) pos)))))

(fn read-number ((src string) (pos s32) (len s32)) token-result
  (parse-number-value src pos len (i32.const 0)))

(fn find-symbol-end-at ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (is-delimiter c)
        pos
        (find-symbol-end-at src (i32.add pos (i32.const 1)) len)))))

(fn read-symbol ((src string) (pos s32) (len s32)) token-result
  (let (start pos)
    (let (end (find-symbol-end-at src pos len))
      (let (sym-str (substring src start end))
        (token-result (symbol sym-str) end)))))

(fn read-token ((src string) (pos s32) (len s32)) token-result
  (let (c (string-ref src pos))
    (if (i32.eq c (i32.const 40))
      (token-result (lparen) (i32.add pos (i32.const 1)))
      (if (i32.eq c (i32.const 41))
        (token-result (rparen) (i32.add pos (i32.const 1)))
        (if (is-digit c)
          (read-number src pos len)
          (read-symbol src pos len))))))

(fn skip-ws ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (is-whitespace c)
        (skip-ws src (i32.add pos (i32.const 1)) len)
        pos))))

(fn tokenize-acc ((src string) (pos s32) (len s32) (tokens (list token))) (list token)
  (let (pos2 (skip-ws src pos len))
    (if (i32.ge_s pos2 len)
      tokens
      (let (result (read-token src pos2 len))
        (let (new-tokens (list-push tokens (token-result.tok result)))
          (tokenize-acc src (token-result.new-pos result) len new-tokens))))))

(fn tokenize ((src string)) (list token)
  (tokenize-acc src (i32.const 0) (string-len src) (list-new token)))

(fn get-symbol-len ((t token)) s32
  (match t
    ((lparen) (i32.const 0))
    ((rparen) (i32.const 0))
    ((number n) (i32.const 0))
    ((symbol s) (string-len s))
    ((str-lit s) (i32.const 0))))

(export (fn test-func () s32
  (let (tokens (tokenize "(abc)"))
    ; tokens: [lparen, symbol "abc", rparen]
    ; index 1 = symbol "abc", length should be 3
    (let (tok (list-get tokens (i32.const 1)))
      (get-symbol-len tok)))))
"#;
    // symbol "abc" should have length 3
    assert_eq!(compile_and_run(&source), 3);
}

// Debug test: tokenize "(a)" WITHOUT sexpr variant
#[test]
fn test_debug_paren_a_no_sexpr() {
    // This test uses only token variant, no sexpr
    let source = r#"
(variant token
  (lparen)
  (rparen)
  (number s32)
  (symbol string)
  (str-lit string))

(fn is-whitespace ((c s32)) s32
  (if (i32.eq c (i32.const 32))
    (i32.const 1)
    (if (i32.eq c (i32.const 9))
      (i32.const 1)
      (if (i32.eq c (i32.const 10))
        (i32.const 1)
        (if (i32.eq c (i32.const 13))
          (i32.const 1)
          (i32.const 0))))))

(fn is-digit ((c s32)) s32
  (if (i32.ge_s c (i32.const 48))
    (if (i32.le_s c (i32.const 57))
      (i32.const 1)
      (i32.const 0))
    (i32.const 0)))

(fn is-delimiter ((c s32)) s32
  (if (is-whitespace c)
    (i32.const 1)
    (if (i32.eq c (i32.const 40))
      (i32.const 1)
      (if (i32.eq c (i32.const 41))
        (i32.const 1)
        (if (i32.eq c (i32.const 59))
          (i32.const 1)
          (if (i32.eq c (i32.const 34))
            (i32.const 1)
            (i32.const 0)))))))

(fn digit-value ((c s32)) s32
  (i32.sub c (i32.const 48)))

(record token-result
  (tok token)
  (new-pos s32))

(fn parse-number-value ((src string) (pos s32) (len s32) (acc s32)) token-result
  (if (i32.ge_s pos len)
    (token-result (number acc) pos)
    (let (c (string-ref src pos))
      (if (is-digit c)
        (parse-number-value src
          (i32.add pos (i32.const 1))
          len
          (i32.add (i32.mul acc (i32.const 10)) (digit-value c)))
        (token-result (number acc) pos)))))

(fn read-number ((src string) (pos s32) (len s32)) token-result
  (parse-number-value src pos len (i32.const 0)))

(fn find-symbol-end-at ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (is-delimiter c)
        pos
        (find-symbol-end-at src (i32.add pos (i32.const 1)) len)))))

(fn read-symbol ((src string) (pos s32) (len s32)) token-result
  (let (start pos)
    (let (end (find-symbol-end-at src pos len))
      (let (sym-str (substring src start end))
        (token-result (symbol sym-str) end)))))

(fn read-token ((src string) (pos s32) (len s32)) token-result
  (let (c (string-ref src pos))
    (if (i32.eq c (i32.const 40))
      (token-result (lparen) (i32.add pos (i32.const 1)))
      (if (i32.eq c (i32.const 41))
        (token-result (rparen) (i32.add pos (i32.const 1)))
        (if (is-digit c)
          (read-number src pos len)
          (read-symbol src pos len))))))

(fn skip-ws ((src string) (pos s32) (len s32)) s32
  (if (i32.ge_s pos len)
    pos
    (let (c (string-ref src pos))
      (if (is-whitespace c)
        (skip-ws src (i32.add pos (i32.const 1)) len)
        pos))))

(fn tokenize-acc ((src string) (pos s32) (len s32) (tokens (list token))) (list token)
  (let (pos2 (skip-ws src pos len))
    (if (i32.ge_s pos2 len)
      tokens
      (let (result (read-token src pos2 len))
        (let (new-tokens (list-push tokens (token-result.tok result)))
          (tokenize-acc src (token-result.new-pos result) len new-tokens))))))

(fn tokenize ((src string)) (list token)
  (tokenize-acc src (i32.const 0) (string-len src) (list-new token)))

(fn get-token-tag ((t token)) s32
  (match t
    ((lparen) (i32.const 0))
    ((rparen) (i32.const 1))
    ((number n) (i32.const 2))
    ((symbol s) (i32.const 3))
    ((str-lit s) (i32.const 4))))

(export (fn test-func () s32
  (let (tokens (tokenize "(a)"))
    (let (tok (list-get tokens (i32.const 1)))
      (get-token-tag tok)))))
"#;
    // symbol "a" should have tag 3
    assert_eq!(compile_and_run(&source), 3);
}

// Debug test: tokenize "(a)" and check tag at index 1
#[test]
fn test_debug_paren_a_index1_tag() {
    let source = format!(
        r#"{}
(fn get-token-tag ((t token)) s32
  (match t
    ((lparen) (i32.const 0))
    ((rparen) (i32.const 1))
    ((number n) (i32.const 2))
    ((symbol s) (i32.const 3))
    ((str-lit s) (i32.const 4))))

(export (fn test-func () s32
  (let (tokens (tokenize "(a)"))
    ; tokens: [lparen, symbol "a", rparen]
    ; index 1 should be symbol "a"
    (let (tok (list-get tokens (i32.const 1)))
      (get-token-tag tok)))))
"#,
        PARSER_PREAMBLE
    );
    // symbol "a" should have tag 3
    assert_eq!(compile_and_run(&source), 3);
}

// Debug test: tokenize single symbol and check its tag
#[test]
fn test_debug_single_symbol_tag() {
    let source = format!(
        r#"{}
(fn get-token-tag ((t token)) s32
  (match t
    ((lparen) (i32.const 0))
    ((rparen) (i32.const 1))
    ((number n) (i32.const 2))
    ((symbol s) (i32.const 3))
    ((str-lit s) (i32.const 4))))

(export (fn test-func () s32
  (let (tokens (tokenize "a"))
    ; should be just one symbol token
    (let (tok (list-get tokens (i32.const 0)))
      (get-token-tag tok)))))
"#,
        PARSER_PREAMBLE
    );
    // symbol "a" should have tag 3
    assert_eq!(compile_and_run(&source), 3);
}

// Debug test: what tag does symbol token have?
// token variant: lparen=0, rparen=1, number=2, symbol=3, str-lit=4
#[test]
fn test_debug_symbol_token_tag() {
    let source = format!(
        r#"{}
; Get the raw tag of a token (read first i32 from memory)
(fn get-token-tag ((t token)) s32
  (match t
    ((lparen) (i32.const 0))
    ((rparen) (i32.const 1))
    ((number n) (i32.const 2))
    ((symbol s) (i32.const 3))
    ((str-lit s) (i32.const 4))))

(export (fn test-func () s32
  (let (tokens (tokenize "(a b)"))
    ; tokens[1] should be symbol "a", tag should be 3
    (let (tok (list-get tokens (i32.const 1)))
      (get-token-tag tok)))))
"#,
        PARSER_PREAMBLE
    );
    // symbol "a" should have tag 3
    assert_eq!(compile_and_run(&source), 3);
}

// Debug test: what does is-lparen return for a symbol token?
#[test]
fn test_debug_is_lparen_on_symbol() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (tokens (tokenize "(a b)"))
    ; tokens[1] should be symbol "a"
    (let (tok (list-get tokens (i32.const 1)))
      (is-lparen tok)))))
"#,
        PARSER_PREAMBLE
    );
    // is-lparen on a symbol should return 0
    assert_eq!(compile_and_run(&source), 0);
}

// Debug test: is-lparen on actual lparen token
#[test]
fn test_debug_is_lparen_on_lparen() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (tokens (tokenize "(a b)"))
    ; tokens[0] should be lparen
    (let (tok (list-get tokens (i32.const 0)))
      (is-lparen tok)))))
"#,
        PARSER_PREAMBLE
    );
    // is-lparen on lparen should return 1
    assert_eq!(compile_and_run(&source), 1);
}

// Debug test: dead lparen branch (always false condition)
#[test]
fn test_debug_dead_lparen_branch() {
    let source = format!(
        r#"{}
; Like parse-full but lparen branch condition is always false
(fn parse-dead-branch ((tokens (list token)) (pos s32) (len s32) (items (list sexpr))) parse-result
  (if (i32.ge_s pos len)
    (parse-result (lst items) pos)
    (let (tok (list-get tokens pos))
      (if (is-rparen tok)
        (parse-result (lst items) (i32.add pos (i32.const 1)))
        (if (i32.const 0)
          ; DEAD BRANCH - never taken
          (let (nested (parse-dead-branch tokens (i32.add pos (i32.const 1)) len (list-new sexpr)))
            (let (new-items (list-push items (parse-result.expr nested)))
              (parse-dead-branch tokens (parse-result.new-pos nested) len new-items)))
          ; Always taken
          (let (atom (parse-atom tokens pos))
            (let (new-items (list-push items (parse-result.expr atom)))
              (parse-dead-branch tokens (parse-result.new-pos atom) len new-items))))))))

(export (fn test-func () s32
  (let (tokens (tokenize "(a b)"))
    (let (result (parse-dead-branch tokens (i32.const 1) (i32.const 4) (list-new sexpr)))
      (sexpr-list-len (parse-result.expr result))))))
"#,
        PARSER_PREAMBLE
    );
    // Lparen branch never taken, should be 2: a and b
    assert_eq!(compile_and_run(&source), 2);
}

// Debug test: list-new in branch, use both expr AND new-pos from nested
#[test]
fn test_debug_list_new_passed_use_both() {
    let source = format!(
        r#"{}
; Like parse-full - use both expr and new-pos from nested
(fn parse-use-both ((tokens (list token)) (pos s32) (len s32) (items (list sexpr))) parse-result
  (if (i32.ge_s pos len)
    (parse-result (lst items) pos)
    (let (tok (list-get tokens pos))
      (if (is-rparen tok)
        (parse-result (lst items) (i32.add pos (i32.const 1)))
        (if (is-lparen tok)
          ; Call recursively with new list, use both expr and new-pos
          (let (nested (parse-use-both tokens (i32.add pos (i32.const 1)) len (list-new sexpr)))
            (let (new-items (list-push items (parse-result.expr nested)))
              (parse-use-both tokens (parse-result.new-pos nested) len new-items)))
          (let (atom (parse-atom tokens pos))
            (let (new-items (list-push items (parse-result.expr atom)))
              (parse-use-both tokens (parse-result.new-pos atom) len new-items))))))))

(export (fn test-func () s32
  (let (tokens (tokenize "(a b)"))
    (let (result (parse-use-both tokens (i32.const 1) (i32.const 4) (list-new sexpr)))
      (sexpr-list-len (parse-result.expr result))))))
"#,
        PARSER_PREAMBLE
    );
    // Input (a b) has no nested lists, lparen branch never taken
    // Should be 2: a and b
    assert_eq!(compile_and_run(&source), 2);
}

// Debug test: list-new in branch, pass to recursive call, use result's expr
#[test]
fn test_debug_list_new_passed_use_expr() {
    let source = format!(
        r#"{}
; Like parse-full but use expr from nested, but use different new-pos
(fn parse-use-expr-only ((tokens (list token)) (pos s32) (len s32) (items (list sexpr))) parse-result
  (if (i32.ge_s pos len)
    (parse-result (lst items) pos)
    (let (tok (list-get tokens pos))
      (if (is-rparen tok)
        (parse-result (lst items) (i32.add pos (i32.const 1)))
        (if (is-lparen tok)
          ; Call recursively, use its expr but not its new-pos
          (let (nested (parse-use-expr-only tokens (i32.add pos (i32.const 1)) len (list-new sexpr)))
            (let (new-items (list-push items (parse-result.expr nested)))
              ; Ignore nested's new-pos, just use atom's new-pos
              (let (atom (parse-atom tokens pos))
                (parse-use-expr-only tokens (parse-result.new-pos atom) len new-items))))
          (let (atom (parse-atom tokens pos))
            (let (new-items (list-push items (parse-result.expr atom)))
              (parse-use-expr-only tokens (parse-result.new-pos atom) len new-items))))))))

(export (fn test-func () s32
  (let (tokens (tokenize "(a b)"))
    (let (result (parse-use-expr-only tokens (i32.const 1) (i32.const 4) (list-new sexpr)))
      (sexpr-list-len (parse-result.expr result))))))
"#,
        PARSER_PREAMBLE
    );
    // Input (a b) has no nested lists, lparen branch never taken
    // Should be 2: a and b
    assert_eq!(compile_and_run(&source), 2);
}

// Debug test: list-new in branch, pass to recursive call but don't use result
#[test]
fn test_debug_list_new_passed_but_result_ignored() {
    let source = format!(
        r#"{}
; Like parse-full but ignore nested result, continue with atom
(fn parse-with-ignored-nested ((tokens (list token)) (pos s32) (len s32) (items (list sexpr))) parse-result
  (if (i32.ge_s pos len)
    (parse-result (lst items) pos)
    (let (tok (list-get tokens pos))
      (if (is-rparen tok)
        (parse-result (lst items) (i32.add pos (i32.const 1)))
        (if (is-lparen tok)
          ; Call recursively with new list but ignore result
          (let (nested (parse-with-ignored-nested tokens (i32.add pos (i32.const 1)) len (list-new sexpr)))
            ; Ignore nested, just do atom
            (let (atom (parse-atom tokens pos))
              (let (new-items (list-push items (parse-result.expr atom)))
                (parse-with-ignored-nested tokens (parse-result.new-pos atom) len new-items))))
          (let (atom (parse-atom tokens pos))
            (let (new-items (list-push items (parse-result.expr atom)))
              (parse-with-ignored-nested tokens (parse-result.new-pos atom) len new-items))))))))

(export (fn test-func () s32
  (let (tokens (tokenize "(a b)"))
    (let (result (parse-with-ignored-nested tokens (i32.const 1) (i32.const 4) (list-new sexpr)))
      (sexpr-list-len (parse-result.expr result))))))
"#,
        PARSER_PREAMBLE
    );
    // Should be 2: a and b - (a b) doesn't have nested lists so lparen branch never taken
    assert_eq!(compile_and_run(&source), 2);
}

#[test]
fn test_parse_empty() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (exprs (read-all ""))
    (list-len exprs))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 0);
}

#[test]
fn test_parse_number() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "42"))
    (is-num e))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_parse_number_value() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "42"))
    (get-num e))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 42);
}

#[test]
fn test_parse_symbol() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "hello"))
    (is-sym e))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_parse_symbol_len() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "hello"))
    (string-len (get-sym e)))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 5);
}

#[test]
fn test_parse_string() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "\"hello\""))
    (is-str e))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_parse_string_len() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "\"hello\""))
    (string-len (get-str e)))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 5);
}

#[test]
fn test_parse_empty_list() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "()"))
    (is-lst e))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_parse_empty_list_len() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "()"))
    (sexpr-list-len e))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 0);
}

#[test]
fn test_parse_simple_list() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "(a b c)"))
    (sexpr-list-len e))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 3);
}

#[test]
fn test_parse_list_numbers() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "(1 2 3)"))
    (sexpr-list-len e))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 3);
}

#[test]
fn test_parse_list_first() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "(42 1 2)"))
    (let (items (get-lst e))
      (let (first (list-get items (i32.const 0)))
        (get-num first))))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 42);
}

#[test]
fn test_parse_nested() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "(a (b c))"))
    (sexpr-list-len e))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 2);
}

#[test]
fn test_parse_nested_inner() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "(a (b c))"))
    (let (items (get-lst e))
      (let (second (list-get items (i32.const 1)))
        (is-lst second))))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_parse_nested_inner_len() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "(a (b c))"))
    (let (items (get-lst e))
      (let (second (list-get items (i32.const 1)))
        (sexpr-list-len second))))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 2);
}

#[test]
fn test_parse_fn_expr() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "(i32.add 1 2)"))
    (sexpr-list-len e))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 3);
}

#[test]
fn test_parse_fn_first_sym() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "(i32.add 1 2)"))
    (let (items (get-lst e))
      (let (first (list-get items (i32.const 0)))
        (is-sym first))))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_parse_multiple() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (exprs (read-all "1 2 3"))
    (list-len exprs))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 3);
}

#[test]
fn test_parse_multiple_sexprs() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (exprs (read-all "(a) (b) (c)"))
    (list-len exprs))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 3);
}

#[test]
fn test_parse_deep_nested() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "(((a)))"))
    (let (l1 (get-lst e))
      (let (e2 (list-get l1 (i32.const 0)))
        (let (l2 (get-lst e2))
          (let (e3 (list-get l2 (i32.const 0)))
            (sexpr-list-len e3))))))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 1);
}

#[test]
fn test_parse_fn_def() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "(fn add ((x s32)) s32 x)"))
    (sexpr-list-len e))))
"#,
        PARSER_PREAMBLE
    );
    // fn add ((x s32)) s32 x = 5 elements
    assert_eq!(compile_and_run(&source), 5);
}

#[test]
fn test_parse_negative() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "-42"))
    (get-num e))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), -42);
}

#[test]
fn test_parse_dotted_symbol() {
    let source = format!(
        r#"{}
(export (fn test-func () s32
  (let (e (read-sexpr "i32.add"))
    (string-len (get-sym e)))))
"#,
        PARSER_PREAMBLE
    );
    assert_eq!(compile_and_run(&source), 7);
}
