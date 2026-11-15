// Vectors and hash maps (wisp/interpreter/containers.wisp, ticket 0121).
use std::path::PathBuf;
use std::sync::OnceLock;

use granite::{
    compiler,
    interpreter::{Interpreter, InterpreterImage},
};

fn session() -> Interpreter {
    static IMAGE: OnceLock<InterpreterImage> = OnceLock::new();
    let image = IMAGE.get_or_init(|| {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        // Compiling the evaluator recurses per expression node and exceeds the
        // default 2 MiB test stack, so build it on a large stack.
        let package = std::thread::scope(|s| {
            std::thread::Builder::new()
                .stack_size(1 << 30)
                .spawn_scoped(s, || {
                    compiler::compile(
                        &root.join("wisp/interpreter/evaluator.wisp"),
                        &root.join(format!(
                            "target/interpreter-tests/{}/evaluator",
                            std::process::id()
                        )),
                        compiler::EmitOptions::default(),
                    )
                    .unwrap()
                    .wasm
                })
                .expect("spawn big-stack compile thread")
                .join()
                .expect("big-stack compile thread panicked")
        });
        InterpreterImage::load(package).unwrap()
    });
    image.session().unwrap()
}

fn eval(session: &mut Interpreter, source: &str, expected: &str) {
    assert_eq!(session.evaluate(source).unwrap(), expected, "{source}");
}

#[test]
fn vectors_index_update_push_and_pop() {
    let mut s = session();
    eval(&mut s, "(define v (vector 1 2 3))", "#<vector (1 2 3)>");
    eval(&mut s, "(vector-ref v 1)", "2");
    eval(&mut s, "(vector-set! v 1 \"two\")", "\"two\"");
    eval(&mut s, "v", "#<vector (1 \"two\" 3)>");
    eval(&mut s, "(vector-push! v 4)", "4");
    eval(&mut s, "(vector-length v)", "4");
    eval(&mut s, "(vector-pop! v)", "4");
    eval(&mut s, "(vector-length v)", "3");
    eval(&mut s, "(make-vector 3 0)", "#<vector (0 0 0)>");
    eval(&mut s, "(vector)", "#<vector ()>");
    eval(&mut s, "(list->vector (list 1 2))", "#<vector (1 2)>");
    eval(
        &mut s,
        "(vector-ref v 3)",
        "error: vector index out of bounds or not an s32",
    );
    eval(
        &mut s,
        "(vector-ref v -1)",
        "error: vector index out of bounds or not an s32",
    );
    eval(
        &mut s,
        "(vector-pop! (vector))",
        "error: vector-pop! of an empty vector",
    );
    eval(
        &mut s,
        "(vector-ref (list 1) 0)",
        "error: vector-ref expects a vector",
    );
}

#[test]
fn vectors_are_shared_and_lists_out_are_copies() {
    let mut s = session();
    eval(&mut s, "(define a (vector 1 2))", "#<vector (1 2)>");
    eval(&mut s, "(define b a)", "#<vector (1 2)>");
    eval(&mut s, "(vector-set! a 0 9)", "9");
    eval(&mut s, "b", "#<vector (9 2)>");
    // A list taken out, a copy, and the list given in do not change with the vector.
    eval(&mut s, "(define xs (vector->list a))", "(9 2)");
    eval(&mut s, "(define c (vector-copy a))", "#<vector (9 2)>");
    eval(&mut s, "(define ys (list 5 6))", "(5 6)");
    eval(&mut s, "(define d (list->vector ys))", "#<vector (5 6)>");
    eval(
        &mut s,
        "(begin (vector-set! a 1 7) (vector-push! a 8) (vector-set! d 0 0) xs)",
        "(9 2)",
    );
    eval(&mut s, "c", "#<vector (9 2)>");
    eval(&mut s, "ys", "(5 6)");
    eval(&mut s, "a", "#<vector (9 7 8)>");
}

#[test]
fn vector_loop_over_many_items() {
    let mut s = session();
    eval(
        &mut s,
        "(define v (make-vector 10000 0))",
        "#<vector (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ... 9936 more)>",
    );
    eval(
        &mut s,
        "(define fill (lambda (i) (if (= i 10000) i (begin (vector-set! v i (* i 2)) (fill (+ i 1))))))",
        "#<closure>",
    );
    eval(&mut s, "(fill 0)", "10000");
    eval(
        &mut s,
        "(define sum (lambda (i acc) (if (= i 10000) acc (sum (+ i 1) (+ acc (vector-ref v i))))))",
        "#<closure>",
    );
    eval(&mut s, "(sum 0 0)", "99990000");
}

#[test]
fn maps_get_set_delete_in_insertion_order() {
    let mut s = session();
    eval(
        &mut s,
        "(define m (hash-map \"a\" 1 \"b\" 2))",
        "#<map ((\"a\" 1) (\"b\" 2))>",
    );
    eval(&mut s, "(map-get m \"a\")", "(some s32 1)");
    eval(
        &mut s,
        "(match (map-get m \"zz\") ((some v) v) ((none) \"missing\"))",
        "\"missing\"",
    );
    eval(&mut s, "(map-get m \"zz\")", "(none (tuple))");
    eval(&mut s, "(map-has? m \"b\")", "1");
    eval(&mut s, "(map-set! m \"c\" 3)", "3");
    eval(&mut s, "(map-set! m \"a\" 10)", "10");
    eval(&mut s, "(map-keys m)", "(\"a\" \"b\" \"c\")");
    eval(&mut s, "(map-values m)", "(10 2 3)");
    eval(&mut s, "(map-delete! m \"b\")", "1");
    eval(&mut s, "(map-delete! m \"b\")", "0");
    eval(&mut s, "(map-count m)", "2");
    eval(&mut s, "(map-set! m \"b\" 4)", "4");
    eval(&mut s, "(map->list m)", "((\"a\" 10) (\"c\" 3) (\"b\" 4))");
    // Keys of other kinds; each kind is distinct.
    eval(&mut s, "(map-set! m 1 \"one\")", "\"one\"");
    eval(&mut s, "(map-set! m 'a \"symbol\")", "\"symbol\"");
    eval(&mut s, "(map-set! m (list 1 \"x\") \"pair\")", "\"pair\"");
    eval(
        &mut s,
        "(map-get m (list 1 \"x\"))",
        "(some string \"pair\")",
    );
    eval(&mut s, "(map-get m 'a)", "(some string \"symbol\")");
    eval(&mut s, "(map-get m \"a\")", "(some s32 10)");
    eval(
        &mut s,
        "(map-set! m 1.5 0)",
        "error: map key must be an integer, string, symbol, byte, boolean, or a list, tuple or record of these",
    );
    eval(
        &mut s,
        "(map-set! m (vector) 0)",
        "error: map key must be an integer, string, symbol, byte, boolean, or a list, tuple or record of these",
    );
    eval(
        &mut s,
        "(hash-map 1)",
        "error: hash-map expects keys and values in pairs",
    );
}

#[test]
fn maps_are_shared_and_lists_out_are_copies() {
    let mut s = session();
    eval(&mut s, "(define m (hash-map 1 2))", "#<map ((1 2))>");
    eval(&mut s, "(define n m)", "#<map ((1 2))>");
    eval(&mut s, "(define pairs (map->list m))", "((1 2))");
    eval(&mut s, "(define keys (map-keys m))", "(1)");
    eval(&mut s, "(define c (map-copy m))", "#<map ((1 2))>");
    eval(
        &mut s,
        "(begin (map-set! m 1 5) (map-set! m 3 4) pairs)",
        "((1 2))",
    );
    eval(&mut s, "keys", "(1)");
    eval(&mut s, "c", "#<map ((1 2))>");
    eval(&mut s, "n", "#<map ((1 5) (3 4))>");
}

#[test]
fn maps_grow_and_compact_over_many_keys() {
    let mut s = session();
    eval(&mut s, "(define m (hash-map))", "#<map ()>");
    eval(
        &mut s,
        "(define put (lambda (i) (if (= i 5000) i (begin (map-set! m i (* i 3)) (put (+ i 1))))))",
        "#<closure>",
    );
    eval(&mut s, "(put 0)", "5000");
    eval(
        &mut s,
        "(define drop (lambda (i) (if (>= i 5000) i (begin (map-delete! m i) (drop (+ i 2))))))",
        "#<closure>",
    );
    eval(&mut s, "(drop 0)", "5000");
    eval(&mut s, "(map-count m)", "2500");
    eval(
        &mut s,
        "(define check (lambda (i bad) (if (= i 5000) bad (check (+ i 1) (if (= (map-has? m i) (i32.rem_s i 2)) bad (+ bad 1))))))",
        "#<closure>",
    );
    eval(&mut s, "(check 0 0)", "0");
    eval(&mut s, "(map-get m 4999)", "(some s32 14997)");
    eval(&mut s, "(car (map-keys m))", "1");
}

#[test]
fn equal_compares_structure() {
    let mut s = session();
    eval(
        &mut s,
        "(equal? (list 1 \"a\" (list 'b)) (list 1 \"a\" (list 'b)))",
        "1",
    );
    eval(&mut s, "(equal? 1 1s64)", "0");
    eval(
        &mut s,
        "(equal? (vector 1 (vector 2)) (vector 1 (vector 2)))",
        "1",
    );
    eval(&mut s, "(equal? (vector 1) (vector 2))", "0");
    eval(
        &mut s,
        "(equal? (hash-map 1 2 3 4) (hash-map 3 4 1 2))",
        "1",
    );
    eval(&mut s, "(equal? (hash-map 1 2) (hash-map 1 3))", "0");
    eval(&mut s, "(equal? (some s32 1) (some s32 1))", "1");
    eval(&mut s, "(define v (vector 0))", "#<vector (0)>");
    eval(
        &mut s,
        "(begin (vector-set! v 0 v) (equal? v v))",
        "error: equal?: nesting deeper than 4096 (a container that holds itself?)",
    );
}

#[test]
fn containers_marshal_as_lists() {
    let mut s = session();
    eval(
        &mut s,
        "(equal? (marshal (vector 1 \"a\")) (marshal (list 1 \"a\")))",
        "1",
    );
    eval(
        &mut s,
        "(equal? (marshal (hash-map \"k\" 2)) (marshal (list (list \"k\" 2))))",
        "1",
    );
    eval(&mut s, "(> (string-index (help) \"vectors:\") 0)", "1");
}
