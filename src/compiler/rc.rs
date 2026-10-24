// Reference counting (docs/proposals/MEMORY.md §3, §7 stage 4: correct before fast).
//
// With `EmitOptions::rc` (or GRANITE_RC=1), heap objects are counted. The `rc` word
// of the block header (heap.rs) is the count; `$__dup` adds one and `$__drop_T`
// takes one away, freeing the object and dropping its children at zero.
//
// Ownership is decided in codegen, which already knows the type of every
// expression. An expression of a counted type leaves an OWNED reference: one the
// code that receives it must store, pass on, return, or drop.
//
//   - A variable's last use on a path moves its reference (backward liveness, the
//     `Liveness` pass below); any earlier use dups it.
//   - A binding that dies on a path without being used there is dropped at the
//     start of that path: after its `let` or parameter, or at the start of the
//     `if` branch or `match` arm where it dies. A drop therefore never follows a
//     tail call, and tail calls stay `return_call`.
//   - Every primitive that reads a counted operand consumes it (drops it after
//     reading). One that hands out a child of its operand (list-get, a record field,
//     a match binding) dups the child first.
//   - Every parameter is owned: the caller passes a reference it gives up.
//   - `global.get` dups; `global.set` stores, drops the old value, and returns a dup.
//   - `list-push` writes in place only when the list's count is 1; otherwise it
//     copies the list (dupping the elements) and pushes into the copy.
//
// Static objects (string literals, below the heap) have no count: `$__dup` and the
// drops skip any pointer below the heap, which also skips null.
//
// In debug builds (`--debug-heap`), dup and drop trap on a freed (poisoned) block
// and on a raw block, and `__granite_heap_verify` checks every count against the
// references reachable from the globals (`emit_verifier`).

use super::*;
use std::cell::RefCell;
use std::collections::{BTreeMap, HashSet};

pub(crate) fn rc_from_env() -> bool {
    std::env::var_os("GRANITE_RC").is_some_and(|v| v != "0" && !v.is_empty())
}

/// `rc` of a raw block (`heap-alloc`, `__pack_alloc`) in debug builds with RC, so
/// the verifier's census can tell raw blocks from leaked objects.
pub(crate) const RAW_RC: u32 = 0xABAB_ABAB;
/// A bit of `rc` the verifier sets on each object it has reached.
const MARK: u32 = 0x4000_0000;

/// Whether values of `ty` are heap objects with a count.
pub(crate) fn is_counted(ty: &Type) -> bool {
    match ty {
        Type::Str
        | Type::Any
        | Type::List(_)
        | Type::Record(_)
        | Type::Variant(_)
        | Type::Option(_)
        | Type::Result(_, _) => true,
        Type::Tuple(elems) => !elems.is_empty(),
        _ => false,
    }
}

/// A WAT-identifier-safe name for a counted type.
fn mangle(ty: &Type) -> String {
    match ty {
        Type::S32 => "a".into(),
        Type::S64 => "b".into(),
        Type::F32 => "c".into(),
        Type::F64 => "d".into(),
        Type::U8 => "e".into(),
        Type::U16 => "f".into(),
        Type::U32 => "g".into(),
        Type::U64 => "h".into(),
        Type::Bool => "k".into(),
        Type::Resource(_) | Type::Borrow(_) => "r".into(),
        Type::Str => "S".into(),
        Type::Any => "A".into(),
        Type::List(t) => format!("L{}", mangle(t)),
        Type::Option(t) => format!("O{}", mangle(t)),
        Type::Result(a, b) => format!("E{}{}", mangle(a), mangle(b)),
        Type::Tuple(ts) => {
            let mut s = format!("T{}", ts.len());
            for t in ts {
                s.push_str(&mangle(t));
            }
            s
        }
        Type::Record(n) => format!("R{}:{}", n.len(), n),
        Type::Variant(n) => format!("V{}:{}", n.len(), n),
    }
}

// ---------------------------------------------------------------------------
// Liveness

/// Per function: which `Var` nodes are a last use, and which names are live at
/// the start of each scope or branch body.
#[derive(Default)]
struct Liveness {
    last: HashSet<*const Expr>,
    /// Names live at the start of: a function body, a `let` body, an `if` branch,
    /// a `match` arm body, a `with-cap` body. A scope's own names are included.
    live_in: HashMap<*const Expr, HashSet<String>>,
    /// Names live after each `match` (for analysing arms synthesized from `_`).
    match_after: HashMap<*const Expr, HashSet<String>>,
}

impl Liveness {
    /// The names read at or after `e`, given `after`, the names read after it.
    /// Each name stands for its innermost binding.
    fn live_before(&mut self, e: &Expr, after: &HashSet<String>) -> HashSet<String> {
        match e {
            Expr::Var(name) => {
                if !after.contains(name) {
                    self.last.insert(e as *const Expr);
                }
                let mut live = after.clone();
                live.insert(name.clone());
                live
            }
            Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let mut live = self.body(then_branch, &[], after);
                live.extend(self.body(else_branch, &[], after));
                self.live_before(cond, &live)
            }
            Expr::Let {
                name, value, body, ..
            } => {
                let live = self.body(body, std::slice::from_ref(name), after);
                self.live_before(value, &live)
            }
            Expr::WithCap { name, body, .. } => self.body(body, std::slice::from_ref(name), after),
            Expr::Match { expr, cases } => {
                self.match_after.insert(e as *const Expr, after.clone());
                let mut live = HashSet::new();
                for arm in cases {
                    live.extend(self.body(&arm.body, &arm.bindings, after));
                }
                self.live_before(expr, &live)
            }
            _ => {
                let mut live = after.clone();
                for child in expr_children(e).into_iter().rev() {
                    live = self.live_before(child, &live);
                }
                live
            }
        }
    }

    /// `body` in a scope that binds `names`. Records the live set at the body's
    /// start; returns the names live before the scope (its own names removed, the
    /// shadowed ones live exactly when `after` says so).
    fn body(&mut self, body: &Expr, names: &[String], after: &HashSet<String>) -> HashSet<String> {
        let mut inner = after.clone();
        for n in names {
            inner.remove(n);
        }
        let mut live = self.live_before(body, &inner);
        self.live_in.insert(body as *const Expr, live.clone());
        for n in names {
            live.remove(n);
            if after.contains(n) {
                live.insert(n.clone());
            }
        }
        live
    }
}

// ---------------------------------------------------------------------------
// Per-module state

struct RcState {
    debug: bool,
    /// The function being emitted; then overlays for match arms synthesized from a
    /// `_` wildcard (codegen clones those, so they are analysed when emitted).
    liveness: Vec<Liveness>,
    /// Drop functions to emit, by mangled name.
    drops: BTreeMap<String, Type>,
    /// Counted operands of `string-addr`/`any-addr` kept alive to the end of their
    /// scope (§3.6), innermost scope last.
    addr_frames: Vec<Vec<(u32, Type)>>,
}

thread_local! {
    static RC: RefCell<Option<RcState>> = const { RefCell::new(None) };
}

fn with_rc<R>(f: impl FnOnce(&mut RcState) -> R) -> Option<R> {
    RC.with(|r| r.borrow_mut().as_mut().map(f))
}

/// RC is on for the module being generated while this lives.
pub(crate) struct RcModuleScope;

impl RcModuleScope {
    pub(crate) fn enter(debug: bool) -> Self {
        RC.with(|r| {
            *r.borrow_mut() = Some(RcState {
                debug,
                liveness: Vec::new(),
                drops: BTreeMap::new(),
                addr_frames: Vec::new(),
            })
        });
        RcModuleScope
    }
}

impl Drop for RcModuleScope {
    fn drop(&mut self) {
        RC.with(|r| *r.borrow_mut() = None);
    }
}

pub(crate) fn rc_on() -> bool {
    RC.with(|r| r.borrow().is_some())
}

pub(crate) fn rc_debug() -> bool {
    with_rc(|s| s.debug).unwrap_or(false)
}

/// Analyse a function body before emitting it.
pub(crate) fn rc_enter_function(body: &Expr) {
    with_rc(|s| {
        let mut l = Liveness::default();
        let live = l.live_before(body, &HashSet::new());
        l.live_in.insert(body as *const Expr, live);
        s.liveness = vec![l];
        s.addr_frames = vec![Vec::new()];
    });
}

/// Analyse match arms that codegen synthesized (cloned) for a `_` wildcard, in
/// the context of `match_expr`. Pop with `rc_pop_overlay` after the match.
pub(crate) fn rc_push_overlay(match_expr: &Expr, arms: &[&MatchArm]) {
    with_rc(|s| {
        let after = s
            .liveness
            .iter()
            .rev()
            .find_map(|l| l.match_after.get(&(match_expr as *const Expr)).cloned())
            .unwrap_or_default();
        let mut l = Liveness::default();
        for arm in arms {
            l.body(&arm.body, &arm.bindings, &after);
        }
        s.liveness.push(l);
    });
}

pub(crate) fn rc_pop_overlay() {
    with_rc(|s| s.liveness.pop());
}

/// Whether this `Var` node is the last use of its binding on its path.
pub(crate) fn rc_is_last(e: &Expr) -> bool {
    with_rc(|s| {
        s.liveness
            .iter()
            .any(|l| l.last.contains(&(e as *const Expr)))
    })
    .unwrap_or(false)
}

/// The names live at the start of a scope or branch body.
pub(crate) fn rc_live_in(body: &Expr) -> HashSet<String> {
    with_rc(|s| {
        s.liveness
            .iter()
            .rev()
            .find_map(|l| l.live_in.get(&(body as *const Expr)).cloned())
    })
    .flatten()
    .unwrap_or_default()
}

// ---------------------------------------------------------------------------
// Emitting dup and drop

/// Dup the reference on top of the stack (it stays there).
pub(crate) fn emit_dup(out: &mut String, pad: &str) {
    out.push_str(&format!("{pad}call $__dup\n"));
}

/// The drop function for `ty`, requested for emission.
pub(crate) fn drop_fn(ty: &Type) -> String {
    let name = format!("$__drop_{}", mangle(ty));
    with_rc(|s| s.drops.insert(name.clone(), ty.clone()));
    name
}

/// Drop the reference of type `ty` on top of the stack.
pub(crate) fn emit_drop(out: &mut String, pad: &str, ty: &Type) {
    out.push_str(&format!("{pad}call {}\n", drop_fn(ty)));
}

pub(crate) fn emit_drop_local(out: &mut String, pad: &str, local: u32, ty: &Type) {
    out.push_str(&format!("{pad}local.get {local}\n"));
    emit_drop(out, pad, ty);
}

// ---------------------------------------------------------------------------
// Addresses of counted objects (§3.6)

/// Whether `e` contains a `string-addr` or `any-addr` outside any nested `let`
/// (a nested `let` keeps its own operands alive).
pub(crate) fn has_direct_addr(e: &Expr) -> bool {
    match e {
        Expr::StringAddr { .. } | Expr::AnyAddr { .. } => true,
        Expr::Let { .. } => false,
        Expr::Match { expr, cases } => {
            has_direct_addr(expr) || cases.iter().any(|a| has_direct_addr(&a.body))
        }
        _ => expr_children(e).into_iter().any(has_direct_addr),
    }
}

pub(crate) fn rc_push_addr_frame() {
    with_rc(|s| s.addr_frames.push(Vec::new()));
}

/// Keep the owned reference in `local` alive to the end of the innermost scope.
pub(crate) fn rc_keep_alive(local: u32, ty: Type) {
    with_rc(|s| {
        if let Some(frame) = s.addr_frames.last_mut() {
            frame.push((local, ty));
        }
    });
}

/// End the innermost scope: drop what it kept alive.
pub(crate) fn rc_pop_addr_frame(out: &mut String, pad: &str) {
    let kept = with_rc(|s| s.addr_frames.pop())
        .flatten()
        .unwrap_or_default();
    for (local, ty) in kept {
        emit_drop_local(out, pad, local, &ty);
    }
}

// ---------------------------------------------------------------------------
// The runtime: $__dup, the drop functions, the verifier

/// One case of an object's layout: its tag (`None` for types without cases), and
/// the (offset, type) of each slot that holds a counted reference.
type CaseSlots = (Option<u32>, Vec<(usize, Type)>);

/// The counted child slots of an object of type `ty`, per case.
fn counted_slots(
    ty: &Type,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
) -> Vec<CaseSlots> {
    let packed = |types: &[Type], start: usize| {
        let mut slots = Vec::new();
        let mut off = start;
        for t in types {
            if is_counted(t) {
                slots.push((off, t.clone()));
            }
            off += type_size(t);
        }
        slots
    };
    match ty {
        Type::Record(n) => {
            let def = &records[n];
            let types: Vec<Type> = def.fields.iter().map(|f| f.ty.clone()).collect();
            vec![(None, packed(&types, 0))]
        }
        Type::Tuple(ts) => vec![(None, packed(ts, 0))],
        Type::Variant(n) => variants[n]
            .cases
            .iter()
            .enumerate()
            .map(|(i, c)| (Some(i as u32), packed(&c.payload, 4)))
            .collect(),
        Type::Option(t) => vec![(Some(1), packed(std::slice::from_ref(t), 4))],
        Type::Result(a, b) => vec![
            (Some(0), packed(std::slice::from_ref(a), 4)),
            (Some(1), packed(std::slice::from_ref(b), 4)),
        ],
        _ => vec![],
    }
}

fn rc_checks(debug: bool, header: &str) -> String {
    if !debug {
        return String::new();
    }
    format!(
        r#"    local.get {header}
    i32.load
    i32.const {POISON_RC}
    i32.eq
    if
      call $__granite_use_after_free
    end
    local.get {header}
    i32.load
    i32.const {RAW_RC}
    i32.eq
    if
      call $__granite_rc_on_raw_block
    end
"#
    )
}

const POISON_RC: u32 = 0xDDDD_DDDD;

/// Emit `$__dup`, every requested drop function (and the ones they need), and in
/// debug builds the verifier. `heap_payload` is the lowest payload address of a
/// heap block: anything below is static or null.
pub(crate) fn emit_rc_runtime(
    out: &mut String,
    heap_payload: u32,
    globals: &[Global],
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
) {
    let Some((debug, mut pending)) = with_rc(|s| (s.debug, s.drops.clone())) else {
        return;
    };
    for name in [
        "__granite_use_after_free",
        "__granite_rc_on_raw_block",
        "__granite_rc_underflow",
    ] {
        out.push_str(&format!("  (func ${name} unreachable)\n"));
    }
    out.push_str(&format!(
        r#"  (func $__dup (param $p i32) (result i32)
    (local $h i32)
    local.get $p
    i32.const {heap_payload}
    i32.ge_u
    if
      local.get $p
      i32.const 8
      i32.sub
      local.set $h
{checks}      local.get $h
      local.get $h
      i32.load
      i32.const 1
      i32.add
      i32.store
    end
    local.get $p
  )
"#,
        checks = rc_checks(debug, "$h"),
    ));

    // Drop functions, emitting each once; a drop may request more.
    let mut done: HashSet<String> = HashSet::new();
    while let Some((name, ty)) = pending.pop_first() {
        if !done.insert(name.clone()) {
            continue;
        }
        emit_drop_fn(
            out,
            &name,
            &ty,
            heap_payload,
            debug,
            records,
            variants,
            &mut pending,
        );
    }

    if debug {
        emit_verifier(out, heap_payload, globals, records, variants);
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_drop_fn(
    out: &mut String,
    name: &str,
    ty: &Type,
    heap_payload: u32,
    debug: bool,
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
    pending: &mut BTreeMap<String, Type>,
) {
    let mut need = |t: &Type| {
        let n = format!("$__drop_{}", mangle(t));
        pending.insert(n.clone(), t.clone());
        n
    };
    let underflow = if debug {
        r#"    local.get $h
    i32.load
    i32.eqz
    if
      call $__granite_rc_underflow
    end
"#
    } else {
        ""
    };
    let mut body = String::new();
    match ty {
        Type::List(elem) => {
            // Elements, then the data buffer, then the header.
            if is_counted(elem) {
                let drop_elem = need(elem);
                let stride = type_size(elem);
                body.push_str(&format!(
                    r#"    local.get $p
    i32.load offset=8
    local.set $c0
    local.get $p
    i32.load
    local.set $c1
    block $done
      loop $next
        local.get $c1
        i32.eqz
        br_if $done
        local.get $c0
        i32.load
        call {drop_elem}
        local.get $c0
        i32.const {stride}
        i32.add
        local.set $c0
        local.get $c1
        i32.const 1
        i32.sub
        local.set $c1
        br $next
      end
    end
"#
                ));
            }
            body.push_str(
                r#"    local.get $p
    i32.load offset=8
    call $__free
    local.get $p
    call $__free
"#,
            );
        }
        Type::Str | Type::Any => body.push_str("    local.get $p\n    call $__free\n"),
        _ => {
            // Read the counted children of the object's case, free the object, then
            // drop the children; the last one as a tail call, so a long chain drops
            // in constant stack.
            for (tag, slots) in counted_slots(ty, records, variants) {
                if slots.is_empty() {
                    continue;
                }
                let mut case = String::new();
                for (i, (off, _)) in slots.iter().enumerate() {
                    case.push_str(&format!(
                        "    local.get $p\n    i32.load offset={off}\n    local.set $c{i}\n"
                    ));
                }
                case.push_str("    local.get $p\n    call $__free\n");
                for (i, (_, t)) in slots.iter().enumerate() {
                    let f = need(t);
                    let call = if i + 1 == slots.len() {
                        "return_call"
                    } else {
                        "call"
                    };
                    case.push_str(&format!("    local.get $c{i}\n    {call} {f}\n"));
                }
                match tag {
                    None => body.push_str(&case),
                    Some(tag) => body.push_str(&format!(
                        "    local.get $p\n    i32.load\n    i32.const {tag}\n    i32.eq\n    if\n{case}    end\n"
                    )),
                }
            }
            body.push_str("    local.get $p\n    call $__free\n");
        }
    }
    let max_slots = counted_slots(ty, records, variants)
        .iter()
        .map(|(_, s)| s.len())
        .max()
        .unwrap_or(0)
        .max(2);
    let locals: String = (0..max_slots)
        .map(|i| format!(" (local $c{i} i32)"))
        .collect();
    out.push_str(&format!(
        r#"  (func {name} (param $p i32) (local $h i32){locals}
    local.get $p
    i32.const {heap_payload}
    i32.lt_u
    if
      return
    end
    local.get $p
    i32.const 8
    i32.sub
    local.set $h
{checks}{underflow}    local.get $h
    local.get $h
    i32.load
    i32.const 1
    i32.sub
    i32.store
    local.get $h
    i32.load
    if
      return
    end
{body}  )
"#,
        checks = rc_checks(debug, "$h"),
    ));
}

// ---------------------------------------------------------------------------
// The verifier (§5.5)

/// `__granite_heap_verify` (debug builds): checks that every counted object's
/// `rc` is exactly the number of references to it reachable from the globals, and
/// that every live block is either reachable or raw (no leaks).
///
/// Pass 1 walks from each global, taking one off the count of every object at
/// each reference and marking it the first time (its children are walked once).
/// Then every block from the heap start to the frontier is checked: a free block
/// (poisoned) or a raw block is skipped; any other block must be marked with a
/// count of exactly 0. Pass 2 walks again to put the counts back.
fn emit_verifier(
    out: &mut String,
    heap_payload: u32,
    globals: &[Global],
    records: &HashMap<String, RecordDef>,
    variants: &HashMap<String, VariantDef>,
) {
    for name in [
        "__granite_verify_dangling",
        "__granite_verify_count_low",
        "__granite_verify_count_high",
        "__granite_verify_leak",
    ] {
        out.push_str(&format!("  (func ${name} unreachable)\n"));
    }
    out.push_str("  (global $__verify_pass (mut i32) (i32.const 1))\n");
    // $__verify_visit(p) -> 1 when the walk must go on into p's children.
    out.push_str(&format!(
        r#"  (func $__verify_visit (param $p i32) (result i32)
    (local $h i32)
    (local $rc i32)
    local.get $p
    i32.const {heap_payload}
    i32.lt_u
    if
      i32.const 0
      return
    end
    local.get $p
    i32.const 8
    i32.sub
    local.tee $h
    i32.load
    local.tee $rc
    i32.const {POISON_RC}
    i32.eq
    local.get $rc
    i32.const {RAW_RC}
    i32.eq
    i32.or
    if
      call $__granite_verify_dangling
    end
    global.get $__verify_pass
    i32.const 1
    i32.eq
    if (result i32)
      local.get $rc
      i32.const {count_mask}
      i32.and
      i32.eqz
      if
        call $__granite_verify_count_low
      end
      local.get $h
      local.get $rc
      i32.const 1
      i32.sub
      i32.const {MARK}
      i32.or
      i32.store
      local.get $rc
      i32.const {MARK}
      i32.and
      i32.eqz
    else
      local.get $h
      local.get $rc
      i32.const 1
      i32.add
      i32.const {count_mask}
      i32.and
      i32.store
      local.get $rc
      i32.const {MARK}
      i32.and
      i32.const 0
      i32.ne
    end
  )
"#,
        count_mask = MARK - 1,
    ));

    // One walk function per type reachable from a global.
    let mut pending: BTreeMap<String, Type> = BTreeMap::new();
    let walk_name = |t: &Type| format!("$__walk_{}", mangle(t));
    for g in globals {
        if is_counted(&g.ty) {
            pending.insert(walk_name(&g.ty), g.ty.clone());
        }
    }
    let mut done: HashSet<String> = HashSet::new();
    while let Some((name, ty)) = pending.pop_first() {
        if !done.insert(name.clone()) {
            continue;
        }
        let mut body = String::new();
        match &ty {
            Type::List(elem) => {
                // The data buffer is one more object the header owns.
                body.push_str(
                    "    local.get $p\n    i32.load offset=8\n    call $__verify_visit\n    drop\n",
                );
                if is_counted(elem) {
                    let w = walk_name(elem);
                    pending.insert(w.clone(), (**elem).clone());
                    body.push_str(&format!(
                        r#"    local.get $p
    i32.load offset=8
    local.set $c0
    local.get $p
    i32.load
    local.set $c1
    block $done
      loop $next
        local.get $c1
        i32.eqz
        br_if $done
        local.get $c0
        i32.load
        call {w}
        local.get $c0
        i32.const {stride}
        i32.add
        local.set $c0
        local.get $c1
        i32.const 1
        i32.sub
        local.set $c1
        br $next
      end
    end
"#,
                        stride = type_size(elem),
                    ));
                }
            }
            _ => {
                for (tag, slots) in counted_slots(&ty, records, variants) {
                    let mut case = String::new();
                    for (off, t) in &slots {
                        let w = walk_name(t);
                        pending.insert(w.clone(), t.clone());
                        case.push_str(&format!(
                            "    local.get $p\n    i32.load offset={off}\n    call {w}\n"
                        ));
                    }
                    match tag {
                        None => body.push_str(&case),
                        Some(tag) if !slots.is_empty() => body.push_str(&format!(
                            "    local.get $p\n    i32.load\n    i32.const {tag}\n    i32.eq\n    if\n{case}    end\n"
                        )),
                        Some(_) => {}
                    }
                }
            }
        }
        out.push_str(&format!(
            r#"  (func {name} (param $p i32) (local $c0 i32) (local $c1 i32)
    local.get $p
    call $__verify_visit
    i32.eqz
    if
      return
    end
{body}  )
"#
        ));
    }

    let mut walks = String::new();
    for g in globals {
        if is_counted(&g.ty) {
            walks.push_str(&format!(
                "    global.get {}\n    call {}\n",
                g.name,
                walk_name(&g.ty)
            ));
        }
    }
    out.push_str(&format!(
        r#"  (func $__heap_verify (export "__granite_heap_verify") (result i32)
    (local $b i32)
    (local $rc i32)
    i32.const 1
    global.set $__verify_pass
{walks}    ;; census: every block that is neither free nor raw was reached, and its
    ;; count is now exactly 0
    i32.const {heap_start}
    local.set $b
    block $done
      loop $next
        local.get $b
        global.get $__heap_ptr
        i32.ge_u
        br_if $done
        local.get $b
        i32.load
        local.tee $rc
        i32.const {POISON_RC}
        i32.ne
        local.get $rc
        i32.const {RAW_RC}
        i32.ne
        i32.and
        if
          local.get $rc
          i32.const {MARK}
          i32.and
          i32.eqz
          if
            call $__granite_verify_leak
          end
          local.get $rc
          i32.const {count_mask}
          i32.and
          if
            call $__granite_verify_count_high
          end
        end
        local.get $b
        local.get $b
        i32.load offset=4
        i32.add
        local.set $b
        br $next
      end
    end
    i32.const 2
    global.set $__verify_pass
{walks}    i32.const 0
  )
"#,
        heap_start = heap_payload - BLOCK_HEADER,
        count_mask = MARK - 1,
    ));
}
