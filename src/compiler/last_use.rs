// Value-semantics `list-push` (docs/proposals/MEMORY.md §2.4, §7 stage 3).
//
// With `EmitOptions::value_lists` (or GRANITE_VALUE_LISTS=1), `list-push` leaves
// its argument unchanged: it copies the list, then pushes into the copy. It writes
// in place only when nothing can see the old list afterwards, which without
// reference counts is a syntactic guess. The list operand must be
//   - a fresh list: `list-new`, or the result of another `list-push`; or
//   - a local's last use (no later read on any path) that has not escaped: its
//     pointer was not stored, passed or rebound earlier on the path. Only being
//     the list operand of list-len, list-get or list-push does not escape it. A
//     `let` binding starts escaped unless its value is a fresh list, and a `match`
//     binding is always escaped (the matched value still holds it). A parameter
//     starts unescaped: the caller is assumed to hand the list over.
// The guess cannot see a caller that keeps using a list it passed. Stage 4's
// uniqueness test (the reference count) replaces it, and until then the flag stays
// off by default: every other push copies the whole list, so pushes onto a global
// go back to O(n) each.

use super::*;
use std::cell::RefCell;
use std::collections::HashSet;

pub(crate) fn value_lists_from_env() -> bool {
    std::env::var_os("GRANITE_VALUE_LISTS").is_some_and(|v| v != "0" && !v.is_empty())
}

thread_local! {
    /// The `list-push` list operands (`Var` nodes) of the function being emitted
    /// that may be written in place. `None` when value semantics are off: every
    /// push is in place.
    static IN_PLACE: RefCell<Option<HashSet<*const Expr>>> = const { RefCell::new(None) };
}

/// Decide the in-place pushes of one function's body while it is emitted.
pub(crate) struct LastUseScope;

impl LastUseScope {
    pub(crate) fn enter(body: &Expr) -> Self {
        let last = last_uses(body);
        let mut unescaped = HashSet::new();
        escapes(body, &mut HashSet::new(), &mut unescaped);
        let in_place = last.intersection(&unescaped).copied().collect();
        IN_PLACE.with(|l| *l.borrow_mut() = Some(in_place));
        LastUseScope
    }
}

impl Drop for LastUseScope {
    fn drop(&mut self) {
        IN_PLACE.with(|l| *l.borrow_mut() = None);
    }
}

/// Whether `list-push` may write into its list operand in place.
pub(crate) fn push_in_place(list: &Expr) -> bool {
    IN_PLACE.with(|l| match &*l.borrow() {
        None => true,
        Some(in_place) => match list {
            Expr::ListNew { .. } | Expr::ListPush { .. } => true,
            Expr::Var(_) => in_place.contains(&(list as *const Expr)),
            _ => false,
        },
    })
}

/// The `Var` nodes in `body` after which their binding is never read again, on
/// any path. A backward liveness pass over the tree in evaluation order.
pub(crate) fn last_uses(body: &Expr) -> HashSet<*const Expr> {
    let mut out = HashSet::new();
    live_before(body, &HashSet::new(), &mut out);
    out
}

/// The names read at or after `e`, given `after`, the names read after it. Each
/// name stands for its innermost binding; a scope removes its own names.
fn live_before(
    e: &Expr,
    after: &HashSet<String>,
    out: &mut HashSet<*const Expr>,
) -> HashSet<String> {
    match e {
        Expr::Var(name) => {
            if !after.contains(name) {
                out.insert(e as *const Expr);
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
            let mut live = live_before(then_branch, after, out);
            live.extend(live_before(else_branch, after, out));
            live_before(cond, &live, out)
        }
        Expr::Let {
            name, value, body, ..
        } => {
            let live = scoped(body, std::slice::from_ref(name), after, out);
            live_before(value, &live, out)
        }
        Expr::WithCap { name, body, .. } => scoped(body, std::slice::from_ref(name), after, out),
        Expr::Match { expr, cases } => {
            let mut live = HashSet::new();
            for arm in cases {
                live.extend(scoped(&arm.body, &arm.bindings, after, out));
            }
            live_before(expr, &live, out)
        }
        _ => {
            let mut live = after.clone();
            for child in expr_children(e).into_iter().rev() {
                live = live_before(child, &live, out);
            }
            live
        }
    }
}

/// `body` in a scope that binds `names`: inside it they are new bindings, dead
/// after the body; outside, the shadowed bindings are live exactly when `after`
/// says so.
fn scoped(
    body: &Expr,
    names: &[String],
    after: &HashSet<String>,
    out: &mut HashSet<*const Expr>,
) -> HashSet<String> {
    let mut inner = after.clone();
    for n in names {
        inner.remove(n);
    }
    let mut live = live_before(body, &inner, out);
    for n in names {
        live.remove(n);
        if after.contains(n) {
            live.insert(n.clone());
        }
    }
    live
}

fn fresh_list(e: &Expr) -> bool {
    matches!(e, Expr::ListNew { .. } | Expr::ListPush { .. })
}

/// Forward pass in evaluation order. `escaped` holds the names whose current
/// binding may be aliased; `unescaped` collects the `list-push` operands that are a
/// binding not yet escaped when the push reads it.
fn escapes(e: &Expr, escaped: &mut HashSet<String>, unescaped: &mut HashSet<*const Expr>) {
    match e {
        Expr::Var(name) => {
            escaped.insert(name.clone());
        }
        Expr::ListLen { list } => {
            if !matches!(list.as_ref(), Expr::Var(_)) {
                escapes(list, escaped, unescaped);
            }
        }
        Expr::ListGet { list, index } => {
            if !matches!(list.as_ref(), Expr::Var(_)) {
                escapes(list, escaped, unescaped);
            }
            escapes(index, escaped, unescaped);
        }
        Expr::ListPush { list, value } => {
            match list.as_ref() {
                Expr::Var(name) => {
                    if !escaped.contains(name) {
                        unescaped.insert(list.as_ref() as *const Expr);
                    }
                    // The push holds the list while `value` runs, so a push in
                    // `value` must not write it in place.
                    escaped.insert(name.clone());
                }
                other => escapes(other, escaped, unescaped),
            }
            escapes(value, escaped, unescaped);
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            escapes(cond, escaped, unescaped);
            let mut other = escaped.clone();
            escapes(then_branch, escaped, unescaped);
            escapes(else_branch, &mut other, unescaped);
            escaped.extend(other);
        }
        Expr::Let {
            name, value, body, ..
        } => {
            escapes(value, escaped, unescaped);
            let bound = [(name.clone(), !fresh_list(value))];
            escapes_scoped(body, &bound, escaped, unescaped);
        }
        Expr::WithCap { name, body, .. } => {
            escapes_scoped(body, &[(name.clone(), true)], escaped, unescaped);
        }
        Expr::Match { expr, cases } => {
            escapes(expr, escaped, unescaped);
            let before = escaped.clone();
            for arm in cases {
                let mut path = before.clone();
                let bound: Vec<_> = arm.bindings.iter().map(|b| (b.clone(), true)).collect();
                escapes_scoped(&arm.body, &bound, &mut path, unescaped);
                escaped.extend(path);
            }
        }
        _ => {
            for child in expr_children(e) {
                escapes(child, escaped, unescaped);
            }
        }
    }
}

/// `body` in a scope binding each name, escaped or not; afterwards the shadowed
/// bindings get their state back.
fn escapes_scoped(
    body: &Expr,
    bound: &[(String, bool)],
    escaped: &mut HashSet<String>,
    unescaped: &mut HashSet<*const Expr>,
) {
    let outer: Vec<bool> = bound.iter().map(|(n, _)| escaped.contains(n)).collect();
    for (n, esc) in bound {
        if *esc {
            escaped.insert(n.clone());
        } else {
            escaped.remove(n);
        }
    }
    escapes(body, escaped, unescaped);
    for ((n, _), was) in bound.iter().zip(outer) {
        if was {
            escaped.insert(n.clone());
        } else {
            escaped.remove(n);
        }
    }
}
