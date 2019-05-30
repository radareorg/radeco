use super::*;

#[test]
fn it_works() {
    let cstore = Storage::new();
    let cctx = cstore.cctx();
    let ab = mk_ab(cctx);
    println!("{:?}", ab);
}

fn mk_ab<'cd>(cctx: Context<'cd, &'static str>) -> Condition<'cd, &'static str> {
    let a = cctx.mk_var(cctx.new_var("a"));
    let b = cctx.mk_var(cctx.new_var("b"));
    let ab = cctx.mk_and(a, b);
    ab
}

#[test]
fn identity() {
    let cstore = Storage::new();
    let cctx = cstore.cctx();
    let a = cctx.mk_var(cctx.new_var("a"));
    let b = cctx.mk_var(cctx.new_var("b"));
    let t = cctx.mk_true();
    let f = cctx.mk_false();

    assert_eq!(cctx.mk_and(a, t), a);
    assert_eq!(cctx.mk_or(a, f), a);
    assert_eq!(cctx.mk_and_from_iter(vec![t, a, t, b]), cctx.mk_and(a, b));
    assert_eq!(cctx.mk_or_from_iter(vec![f, a, f, b]), cctx.mk_or(a, b));
}

#[test]
fn annihilation_ptr_eq() {
    use std::ptr;

    let cstore = Storage::new();
    let cctx = cstore.cctx();
    let a = cctx.mk_var(cctx.new_var("a"));
    let b = cctx.mk_var(cctx.new_var("b"));
    let t = cctx.mk_true();
    let f = cctx.mk_false();

    assert!(ptr::eq(cctx.mk_and(a, f).0, f.0));
    assert!(ptr::eq(cctx.mk_or(a, t).0, t.0));
    assert!(ptr::eq(cctx.mk_and_from_iter(vec![f, a, f, b]).0, f.0));
    assert!(ptr::eq(cctx.mk_or_from_iter(vec![t, a, t, b]).0, t.0));
    assert!(ptr::eq(cctx.mk_and_from_iter(vec![a, b, f]).0, f.0));
    assert!(ptr::eq(cctx.mk_or_from_iter(vec![a, b, t]).0, t.0));
}

#[test]
fn idempotence() {
    let cstore = Storage::new();
    let cctx = cstore.cctx();
    let a = cctx.mk_var(cctx.new_var("a"));
    let b = cctx.mk_var(cctx.new_var("b"));
    let a_and_b = cctx.mk_and(a, b);
    let a_or_b = cctx.mk_or(a, b);

    assert_eq!(cctx.mk_and(a, a), a);
    assert_eq!(cctx.mk_or(a, a), a);
    assert_eq!(cctx.mk_and(a_and_b, a), a_and_b);
    assert_eq!(cctx.mk_or(a_or_b, a), a_or_b);
    assert_eq!(cctx.mk_and(a_and_b, b), a_and_b);
    assert_eq!(cctx.mk_or(a_or_b, b), a_or_b);
    assert_eq!(cctx.mk_and_from_iter(vec![a, a, a, b, a, b]), a_and_b);
    assert_eq!(cctx.mk_or_from_iter(vec![a, a, a, b, a, b]), a_or_b);
}

#[test]
fn assoc_commut() {
    let cstore = Storage::new();
    let cctx = cstore.cctx();
    let a = cctx.mk_var(cctx.new_var("a"));
    let b = cctx.mk_var(cctx.new_var("b"));
    let c = cctx.mk_var(cctx.new_var("c"));
    let d = cctx.mk_var(cctx.new_var("d"));

    assert_eq!(cctx.mk_and(a, b), cctx.mk_and(b, a));
    assert_eq!(cctx.mk_or(a, b), cctx.mk_or(b, a));
    assert_eq!(cctx.mk_and_from_iter(vec![a, b]), cctx.mk_and(b, a));
    assert_eq!(cctx.mk_or_from_iter(vec![a, b]), cctx.mk_or(b, a));
    assert_eq!(
        cctx.mk_and(a, cctx.mk_and(b, c)),
        cctx.mk_and(cctx.mk_and(a, b), c),
    );
    assert_eq!(
        cctx.mk_or(a, cctx.mk_or(b, c)),
        cctx.mk_or(cctx.mk_or(a, b), c),
    );

    assert_eq!(
        cctx.mk_and_from_iter(vec![a, b, c]),
        cctx.mk_and_from_iter(vec![a, c, b]),
    );
    assert_eq!(
        cctx.mk_and_from_iter(vec![c, b, a]),
        cctx.mk_and_from_iter(vec![b, c, a]),
    );
    assert_eq!(
        cctx.mk_or_from_iter(vec![a, b, c]),
        cctx.mk_or_from_iter(vec![a, c, b]),
    );
    assert_eq!(
        cctx.mk_or_from_iter(vec![c, b, a]),
        cctx.mk_or_from_iter(vec![b, c, a]),
    );

    assert_eq!(
        cctx.mk_and_from_iter(vec![a, b, c, d]),
        cctx.mk_and(
            cctx.mk_and_from_iter(vec![a, c]),
            cctx.mk_and_from_iter(vec![b, d]),
        ),
    );
    assert_eq!(
        cctx.mk_or_from_iter(vec![a, b, c, d]),
        cctx.mk_or(
            cctx.mk_or_from_iter(vec![a, c]),
            cctx.mk_or_from_iter(vec![b, d]),
        ),
    );
    assert_eq!(
        cctx.mk_and_from_iter(vec![a, b, c, d]),
        cctx.mk_and_from_iter(vec![b, c, cctx.mk_and(d, a)]),
    );
    assert_eq!(
        cctx.mk_or_from_iter(vec![a, b, c, d]),
        cctx.mk_or_from_iter(vec![b, c, cctx.mk_or(d, a)]),
    );
}

#[test]
fn negate_tf_ptr_eq() {
    use std::ptr;

    let cstore = Storage::new();
    let cctx = cstore.cctx();
    let _ = cctx.mk_var(cctx.new_var(""));
    let t = cctx.mk_true();
    let f = cctx.mk_false();

    assert!(ptr::eq(cctx.mk_not(t).0, f.0));
    assert!(ptr::eq(cctx.mk_not(f).0, t.0));
}

#[test]
fn double_negation() {
    let cstore = Storage::new();
    let cctx = cstore.cctx();
    let a = cctx.mk_var(cctx.new_var("a"));

    assert_eq!(cctx.mk_not(cctx.mk_not(a)), a);
}

#[test]
fn demorgans() {
    let cstore = Storage::new();
    let cctx = cstore.cctx();
    let a = cctx.mk_var(cctx.new_var("a"));
    let b = cctx.mk_var(cctx.new_var("b"));
    let c = cctx.mk_var(cctx.new_var("c"));
    let d = cctx.mk_var(cctx.new_var("d"));
    let na = cctx.mk_not(a);
    let nb = cctx.mk_not(b);
    let nc = cctx.mk_not(c);
    let nd = cctx.mk_not(d);

    assert_eq!(cctx.mk_not(cctx.mk_and(a, b)), cctx.mk_or(na, nb));
    assert_eq!(cctx.mk_not(cctx.mk_or(a, b)), cctx.mk_and(na, nb));

    assert_eq!(
        cctx.mk_not(cctx.mk_and_from_iter(vec![a, b, c, d])),
        cctx.mk_or_from_iter(vec![na, nb, nc, nd]),
    );
    assert_eq!(
        cctx.mk_not(cctx.mk_or_from_iter(vec![a, b, c, d])),
        cctx.mk_and_from_iter(vec![na, nb, nc, nd]),
    );
    assert_eq!(
        cctx.mk_not(cctx.mk_and_from_iter(vec![na, nb, nc, nd])),
        cctx.mk_or_from_iter(vec![a, b, c, d]),
    );
    assert_eq!(
        cctx.mk_not(cctx.mk_or_from_iter(vec![na, nb, nc, nd])),
        cctx.mk_and_from_iter(vec![a, b, c, d]),
    );
}

#[test]
fn distributivity() {
    let cstore = Storage::new();
    let cctx = cstore.cctx();
    let a = cctx.mk_var(cctx.new_var("a"));
    let b = cctx.mk_var(cctx.new_var("b"));
    let c = cctx.mk_var(cctx.new_var("c"));
    let d = cctx.mk_var(cctx.new_var("d"));

    assert_eq!(
        cctx.mk_and(cctx.mk_or(a, b), cctx.mk_or(a, c)),
        cctx.mk_or(a, cctx.mk_and(b, c)),
    );
    assert_eq!(
        cctx.mk_or(cctx.mk_and(a, b), cctx.mk_and(a, c)),
        cctx.mk_and(a, cctx.mk_or(b, c)),
    );

    assert_eq!(
        cctx.mk_and_from_iter(vec![cctx.mk_or(a, b), d, cctx.mk_or(c, a)]),
        cctx.mk_and(cctx.mk_or(a, cctx.mk_and(b, c)), d),
    );
    assert_eq!(
        cctx.mk_or_from_iter(vec![cctx.mk_and(a, b), d, cctx.mk_and(c, a)]),
        cctx.mk_or(cctx.mk_and(a, cctx.mk_or(b, c)), d),
    );
}

#[test]
fn absorption() {
    let cstore = Storage::new();
    let cctx = cstore.cctx();
    let a = cctx.mk_var(cctx.new_var("a"));
    let b = cctx.mk_var(cctx.new_var("b"));
    let c = cctx.mk_var(cctx.new_var("c"));

    assert_eq!(cctx.mk_and(a, cctx.mk_or(a, b)), a);
    assert_eq!(cctx.mk_or(a, cctx.mk_and(a, b)), a);

    assert_eq!(
        cctx.mk_and_from_iter(vec![cctx.mk_or(a, b), c, a]),
        cctx.mk_and(a, c),
    );
    assert_eq!(
        cctx.mk_or_from_iter(vec![cctx.mk_and(a, b), c, a]),
        cctx.mk_or(a, c),
    );
}

#[test]
fn complementation() {
    let cstore = Storage::new();
    let cctx = cstore.cctx();
    let a = cctx.mk_var(cctx.new_var("a"));
    let na = cctx.mk_not(a);
    let b = cctx.mk_var(cctx.new_var("b"));

    assert_eq!(cctx.mk_and(a, na), cctx.mk_false());
    assert_eq!(cctx.mk_or(a, na), cctx.mk_true());
    assert_eq!(cctx.mk_and(na, a), cctx.mk_false());
    assert_eq!(cctx.mk_or(na, a), cctx.mk_true());

    assert_eq!(cctx.mk_and_from_iter(vec![a, b, na]), cctx.mk_false());
    assert_eq!(cctx.mk_or_from_iter(vec![a, b, na]), cctx.mk_true());
    assert_eq!(cctx.mk_and_from_iter(vec![a, na, b]), cctx.mk_false());
    assert_eq!(cctx.mk_or_from_iter(vec![a, na, b]), cctx.mk_true());
}

#[test]
fn cover() {
    let cstore = Storage::new();
    let cctx = cstore.cctx();
    let a = cctx.mk_var(cctx.new_var("a"));
    let na = cctx.mk_not(a);
    let b = cctx.mk_var(cctx.new_var("b"));

    assert_eq!(cctx.mk_and(a, cctx.mk_or(na, b)), cctx.mk_and(a, b));
    assert_eq!(cctx.mk_or(a, cctx.mk_and(na, b)), cctx.mk_or(a, b));
}

#[test]
fn big_complementation() {
    let cstore = Storage::new();
    let cctx = cstore.cctx();
    let a = cctx.mk_var(cctx.new_var("a"));
    let b = cctx.mk_var(cctx.new_var("b"));
    let c = cctx.mk_var(cctx.new_var("c"));
    let d = cctx.mk_var(cctx.new_var("d"));
    let e = cctx.mk_var(cctx.new_var("e"));

    let expr = cctx.mk_and(a, cctx.mk_or(b, cctx.mk_and(c, cctx.mk_or(d, e))));

    assert_eq!(cctx.mk_and(expr, cctx.mk_not(expr)), cctx.mk_false());
    assert_eq!(cctx.mk_or(expr, cctx.mk_not(expr)), cctx.mk_true());
}

#[test]
fn nested_complementation() {
    let cstore = Storage::new();
    let cctx = cstore.cctx();
    let a = cctx.mk_var(cctx.new_var("a"));
    let b = cctx.mk_var(cctx.new_var("b"));
    let c = cctx.mk_var(cctx.new_var("c"));
    let nc = cctx.mk_not(c);

    let a_or_b = cctx.mk_or(a, b);
    assert_eq!(
        cctx.mk_or(cctx.mk_and(a_or_b, c), cctx.mk_and(a_or_b, nc)),
        a_or_b
    );
    let a_and_b = cctx.mk_and(a, b);
    assert_eq!(
        cctx.mk_and(cctx.mk_or(a_and_b, c), cctx.mk_or(a_and_b, nc)),
        a_and_b
    );
}
