use super::ast::LoopType;
use super::condition;
use super::*;

#[derive(Default, Debug)]
struct StringAst {
    var_count: u64,
}

impl AstContext for StringAst {
    type Block = String;
    type Variable = String;
    type Condition = String;
}

impl AstContextMut for StringAst {
    fn mk_fresh_var(&mut self) -> String {
        let ret = format!("i_{}", self.var_count);
        self.var_count += 1;
        ret
    }

    fn mk_cond_equals(&mut self, var: &String, val: u64) -> String {
        format!("{} == {}", var, val)
    }

    fn mk_var_assign(&mut self, var: &String, val: u64) -> String {
        format!("{} = {}", var, val)
    }
}

#[test]
fn ast_nmg_example() {
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(cnode());
    let c1 = graph.add_node(cnode());
    let c2 = graph.add_node(cnode());
    let c3 = graph.add_node(cnode());
    let b1 = graph.add_node(cnode());
    let b2 = graph.add_node(cnode());
    let d1 = graph.add_node(cnode());
    let d2 = graph.add_node(cnode());
    let d3 = graph.add_node(cnode());
    let n1 = graph.add_node(node("n1"));
    let n2 = graph.add_node(node("n2"));
    let n3 = graph.add_node(node("n3"));
    let n4 = graph.add_node(node("n4"));
    let n5 = graph.add_node(node("n5"));
    let n6 = graph.add_node(node("n6"));
    let n7 = graph.add_node(node("n7"));
    let n8 = graph.add_node(node("n8"));
    let n9 = graph.add_node(node("n9"));
    let exit = graph.add_node(empty_node());

    #[allow(non_snake_case)]
    let c_A = cond_s(&cctx, "A");
    let c_c1 = cond_s(&cctx, "c1");
    let c_c2 = cond_s(&cctx, "c2");
    let c_c3 = cond_s(&cctx, "c3");
    let c_b1 = cond_s(&cctx, "b1");
    let c_b2 = cond_s(&cctx, "b2");
    let c_d1 = cond_s(&cctx, "d1");
    let c_d2 = cond_s(&cctx, "d2");
    let c_d3 = cond_s(&cctx, "d3");

    graph.add_edge(entry, c1, Some(c_A));
    graph.add_edge(entry, b1, neg_c(&cctx, c_A));
    graph.add_edge(n9, exit, None);
    // R1
    graph.add_edge(c1, n1, Some(c_c1));
    graph.add_edge(n1, c1, None);
    graph.add_edge(c1, c2, neg_c(&cctx, c_c1));
    graph.add_edge(c2, n2, Some(c_c2));
    graph.add_edge(n2, n9, None);
    graph.add_edge(c2, n3, neg_c(&cctx, c_c2));
    graph.add_edge(n3, c3, None);
    graph.add_edge(c3, c1, Some(c_c3));
    graph.add_edge(c3, n9, neg_c(&cctx, c_c3));
    // R2
    graph.add_edge(b1, b2, Some(c_b1));
    graph.add_edge(b2, n6, Some(c_b2));
    graph.add_edge(n6, n7, None);
    graph.add_edge(n7, d1, None);
    graph.add_edge(b2, n5, neg_c(&cctx, c_b2));
    graph.add_edge(n5, n7, None);
    graph.add_edge(b1, n4, neg_c(&cctx, c_b1));
    graph.add_edge(n4, n5, None);
    // R3
    graph.add_edge(d1, d3, Some(c_d1));
    graph.add_edge(d3, n8, Some(c_d3));
    graph.add_edge(n8, d1, None);
    graph.add_edge(d3, n9, neg_c(&cctx, c_d3));
    graph.add_edge(d1, d2, neg_c(&cctx, c_d1));
    graph.add_edge(d2, n8, Some(c_d2));
    graph.add_edge(d2, n9, neg_c(&cctx, c_d2));

    let actx = StringAst::default();
    let cfg = ControlFlowGraph {
        graph,
        entry,
        exit,
        cctx,
        actx,
    };
    let ast = cfg.structure_whole();
    println!("{:#?}", ast);

    use self::AstNodeC::*;
    assert_eq!(
        Seq(vec![
            Cond(
                c_A,
                Box::new(Loop(
                    LoopType::PostChecked(c_c3),
                    Box::new(Seq(vec![
                        Loop(
                            LoopType::PreChecked(c_c1),
                            Box::new(BasicBlock("n1".to_owned())),
                        ),
                        Cond(
                            c_c2,
                            Box::new(Seq(vec![BasicBlock("n2".to_owned()), Break])),
                            None,
                        ),
                        BasicBlock("n3".to_owned()),
                    ])),
                )),
                Some(Box::new(Seq(vec![
                    Cond(
                        cctx.mk_not(c_b1),
                        Box::new(BasicBlock("n4".to_owned())),
                        None,
                    ),
                    Cond(
                        cctx.mk_and(c_b1, c_b2),
                        Box::new(BasicBlock("n6".to_owned())),
                        Some(Box::new(BasicBlock("n5".to_owned()))),
                    ),
                    BasicBlock("n7".to_owned()),
                    Loop(
                        LoopType::PreChecked(
                            cctx.mk_or(cctx.mk_and(c_d1, c_d3), cctx.mk_and(cctx.mk_not(c_d1), c_d2)),
                        ),
                        Box::new(BasicBlock("n8".to_owned())),
                    ),
                ]))),
            ),
            BasicBlock("n9".to_owned()),
        ]),
        ast
    );
}

#[test]
fn ast_nmg_r1() {
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(empty_node());
    let c1 = graph.add_node(cnode());
    let c2 = graph.add_node(cnode());
    let c3 = graph.add_node(cnode());
    let n1 = graph.add_node(node("n1"));
    let n2 = graph.add_node(node("n2"));
    let n3 = graph.add_node(node("n3"));
    let exit = graph.add_node(empty_node());

    let c_c1 = cond_s(&cctx, "c1");
    let c_c2 = cond_s(&cctx, "c2");
    let c_c3 = cond_s(&cctx, "c3");
    let nc_c1 = cctx.mk_not(c_c1);
    let nc_c2 = cctx.mk_not(c_c2);
    let nc_c3 = cctx.mk_not(c_c3);

    graph.add_edge(entry, c1, None);
    graph.add_edge(c1, n1, Some(c_c1));
    graph.add_edge(n1, c1, None);
    graph.add_edge(c1, c2, Some(nc_c1));
    graph.add_edge(c2, n2, Some(c_c2));
    graph.add_edge(n2, exit, None);
    graph.add_edge(c2, n3, Some(nc_c2));
    graph.add_edge(n3, c3, None);
    graph.add_edge(c3, c1, Some(c_c3));
    graph.add_edge(c3, exit, Some(nc_c3));

    let actx = StringAst::default();
    let cfg = ControlFlowGraph {
        graph,
        entry,
        exit,
        cctx,
        actx,
    };
    let ast = cfg.structure_whole();
    println!("{:#?}", ast);

    use self::AstNodeC::*;
    assert_eq!(
        Loop(
            LoopType::PostChecked(c_c3),
            Box::new(Seq(vec![
                Loop(
                    LoopType::PreChecked(c_c1),
                    Box::new(BasicBlock("n1".to_owned())),
                ),
                Cond(
                    c_c2,
                    Box::new(Seq(vec![BasicBlock("n2".to_owned()), Break])),
                    None,
                ),
                BasicBlock("n3".to_owned()),
            ]))
        ),
        ast
    );
}

#[test]
fn ast_nmg_r2() {
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let mut graph = StableDiGraph::new();
    let b1 = graph.add_node(cnode());
    let b2 = graph.add_node(cnode());
    let n4 = graph.add_node(node("n4"));
    let n5 = graph.add_node(node("n5"));
    let n6 = graph.add_node(node("n6"));
    let n7 = graph.add_node(node("n7"));
    let exit = graph.add_node(empty_node());

    let c_b1 = cond_s(&cctx, "b1");
    let c_b2 = cond_s(&cctx, "b2");

    graph.add_edge(b1, b2, Some(c_b1));
    graph.add_edge(b2, n6, Some(c_b2));
    graph.add_edge(n6, n7, None);
    graph.add_edge(b2, n5, neg_c(&cctx, c_b2));
    graph.add_edge(n5, n7, None);
    graph.add_edge(b1, n4, neg_c(&cctx, c_b1));
    graph.add_edge(n4, n5, None);
    graph.add_edge(n7, exit, None);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph {
        graph,
        entry: b1,
        exit,
        cctx,
        actx,
    };
    let ast = cfg.structure_whole();
    println!("{:#?}", ast);

    use self::AstNodeC::*;
    assert_eq!(
        Seq(vec![
            Cond(
                cctx.mk_not(c_b1),
                Box::new(BasicBlock("n4".to_owned())),
                None,
            ),
            Cond(
                cctx.mk_and(c_b1, c_b2),
                Box::new(BasicBlock("n6".to_owned())),
                Some(Box::new(BasicBlock("n5".to_owned()))),
            ),
            BasicBlock("n7".to_owned()),
        ]),
        ast
    );
}

#[test]
fn ast_nmg_r3() {
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(empty_node());
    let d1 = graph.add_node(cnode());
    let d2 = graph.add_node(cnode());
    let d3 = graph.add_node(cnode());
    let n8 = graph.add_node(node("n8"));
    let exit = graph.add_node(empty_node());

    let c_d1 = cond_s(&cctx, "d1");
    let c_d2 = cond_s(&cctx, "d2");
    let c_d3 = cond_s(&cctx, "d3");
    let nc_d1 = cctx.mk_not(c_d1);
    let nc_d2 = cctx.mk_not(c_d2);
    let nc_d3 = cctx.mk_not(c_d3);

    graph.add_edge(entry, d1, None);
    graph.add_edge(d1, d3, Some(c_d1));
    graph.add_edge(d1, d2, Some(nc_d1));
    graph.add_edge(d2, n8, Some(c_d2));
    graph.add_edge(d2, exit, Some(nc_d2));
    graph.add_edge(d3, n8, Some(c_d3));
    graph.add_edge(d3, exit, Some(nc_d3));
    graph.add_edge(n8, d1, None);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph {
        graph,
        entry,
        exit,
        cctx,
        actx,
    };
    let ast = cfg.structure_whole();
    println!("{:#?}", ast);

    use self::AstNodeC::*;
    assert_eq!(
        Loop(
            LoopType::PreChecked(cctx.mk_or(cctx.mk_and(c_d1, c_d3), cctx.mk_and(nc_d1, c_d2))),
            Box::new(BasicBlock("n8".to_owned())),
        ),
        ast
    );
}

#[test]
#[ignore] // TODO
fn ast_switchy() {
    /*
     * switch (n) {
     * case 1:
     * case 4:
     * case 7:
     * case 94:
     *   puts("1");
     *   break;
     * case 34:
     * case 88:
     *   puts("2");
     *   break;
     * default:
     *   puts("3");
     *   break;
     * }
     */
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let mut graph = StableDiGraph::new();
    let c1 = graph.add_node(cnode());
    let c2 = graph.add_node(cnode());
    let c3 = graph.add_node(cnode());
    let c4 = graph.add_node(cnode());
    let c5 = graph.add_node(cnode());
    let c6 = graph.add_node(cnode());
    let c7 = graph.add_node(cnode());
    let n1 = graph.add_node(node("n1"));
    let n2 = graph.add_node(node("n2"));
    let n3 = graph.add_node(node("n3"));
    let exit = graph.add_node(empty_node());

    let c_c1 = cond_s(&cctx, "n == 7");
    let c_c2 = cond_s(&cctx, "n <= 7");
    let c_c3 = cond_s(&cctx, "n == 88");
    let c_c4 = cond_s(&cctx, "n == 1");
    let c_c5 = cond_s(&cctx, "n == 98");
    let c_c6 = cond_s(&cctx, "n != 4");
    let c_c7 = cond_s(&cctx, "n == 34");

    graph.add_edge(c1, n1, Some(c_c1));
    graph.add_edge(c1, c2, Some(cctx.mk_not(c_c1)));
    graph.add_edge(c2, c4, Some(c_c2));
    graph.add_edge(c2, c3, Some(cctx.mk_not(c_c2)));
    graph.add_edge(c3, n2, Some(c_c3));
    graph.add_edge(c3, c5, Some(cctx.mk_not(c_c3)));
    graph.add_edge(c4, n1, Some(c_c4));
    graph.add_edge(c4, c6, Some(cctx.mk_not(c_c4)));
    graph.add_edge(c5, n1, Some(c_c5));
    graph.add_edge(c5, c7, Some(cctx.mk_not(c_c5)));
    graph.add_edge(c6, n3, Some(c_c6));
    graph.add_edge(c6, n1, Some(cctx.mk_not(c_c6)));
    graph.add_edge(c7, n2, Some(c_c7));
    graph.add_edge(c7, n3, Some(cctx.mk_not(c_c7)));
    graph.add_edge(n1, exit, None);
    graph.add_edge(n2, exit, None);
    graph.add_edge(n3, exit, None);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph {
        graph,
        entry: c1,
        exit,
        cctx,
        actx,
    };
    let ast = cfg.structure_whole();
    println!("{:#?}", ast);

    use self::AstNodeC::*;
    // XXX: need actual value set impl
    assert_eq!(
        Switch(
            "n".to_owned(),
            vec![
                ((), BasicBlock("n1".to_owned())),
                ((), BasicBlock("n2".to_owned())),
            ],
            Box::new(BasicBlock("n3".to_owned())),
        ),
        ast
    );
}

#[test]
fn ast_ifelse_cascade() {
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let mut graph = StableDiGraph::new();
    let c1 = graph.add_node(cnode());
    let c2 = graph.add_node(cnode());
    let c3 = graph.add_node(cnode());
    let c4 = graph.add_node(cnode());
    let c5 = graph.add_node(cnode());
    let n1 = graph.add_node(node("n1"));
    let n2 = graph.add_node(node("n2"));
    let n3 = graph.add_node(node("n3"));
    let exit = graph.add_node(empty_node());

    let c_c1 = cond_s(&cctx, "c1");
    let c_c2 = cond_s(&cctx, "c2");
    let c_c3 = cond_s(&cctx, "c3");
    let c_c4 = cond_s(&cctx, "c4");
    let c_c5 = cond_s(&cctx, "c5");
    let nc_c1 = cctx.mk_not(c_c1);
    let nc_c2 = cctx.mk_not(c_c2);
    let nc_c3 = cctx.mk_not(c_c3);
    let nc_c4 = cctx.mk_not(c_c4);
    let nc_c5 = cctx.mk_not(c_c5);

    graph.add_edge(c1, c4, Some(c_c1));
    graph.add_edge(c1, c2, Some(nc_c1));
    graph.add_edge(c2, n3, Some(c_c2));
    graph.add_edge(c2, c3, Some(nc_c2));
    graph.add_edge(c3, c5, Some(c_c3));
    graph.add_edge(c3, c4, Some(nc_c3));
    graph.add_edge(c4, n2, Some(c_c4));
    graph.add_edge(c4, n1, Some(nc_c4));
    graph.add_edge(c5, n3, Some(c_c5));
    graph.add_edge(c5, n2, Some(nc_c5));
    graph.add_edge(n1, exit, None);
    graph.add_edge(n2, exit, None);
    graph.add_edge(n3, exit, None);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph {
        graph,
        entry: c1,
        exit,
        cctx,
        actx,
    };
    let ast = cfg.structure_whole();
    println!("{:#?}", ast);

    use self::AstNodeC::*;
    assert_eq!(
        Cond(
            cctx.mk_and(nc_c1, cctx.mk_or(c_c2, cctx.mk_and(c_c3, c_c5))),
            Box::new(BasicBlock("n3".to_owned())),
            Some(Box::new(Cond(
                cctx.mk_and(cctx.mk_or(c_c1, cctx.mk_and(nc_c2, nc_c3)), nc_c4),
                Box::new(BasicBlock("n1".to_owned())),
                Some(Box::new(BasicBlock("n2".to_owned()))),
            ))),
        ),
        ast
    );
}

#[test]
fn ast_while() {
    /*
     * while (c) {
     *   puts("n");
     * }
     */
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(empty_node());
    let c = graph.add_node(cnode());
    let n = graph.add_node(node("n"));
    let exit = graph.add_node(empty_node());

    let c_c = cond_s(&cctx, "c");
    let nc_c = cctx.mk_not(c_c);

    graph.add_edge(entry, c, None);
    graph.add_edge(c, n, Some(c_c));
    graph.add_edge(c, exit, Some(nc_c));
    graph.add_edge(n, c, None);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph {
        graph,
        entry,
        exit,
        cctx,
        actx,
    };
    let ast = cfg.structure_whole();
    println!("{:#?}", ast);

    use self::AstNodeC::*;
    assert_eq!(
        Loop(
            LoopType::PreChecked(c_c),
            Box::new(BasicBlock("n".to_owned()))
        ),
        ast
    );
}

#[test]
fn ast_do_while() {
    /*
     * do {
     *   puts("n");
     * } while (c);
     */
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(empty_node());
    let c = graph.add_node(cnode());
    let n = graph.add_node(node("n"));
    let exit = graph.add_node(empty_node());

    let c_c = cond_s(&cctx, "c");
    let nc_c = cctx.mk_not(c_c);

    graph.add_edge(entry, n, None);
    graph.add_edge(n, c, None);
    graph.add_edge(c, n, Some(c_c));
    graph.add_edge(c, exit, Some(nc_c));

    let actx = StringAst::default();
    let cfg = ControlFlowGraph {
        graph,
        entry,
        exit,
        cctx,
        actx,
    };
    let ast = cfg.structure_whole();
    println!("{:#?}", ast);

    use self::AstNodeC::*;
    assert_eq!(
        Loop(
            LoopType::PostChecked(c_c),
            Box::new(BasicBlock("n".to_owned()))
        ),
        ast
    );
}

#[test]
fn ast_infinite_loop() {
    /*
     * while (1) {
     *   puts("1");
     * }
     */
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(empty_node());
    let n1 = graph.add_node(node("n1"));
    let exit = graph.add_node(empty_node());

    graph.add_edge(entry, n1, None);
    graph.add_edge(n1, n1, None);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph {
        graph,
        entry,
        exit,
        cctx,
        actx,
    };
    let ast = cfg.structure_whole();
    println!("{:#?}", ast);

    use self::AstNodeC::*;
    assert_eq!(
        Loop(LoopType::Endless, Box::new(BasicBlock("n1".to_owned()))),
        ast
    );
}

#[test]
fn ast_complex_while_and() {
    /*
     * while (c1 && c2 && c3 && c4) {
     *   puts("1");
     * }
     */
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(empty_node());
    let c1 = graph.add_node(cnode());
    let c2 = graph.add_node(cnode());
    let c3 = graph.add_node(cnode());
    let c4 = graph.add_node(cnode());
    let n1 = graph.add_node(node("n1"));
    let exit = graph.add_node(empty_node());

    let c_c1 = cond_s(&cctx, "c1");
    let c_c2 = cond_s(&cctx, "c2");
    let c_c3 = cond_s(&cctx, "c3");
    let c_c4 = cond_s(&cctx, "c4");
    let nc_c1 = cctx.mk_not(c_c1);
    let nc_c2 = cctx.mk_not(c_c2);
    let nc_c3 = cctx.mk_not(c_c3);
    let nc_c4 = cctx.mk_not(c_c4);

    graph.add_edge(entry, c1, None);
    graph.add_edge(c1, c2, Some(c_c1));
    graph.add_edge(c1, exit, Some(nc_c1));
    graph.add_edge(c2, c3, Some(c_c2));
    graph.add_edge(c2, exit, Some(nc_c2));
    graph.add_edge(c3, c4, Some(c_c3));
    graph.add_edge(c3, exit, Some(nc_c3));
    graph.add_edge(c4, n1, Some(c_c4));
    graph.add_edge(c4, exit, Some(nc_c4));
    graph.add_edge(n1, c1, None);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph {
        graph,
        entry,
        exit,
        cctx,
        actx,
    };
    let ast = cfg.structure_whole();
    println!("{:#?}", ast);

    use self::AstNodeC::*;
    assert_eq!(
        Loop(
            LoopType::PreChecked(cctx.mk_and_from_iter(vec![c_c1, c_c2, c_c3, c_c4])),
            Box::new(BasicBlock("n1".to_owned()))
        ),
        ast
    );
}

#[test]
fn ast_complex_while_or() {
    /*
     * while (c1 || c2 || c3 || c4) {
     *   puts("1");
     * }
     */
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(empty_node());
    let c1 = graph.add_node(cnode());
    let c2 = graph.add_node(cnode());
    let c3 = graph.add_node(cnode());
    let c4 = graph.add_node(cnode());
    let n1 = graph.add_node(node("n1"));
    let exit = graph.add_node(empty_node());

    let c_c1 = cond_s(&cctx, "c1");
    let c_c2 = cond_s(&cctx, "c2");
    let c_c3 = cond_s(&cctx, "c3");
    let c_c4 = cond_s(&cctx, "c4");
    let nc_c1 = cctx.mk_not(c_c1);
    let nc_c2 = cctx.mk_not(c_c2);
    let nc_c3 = cctx.mk_not(c_c3);
    let nc_c4 = cctx.mk_not(c_c4);

    graph.add_edge(entry, c1, None);
    graph.add_edge(c1, n1, Some(c_c1));
    graph.add_edge(c1, c2, Some(nc_c1));
    graph.add_edge(c2, n1, Some(c_c2));
    graph.add_edge(c2, c3, Some(nc_c2));
    graph.add_edge(c3, n1, Some(c_c3));
    graph.add_edge(c3, c4, Some(nc_c3));
    graph.add_edge(c4, n1, Some(c_c4));
    graph.add_edge(c4, exit, Some(nc_c4));
    graph.add_edge(n1, c1, None);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph {
        graph,
        entry,
        exit,
        cctx,
        actx,
    };
    let ast = cfg.structure_whole();
    println!("{:#?}", ast);

    use self::AstNodeC::*;
    assert_eq!(
        Loop(
            LoopType::PreChecked(cctx.mk_or_from_iter(vec![c_c1, c_c2, c_c3, c_c4])),
            Box::new(BasicBlock("n1".to_owned()))
        ),
        ast
    );
}

#[test]
fn abnormal_entries() {
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(cnode());
    let n1 = graph.add_node(cnode());
    let n2 = graph.add_node(cnode());
    let n3 = graph.add_node(cnode());
    let n4 = graph.add_node(cnode());
    let n5 = graph.add_node(cnode());
    let f = graph.add_node(node("f"));
    let l1 = graph.add_node(cnode());
    let l2 = graph.add_node(node("l2"));
    let l3 = graph.add_node(node("l3"));

    let c_e1 = cond_s(&cctx, "e1");
    let c_n1 = cond_s(&cctx, "n1");
    let c_n2 = cond_s(&cctx, "n2");
    let c_n3 = cond_s(&cctx, "n3");
    let c_n4 = cond_s(&cctx, "n4");
    let c_n5 = cond_s(&cctx, "n5");
    let c_l1 = cond_s(&cctx, "l1");

    graph.add_edge(entry, l1, Some(c_e1));
    graph.add_edge(entry, n1, neg_c(&cctx, c_e1));
    graph.add_edge(n1, n2, Some(c_n1));
    graph.add_edge(n2, n3, Some(c_n2));
    graph.add_edge(n3, n4, Some(c_n3));
    graph.add_edge(n4, n5, Some(c_n4));
    graph.add_edge(n5, f, Some(c_n5));
    // loop
    graph.add_edge(l1, l2, Some(c_l1));
    graph.add_edge(l2, l3, None);
    graph.add_edge(l3, l1, None);
    // loop exit
    graph.add_edge(l1, f, neg_c(&cctx, c_l1));
    // abnormal entries
    graph.add_edge(n1, l1, neg_c(&cctx, c_n1));
    graph.add_edge(n2, l2, neg_c(&cctx, c_n2));
    graph.add_edge(n3, l3, neg_c(&cctx, c_n3));
    graph.add_edge(n4, l2, neg_c(&cctx, c_n4));
    graph.add_edge(n5, l2, neg_c(&cctx, c_n5));

    let actx = StringAst::default();
    let cfg = ControlFlowGraph {
        graph,
        entry,
        exit: f,
        cctx,
        actx,
    };
    let ast = cfg.structure_whole();
    println!("{:#?}", ast);
}

#[test]
fn abnormal_exits() {
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(cnode());
    let n1 = graph.add_node(node("n1"));
    let n2 = graph.add_node(node("n2"));
    let n3 = graph.add_node(node("n3"));
    let n4 = graph.add_node(node("n4"));
    let n5 = graph.add_node(node("n5"));
    let f = graph.add_node(node("f"));
    let l1 = graph.add_node(cnode());
    let l2 = graph.add_node(cnode());
    let l3 = graph.add_node(cnode());
    let l4 = graph.add_node(cnode());
    let l5 = graph.add_node(cnode());

    let c_e1 = cond_s(&cctx, "e1");
    let c_l1 = cond_s(&cctx, "l1");
    let c_l2 = cond_s(&cctx, "l2");
    let c_l3 = cond_s(&cctx, "l3");
    let c_l4 = cond_s(&cctx, "l4");
    let c_l5 = cond_s(&cctx, "l5");

    graph.add_edge(entry, l1, Some(c_e1));
    graph.add_edge(entry, n1, neg_c(&cctx, c_e1));
    graph.add_edge(n1, n2, None);
    graph.add_edge(n2, n3, None);
    graph.add_edge(n3, n4, None);
    graph.add_edge(n4, n5, None);
    graph.add_edge(n5, f, None);
    // loop
    graph.add_edge(l1, l2, Some(c_l1));
    graph.add_edge(l2, l3, Some(c_l2));
    graph.add_edge(l3, l4, Some(c_l3));
    graph.add_edge(l4, l5, Some(c_l4));
    graph.add_edge(l5, l1, Some(c_l5));
    // loop exit
    graph.add_edge(l1, f, neg_c(&cctx, c_l1));
    graph.add_edge(l4, f, neg_c(&cctx, c_l4));
    // abnormal exits
    graph.add_edge(l2, n2, neg_c(&cctx, c_l2));
    graph.add_edge(l3, n2, neg_c(&cctx, c_l3));
    graph.add_edge(l5, n5, neg_c(&cctx, c_l5));

    let actx = StringAst::default();
    let cfg = ControlFlowGraph {
        graph,
        entry,
        exit: f,
        cctx,
        actx,
    };
    let ast = cfg.structure_whole();
    println!("{:#?}", ast);
}

fn cond_s<'cd>(cctx: &condition::Context<'cd, String>, c: &str) -> Condition<'cd, StringAst> {
    cctx.mk_var(c.to_owned())
}

fn neg_c<'cd>(
    cctx: &condition::Context<'cd, String>,
    c: Condition<'cd, StringAst>,
) -> Option<Condition<'cd, StringAst>> {
    Some(cctx.mk_not(c))
}

fn node(n: &str) -> CfgNode<'static, StringAst> {
    CfgNode::Code(AstNodeC::BasicBlock(n.to_owned()))
}

fn empty_node() -> CfgNode<'static, StringAst> {
    CfgNode::Code(AstNodeC::default())
}

fn cnode() -> CfgNode<'static, StringAst> {
    CfgNode::Condition
}
