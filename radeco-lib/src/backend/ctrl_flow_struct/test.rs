use super::ast::LoopType;
use super::condition;
use super::*;

use super::CfgEdge::False as CEFalse;
use super::CfgEdge::True as CETrue;

// NOTE: If a loop dominates the exit node, the algorithm tends to "suck" the
// `return` up into the loop body, which may end up not testing what you wanted.
// To work around this, simply add an additional branch at the entry node that
// just jumps to the exit.

#[derive(Default, Debug)]
struct StringAst {
    vars: Vec<Option<u64>>,
}

impl AstContext for StringAst {
    type Block = String;
    type Variable = String;
    type BoolVariable = String;
    type Condition = String;
}

impl AstContextMut for StringAst {
    fn mk_fresh_var(&mut self) -> String {
        let ret = format!("i_{}", self.vars.len());
        self.vars.push(None);
        ret
    }

    fn mk_fresh_var_zeroed(&mut self) -> String {
        let ret = format!("i_{}", self.vars.len());
        self.vars.push(Some(0));
        ret
    }

    fn mk_fresh_bool_var(&mut self) -> String {
        let ret = format!("c_{}", self.vars.len());
        self.vars.push(None);
        ret
    }

    fn mk_cond_equals(&mut self, var: &String, val: u64) -> String {
        format!("{} == {}", var, val)
    }

    fn mk_cond_from_bool_var(&mut self, var: &String) -> String {
        format!("{}", var)
    }

    fn mk_var_assign(&mut self, var: &String, val: u64) -> String {
        format!("{} = {}", var, val)
    }

    fn mk_bool_var_assign(&mut self, var: &String, val: &String) -> String {
        format!("{} = {}", var, val)
    }
}

#[test]
fn ast_nmg_example() {
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    #[allow(non_snake_case)]
    let v_A = cond_s(cctx, "A");
    let v_c1 = cond_s(cctx, "c1");
    let v_c2 = cond_s(cctx, "c2");
    let v_c3 = cond_s(cctx, "c3");
    let v_b1 = cond_s(cctx, "b1");
    let v_b2 = cond_s(cctx, "b2");
    let v_d1 = cond_s(cctx, "d1");
    let v_d2 = cond_s(cctx, "d2");
    let v_d3 = cond_s(cctx, "d3");

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(cnode(v_A));
    let c1 = graph.add_node(cnode(v_c1));
    let c2 = graph.add_node(cnode(v_c2));
    let c3 = graph.add_node(cnode(v_c3));
    let b1 = graph.add_node(cnode(v_b1));
    let b2 = graph.add_node(cnode(v_b2));
    let d1 = graph.add_node(cnode(v_d1));
    let d2 = graph.add_node(cnode(v_d2));
    let d3 = graph.add_node(cnode(v_d3));
    let n1 = graph.add_node(node("n1"));
    let n2 = graph.add_node(node("n2"));
    let n3 = graph.add_node(node("n3"));
    let n4 = graph.add_node(node("n4"));
    let n5 = graph.add_node(node("n5"));
    let n6 = graph.add_node(node("n6"));
    let n7 = graph.add_node(node("n7"));
    let n8 = graph.add_node(node("n8"));
    let n9 = graph.add_node(node("n9"));
    let exit = graph.add_node(node("return"));

    graph.add_edge(entry, c1, CETrue);
    graph.add_edge(entry, b1, CEFalse);
    graph.add_edge(n9, exit, CETrue);
    // R1
    graph.add_edge(c1, n1, CETrue);
    graph.add_edge(n1, c1, CETrue);
    graph.add_edge(c1, c2, CEFalse);
    graph.add_edge(c2, n2, CETrue);
    graph.add_edge(n2, n9, CETrue);
    graph.add_edge(c2, n3, CEFalse);
    graph.add_edge(n3, c3, CETrue);
    graph.add_edge(c3, c1, CETrue);
    graph.add_edge(c3, n9, CEFalse);
    // R2
    graph.add_edge(b1, b2, CETrue);
    graph.add_edge(b2, n6, CETrue);
    graph.add_edge(n6, n7, CETrue);
    graph.add_edge(n7, d1, CETrue);
    graph.add_edge(b2, n5, CEFalse);
    graph.add_edge(n5, n7, CETrue);
    graph.add_edge(b1, n4, CEFalse);
    graph.add_edge(n4, n5, CETrue);
    // R3
    graph.add_edge(d1, d3, CETrue);
    graph.add_edge(d3, n8, CETrue);
    graph.add_edge(n8, d1, CETrue);
    graph.add_edge(d3, n9, CEFalse);
    graph.add_edge(d1, d2, CEFalse);
    graph.add_edge(d2, n8, CETrue);
    graph.add_edge(d2, n9, CEFalse);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph::new(graph, entry, cctx, actx);
    let ast = cfg.structure_whole().0;
    println!("{:#?}", ast);

    #[allow(non_snake_case)]
    let c_A = cctx.mk_var(v_A);
    let c_c1 = cctx.mk_var(v_c1);
    let c_c2 = cctx.mk_var(v_c2);
    let c_c3 = cctx.mk_var(v_c3);
    let _c_b1 = cctx.mk_var(v_b1);
    let _c_b2 = cctx.mk_var(v_b2);
    let c_d1 = cctx.mk_var(v_d1);
    let c_d2 = cctx.mk_var(v_d2);
    let c_d3 = cctx.mk_var(v_d3);

    use self::AstNodeC::*;
    assert_eq!(
        Seq(vec![
            Cond(
                format!("{:?}", c_A),
                Box::new(stringify_conds(Loop(
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
                ))),
                Some(Box::new(Seq(vec![
                    BasicBlock("c_0 = b1".to_owned()),
                    Cond(
                        r#"-"c_0""#.to_owned(),
                        Box::new(BasicBlock("n4".to_owned())),
                        None,
                    ),
                    Cond(
                        r#"And{"c_0", "b2"}"#.to_owned(),
                        Box::new(BasicBlock("n6".to_owned())),
                        Some(Box::new(BasicBlock("n5".to_owned()))),
                    ),
                    BasicBlock("n7".to_owned()),
                    stringify_conds(Loop(
                        LoopType::PreChecked(cctx.mk_or(
                            cctx.mk_and(cctx.mk_not(c_d1), c_d2),
                            cctx.mk_and(c_d1, c_d3),
                        )),
                        Box::new(BasicBlock("n8".to_owned())),
                    )),
                ]))),
            ),
            BasicBlock("n9".to_owned()),
            BasicBlock("return".to_owned()),
        ]),
        stringify_conds(ast)
    );
}

#[test]
fn ast_nmg_r1() {
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let v_ce = cond_s(cctx, "ce");
    let v_c1 = cond_s(cctx, "c1");
    let v_c2 = cond_s(cctx, "c2");
    let v_c3 = cond_s(cctx, "c3");

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(cnode(v_ce));
    let c1 = graph.add_node(cnode(v_c1));
    let c2 = graph.add_node(cnode(v_c2));
    let c3 = graph.add_node(cnode(v_c3));
    let n1 = graph.add_node(node("n1"));
    let n2 = graph.add_node(node("n2"));
    let n3 = graph.add_node(node("n3"));
    let exit = graph.add_node(node("return"));

    graph.add_edge(entry, c1, CETrue);
    graph.add_edge(entry, exit, CEFalse);
    graph.add_edge(c1, n1, CETrue);
    graph.add_edge(n1, c1, CETrue);
    graph.add_edge(c1, c2, CEFalse);
    graph.add_edge(c2, n2, CETrue);
    graph.add_edge(n2, exit, CETrue);
    graph.add_edge(c2, n3, CEFalse);
    graph.add_edge(n3, c3, CETrue);
    graph.add_edge(c3, c1, CETrue);
    graph.add_edge(c3, exit, CEFalse);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph::new(graph, entry, cctx, actx);
    let ast = cfg.structure_whole().0;
    println!("{:#?}", ast);

    let c_ce = cctx.mk_var(v_ce);
    let c_c1 = cctx.mk_var(v_c1);
    let c_c2 = cctx.mk_var(v_c2);
    let c_c3 = cctx.mk_var(v_c3);

    use self::AstNodeC::*;
    assert_eq!(
        Seq(vec![
            Cond(
                c_ce,
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
                None,
            ),
            BasicBlock("return".to_owned()),
        ]),
        ast
    );
}

#[test]
fn ast_nmg_r2() {
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let v_b1 = cond_s(cctx, "b1");
    let v_b2 = cond_s(cctx, "b2");

    let mut graph = StableDiGraph::new();
    let b1 = graph.add_node(cnode(v_b1));
    let b2 = graph.add_node(cnode(v_b2));
    let n4 = graph.add_node(node("n4"));
    let n5 = graph.add_node(node("n5"));
    let n6 = graph.add_node(node("n6"));
    let n7 = graph.add_node(node("n7"));
    let exit = graph.add_node(node("return"));

    graph.add_edge(b1, b2, CETrue);
    graph.add_edge(b2, n6, CETrue);
    graph.add_edge(n6, n7, CETrue);
    graph.add_edge(b2, n5, CEFalse);
    graph.add_edge(n5, n7, CETrue);
    graph.add_edge(b1, n4, CEFalse);
    graph.add_edge(n4, n5, CETrue);
    graph.add_edge(n7, exit, CETrue);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph::new(graph, b1, cctx, actx);
    let ast = cfg.structure_whole().0;
    println!("{:#?}", ast);

    use self::AstNodeC::*;
    assert_eq!(
        Seq(vec![
            BasicBlock("c_0 = b1".to_owned()),
            Cond(
                r#"-"c_0""#.to_owned(),
                Box::new(BasicBlock("n4".to_owned())),
                None,
            ),
            Cond(
                r#"And{"c_0", "b2"}"#.to_owned(),
                Box::new(BasicBlock("n6".to_owned())),
                Some(Box::new(BasicBlock("n5".to_owned()))),
            ),
            BasicBlock("n7".to_owned()),
            BasicBlock("return".to_owned()),
        ]),
        stringify_conds(ast)
    );
}

#[test]
fn ast_nmg_r3() {
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let v_d1 = cond_s(cctx, "d1");
    let v_d2 = cond_s(cctx, "d2");
    let v_d3 = cond_s(cctx, "d3");

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(empty_node());
    let d1 = graph.add_node(cnode(v_d1));
    let d2 = graph.add_node(cnode(v_d2));
    let d3 = graph.add_node(cnode(v_d3));
    let n8 = graph.add_node(node("n8"));
    let exit = graph.add_node(node("return"));

    graph.add_edge(entry, d1, CETrue);
    graph.add_edge(d1, d3, CETrue);
    graph.add_edge(d1, d2, CEFalse);
    graph.add_edge(d2, n8, CETrue);
    graph.add_edge(d2, exit, CEFalse);
    graph.add_edge(d3, n8, CETrue);
    graph.add_edge(d3, exit, CEFalse);
    graph.add_edge(n8, d1, CETrue);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph::new(graph, entry, cctx, actx);
    let ast = cfg.structure_whole().0;
    println!("{:#?}", ast);

    let c_d1 = cctx.mk_var(v_d1);
    let c_d2 = cctx.mk_var(v_d2);
    let c_d3 = cctx.mk_var(v_d3);

    use self::AstNodeC::*;
    assert_eq!(
        Seq(vec![
            Loop(
                LoopType::PreChecked(cctx.mk_or(
                    cctx.mk_and(c_d1, c_d3),
                    cctx.mk_and(cctx.mk_not(c_d1), c_d2),
                )),
                Box::new(BasicBlock("n8".to_owned())),
            ),
            BasicBlock("return".to_owned()),
        ]),
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

    let v_c1 = cond_s(cctx, "n == 7");
    let v_c2 = cond_s(cctx, "n <= 7");
    let v_c3 = cond_s(cctx, "n == 88");
    let v_c4 = cond_s(cctx, "n == 1");
    let v_c5 = cond_s(cctx, "n == 98");
    let v_c6 = cond_s(cctx, "n != 4");
    let v_c7 = cond_s(cctx, "n == 34");

    let mut graph = StableDiGraph::new();
    let c1 = graph.add_node(cnode(v_c1));
    let c2 = graph.add_node(cnode(v_c2));
    let c3 = graph.add_node(cnode(v_c3));
    let c4 = graph.add_node(cnode(v_c4));
    let c5 = graph.add_node(cnode(v_c5));
    let c6 = graph.add_node(cnode(v_c6));
    let c7 = graph.add_node(cnode(v_c7));
    let n1 = graph.add_node(node("n1"));
    let n2 = graph.add_node(node("n2"));
    let n3 = graph.add_node(node("n3"));
    let exit = graph.add_node(node("return"));

    graph.add_edge(c1, n1, CETrue);
    graph.add_edge(c1, c2, CEFalse);
    graph.add_edge(c2, c4, CETrue);
    graph.add_edge(c2, c3, CEFalse);
    graph.add_edge(c3, n2, CETrue);
    graph.add_edge(c3, c5, CEFalse);
    graph.add_edge(c4, n1, CETrue);
    graph.add_edge(c4, c6, CEFalse);
    graph.add_edge(c5, n1, CETrue);
    graph.add_edge(c5, c7, CEFalse);
    graph.add_edge(c6, n3, CETrue);
    graph.add_edge(c6, n1, CEFalse);
    graph.add_edge(c7, n2, CETrue);
    graph.add_edge(c7, n3, CEFalse);
    graph.add_edge(n1, exit, CETrue);
    graph.add_edge(n2, exit, CETrue);
    graph.add_edge(n3, exit, CETrue);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph::new(graph, c1, cctx, actx);
    let ast = cfg.structure_whole().0;
    println!("{:#?}", ast);

    use self::AstNodeC::*;
    // XXX: need actual value set impl
    assert_eq!(
        Seq(vec![
            Switch(
                "n".to_owned(),
                vec![
                    ((), BasicBlock("n1".to_owned())),
                    ((), BasicBlock("n2".to_owned())),
                ],
                Box::new(BasicBlock("n3".to_owned())),
            ),
            BasicBlock("return".to_owned()),
        ]),
        ast
    );
}

#[test]
fn ast_ifelse_cascade() {
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let v_c1 = cond_s(cctx, "c1");
    let v_c2 = cond_s(cctx, "c2");
    let v_c3 = cond_s(cctx, "c3");
    let v_c4 = cond_s(cctx, "c4");
    let v_c5 = cond_s(cctx, "c5");

    let mut graph = StableDiGraph::new();
    let c1 = graph.add_node(cnode(v_c1));
    let c2 = graph.add_node(cnode(v_c2));
    let c3 = graph.add_node(cnode(v_c3));
    let c4 = graph.add_node(cnode(v_c4));
    let c5 = graph.add_node(cnode(v_c5));
    let n1 = graph.add_node(node("n1"));
    let n2 = graph.add_node(node("n2"));
    let n3 = graph.add_node(node("n3"));
    let exit = graph.add_node(node("return"));

    graph.add_edge(c1, c4, CETrue);
    graph.add_edge(c1, c2, CEFalse);
    graph.add_edge(c2, n3, CETrue);
    graph.add_edge(c2, c3, CEFalse);
    graph.add_edge(c3, c5, CETrue);
    graph.add_edge(c3, c4, CEFalse);
    graph.add_edge(c4, n2, CETrue);
    graph.add_edge(c4, n1, CEFalse);
    graph.add_edge(c5, n3, CETrue);
    graph.add_edge(c5, n2, CEFalse);
    graph.add_edge(n1, exit, CETrue);
    graph.add_edge(n2, exit, CETrue);
    graph.add_edge(n3, exit, CETrue);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph::new(graph, c1, cctx, actx);
    let ast = cfg.structure_whole().0;
    println!("{:#?}", ast);

    use self::AstNodeC::*;
    assert_eq!(
        Seq(vec![
            BasicBlock("c_0 = c1".to_owned()),
            BasicBlock("c_1 = c2".to_owned()),
            BasicBlock("c_2 = c3".to_owned()),
            Cond(
                r#"And{-"c_0", Or{"c_1", And{"c5", "c_2"}}}"#.to_owned(),
                Box::new(BasicBlock("n3".to_owned())),
                Some(Box::new(Cond(
                    r#"And{Or{"c_0", And{-"c_2", -"c_1"}}, -"c4"}"#.to_owned(),
                    Box::new(BasicBlock("n1".to_owned())),
                    Some(Box::new(BasicBlock("n2".to_owned()))),
                ))),
            ),
            BasicBlock("return".to_owned()),
        ]),
        stringify_conds(ast)
    );
}

#[test]
fn ast_while() {
    /*
     * if (ce) {
     *   while (c1) {
     *     puts("n");
     *   }
     * }
     * return;
     */
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let v_ce = cond_s(cctx, "ce");
    let v_c1 = cond_s(cctx, "c1");

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(cnode(v_ce));
    let c = graph.add_node(cnode(v_c1));
    let n = graph.add_node(node("n"));
    let exit = graph.add_node(node("return"));

    graph.add_edge(entry, c, CETrue);
    graph.add_edge(entry, exit, CEFalse);
    graph.add_edge(c, n, CETrue);
    graph.add_edge(c, exit, CEFalse);
    graph.add_edge(n, c, CETrue);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph::new(graph, entry, cctx, actx);
    let ast = cfg.structure_whole().0;
    println!("{:#?}", ast);

    let c_ce = cctx.mk_var(v_ce);
    let c_c1 = cctx.mk_var(v_c1);

    use self::AstNodeC::*;
    assert_eq!(
        Seq(vec![
            Cond(
                c_ce,
                Box::new(Loop(
                    LoopType::PreChecked(c_c1),
                    Box::new(BasicBlock("n".to_owned())),
                )),
                None,
            ),
            BasicBlock("return".to_owned()),
        ]),
        ast
    );
}

#[test]
fn ast_do_while() {
    /*
     * if (ce) {
     *   do {
     *     puts("n");
     *   } while (c);
     * }
     * return;
     */
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let v_ce = cond_s(cctx, "ce");
    let v_c1 = cond_s(cctx, "c1");

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(cnode(v_ce));
    let c = graph.add_node(cnode(v_c1));
    let n = graph.add_node(node("n"));
    let exit = graph.add_node(node("return"));

    graph.add_edge(entry, n, CETrue);
    graph.add_edge(entry, exit, CEFalse);
    graph.add_edge(n, c, CETrue);
    graph.add_edge(c, n, CETrue);
    graph.add_edge(c, exit, CEFalse);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph::new(graph, entry, cctx, actx);
    let ast = cfg.structure_whole().0;
    println!("{:#?}", ast);

    let c_ce = cctx.mk_var(v_ce);
    let c_c1 = cctx.mk_var(v_c1);

    use self::AstNodeC::*;
    assert_eq!(
        Seq(vec![
            Cond(
                c_ce,
                Box::new(Loop(
                    LoopType::PostChecked(c_c1),
                    Box::new(BasicBlock("n".to_owned())),
                )),
                None,
            ),
            BasicBlock("return".to_owned()),
        ]),
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

    graph.add_edge(entry, n1, CETrue);
    graph.add_edge(n1, n1, CETrue);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph::new(graph, entry, cctx, actx);
    let ast = cfg.structure_whole().0;
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
     * if (ce) {
     *   while (c1 && c2 && c3 && c4) {
     *     puts("1");
     *   }
     * }
     * return;
     */
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let v_ce = cond_s(cctx, "ce");
    let v_c1 = cond_s(cctx, "c1");
    let v_c2 = cond_s(cctx, "c2");
    let v_c3 = cond_s(cctx, "c3");
    let v_c4 = cond_s(cctx, "c4");

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(cnode(v_ce));
    let c1 = graph.add_node(cnode(v_c1));
    let c2 = graph.add_node(cnode(v_c2));
    let c3 = graph.add_node(cnode(v_c3));
    let c4 = graph.add_node(cnode(v_c4));
    let n1 = graph.add_node(node("n1"));
    let exit = graph.add_node(node("return"));

    graph.add_edge(entry, c1, CETrue);
    graph.add_edge(entry, exit, CEFalse);
    graph.add_edge(c1, c2, CETrue);
    graph.add_edge(c1, exit, CEFalse);
    graph.add_edge(c2, c3, CETrue);
    graph.add_edge(c2, exit, CEFalse);
    graph.add_edge(c3, c4, CETrue);
    graph.add_edge(c3, exit, CEFalse);
    graph.add_edge(c4, n1, CETrue);
    graph.add_edge(c4, exit, CEFalse);
    graph.add_edge(n1, c1, CETrue);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph::new(graph, entry, cctx, actx);
    let ast = cfg.structure_whole().0;
    println!("{:#?}", ast);

    let c_ce = cctx.mk_var(v_ce);
    let c_c1 = cctx.mk_var(v_c1);
    let c_c2 = cctx.mk_var(v_c2);
    let c_c3 = cctx.mk_var(v_c3);
    let c_c4 = cctx.mk_var(v_c4);

    use self::AstNodeC::*;
    assert_eq!(
        Seq(vec![
            Cond(
                c_ce,
                Box::new(Loop(
                    LoopType::PreChecked(cctx.mk_and_from_iter(vec![c_c1, c_c2, c_c3, c_c4])),
                    Box::new(BasicBlock("n1".to_owned())),
                )),
                None,
            ),
            BasicBlock("return".to_owned()),
        ]),
        ast
    );
}

#[test]
fn ast_complex_while_or() {
    /*
     * if (ce) {
     *   while (c1 || c2 || c3 || c4) {
     *     puts("1");
     *   }
     * }
     * return;
     */
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let v_ce = cond_s(cctx, "ce");
    let v_c1 = cond_s(cctx, "c1");
    let v_c2 = cond_s(cctx, "c2");
    let v_c3 = cond_s(cctx, "c3");
    let v_c4 = cond_s(cctx, "c4");

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(cnode(v_ce));
    let c1 = graph.add_node(cnode(v_c1));
    let c2 = graph.add_node(cnode(v_c2));
    let c3 = graph.add_node(cnode(v_c3));
    let c4 = graph.add_node(cnode(v_c4));
    let n1 = graph.add_node(node("n1"));
    let exit = graph.add_node(node("return"));

    graph.add_edge(entry, c1, CETrue);
    graph.add_edge(entry, exit, CEFalse);
    graph.add_edge(c1, n1, CETrue);
    graph.add_edge(c1, c2, CEFalse);
    graph.add_edge(c2, n1, CETrue);
    graph.add_edge(c2, c3, CEFalse);
    graph.add_edge(c3, n1, CETrue);
    graph.add_edge(c3, c4, CEFalse);
    graph.add_edge(c4, n1, CETrue);
    graph.add_edge(c4, exit, CEFalse);
    graph.add_edge(n1, c1, CETrue);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph::new(graph, entry, cctx, actx);
    let ast = cfg.structure_whole().0;
    println!("{:#?}", ast);

    let c_ce = cctx.mk_var(v_ce);
    let c_c1 = cctx.mk_var(v_c1);
    let c_c2 = cctx.mk_var(v_c2);
    let c_c3 = cctx.mk_var(v_c3);
    let c_c4 = cctx.mk_var(v_c4);

    use self::AstNodeC::*;
    assert_eq!(
        Seq(vec![
            Cond(
                c_ce,
                Box::new(Loop(
                    LoopType::PreChecked(cctx.mk_or_from_iter(vec![c_c1, c_c2, c_c3, c_c4])),
                    Box::new(BasicBlock("n1".to_owned())),
                )),
                None,
            ),
            BasicBlock("return".to_owned()),
        ]),
        ast
    );
}

#[test]
fn ast_single_node() {
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(node("stuff"));

    let actx = StringAst::default();
    let cfg = ControlFlowGraph::new(graph, entry, cctx, actx);
    let ast = cfg.structure_whole().0;
    println!("{:#?}", ast);

    use self::AstNodeC::*;
    assert_eq!(BasicBlock("stuff".to_owned()), ast);
}

#[test]
fn too_many_loops() {
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let v_e1 = cond_s(cctx, "e1");
    let v_a2 = cond_s(cctx, "a2");
    let v_a3 = cond_s(cctx, "a3");
    let v_b2 = cond_s(cctx, "b2");
    let v_b3 = cond_s(cctx, "b3");
    let v_c2 = cond_s(cctx, "c2");
    let v_c4 = cond_s(cctx, "c4");

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(cnode(v_e1));
    let exit = graph.add_node(node("return"));
    // loop a
    let a1 = graph.add_node(node("a1"));
    let a2 = graph.add_node(cnode(v_a2));
    let a3 = graph.add_node(cnode(v_a3));
    // loop b
    let b1 = graph.add_node(node("b1"));
    let b2 = graph.add_node(cnode(v_b2));
    let b3 = graph.add_node(cnode(v_b3));
    // loop c
    let c1 = graph.add_node(node("c1"));
    let c2 = graph.add_node(cnode(v_c2));
    let c3 = graph.add_node(node("c3"));
    let c4 = graph.add_node(cnode(v_c4));

    graph.add_edge(entry, a1, CETrue);
    graph.add_edge(entry, b1, CEFalse);
    // loop a
    graph.add_edge(a1, a2, CETrue);
    graph.add_edge(a2, a3, CETrue);
    graph.add_edge(a2, c1, CEFalse);
    graph.add_edge(a3, a1, CETrue);
    graph.add_edge(a3, c3, CEFalse);
    // loop b
    graph.add_edge(b1, b2, CETrue);
    graph.add_edge(b2, b3, CETrue);
    graph.add_edge(b2, c1, CEFalse);
    graph.add_edge(b3, b1, CETrue);
    graph.add_edge(b3, c3, CEFalse);
    // loop c
    graph.add_edge(c1, c2, CETrue);
    graph.add_edge(c2, c3, CETrue);
    graph.add_edge(c2, exit, CEFalse);
    graph.add_edge(c3, c4, CETrue);
    graph.add_edge(c4, c1, CETrue);
    graph.add_edge(c4, exit, CEFalse);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph::new(graph, entry, cctx, actx);
    let ast = cfg.structure_whole().0;
    println!("{:#?}", ast);
}

#[test]
fn nested_continue() {
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let v_ce = cond_s(cctx, "ce");
    let v_n2 = cond_s(cctx, "n2");
    let v_n4 = cond_s(cctx, "n4");
    let v_n6 = cond_s(cctx, "n6");
    let v_n8 = cond_s(cctx, "n8");

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(cnode(v_ce));
    let n1 = graph.add_node(node("n1"));
    let n2 = graph.add_node(cnode(v_n2));
    let n3 = graph.add_node(node("n3"));
    let n4 = graph.add_node(cnode(v_n4));
    let n8 = graph.add_node(cnode(v_n8));
    let n5 = graph.add_node(node("n5"));
    let n6 = graph.add_node(cnode(v_n6));
    let n7 = graph.add_node(node("n7"));
    let exit = graph.add_node(node("return"));

    graph.add_edge(entry, n1, CETrue);
    graph.add_edge(entry, exit, CEFalse);
    graph.add_edge(n1, n2, CETrue);
    graph.add_edge(n2, n3, CETrue);
    graph.add_edge(n2, n5, CEFalse);
    graph.add_edge(n3, n4, CETrue);
    graph.add_edge(n4, n1, CETrue);
    graph.add_edge(n4, n8, CEFalse);
    graph.add_edge(n8, exit, CETrue);
    graph.add_edge(n8, n7, CEFalse);
    graph.add_edge(n5, n6, CETrue);
    graph.add_edge(n6, exit, CETrue);
    graph.add_edge(n6, n7, CEFalse);
    graph.add_edge(n7, n1, CETrue);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph::new(graph, entry, cctx, actx);
    let ast = cfg.structure_whole().0;
    println!("{:#?}", ast);
}

#[test]
fn abnormal_entries() {
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let v_e1 = cond_s(cctx, "e1");
    let v_n1 = cond_s(cctx, "n1");
    let v_n2 = cond_s(cctx, "n2");
    let v_n3 = cond_s(cctx, "n3");
    let v_n4 = cond_s(cctx, "n4");
    let v_n5 = cond_s(cctx, "n5");
    let v_l1 = cond_s(cctx, "l1");

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(cnode(v_e1));
    let n1 = graph.add_node(cnode(v_n1));
    let n2 = graph.add_node(cnode(v_n2));
    let n3 = graph.add_node(cnode(v_n3));
    let n4 = graph.add_node(cnode(v_n4));
    let n5 = graph.add_node(cnode(v_n5));
    let l1 = graph.add_node(cnode(v_l1));
    let l2 = graph.add_node(node("l2"));
    let l3 = graph.add_node(node("l3"));
    let exit = graph.add_node(node("return"));

    graph.add_edge(entry, l1, CETrue);
    graph.add_edge(entry, n1, CEFalse);
    graph.add_edge(n1, n2, CETrue);
    graph.add_edge(n2, n3, CETrue);
    graph.add_edge(n3, n4, CETrue);
    graph.add_edge(n4, n5, CETrue);
    graph.add_edge(n5, exit, CETrue);
    // loop
    graph.add_edge(l1, l2, CETrue);
    graph.add_edge(l2, l3, CETrue);
    graph.add_edge(l3, l1, CETrue);
    // loop exit
    graph.add_edge(l1, exit, CEFalse);
    // abnormal entries
    graph.add_edge(n1, l1, CEFalse);
    graph.add_edge(n2, l2, CEFalse);
    graph.add_edge(n3, l3, CEFalse);
    graph.add_edge(n4, l2, CEFalse);
    graph.add_edge(n5, l2, CEFalse);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph::new(graph, entry, cctx, actx);
    let ast = cfg.structure_whole().0;
    println!("{:#?}", ast);
}

#[test]
fn abnormal_exits() {
    let cstore = condition::Storage::new();
    let cctx = cstore.cctx();

    let v_e1 = cond_s(cctx, "e1");
    let v_l1 = cond_s(cctx, "l1");
    let v_l2 = cond_s(cctx, "l2");
    let v_l3 = cond_s(cctx, "l3");
    let v_l4 = cond_s(cctx, "l4");
    let v_l5 = cond_s(cctx, "l5");

    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(cnode(v_e1));
    let n1 = graph.add_node(node("n1"));
    let n2 = graph.add_node(node("n2"));
    let n3 = graph.add_node(node("n3"));
    let n4 = graph.add_node(node("n4"));
    let n5 = graph.add_node(node("n5"));
    let l1 = graph.add_node(cnode(v_l1));
    let l2 = graph.add_node(cnode(v_l2));
    let l3 = graph.add_node(cnode(v_l3));
    let l4 = graph.add_node(cnode(v_l4));
    let l5 = graph.add_node(cnode(v_l5));
    let exit = graph.add_node(node("return"));

    graph.add_edge(entry, l1, CETrue);
    graph.add_edge(entry, n1, CEFalse);
    graph.add_edge(n1, n2, CETrue);
    graph.add_edge(n2, n3, CETrue);
    graph.add_edge(n3, n4, CETrue);
    graph.add_edge(n4, n5, CETrue);
    graph.add_edge(n5, exit, CETrue);
    // loop
    graph.add_edge(l1, l2, CETrue);
    graph.add_edge(l2, l3, CETrue);
    graph.add_edge(l3, l4, CETrue);
    graph.add_edge(l4, l5, CETrue);
    graph.add_edge(l5, l1, CETrue);
    // loop exit
    graph.add_edge(l1, exit, CEFalse);
    graph.add_edge(l4, exit, CEFalse);
    // abnormal exits
    graph.add_edge(l2, n2, CEFalse);
    graph.add_edge(l3, n2, CEFalse);
    graph.add_edge(l5, n5, CEFalse);

    let actx = StringAst::default();
    let cfg = ControlFlowGraph::new(graph, entry, cctx, actx);
    let ast = cfg.structure_whole().0;
    println!("{:#?}", ast);
}

fn cond_s<'cd>(cctx: condition::Context<'cd, String>, c: &str) -> CondVar<'cd, StringAst> {
    cctx.new_var(c.to_owned())
}

fn node(n: &str) -> CfgNode<'static, StringAst> {
    CfgNode::Code(AstNodeC::BasicBlock(n.to_owned()))
}

fn empty_node() -> CfgNode<'static, StringAst> {
    CfgNode::Code(AstNodeC::default())
}

fn cnode<'cd>(c: CondVar<'cd, StringAst>) -> CfgNode<'cd, StringAst> {
    CfgNode::Condition(c)
}

/// note that this makes the test more fragile as it becomes sensitive to the
/// exact internal representation of conditions
fn stringify_conds<'cd>(ast: AstNode<'cd, StringAst>) -> AstNodeC<String, String, String> {
    use self::AstNodeC::*;
    use self::LoopType::*;

    match ast {
        BasicBlock(b) => BasicBlock(b),
        Seq(seq) => Seq(seq.into_iter().map(stringify_conds).collect()),
        Cond(c, t, oe) => Cond(
            format!("{:?}", c),
            Box::new(stringify_conds(*t)),
            oe.map(|e| Box::new(stringify_conds(*e))),
        ),
        Loop(PreChecked(c), b) => Loop(
            PreChecked(format!("{:?}", c)),
            Box::new(stringify_conds(*b)),
        ),
        Loop(PostChecked(c), b) => Loop(
            PostChecked(format!("{:?}", c)),
            Box::new(stringify_conds(*b)),
        ),
        Loop(Endless, b) => Loop(Endless, Box::new(stringify_conds(*b))),
        Break => Break,
        Switch(v, cases, default) => Switch(
            v,
            cases
                .into_iter()
                .map(|(vs, a)| (vs, stringify_conds(a)))
                .collect(),
            Box::new(stringify_conds(*default)),
        ),
    }
}
