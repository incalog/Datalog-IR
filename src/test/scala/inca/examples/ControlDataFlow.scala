package inca.examples

object ControlDataFlow {
  val AST_code =
    s"""data Exp = Var(`String`) |
       |           Num(`Int`) |
       |           GreaterThan(Exp, Exp) |
       |           Mul(Exp, Exp) |
       |           Add(Exp, Exp) |
       |           Sub(Exp, Exp)
       |data Stm = Assign(`String`, Exp) | Skip() | Sequence(Stm, Stm) | If(Exp, Stm, Stm) | While(Exp, Stm)
       |""".stripMargin

  val initFunction =
    s"""def init(stm: Stm): Stm = stm match {
       |  case Assign(x, a) => stm
       |  case Skip() => stm
       |  case Sequence(s1, s2) => init(s1)
       |  case If(b, s1, s2) => stm
       |  case While(b, s) => stm
       |}
       |""".stripMargin

  val finalFunction =
    s"""def final(stm: Stm): Set[Stm] = stm match {
       |  case Assign(x, a) => {stm}
       |  case Skip() => {stm}
       |  case Sequence(s1, s2) => final(s2)
       |  case If(b, s1, s2) => final(s1) ++ final(s2)
       |  case While(b, s) => {stm}
       |}
       |""".stripMargin

  val flowFunction =
    s"""@main def flow(stm: Stm): Set[(Stm, Stm)] = stm match {
       |  case Assign(x, a) => {}
       |  case Skip() => {}
       |  case Sequence(s1, s2) => flow(s1) ++ flow(s2) ++ {(l1, init(s2)) | l1 in final(s1)}
       |  case If(c, s1, s2) => flow(s1) ++ flow(s2) ++ {(stm, init(s1)), (stm, init(s2))}
       |  case While(c, s) => flow(s) ++ {(stm, init(s))} ++ {(l,stm) | l in final(s)}
       |}
       |""".stripMargin

  val flowR =
    s"""@main def flowR(stm: Stm): Set[(Stm, Stm)] =
       |   {(l2, l1) | (l1, l2) in flow(stm)}
       |""".stripMargin

  val cflowModule = Code.module(
    AST_code,
    initFunction,
    finalFunction,
    flowFunction,
    flowR
  )

  val freevars =
    s"""def freevars(exp: Exp): Set[`String`] = exp match {
       |  case Var(s) => {s}
       |  case Num(i) => {}
       |  case GreaterThan(e1, e2) => freevars(e1) ++ freevars(e2)
       |  case Mul(e1, e2) => freevars(e1) ++ freevars(e2)
       |  case Add(e1, e2) => freevars(e1) ++ freevars(e2)
       |  case Sub(e1, e2) => freevars(e1) ++ freevars(e2)
       |}
       |@main def freevarsStm(stm: Stm): Set[`String`] = stm match {
       |  case Assign(x, a) => freevars(a) // weird, but in accordance with POPA
       |  case Skip() => {}
       |  case Sequence(s1, s2) => freevarsStm(s1) ++ freevarsStm(s2)
       |  case If(c, s1, s2) => freevars(c) ++ freevarsStm(s1) ++ freevarsStm(s2)
       |  case While(c, s) => freevars(c) ++ freevarsStm(s)
       |}
       |""".stripMargin

  val availableExpression =
    s"""
       |def AExp(exp: Exp): Set[Exp] = exp match {
       |  case Var(s) => {}
       |  case Num(i) => {}
       |  case GreaterThan(e1, e2) => AExp(e1) ++ AExp(e2)
       |  case Mul(e1, e2) => {exp} ++ AExp(e1) ++ AExp(e2)
       |  case Add(e1, e2) => {exp} ++ AExp(e1) ++ AExp(e2)
       |  case Sub(e1, e2) => {exp} ++ AExp(e1) ++ AExp(e2)
       |}
       |def AExpStm(stm: Stm): Set[Exp] = stm match {
       |  case Assign(x, a) => AExp(a)
       |  case Skip() => {}
       |  case Sequence(s1, s2) => AExpStm(s1) ++ AExpStm(s2)
       |  case If(c, s1, s2) => AExp(c) ++ AExpStm(s1) ++ AExpStm(s2)
       |  case While(c, s) => AExp(c) ++ AExpStm(s)
       |}
       |def kill_AE(stm: Stm, prog: Stm): Set[Exp] = stm match {
       |  case Assign(x, a) => {a2 | a2 in AExpStm(prog), x in freevars(a2)}
       |  case Skip() => {}
       |  case Sequence(s1, s2) => {}
       |  case If(c, s1, s2) => {}
       |  case While(c, s) => {}
       |}
       |def gen_AE(stm: Stm): Set[Exp] = stm match {
       |  case Assign(x, a) => {a2 | a2 in AExp(a), x not in freevars(a2)}
       |  case Skip() => {}
       |  case Sequence(s1, s2) => {}
       |  case If(c, s1, s2) => AExp(c)
       |  case While(c, s) => AExp(c)
       |}
       |
       |def entry_AE(stm: Stm, prog: Stm): Set[Exp] =
       |  if (stm == init(prog))
       |    {}
       |  else
       |    intersect({exit_AE(pred, prog) | (pred, stm) in flow(prog)})
       |
       |def exit_AE(stm: Stm, prog: Stm): Set[Exp] =
       |  gen_AE(stm) ++ {e | e in entry_AE(stm, prog), e not in kill_AE(stm, prog)}
       |
       |def intersect(sets: Set[Set[Exp]]): Set[Exp] = {}
       |
       |@main def final_AE(prog: Stm): Set[Exp] =
       |  {rd | s in final(prog), rd in exit_AE(s, prog)}
       |""".stripMargin
       // TODO how to implement intersection?

  val AEModule = Code.module(
    AST_code,
    initFunction,
    finalFunction,
    flowFunction,
    flowR,
    freevars,
    availableExpression
  )

  val reachingDefinitions =
    s"""
       |data MaybeDef = Undef() | Def(Stm)
       |
       |@main def assignments(stm: Stm, x: `String`): Set[Stm] = stm match {
       |  case Assign(y, a) => if (x == y) {stm} else {}
       |  case Skip() => {}
       |  case Sequence(s1, s2) => assignments(s1, x) ++ assignments(s2, x)
       |  case If(c, s1, s2) => assignments(s1, x) ++ assignments(s2, x)
       |  case While(c, s) => assignments(s, x)
       |}
       |def kill_RD(stm: Stm, prog: Stm): Set[(`String`,MaybeDef)] = stm match {
       |  case Assign(x, a) => {(x,Undef())} ++ {(x, Def(s)) | s in assignments(prog, x)}
       |  case Skip() => {}
       |  case Sequence(s1, s2) => {}
       |  case If(c, s1, s2) => {}
       |  case While(c, s) => {}
       |}
       |def gen_RD(stm: Stm): Set[(`String`,MaybeDef)] = stm match {
       |  case Assign(x, a) => {(x, Def(stm))}
       |  case Skip() => {}
       |  case Sequence(s1, s2) => {}
       |  case If(c, s1, s2) => {}
       |  case While(c, s) => {}
       |}
       |
       |def entry_RD(stm: Stm, prog: Stm): Set[(`String`,MaybeDef)] =
       |  if (stm == init(prog))
       |    {(x, Undef()) | x in freevarsStm(prog)}
       |  else
       |    {(x,s) | (pred, stm) in flow(prog), (x,s) in exit_RD(pred, prog)}
       |
       |def exit_RD(stm: Stm, prog: Stm): Set[(`String`,MaybeDef)] =
       |  gen_RD(stm) ++ {(r,d) | (r,d) in entry_RD(stm, prog), (r,d) not in kill_RD(stm, prog)}
       |
       |@main def final_RD(prog: Stm): Set[(`String`,MaybeDef)] =
       |  {(x,a) | s in final(prog), (x,a) in exit_RD(s, prog)}
       |
       |@main def killAll_RD(prog: Stm): Set[(Stm, `String`, MaybeDef)] =
       |  {(s, x, d) | s in Stm, (x, d) in kill_RD(s, prog)}
       |@main def genAll_RD(prog: Stm): Set[(Stm, `String`, MaybeDef)] =
       |  {(s, x, d) | s in Stm, (x, d) in gen_RD(s)}
       |@main def entryAll_RD(prog: Stm): Set[(Stm, `String`, MaybeDef)] =
       |  {(s, x, d) | s in Stm, (x, d) in entry_RD(s, prog)}
       |@main def exitAll_RD(prog: Stm): Set[(Stm, `String`, MaybeDef)] =
       |  {(s, x, d) | s in Stm, (x, d) in exit_RD(s, prog)}
       |""".stripMargin

  val RDmodule = Code.module(
    AST_code,
    initFunction,
    finalFunction,
    flowFunction,
    flowR,
    freevars,
    reachingDefinitions
  )


  import scala.meta.XtensionQuasiquoteTerm

  // Example 2.1 from POPA, page 37
  val example_2_1 =
    q"""Sequence(
          Assign("z", Num(1)),
          While(GreaterThan(Var("x"), Num(0)),
            Sequence(
              Assign("z", Mul(Var("z"), Var("y"))),
              Assign("x", Sub(Var("x"), Num(-1))))))
       """

  // Example 2.4 from POPA, page 39
  val example_2_4 =
    q"""Sequence(
          Assign("x", Add(Var("a"), Var("b"))),
          Sequence(
            Assign("y", Mul(Var("a"), Var("b"))),
            While(GreaterThan(Var("y"), Add(Var("a"), Var("b"))),
              Sequence(
                Assign("a", Add(Var("a"), Num(1))),
                Assign("x", Add(Var("a"), Var("b")))))))
       """

  // Example 2.7 from POPA, page 45
  val example_2_7 =
    q"""Sequence(
          Assign("x", Num(5)),
          Sequence(
            Assign("y", Num(1)),
            While(GreaterThan(Var("x"), Num(1)),
              Sequence(
                Assign("y", Mul(Var("x"), Var("y"))),
                Assign("x", Sub(Var("x"), Num(1)))))))
       """

}
