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
       |
       |def transitive(cfg: () => Set[(Stm, Stm)]): Set[(Stm, Stm)] =
       |  cfg() ++ {(s1,s3) | (s1,s2) in cfg(), (s2,s3) in transitive(cfg)}
       |
       |@main def transitiveFlow(prog: Stm): Set[(Stm, Stm)] =
       |  let cfg = () => flow(prog)
       |  in transitive(cfg)
       |
       |""".stripMargin

  val flowR =
    s"""@main def flowR(stm: Stm): Set[(Stm, Stm)] =
       |   {(l2, l1) | (l1, l2) in flow(stm)}
       |
       |def flowReverse(prog: Stm): Set[(Stm, Stm)] = {(l2, l1) | (l1, l2) in flow(prog)}
       |def flowTransitive(prog: Stm): Set[(Stm, Stm)] =
       |  flow(prog) ++ {(l1, l3) | (l1, l2) in flow(prog), (l2, l3) in flowTransitive(prog)}
       |def flowCyclic(prog: Stm): Set[Stm] = {l | (l, l) in flowTransitive(prog)}
       |
       |""".stripMargin

  val cflowModule = Code.module(
    AST_code,
    initFunction,
    finalFunction,
    flowFunction,
    flowR
  )

  val freevars =
    s"""
       |def findExps(exp: Exp, f: Exp => `Boolean`): Set[Exp] = (exp match {
       |  case Var(s) => {}
       |  case Num(i) => {}
       |  case GreaterThan(e1, e2) => findExps(e1, f) ++ findExps(e2, f)
       |  case Mul(e1, e2) => findExps(e1, f) ++ findExps(e2, f)
       |  case Add(e1, e2) => findExps(e1, f) ++ findExps(e2, f)
       |  case Sub(e1, e2) => findExps(e1, f) ++ findExps(e2, f)
       |}) ++ (if (f(exp)) {exp} else {})
       |
       |def isVar(exp: Exp): `Boolean` = exp match {
       |  case Var(s) => true
       |  case Num(i) => false
       |  case GreaterThan(e1, e2) => false
       |  case Mul(e1, e2) => false
       |  case Add(e1, e2) => false
       |  case Sub(e1, e2) => false
       |}
       |
       |def varName(exp: Exp): `String` = exp match {
       |  case Var(s) => s
       |  case Num(i) => ""
       |  case GreaterThan(e1, e2) => ""
       |  case Mul(e1, e2) => ""
       |  case Add(e1, e2) => ""
       |  case Sub(e1, e2) => ""
       |}
       |
       |def freevars(exp: Exp): Set[`String`] =
       |  {varName(e) | e in findExps(exp, isVar)}
       |
//       |def freevars(exp: Exp): Set[`String`] = exp match {
//       |  case Var(s) => {s}
//       |  case Num(i) => {}
//       |  case GreaterThan(e1, e2) => freevars(e1) ++ freevars(e2)
//       |  case Mul(e1, e2) => freevars(e1) ++ freevars(e2)
//       |  case Add(e1, e2) => freevars(e1) ++ freevars(e2)
//       |  case Sub(e1, e2) => freevars(e1) ++ freevars(e2)
//       |}
       |def notFreeIn(x: `String`, exp: Exp): `Boolean` = exp match {
       |  case Var(s) => x != s
       |  case Num(i) => true
       |  case GreaterThan(e1, e2) => notFreeIn(x, e1) && notFreeIn(x, e2)
       |  case Mul(e1, e2) => notFreeIn(x, e1) && notFreeIn(x, e2)
       |  case Add(e1, e2) => notFreeIn(x, e1) && notFreeIn(x, e2)
       |  case Sub(e1, e2) => notFreeIn(x, e1) && notFreeIn(x, e2)
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
       |def availableExps(exp: Exp): Set[Exp] = findExps(exp, (e: Exp) => e match {
       |  case Var(s) => false
       |  case Num(i) => false
       |  case GreaterThan(e1, e2) => false
       |  case Mul(e1, e2) => true
       |  case Add(e1, e2) => true
       |  case Sub(e1, e2) => true
       |})
       |def AExpStm(stm: Stm): Set[Exp] = stm match {
       |  case Assign(x, a) => availableExps(a)
       |  case Skip() => {}
       |  case Sequence(s1, s2) => AExpStm(s1) ++ AExpStm(s2)
       |  case If(c, s1, s2) => availableExps(c) ++ AExpStm(s1) ++ AExpStm(s2)
       |  case While(c, s) => availableExps(c) ++ AExpStm(s)
       |}
       |def retain_AE(stm: Stm, prog: Stm, e: Exp): `Boolean` = stm match {
       |  case Assign(x, a) => notFreeIn(x, e)
       |  case Skip() => `true`
       |  case Sequence(s1, s2) => `true`
       |  case If(c, s1, s2) => `true`
       |  case While(c, s) => `true`
       |}
       |def gen_AE(stm: Stm): Set[Exp] = stm match {
       |  case Assign(x, a) => {a2 | a2 in availableExps(a), notFreeIn(x, a2)}
       |  case Skip() => {}
       |  case Sequence(s1, s2) => {}
       |  case If(c, s1, s2) => availableExps(c)
       |  case While(c, s) => availableExps(c)
       |}
       |
       |def intersect(s1: () => Set[Exp], s2: () => Set[Exp]): () => Set[Exp] =
       |  () => {e | e in s1(), e in s2()}
       |
       |def entry_AE(stm: Stm, prog: Stm): Set[Exp] =
       |  if (stm == init(prog))
       |    {}
       |  else
       |    fold(
       |      () => {e | e in Exp},
       |      intersect,
       |      {() => exit_AE(pred, prog) | (pred, stm) in flow(prog)}
       |    )()
       |
       |def exit_AE(stm: Stm, prog: Stm): Set[Exp] =
       |  gen_AE(stm) ++ {e | e in entry_AE(stm, prog), retain_AE(stm, prog, e)}
       |
       |@main def final_AE(prog: Stm): Set[Exp] =
       |  {rd | s in final(prog), rd in exit_AE(s, prog)}
       |@main def allEntries_AE(prog: Stm): Set[(Stm, Exp)] =
       |  {(s, e) | s in Stm, e in entry_AE(s, prog)}
       |@main def allExits_AE(prog: Stm): Set[(Stm, Exp)] =
       |  {(s, e) | s in Stm, e in exit_AE(s, prog)}
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
       |def retain_RD(stm: Stm, x: `String`): `Boolean` = stm match {
       |  case Assign(y, a) => x != y
       |  case Skip() => true
       |  case Sequence(s1, s2) => true
       |  case If(c, s1, s2) => true
       |  case While(c, s) => true
       |}
       |
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
       |    {(x,d) | (pred, stm) in flow(prog), (x,d) in exit_RD(pred, prog)}
       |
       |def exit_RD(stm: Stm, prog: Stm): Set[(`String`,MaybeDef)] =
       |  gen_RD(stm) ++ {(x,d) | (x,d) in entry_RD(stm, prog), retain_RD(stm, x)}
       |
       |@main def final_RD(prog: Stm): Set[(`String`,MaybeDef)] =
       |  {(x,a) | s in final(prog), (x,a) in exit_RD(s, prog)}
       |@main def allEntries_RD(prog: Stm): Set[(Stm, `String`, MaybeDef)] =
       |  {(s, x, d) | s in Stm, (x, d) in entry_RD(s, prog)}
       |@main def allExits_RD(prog: Stm): Set[(Stm, `String`, MaybeDef)] =
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

  val intervals =
    """data Interval = IV(`Int`, `Int`) | TopInterval()
      |data Bool = True() | False() | TopBool()
      |data Val = BotVal() | IntervalVal(Interval) | BoolVal(Bool) | TopVal()
      |
      |def joinVal(v1: Val, v2: Val): Val = v1 match {
      |  case BotVal() => v2
      |  case IntervalVal(iv1) => v2 match {
      |    case BotVal() => v1
      |    case IntervalVal(iv2) => IntervalVal(joinInterval(iv1, iv2))
      |    case BoolVal(b2) => TopVal()
      |    case TopVal() => TopVal()
      |  }
      |  case BoolVal(b1) => v2 match {
      |    case BotVal() => v1
      |    case IntervalVal(iv2) => TopVal()
      |    case BoolVal(b2) => BoolVal(joinBool(b1, b2))
      |    case TopVal() => TopVal()
      |  }
      |  case TopVal() => TopVal()
      |}
      |def joinInterval(iv1: Interval, iv2: Interval): Interval = iv1 match {
      |  case TopInterval() => TopInterval()
      |  case IV(l1, h1) => iv2 match {
      |    case TopInterval() => TopInterval()
      |    case IV(l2, h2) => widenInterval(IV(`Math.min`(l1, l2), `Math.max`(h1, h2)))
      |  }
      |}
      |def widenInterval(iv: Interval): Interval = iv match {
      |  case TopInterval() => TopInterval()
      |  case IV(l, h) =>
      |    if (`Math.abs`(h - l) <= 10)
      |      iv
      |    else
      |      TopInterval()
      |}
      |def joinBool(b1: Bool, b2: Bool): Bool = b1 match {
      |  case True() => b2 match {
      |    case True() => True()
      |    case False() => TopBool()
      |    case TopBool() => TopBool()
      |  }
      |  case False() => b2 match {
      |    case True() => TopBool()
      |    case False() => False()
      |    case TopBool() => TopBool()
      |  }
      |  case TopBool() => TopBool()
      |}
      |
      |def entry_var(stm: Stm, prog: Stm, x: `String`): Val =
      |  fold(BotVal(), joinVal,
      |    {exit_var(pred, prog, x) | (pred,stm) in flow(prog)})
      |
      |def exit_var(stm: Stm, prog: Stm, x: `String`): Val = stm match {
      |  case Assign(y, exp) =>
      |    if (x == y)
      |      aeval(exp, stm, prog)
      |    else
      |      entry_var(stm, prog, x)
      |  case Skip() => entry_var(stm, prog, x)
      |  case Sequence(s1, s2) => entry_var(stm, prog, x)
      |  case If(c, s1, s2) => entry_var(stm, prog, x)
      |  case While(c, s) => entry_var(stm, prog, x)
      |}
      |
      |def exit_var_external(stm: Stm, prog: Stm, x: `String`): Val =
      |  fold(BotVal(), joinVal, {exit_var(stm, prog, x)})
      |@main def final_var(prog: Stm): Set[(`String`,Val)] =
      |  {(x, exit_var_external(s, prog, x)) | s in final(prog), x in freevarsStm(prog)}
      |
      |def aeval(exp: Exp, node: Stm, prog: Stm): Val = exp match {
      |  case Var(x) => entry_var(node, prog, x)
      |  case Num(i) => IntervalVal(IV(i, i))
      |  case GreaterThan(e1, e2) => greaterThan(aeval(e1, node, prog), aeval(e2, node, prog))
      |  case Add(e1, e2) => add(aeval(e1, node, prog), aeval(e2, node, prog))
      |  case Sub(e1, e2) => sub(aeval(e1, node, prog), aeval(e2, node, prog))
      |  case Mul(e1, e2) => mul(aeval(e1, node, prog), aeval(e2, node, prog))
      |}
      |
      |def greaterThan(v1: Val, v2: Val): Val = v1 match {
      |  case BotVal() => BotVal()
      |  case BoolVal(b1) => BotVal()
      |  case TopVal() => BoolVal(TopBool())
      |  case IntervalVal(iv1) => v2 match {
      |    case BotVal() => BotVal()
      |    case BoolVal(b2) => BotVal()
      |    case TopVal() => BoolVal(TopBool())
      |    case IntervalVal(iv2) => BoolVal(greaterThanInterval(iv1, iv2))
      |  }
      |}
      |def greaterThanInterval(iv1: Interval, iv2: Interval): Bool = iv1 match {
      |  case TopInterval() => TopBool()
      |  case IV(l1, h1) => iv2 match {
      |    case TopInterval() => TopBool()
      |    case IV(l2, h2) =>
      |      if (l1 > h2) True()
      |      else if (l2 > h1) False()
      |      else TopBool()
      |  }
      |}
      |def add(v1: Val, v2: Val): Val = v1 match {
      |  case BotVal() => BotVal()
      |  case BoolVal(b1) => BotVal()
      |  case TopVal() => IntervalVal(TopInterval())
      |  case IntervalVal(iv1) => v2 match {
      |    case BotVal() => BotVal()
      |    case BoolVal(b2) => BotVal()
      |    case TopVal() => IntervalVal(TopInterval())
      |    case IntervalVal(iv2) => IntervalVal(addInterval(iv1, iv2))
      |  }
      |}
      |def addInterval(iv1: Interval, iv2: Interval): Interval = iv1 match {
      |  case TopInterval() => TopInterval()
      |  case IV(l1, h1) => iv2 match {
      |    case TopInterval() => TopInterval()
      |    case IV(l2, h2) => IV(l1 + l2, h1 + h2)
      |  }
      |}
      |def sub(v1: Val, v2: Val): Val = v1 match {
      |  case BotVal() => BotVal()
      |  case BoolVal(b1) => BotVal()
      |  case TopVal() => IntervalVal(TopInterval())
      |  case IntervalVal(iv1) => v2 match {
      |    case BotVal() => BotVal()
      |    case BoolVal(b2) => BotVal()
      |    case TopVal() => IntervalVal(TopInterval())
      |    case IntervalVal(iv2) => IntervalVal(subInterval(iv1, iv2))
      |  }
      |}
      |def subInterval(iv1: Interval, iv2: Interval): Interval = iv1 match {
      |  case TopInterval() => TopInterval()
      |  case IV(l1, h1) => iv2 match {
      |    case TopInterval() => TopInterval()
      |    case IV(l2, h2) => IV(l1 - h2, h1 - l2)
      |  }
      |}
      |def mul(v1: Val, v2: Val): Val = v1 match {
      |  case BotVal() => BotVal()
      |  case BoolVal(b1) => BotVal()
      |  case TopVal() => IntervalVal(TopInterval())
      |  case IntervalVal(iv1) => v2 match {
      |    case BotVal() => BotVal()
      |    case BoolVal(b2) => BotVal()
      |    case TopVal() => IntervalVal(TopInterval())
      |    case IntervalVal(iv2) => IntervalVal(mulInterval(iv1, iv2))
      |  }
      |}
      |def mulInterval(iv1: Interval, iv2: Interval): Interval = iv1 match {
      |  case TopInterval() => TopInterval()
      |  case IV(l1, h1) => iv2 match {
      |    case TopInterval() => TopInterval()
      |    case IV(l2, h2) =>
      |      let v1 = l1 * l2 in
      |      let v2 = l1 * h2 in
      |      let v3 = h1 * l2 in
      |      let v4 = h1 * h2 in
      |      let low = `Math.min`(v1, `Math.min`(v2, `Math.min`(v3, v4))) in
      |      let high = `Math.max`(v1, `Math.max`(v2, `Math.max`(v3, v4))) in
      |      IV(low, high)
      |  }
      |}
      |""".stripMargin

  /*
        let v1 = l1 * l2 in
      let v2 = l1 * h2 in
      let v3 = h1 * l2 in
      let v4 = h1 * h2 in
      let low = `Math.min`(v1, `Math.min`(v2, `Math.min`(v3, v4))) in
      let high = `Math.max`(v1, `Math.max`(v2, `Math.max`(v3, v4))) in
      IV(low, high)

   */

  val IntervalModule = Code.module(
    AST_code,
    initFunction,
    finalFunction,
    flowFunction,
    flowR,
    freevars,
    intervals
  )

  val aeval =
    """data Interval = IV(`Int`, `Int`) | TopInterval()
      |data Bool = True() | False() | TopBool()
      |data Val = BotVal() | IntervalVal(Interval) | BoolVal(Bool) | TopVal()
      |
      |def entry_var(stm: Stm, prog: Stm, x: `String`): Val = TopVal()
      |
      |@main def aeval(exp: Exp, node: Stm, prog: Stm): Val = exp match {
      |  case Var(x) => entry_var(node, prog, x)
      |  case Num(i) => IntervalVal(IV(i, i))
      |  case GreaterThan(e1, e2) => greaterThan(aeval(e1, node, prog), aeval(e2, node, prog))
      |  case Add(e1, e2) => add(aeval(e1, node, prog), aeval(e2, node, prog))
      |  case Sub(e1, e2) => sub(aeval(e1, node, prog), aeval(e2, node, prog))
      |  case Mul(e1, e2) => mul(aeval(e1, node, prog), aeval(e2, node, prog))
      |}
      |
      |def greaterThan(v1: Val, v2: Val): Val = v1 match {
      |  case BotVal() => BotVal()
      |  case BoolVal(b1) => BotVal()
      |  case TopVal() => BoolVal(TopBool())
      |  case IntervalVal(iv1) => v2 match {
      |    case BotVal() => BotVal()
      |    case BoolVal(b2) => BotVal()
      |    case TopVal() => BoolVal(TopBool())
      |    case IntervalVal(iv2) => BoolVal(greaterThanInterval(iv1, iv2))
      |  }
      |}
      |def greaterThanInterval(iv1: Interval, iv2: Interval): Bool = iv1 match {
      |  case TopInterval() => TopBool()
      |  case IV(l1, h1) => iv2 match {
      |    case TopInterval() => TopBool()
      |    case IV(l2, h2) =>
      |      if (l1 > h2) True()
      |      else if (l2 > h1) False()
      |      else TopBool()
      |  }
      |}
      |def add(v1: Val, v2: Val): Val = v1 match {
      |  case BotVal() => BotVal()
      |  case BoolVal(b1) => BotVal()
      |  case TopVal() => IntervalVal(TopInterval())
      |  case IntervalVal(iv1) => v2 match {
      |    case BotVal() => BotVal()
      |    case BoolVal(b2) => BotVal()
      |    case TopVal() => IntervalVal(TopInterval())
      |    case IntervalVal(iv2) => IntervalVal(addInterval(iv1, iv2))
      |  }
      |}
      |def addInterval(iv1: Interval, iv2: Interval): Interval = iv1 match {
      |  case TopInterval() => TopInterval()
      |  case IV(l1, h1) => iv2 match {
      |    case TopInterval() => TopInterval()
      |    case IV(l2, h2) => IV(l1 + l2, h1 + h2)
      |  }
      |}
      |def sub(v1: Val, v2: Val): Val = v1 match {
      |  case BotVal() => BotVal()
      |  case BoolVal(b1) => BotVal()
      |  case TopVal() => IntervalVal(TopInterval())
      |  case IntervalVal(iv1) => v2 match {
      |    case BotVal() => BotVal()
      |    case BoolVal(b2) => BotVal()
      |    case TopVal() => IntervalVal(TopInterval())
      |    case IntervalVal(iv2) => IntervalVal(subInterval(iv1, iv2))
      |  }
      |}
      |def subInterval(iv1: Interval, iv2: Interval): Interval = iv1 match {
      |  case TopInterval() => TopInterval()
      |  case IV(l1, h1) => iv2 match {
      |    case TopInterval() => TopInterval()
      |    case IV(l2, h2) => IV(l1 - h2, h1 - l2)
      |  }
      |}
      |def mul(v1: Val, v2: Val): Val = v1 match {
      |  case BotVal() => BotVal()
      |  case BoolVal(b1) => BotVal()
      |  case TopVal() => IntervalVal(TopInterval())
      |  case IntervalVal(iv1) => v2 match {
      |    case BotVal() => BotVal()
      |    case BoolVal(b2) => BotVal()
      |    case TopVal() => IntervalVal(TopInterval())
      |    case IntervalVal(iv2) => IntervalVal(mulInterval(iv1, iv2))
      |  }
      |}
      |def mulInterval(iv1: Interval, iv2: Interval): Interval = iv1 match {
      |  case TopInterval() => TopInterval()
      |  case IV(l1, h1) => iv2 match {
      |    case TopInterval() => TopInterval()
      |    case IV(l2, h2) =>
      |      let v1 = l1 * l2 in
      |      let v2 = l1 * h2 in
      |      let v3 = h1 * l2 in
      |      let v4 = h1 * h2 in
      |      let low = `Math.min`(v1, `Math.min`(v2, `Math.min`(v3, v4))) in
      |      let high = `Math.max`(v1, `Math.max`(v2, `Math.max`(v3, v4))) in
      |      IV(low, high)
      |  }
      |}
      |""".stripMargin

  val AEvalModule = Code.module(
    AST_code,
    aeval
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
