package inca.examples.functional

object ControlDataFlow {

  def module(strs: String*): String =
    s"""module Module
       |${strs.mkString("\n")}
       |""".stripMargin

  // Had to implement new version of ADT freevars because Sub and Mul at not present in this version
  def parametricIntValues(bound: Int, default: Int): String =
    s"""
      |data Exp = Var(String) | Num(Int) | GreaterThan(Exp, Exp) | Add(Exp, Exp)
      |data Stm = Assign(String, Exp) | Skip() | Sequence(Stm, Stm) | If(Exp, Stm, Stm) | While(Exp, Stm)
      |
      |def init(stm: Stm): Stm = stm match {
      |  case Assign(x, a) => stm
      |  case Skip() => stm
      |  case Sequence(s1, s2) => init(s1)
      |  case If(b, s1, s2) => stm
      |  case While(b, s) => stm
      |}
      |def final(stm: Stm): Set[Stm] = stm match {
      |  case Assign(x, a) => {stm}
      |  case Skip() => {stm}
      |  case Sequence(s1, s2) => final(s2)
      |  case If(b, s1, s2) => final(s1) ++ final(s2)
      |  case While(b, s) => {stm}
      |}
      |
      |def flow(stm: Stm): Set[(Stm, Stm)] = stm match {
      |  case Assign(x, a) => {}
      |  case Skip() => {}
      |  case Sequence(s1, s2) => flow(s1) ++ flow(s2) ++ {(l1, init(s2)) | l1 in final(s1)}
      |  case If(c, s1, s2) => flow(s1) ++ flow(s2) ++ {(stm, init(s1)), (stm, init(s2))}
      |  case While(c, s) => flow(s) ++ {(stm, init(s))} ++ {(l,stm) | l in final(s)}
      |}
      |
      |def freevars(exp: Exp): Set[String] = exp match {
      |  case Var(s) => {s}
      |  case Num(i) => {}
      |  case GreaterThan(e1, e2) => freevars(e1) ++ freevars(e2)
      |  case Add(e1, e2) => freevars(e1) ++ freevars(e2)
      |}
      |
      |def freevarsStm(stm: Stm): Set[String] = stm match {
      |  case Assign(x, a) => freevars(a) // weird, but in accordance with POPA
      |  case Skip() => {}
      |  case Sequence(s1, s2) => freevarsStm(s1) ++ freevarsStm(s2)
      |  case If(c, s1, s2) => freevars(c) ++ freevarsStm(s1) ++ freevarsStm(s2)
      |  case While(c, s) => freevars(c) ++ freevarsStm(s)
      |}
      |
      |data Val = VBool(Boolean) | VNum(Int)
      |
      |def entry_var(stm: Stm, prog: Stm, x: String): Set[Val] =
      |  {v | (pred,stm) in flow(prog), v in exit_var(pred, prog, x)}
      |
      |def exit_var(stm: Stm, prog: Stm, x: String): Set[Val] = stm match {
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
      |@main def final_var(prog: Stm): Set[(String, Val)] =
      |  {(x, v) | s in final(prog), x in freevarsStm(prog), v in exit_var(s, prog, x)}
      |
      |def aeval(exp: Exp, node: Stm, prog: Stm): Set[Val] = exp match {
      |  case Num(i) => {VNum(i)}
      |  case Var(x) => entry_var(node, prog, x)
      |  case GreaterThan(e1, e2) => {greaterThan(v1, v2) | v1 in aeval(e1, node, prog), v2 in aeval(e2, node, prog)}
      |  case Add(e1, e2) => {add(v1, v2) | v1 in aeval(e1, node, prog), v2 in aeval(e2, node, prog)}
      |}
      |
      |def greaterThan(v1: Val, v2: Val): Val = v1 match {
      |  case VNum(n1) => v2 match {
      |    case VNum(n2) => VBool(n1 > n2)
      |    case VBool(b2) => VBool(false)
      |  }
      |  case VBool(b1) => VBool(false)
      |}
      |
      |def add(v1: Val, v2: Val): Val = v1 match {
      |  case VNum(n1) => v2 match {
      |    case VNum(n2) =>
      |      if ((n1 + n2) <= -${bound}) VNum(-${default})
      |      else if ((n1 + n2) >= ${bound}) VNum(${default})
      |      else VNum(n1 + n2)
      |    case VBool(b2) => VBool(false)
      |  }
      |  case VBool(b1) => VBool(false)
      |}
      |""".stripMargin
  def intValues: String = parametricIntValues(100, 1000)
  def ParametricIntValuesModule(bound: Int, default: Int): String = module(parametricIntValues(bound, default))

  def parametricIntValuesDEBUG(bound: Int, default: Int): String =
    s"""
       |data Exp = Var(String) | Num(Int) | GreaterThan(Exp, Exp) | Add(Exp, Exp)
       |data Stm = Assign(String, Exp) | Skip() | Sequence(Stm, Stm) | If(Exp, Stm, Stm) | While(Exp, Stm)
       |data MyInt = Zero() | Succ(MyInt)
       |
       |def init(stm: Stm): Stm = stm match {
       |  case Assign(x, a) => stm
       |  case Skip() => stm
       |  case Sequence(s1, s2) => init(s1)
       |  case If(b, s1, s2) => stm
       |  case While(b, s) => stm
       |}
       |def final(stm: Stm): Set[Stm] = stm match {
       |  case Assign(x, a) => {stm}
       |  case Skip() => {stm}
       |  case Sequence(s1, s2) => final(s2)
       |  case If(b, s1, s2) => final(s1) ++ final(s2)
       |  case While(b, s) => {stm}
       |}
       |
       |def flow(stm: Stm): Set[(Stm, Stm)] = stm match {
       |  case Assign(x, a) => {}
       |  case Skip() => {}
       |  case Sequence(s1, s2) => flow(s1) ++ flow(s2) ++ {(l1, init(s2)) | l1 in final(s1)}
       |  case If(c, s1, s2) => flow(s1) ++ flow(s2) ++ {(stm, init(s1)), (stm, init(s2))}
       |  case While(c, s) => flow(s) ++ {(stm, init(s))} ++ {(l,stm) | l in final(s)}
       |}
       |
       |def freevars(exp: Exp): Set[String] = exp match {
       |  case Var(s) => {s}
       |  case Num(i) => {}
       |  case GreaterThan(e1, e2) => freevars(e1) ++ freevars(e2)
       |  case Add(e1, e2) => freevars(e1) ++ freevars(e2)
       |}
       |
       |def freevarsStm(stm: Stm): Set[String] = stm match {
       |  case Assign(x, a) => freevars(a) // weird, but in accordance with POPA
       |  case Skip() => {}
       |  case Sequence(s1, s2) => freevarsStm(s1) ++ freevarsStm(s2)
       |  case If(c, s1, s2) => freevars(c) ++ freevarsStm(s1) ++ freevarsStm(s2)
       |  case While(c, s) => freevars(c) ++ freevarsStm(s)
       |}
       |
       |data Val = VBool(Boolean) | VNum(MyInt)
       |
       |def entry_var(stm: Stm, prog: Stm, x: String): Set[Val] =
       |  {v | (pred,stm) in flow(prog), v in exit_var(pred, prog, x)}
       |
       |def exit_var(stm: Stm, prog: Stm, x: String): Set[Val] = stm match {
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
       |@main def final_var(prog: Stm): Set[(String, Val)] =
       |  {(x, v) | s in final(prog), x in freevarsStm(prog), v in exit_var(s, prog, x)}
       |
       |def aeval(exp: Exp, node: Stm, prog: Stm): Set[Val] = exp match {
       |  case Num(i) => {VNum(Zero())}
       |  case Var(x) => entry_var(node, prog, x)
       |  case GreaterThan(e1, e2) => {greaterThan(v1, v2) | v1 in aeval(e1, node, prog), v2 in aeval(e2, node, prog)}
       |  case Add(e1, e2) => {add(v1, v2) | v1 in aeval(e1, node, prog), v2 in aeval(e2, node, prog)}
       |}
       |
       |def greaterThan(v1: Val, v2: Val): Val = v1 match {
       |  case VNum(n1) => v2 match {
       |    case VNum(n2) => n1 match {
       |      case Zero() => VBool(false)
       |      case Succ(i1) => n2 match {
       |        case Zero() => VBool(true)
       |        case Succ(i2) => VBool(false)
       |      }
       |    }
       |    case VBool(b2) => VBool(false)
       |  }
       |  case VBool(b1) => VBool(false)
       |}
       |
       |def add(v1: Val, v2: Val): Val = v1 match {
       |  case VNum(n1) => v2 match {
       |    case VNum(n2) => n1 match {
       |      case Zero() => n2 match {
       |        case Zero() =>
       |          VNum(Succ(Zero()))
       |        case Succ(i2) =>
       |          VNum(Succ(Zero()))
       |      }
       |      case Succ(i1) => n2 match {
       |        case Zero() =>
       |          VNum(Succ(Zero()))
       |        case Succ(i2) =>
       |          VNum(Succ(Succ(Zero())))
       |      }
       |    }
       |    case VBool(b2) => VBool(false)
       |  }
       |  case VBool(b1) => VBool(false)
       |}
       |""".stripMargin
  def intValuesDEBUG: String = parametricIntValuesDEBUG(100, 1000)
  def ParametricIntValuesModuleDEBUG(bound: Int, default: Int): String = module(parametricIntValuesDEBUG(bound, default))


  def parametricIntValuesAGGREGATE(bound: Int, default: Int): String =
    s"""
       |data Exp = Var(String) | Num(Int) | GreaterThan(Exp, Exp) | Add(Exp, Exp)
       |data Stm = Assign(String, Exp) | Skip() | Sequence(Stm, Stm) | If(Exp, Stm, Stm) | While(Exp, Stm)
       |
       |def init(stm: Stm): Stm = stm match {
       |  case Assign(x, a) => stm
       |  case Skip() => stm
       |  case Sequence(s1, s2) => init(s1)
       |  case If(b, s1, s2) => stm
       |  case While(b, s) => stm
       |}
       |def final(stm: Stm): Set[Stm] = stm match {
       |  case Assign(x, a) => {stm}
       |  case Skip() => {stm}
       |  case Sequence(s1, s2) => final(s2)
       |  case If(b, s1, s2) => final(s1) ++ final(s2)
       |  case While(b, s) => {stm}
       |}
       |
       |def flow(stm: Stm): Set[(Stm, Stm)] = stm match {
       |  case Assign(x, a) => {}
       |  case Skip() => {}
       |  case Sequence(s1, s2) => flow(s1) ++ flow(s2) ++ {(l1, init(s2)) | l1 in final(s1)}
       |  case If(c, s1, s2) => flow(s1) ++ flow(s2) ++ {(stm, init(s1)), (stm, init(s2))}
       |  case While(c, s) => flow(s) ++ {(stm, init(s))} ++ {(l,stm) | l in final(s)}
       |}
       |
       |def freevars(exp: Exp): Set[String] = exp match {
       |  case Var(s) => {s}
       |  case Num(i) => {}
       |  case GreaterThan(e1, e2) => freevars(e1) ++ freevars(e2)
       |  case Add(e1, e2) => freevars(e1) ++ freevars(e2)
       |}
       |
       |def freevarsStm(stm: Stm): Set[String] = stm match {
       |  case Assign(x, a) => freevars(a) // weird, but in accordance with POPA
       |  case Skip() => {}
       |  case Sequence(s1, s2) => freevarsStm(s1) ++ freevarsStm(s2)
       |  case If(c, s1, s2) => freevars(c) ++ freevarsStm(s1) ++ freevarsStm(s2)
       |  case While(c, s) => freevars(c) ++ freevarsStm(s)
       |}
       |
       |data Val = VBool(Boolean) | VNum(Int)
       |
       |def join(v1: Val, v2: Val): Val = v1 match {
       |  case VNum(n1) => v2 match {
       |    case VNum(n2) => VNum(`Math.max`(n1, n2))
       |    case VBool(b2) => VBool(b2)
       |  }
       |  case VBool(b1) => VBool(b1)
       |}
       |
       |def entry_var(stm: Stm, prog: Stm, x: String): Val =
       |  fold(VNum(0), join, {exit_var(pred, prog, x) | (pred,stm) in flow(prog)})
       |
       |def exit_var(stm: Stm, prog: Stm, x: String): Val = stm match {
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
       |@main def final_var(prog: Stm): Set[(String, Val)] =
       |  {(x, exit_var(s, prog, x)) | s in final(prog), x in freevarsStm(prog)}
       |
       |def aeval(exp: Exp, node: Stm, prog: Stm): Val = exp match {
       |  case Num(i) => VNum(i)
       |  case Var(x) => entry_var(node, prog, x)
       |  case GreaterThan(e1, e2) => greaterThan(aeval(e1, node, prog), aeval(e2, node, prog))
       |  case Add(e1, e2) => add(aeval(e1, node, prog), aeval(e2, node, prog))
       |}
       |
       |def greaterThan(v1: Val, v2: Val): Val = v1 match {
       |  case VNum(n1) => v2 match {
       |    case VNum(n2) => VBool(n1 > n2)
       |    case VBool(b2) => VBool(false)
       |  }
       |  case VBool(b1) => VBool(false)
       |}
       |
       |def add(v1: Val, v2: Val): Val = v1 match {
       |  case VNum(n1) => v2 match {
       |    case VNum(n2) =>
       |      if ((n1 + n2) <= -${bound}) VNum(-${default})
       |      else if ((n1 + n2) >= ${bound}) VNum(${default})
       |      else VNum(n1 + n2)
       |    case VBool(b2) => VBool(false)
       |  }
       |  case VBool(b1) => VBool(false)
       |}
       |""".stripMargin
  def intValuesAGGREGATE: String = parametricIntValuesAGGREGATE(100, 1000)
  def ParametricIntValuesModuleAGGREGATE(bound: Int, default: Int): String = module(parametricIntValuesAGGREGATE(bound, default))


  def IntValuesModule: String = module(intValues)

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

  /*
    x = 2
    y = 2
    while(x > 1) {
      y = x + y
      x = x + 2
    }
   */
  val exampleDataflow1 =
    q"""Sequence(
          Assign("x", Num(2)),
          Sequence(
            Assign("y", Num(2)),
            While(GreaterThan(Var("x"), Num(1)),
              Sequence(
                Assign("y", Add(Var("x"), Var("y"))),
                Sequence(
                  Skip(),
                  Assign("x", Add(Var("x"), Num(2))))))))
       """

  // lhs of assign in while loop adds + 1
  // y = x + y -> y = (x + y) + 1
  val exampleDataflow1Change1 =
    q"""Sequence(
          Assign("x", Num(2)),
          Sequence(
            Assign("y", Num(2)),
            While(GreaterThan(Var("x"), Num(1)),
              Sequence(
                Assign("y", Add(Add(Var("x"), Var("y")), Num(1))),
                Sequence(
                  Skip(),
                  Assign("x", Add(Var("x"), Num(2))))))))
       """
  // insert y = y after y = x + y
  val exampleDataflow1Change2 =
    q"""Sequence(
          Assign("x", Num(2)),
          Sequence(
            Assign("y", Num(2)),
            While(GreaterThan(Var("x"), Num(1)),
              Sequence(
                Sequence(
                  Assign("y", Add(Var("x"), Var("y"))),
                  Assign("y", Var("y"))
                ),
                Sequence(
                  Skip(),
                  Assign("x", Add(Var("x"), Num(2))))))))
       """

  // change initial assignment of x to 3 instead of 2

  val exampleDataflow1Change3 =
    q"""Sequence(
          Assign("x", Num(3)),
          Sequence(
            Assign("y", Num(2)),
            While(GreaterThan(Var("x"), Num(1)),
              Sequence(
                Assign("y", Add(Var("x"), Var("y"))),
                Sequence(
                  Skip(),
                  Assign("x", Add(Var("x"), Num(2))))))))
       """

  // sub millisecond update time
  // introduce new var before while
  val exampleDataflow1Change4 =
    q"""Sequence(
          Sequence(
            Assign("z", Num(1)),
            Assign("x", Num(2)),
          ),
          Sequence(
            Assign("y", Num(2)),
            While(GreaterThan(Var("x"), Num(1)),
              Sequence(
                Assign("y", Add(Var("x"), Var("y"))),
                Sequence(
                  Skip(),
                  Assign("x", Add(Var("x"), Num(2))))))))
       """

  // update times around 5-8 ms
  // introduce var that is static in loop
  val exampleDataflow1Change5 =
    q"""Sequence(
          Assign("x", Num(2)),
          Sequence(
            Assign("y", Num(2)),
            While(GreaterThan(Var("x"), Num(1)),
              Sequence(
                Assign("y", Add(Var("x"), Var("y"))),
                Sequence(
                  Assign("z", Num(12)),
                  Assign("x", Add(Var("x"), Num(2))))))))
       """

  // update times around 10-13 ms
  // introduce var in loop that changes it value each iteration
  val exampleDataflow1Change6 =
  q"""Sequence(
          Assign("x", Num(2)),
          Sequence(
            Assign("y", Num(2)),
            While(GreaterThan(Var("x"), Num(1)),
              Sequence(
                Assign("y", Add(Var("x"), Var("y"))),
                Sequence(
                  Assign("z", Add(Var("y"), Num(3))),
                  Assign("x", Add(Var("x"), Num(2))))))))
       """
  // add assign after while
  // x = x + 12
  // takes > 200 ms
  val exampleDataflow1Change7 =
    q"""Sequence(
          Assign("x", Num(2)),
          Sequence(
            Sequence(
              Assign("y", Num(2)),
              While(GreaterThan(Var("x"), Num(1)),
                Sequence(
                  Assign("y", Add(Var("x"), Var("y"))),
                  Sequence(
                    Skip(),
                    Assign("x", Add(Var("x"), Num(2))))))),
            Assign("x", Add(Var("x"), Num(12)))))
       """
  /*
    x = 1
    y = 2
    z = 3
    if (3 > x) {
      x = 6
    } else {
      y = 7
    }
    while (10 > x) {
      y = y + 1
    }
    while(x > 6) {
      x = x + y
      while (y > 7) {
        y = x + y
        while (z > 2) {
          z = 2
        }
      }
    }
   */
  val exampleDataflow2 =
    q"""Sequence(
          Assign("x", Num(1)),
          Sequence(
            Assign("y", Num(2)),
            Sequence(
              Assign("z", Num(3)),
              Sequence(
                If(
                  GreaterThan(Num(3), Var("x")),
                  Assign("x", Num(6)),
                  Assign("y", Num(7))),
                Sequence(
                  While(
                    GreaterThan(Num(10), Var("x")),
                    Assign("y", Add(Var("y"), Num(1)))),
                  While(
                    GreaterThan(Var("x"), Num(6)),
                    Sequence(
                      Assign("x", Add(Var("x"), Var("y"))),
                      While(
                        GreaterThan(Var("y"), Num(7)),
                        Sequence(
                          Assign("y", Add(Var("x"), Var("y"))),
                          While(
                            GreaterThan(Var("z"), Num(2)),
                            Assign("z", Num(2)))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
       """

  /*
    x = 10
    y = 1
    z = 4
    if (x > 1) {
      y = 2
      while (z > 2) {
        z = z + y
      }
    } else {
      y = 3
      while (z > 2) {
        z = z + y
      }
      while (z > x) {
        x = x + 100
      }
    }
    while (x > y) {
      x = x + z
    }
   */

  val exampleDataflow3 = q"""
    Sequence(
      Assign("x", Num(10)),
      Sequence(
        Assign("y", Num(1)),
        Sequence(
          Assign("z", Num(4)),
          Sequence(
            If(
              GreaterThan(Var("x"), Num(1)),
              Sequence(
                Assign("y", Num(2)),
                While(
                  GreaterThan(Var("z"), Num(2)),
                  Assign("z", Add(Var("z"), Var("y")))
                )
              ),
              Sequence(
                Assign("y", Num(3)),
                Sequence(
                  While(
                    GreaterThan(Var("z"), Num(2)),
                    Assign("z", Add(Var("z"), Var("y")))
                  ),
                  While(
                    GreaterThan(Var("z"), Var("x")),
                    Assign("x", Add(Var("x"), Num(100)))
                  )
                )
              )
            ),
            While(
              GreaterThan(Var("x"), Var("y")),
              Assign("x", Add(Var("x"), Var("z")))
            )
          )
        )
      )
    )
    """

  /*
    x = 10
    y = 1
    while (10 > x) {
      x = x + 1
      while (x > y) {
        y = x + y
        while (x > 10) {
          y = y + y
          while (y > 10) {
            x = x + 1
          }
        }
      }
    }
   */
  val exampleDataflow4 = q"""
    Sequence(
      Assign("x", Num(10)),
      Sequence(
        Assign("y", Num(1)),
        While(
          GreaterThan(Num(10), Var("x")),
          Sequence(
            Assign("x", Add(Var("x"), Num(1))),
            While(
              GreaterThan(Var("x"), Var("y")),
              Sequence(
                Assign("y", Add(Var("x"), Var("y"))),
                While(
                  GreaterThan(Var("x"), Num(10)),
                  Sequence(
                    Assign("y", Add(Var("y"), Var("y"))),
                    While(
                      GreaterThan(Var("y"), Num(10)),
                      Assign("x", Add(Var("x"), Num(1)))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    """


  /*
    x = 0
    while (x < 1000) {
      x = x + 1
      y = 0
      while (y < 1000) {
        y = y + 1
      }
    }
   */
  val exampleDataflow5 = q"""
    Sequence(
      Assign("x", Num(0)),
      While(
        GreaterThan(Num(1000), Var("x")),
        Sequence(
          Assign("x", Add(Var("x"), Num(1))),
          Sequence(
            Assign("y", Num(0)),
            While(
              GreaterThan(Num(1000), Var("y")),
              Assign("y", Add(Var("y"), Num(1))),
            )
          )
        )
      )
    )
    """

  /*
   x = 0
   while (x < 1000) {
     x = x + 1
     y = 0
     while (y < 1000) {
       y = y + 1
       z = 0
       while (z < 1000) {
         z = z + 1
       }
     }
   }
  */
  val exampleDataflow6 = q"""
    Sequence(
      Assign("x", Num(0)),
      While(
        GreaterThan(Num(1000), Var("x")),
        Sequence(
          Assign("x", Add(Var("x"), Num(1))),
          Sequence(
            Assign("y", Num(0)),
            While(
              GreaterThan(Num(1000), Var("y")),
              Sequence(
                Assign("y", Add(Var("y"), Num(1))),
                Sequence(
                  Assign("z", Num(0)),
                  While(
                    GreaterThan(Num(1000), Var("z")),
                    Assign("z", Add(Var("z"), Num(1))),
                  )
                )
              )
            )
          )
        )
      )
    )
    """

  val exampleDataflow7 =
    q"""
       Sequence(
         Assign("x", Num(99)),
         While(
           GreaterThan(Var("x"), Num(0)),
           Assign("x", Add(Var("x"), Num(1)))
         )
       )
      """
  val exampleDataflow7Change1 =
    q"""
       Sequence(
         Assign("x", Num(100)),
         While(
           GreaterThan(Var("x"), Num(0)),
           Assign("x", Add(Var("x"), Num(1)))
         )
       )
      """
  val exampleDataflow7Change2 =
    q"""
       Sequence(
         Assign("x", Num(98)),
         While(
           GreaterThan(Var("x"), Num(0)),
           Assign("x", Add(Var("x"), Num(1)))
         )
       )
      """

  val exampleDataflow8 =
    q"""
       Sequence(
         Assign("x", Num(1)),
         While(
           GreaterThan(Var("x"), Num(0)),
           Assign("x", Add(Var("x"), Num(1)))
         )
       )
      """
  val exampleDataflow8Change1 =
    q"""
       Sequence(
         Assign("x", Num(2)),
         While(
           GreaterThan(Var("x"), Num(0)),
           Assign("x", Add(Var("x"), Num(1)))
         )
       )
      """
  val exampleDataflow8Change2 =
    q"""
       Sequence(
         Assign("x", Num(0)),
         While(
           GreaterThan(Var("x"), Num(0)),
           Assign("x", Add(Var("x"), Num(1)))
         )
       )
      """
  val exampleDataflow8Change3 =
    q"""
       Sequence(
         Assign("x", Num(1)),
         While(
           GreaterThan(Var("x"), Num(0)),
           Sequence(
             Assign("x", Add(Var("x"), Num(1))),
             Skip()
           )
         )
       )
      """
  val exampleDataflow8Change4 =
    q"""
       Sequence(
         Assign("x", Num(1)),
         While(
           GreaterThan(Var("x"), Num(0)),
           Sequence(
             Skip(),
             Assign("x", Add(Var("x"), Num(1)))
           )
         )
       )
      """

}
