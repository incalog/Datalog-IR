package inca.examples

import inca.examples.Code.module

object CFlow {
  val AST_code =
    s"""data Exp = Var(`String`) | Num(`Int`)
       |data Stmt = Assign(`String`, Exp) | Skip() | Seq(Stmt, Stmt) | If(Exp, Stmt, Stmt) | While(Exp, Stmt)
       |""".stripMargin

  // init: stmt -> Lab
  val initFunction =
    s"""def init(stmt: Stmt): Stmt = match {
       |  case Assign(x, a) => stmt
       |  case Skip() => stmt
       |  case Seq(s1, s2) => init(s1)
       |  case If(b, s1, s2) => stmt
       |  case While(b, s) => stmt
       |}
       |""".stripMargin

  // final: stmt -> P(Lab)
  val finalFunction =
    s"""def final(stmt: Stmt): Set[Stmt] = match {
       |  case Assign(x, a) => SomeSet(stmt) // return set containing just stmt
       |  case Skip() => SomeSet(stmt)
       |  case Seq(s1, s2) => final(s2)
       |  case If(b, s1, s2) => alt {
       |    final(s1)
       |  } or {
       |    final(s2)
       |  }
       |  case While(b, s) => SomeSet(stmt)
       |}
       |""".stripMargin

  // flow: stmt -> P(Lab x Lab)
  val flowFunction =
    s"""def flow(stmt: Stmt): Set[(Stmt, Stmt)] = match {
       |  case Assign(x, a) => EmptySet // return empty set
       |  case Skip() => EmptySet
       |  case Seq(s1, s2) => alt {
       |    flow(s1)
       |  } or {
       |    flow(s2)
       |  } or {
       |    let init = init(s1) in
       |      foreach finalS2 in final(s2) { // need a way of iterating over a set, which will produce another set
       |        (l, finalS2)
       |      }
       |  }
       |  case If(b, s1, s2) => alt {
       |    flow(s1)
       |  } or {
       |    flow(s2)
       |  } or {
       |    let initS1 = init(s1) in
       |      let initS2 = init(s2) in
       |        SomeSet((stmt, initS1), (stmt, initS2))
       |  }
       |  case While(b, s) => alt {
       |    flow(s)
       |  } or {
       |    yield SomeSet((stmt, init(s))
       |  } or {
       |    foreach finalS in final(s) {
       |      (finalS, stmt)
       |    }
       |  }
       |}
       |""".stripMargin

  val flowModule = module(
    AST_code,
    initFunction,
    finalFunction,
    flowFunction
  )

}
