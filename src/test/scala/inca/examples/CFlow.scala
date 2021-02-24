package inca.examples

import inca.examples.Code.module

object CFlow {
  val AST_code =
    s"""data Exp = Var(`String`) |
       |           Num(`Int`) |
       |           GreaterThan(Exp, Exp) |
       |           Mul(Exp, Exp) |
       |           Add(Exp, Exp) |
       |           Sub(Exp, Exp)
       |data Stmt = Assign(`String`, Exp) | Skip() | Sequence(Stmt, Stmt) | If(Exp, Stmt, Stmt) | While(Exp, Stmt)
       |""".stripMargin

  // init: stmt -> Lab
  val initFunction =
    s"""def init(stmt: Stmt): Stmt = stmt match {
       |  case Assign(x, a) => stmt
       |  case Skip() => stmt
       |  case Sequence(s1, s2) => init(s1)
       |  case If(b, s1, s2) => stmt
       |  case While(b, s) => stmt
       |}
       |""".stripMargin

  // final: stmt -> P(Lab)
  val finalFunction =
    s"""def final(stmt: Stmt): Set[Stmt] = stmt match {
       |  case Assign(x, a) => {stmt}
       |  case Skip() => {stmt}
       |  case Sequence(s1, s2) => final(s2)
       |  case If(b, s1, s2) => final(s1) ++ final(s2)
       |  case While(b, s) => {stmt}
       |}
       |""".stripMargin

  // flow: stmt -> P(Lab x Lab)
  val flowFunction =
    s"""@main def flow(stmt: Stmt): Set[(Stmt, Stmt)] = stmt match {
       |  case Assign(x, a) => {}
       |  case Skip() => {}
       |  case Sequence(s1, s2) => flow(s1) ++ flow(s2) ++ {(l1, init(s2)) | l1 in final(s1)}
       |  case If(c, s1, s2) => flow(s1) ++ flow(s2) ++ {(stmt, init(s1)), (stmt, init(s2))}
       |  case While(c, s) => flow(s) ++ {(stmt, init(s))} ++ {(l,stmt) | l in final(s)}
       |}
       |""".stripMargin

  val flowModule = module(
    AST_code,
    initFunction,
    finalFunction,
    flowFunction
  )

  import scala.meta.XtensionQuasiquoteTerm

  // Example 2.1 from POPA, page 37
  val example_2_1 =
    q"""{
        val s1 = Assign("z", Num(1))
        val s3 = Assign("z", Mul(Var("z"), Var("y")))
        val s4 = Assign("x", Sub(Var("x"), Num(-1)))
        val s2 = While(GreaterThan(Var("x"), Num(0)), Sequence(s3, s4))
        Sequence(s1, s2)
        }
       """
}
