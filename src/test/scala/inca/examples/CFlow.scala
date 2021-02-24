package inca.examples

import inca.examples.Code.module

object CFlow {
  val AST_code =
    s"""data Exp = Var(`String`) | Num(`Int`)
       |data Stmt = Assign(`String`, Exp) | Skip() | Seq(Stmt, Stmt) | If(Exp, Stmt, Stmt) | While(Exp, Stmt)
       |""".stripMargin

  // init: stmt -> Lab
  val initFunction =
    s"""def init(stmt: Stmt): Stmt = stmt match {
       |  case Assign(x, a) => stmt
       |  case Skip() => stmt
       |  case Seq(s1, s2) => init(s1)
       |  case If(b, s1, s2) => stmt
       |  case While(b, s) => stmt
       |}
       |""".stripMargin

  // final: stmt -> P(Lab)
  val finalFunction =
    s"""def final(stmt: Stmt): Set[Stmt] = stmt match {
       |  case Assign(x, a) => {stmt}
       |  case Skip() => {stmt}
       |  case Seq(s1, s2) => final(s2)
       |  case If(b, s1, s2) => final(s1) ++ final(s2)
       |  case While(b, s) => {stmt}
       |}
       |""".stripMargin

  // flow: stmt -> P(Lab x Lab)
  val flowFunction =
    s"""def flow(stmt: Stmt): Set[(Stmt, Stmt)] = stmt match {
       |  case Assign(x, a) => {}
       |  case Skip() => {}
       |  case Seq(s1, s2) => flow(s1) ++ flow(s2) ++ {(l1, init(s2)) | l1 in final(s2)}
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

}
