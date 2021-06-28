package inca.backend.souffle

import inca.backend.ir.GPPrinter
import inca.examples.functional.{Code, ControlDataFlow}
import inca.util.measurement.BenchmarkUtils.writeFile
import org.scalatest.funsuite.AnyFunSuite


class TestGenerateSouffle extends AnyFunSuite {

  test("plus") {
    val compiled = CompiledFunctionalToSouffleModule(Code.plusModule)
    println(compiled.souffleSource)
  }

  test("plus main input") {
    val compiled = CompiledFunctionalToSouffleModule(Code.plusRealModule)
    println(GPPrinter.prettyModule(compiled.optimized))
    println(compiled.souffleSource)
  }

  val prog =
    """Sequence(
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
  val intValuesNoInput =
    s"""
      |module Dataflow
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
      |@main def final_var(): Set[(String, Val)] =
      |  let prog = ${prog} in
      |    {(x, v) | s in final(prog), x in freevarsStm(prog), v in exit_var(s, prog, x)}
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
      |      if ((n1 + n2) <= -100) VNum(-1000)
      |      else if ((n1 + n2) >= 100) VNum(1000)
      |      else VNum(n1 + n2)
      |    case VBool(b2) => VBool(false)
      |  }
      |  case VBool(b1) => VBool(false)
      |}
      |""".stripMargin


  test("powerset dataflow analysis no input") {
    val compiled = CompiledFunctionalToSouffleModule(intValuesNoInput)
//    println(GPPrinter.prettyModule(compiled.optimized))
    println(compiled.souffleSource)
  }

  test("powerset dataflow analysis") {
    val compiled = CompiledFunctionalToSouffleModule(ControlDataFlow.IntValuesModule)
    println(GPPrinter.prettyModule(compiled.optimized))
    println(compiled.souffleSource)
  }

}
