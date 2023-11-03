package inca.souffle.compile

import inca.ir.*
import inca.ir.extension.arithmetic as arith
import org.scalatest.funsuite.AnyFunSuite

class GenerateSouffleTest extends AnyFunSuite {
  val edgeRel = ExtensionalRelation("edge", Seq(Param(Name("X"), arith.TInt), Param(Name("Y"), arith.TInt)))
  val pathRel = Relation("path", Seq(Param(Name("X"), arith.TInt), Param(Name("Y"), arith.TInt)),
      Seq(
        Body(Seq(ExtensionalCall(Name("edge"), Seq(Var("X"), Var("Y"))))),
        Body(Seq(ExtensionalCall(Name("edge"), Seq(Var("X"), Var("Z"))), Call(Name("path"), Seq(Var("Z"), Var("Y"))))),
      )
    )
  val nodeRel = Relation("node", Seq(Param(Name("X"), arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall(Name("edge"), Seq(Var("X"), Var("Y"))))),
      Body(Seq(ExtensionalCall(Name("edge"), Seq(Var("Y"), Var("X")))))
    )
  )
  val notConnectedRel= Relation("notconnected", Seq(Param(Name("X"), arith.TInt), Param(Name("Y"), arith.TInt)),
    Seq(
      Body(Seq(Call(Name("node"), Seq(Var("X"))), Call(Name("node"), Seq(Var("Y"))), NegCall(Name("path"), Seq(Var("X"), Var("Y")))))
    )
  )
  val maxRel = Relation("test", Seq(Param(Name("X"), arith.TInt)), Seq(
    Body(Seq(
      Eq(Var("Y"), arith.IntNum(5)),
      Eq(Var("X"), arith.Max(arith.Add(Var("Y"), arith.IntNum(1)), arith.IntNum(2)))))
  ))
  val minRel = Relation("test", Seq(Param(Name("X"), arith.TInt)), Seq(
    Body(Seq(
      Eq(Var("Y"), arith.IntNum(5)),
      Eq(Var("X"), arith.Min(arith.Add(Var("Y"), arith.IntNum(1)), arith.IntNum(2)))))
  ))
  val absRel = Relation("test", Seq(Param(Name("X"), arith.TInt)), Seq(
    Body(Seq(
      Eq(Var("Y"), arith.IntNum(5)),
      Eq(Var("X"), arith.Abs(Var("Y")))))
  ))

  test("path example") {
    val module = Module(Name("PathExample"), Language.Datalog, Seq(edgeRel, pathRel))
    val prog = GenerateSouffle.compileModule(module)
    println(prog)
  }
  test("notconnected example") {
    val module = Module(Name("PathExample"), Language.Datalog, Seq(edgeRel, nodeRel, pathRel, notConnectedRel) )
    val prog = GenerateSouffle.compileModule(module)
    println(prog)
  }
  test("max example") {
    val module = Module(Name("PathExample"), Language.Datalog, Seq(maxRel))
    val prog = GenerateSouffle.compileModule(module)
    println(prog)
  }
  test("min example") {
    val module = Module(Name("PathExample"), Language.Datalog, Seq(minRel))
    val prog = GenerateSouffle.compileModule(module)
    println(prog)
  }

  test("abs exampl") {
    val module = Module(Name("PathExample"), Language.Datalog, Seq(absRel))
    val prog = GenerateSouffle.compileModule(module)
    println(prog)
  }
}
