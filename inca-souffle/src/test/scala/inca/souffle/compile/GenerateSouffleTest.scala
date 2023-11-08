package inca.souffle.compile

import inca.ir.*
import inca.ir.extension.arithmetic as arith
import inca.ir.extension.data
import inca.ir.execution.Relation as Rel
import inca.ir.util.SourceLocation
import inca.souffle.Executor
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.extension.{aggregateset, block, bool, datamatch, demand, disjunction, impure, not, set, tuple}
import inca.ir.visitors.BaseIRVisitor

class GenerateSouffleTest extends AnyFunSuite {

    val pipeline: List[() => BaseIRVisitor] = List(
      () => new aggregateset.Lowering {},
      () => new set.Lowering {},
      () => new bool.Lowering {},
      () => new datamatch.Lowering {},
      () => new block.Lowering {},
      () => new disjunction.Lowering {},
      () => new not.Lowering {},
      () => new demand.Lowering {},
      () => new tuple.Lowering {}
    ) // arith + string + data

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
  val natDecl = data.DataDefinition("Nat", Seq(
    data.CaseDefinition("Zero", Seq()),
    data.CaseDefinition("Succ", Seq(data.TData("Nat")))
  ))
  val natRel = Relation("test", Seq(Param(Name("X"), data.TData("Nat"))), Seq(
    Body(Seq(
      Eq(Var("Y"), data.Construct("Succ", Seq(data.Construct("Zero", Seq())))),
      data.Deconstruct(Var("Y"), "Succ", Seq(Var("X"))),
      data.Deconstruct(Var("X"), "Zero", Seq())))
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
  test("abs example") {
    val module = Module(Name("PathExample"), Language.Datalog, Seq(absRel))
    val prog = GenerateSouffle.compileModule(module)
    println(prog)
  }

  test("data example") {
    val module = Module(Name("PathExample"), Language.Datalog, Seq(natDecl, natRel))
    val prog = GenerateSouffle.compileModule(module)
    println(prog)
  }

  test("process test") {
    val irModule = Module(Name("MaxExample"), Language.Datalog, Seq(maxRel))
    val compiledModule = new CompiledModule:
      override def name: Name = "MaxExample"
      override def sourceLocation: SourceLocation = ???
      override def ir: Module = irModule
    compiledModule.setPipeline(pipeline)
    val engine = Executor.instantiate(compiledModule)
    val rels = engine.readAll()
    println(rels)
  }
  test("process test 2") {
    val irModule = Module(Name("PathExample"), Language.Datalog, Seq(pathRel, edgeRel))
    val compiledModule = new CompiledModule:
      override def name: Name = "PathExample"
      override def sourceLocation: SourceLocation = ???
      override def ir: Module = irModule
    compiledModule.setPipeline(pipeline)
    val engine = Executor.instantiate(compiledModule)
    engine.insert(Rel.from("edge", Seq("X", "Y"), Seq(Seq(1, 2), Seq(2, 3), Seq(3, 4))))
    val rels = engine.readAll()
    println(rels)
  }
  //    val funCompiled = ???
  //    funCompiled.setPipeline(pipeline)
}
