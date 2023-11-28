package inca.souffle.compile

import inca.ir.*
import inca.ir.extension.arithmetic as arith
import inca.ir.extension.data
import inca.ir.execution.Relation as Rel
import inca.ir.util.SourceLocation
import inca.souffle.Executor
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.extension.{aggregateset, aggregate, block, bool, datamatch, demand, disjunction, impure, not, set, tuple}
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

  val edgeRel = ExtensionalRelation("edge", Seq(Param("X", arith.TInt), Param("Y", arith.TInt)))
  val pathRel = Relation("path", Seq(Param("X", arith.TInt), Param("Y", arith.TInt)),
      Seq(
        Body(Seq(ExtensionalCall("edge", Seq(Var("X"), Var("Y"))))),
        Body(Seq(ExtensionalCall("edge", Seq(Var("X"), Var("Z"))), Call("path", Seq(Var("Z"), Var("Y"))))),
      )
    )
  val nodeRel = Relation("node", Seq(Param("X", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("X"), Var("Y"))))),
      Body(Seq(ExtensionalCall("edge", Seq(Var("Y"), Var("X")))))
    )
  )
  val notConnectedRel= Relation("notconnected", Seq(Param("X", arith.TInt), Param("Y", arith.TInt)),
    Seq(
      Body(Seq(Call("node", Seq(Var("X"))), Call("node", Seq(Var("Y"))), NegCall("path", Seq(Var("X"), Var("Y")))))
    )
  )
  val maxRel = Relation("test", Seq(Param("X", arith.TInt)), Seq(
    Body(Seq(
      Eq(Var("Y"), arith.IntNum(5)),
      Eq(Var("X"), arith.Max(arith.Add(Var("Y"), arith.IntNum(1)), arith.IntNum(2)))))
  ))
  val minRel = Relation("test", Seq(Param("X", arith.TInt)), Seq(
    Body(Seq(
      Eq(Var("Y"), arith.IntNum(5)),
      Eq(Var("X"), arith.Min(arith.Add(Var("Y"), arith.IntNum(1)), arith.IntNum(2)))))
  ))
  val absRel = Relation("test", Seq(Param("X", arith.TInt)), Seq(
    Body(Seq(
      Eq(Var("Y"), arith.IntNum(5)),
      Eq(Var("X"), arith.Abs(Var("Y")))))
  ))
  val natDecl = data.DataDefinition("Nat", Seq(
    data.CaseDefinition("Zero", Seq()),
    data.CaseDefinition("Succ", Seq(data.TData("Nat")))
  ))
  val natRel = Relation("test", Seq(Param("X", data.TData("Nat"))), Seq(
    Body(Seq(
      Eq(Var("Y"), data.Construct("Succ", Seq(data.Construct("Zero", Seq())))),
      data.Deconstruct(Var("Y"), "Succ", Seq(Var("X"))),
      data.Deconstruct(Var("X"), "Zero", Seq())))
  ))

  val edgeWithDistanceRel = ExtensionalRelation("edge", Seq(Param("X", arith.TInt), Param("Y", arith.TInt), Param("D", arith.TInt)))
  val pathColWithDistanceRel = Relation("pathCol", Seq(Param("X", arith.TInt), Param("Y", arith.TInt), Param("D", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("X"), Var("Y"), Var("D"))))),
      Body(Seq(ExtensionalCall("edge", Seq(Var("X"), Var("Z"), Var("D1"))), Call("path", Seq(Var("Z"), Var("Y"), Var("D2"))), Eq(Var("D"), arith.Add(Var("D1"), Var("D2"))))),
    )
  )
  val pathWithDistanceRel = Relation("path", Seq(Param("X", arith.TInt), Param("Y", arith.TInt), Param("D", arith.TInt)),
    Seq(
      Body(Seq(
        Call("pathCol", Seq(Var("X"), Var("Y"), Var("DUMMY"))),
        aggregate.Aggregate("pathCol", Seq(aggregate.AggregateArg.Arg(Var("X")), aggregate.AggregateArg.Arg(Var("Y")),  aggregate.AggregateArg.AggregateColumn(Var("D"))), arith.ArithmeticAggregationOperator.MinInt))),
    )
  )

  val maxTargetNode = Relation("maxTargetNode", Seq(Param("X", arith.TInt), Param("M", arith.TInt)),
    Seq(
      Body(Seq(
        Call("edge", Seq(Var("X"), Var("DUMMY"))),
        aggregate.Aggregate("edge", Seq(aggregate.AggregateArg.Arg(Var("X")), aggregate.AggregateArg.AggregateColumn(Var("M"))), arith.ArithmeticAggregationOperator.MaxInt))),
    )
  )

  test("path example") {
    val module = Module("PathExample", Language.Datalog, Seq(edgeRel, pathRel))
    val prog = GenerateSouffle.compileModule(module)
    println(prog)
  }
  test("notconnected example") {
    val module = Module("PathExample", Language.Datalog, Seq(edgeRel, nodeRel, pathRel, notConnectedRel) )
    val prog = GenerateSouffle.compileModule(module)
    println(prog)
  }
  test("max example") {
    val module = Module("PathExample", Language.Datalog, Seq(maxRel))
    val prog = GenerateSouffle.compileModule(module)
    println(prog)
  }
  test("min example") {
    val module = Module("PathExample", Language.Datalog, Seq(minRel))
    val prog = GenerateSouffle.compileModule(module)
    println(prog)
  }
  test("abs example") {
    val module = Module("PathExample", Language.Datalog, Seq(absRel))
    val prog = GenerateSouffle.compileModule(module)
    println(prog)
  }

  test("data example") {
    val module = Module("PathExample", Language.Datalog, Seq(natDecl, natRel))
    val prog = GenerateSouffle.compileModule(module)
    println(prog)
  }

  test("max aggregation example") {
    val module = Module("MaxExample", Language.Datalog, Seq(edgeRel, maxTargetNode))
    val prog = GenerateSouffle.compileModule(module)
    println(prog)
  }

  test("process test") {
    val irModule = Module("MaxExample", Language.Datalog, Seq(maxRel))
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
    val irModule = Module("PathExample", Language.Datalog, Seq(pathRel, edgeRel))
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

  test("process test 3") {
    // Souffle does not support recursive aggregation
    val irModule = Module("MaxExample", Language.Datalog, Seq(edgeRel, maxTargetNode))
    val compiledModule = new CompiledModule:
      override def name: Name = "MaxExample"
      override def sourceLocation: SourceLocation = ???
      override def ir: Module = irModule
    compiledModule.setPipeline(pipeline)
    val engine = Executor.instantiate(compiledModule)
    engine.insert(Rel.from("edge", Seq("X", "Y"), Seq(Seq(1, 2), Seq(1, 5), Seq(2, 3), Seq(2, 1), Seq(3, 4), Seq(3, 5), Seq(4, 8))))
    val rels = engine.readAll()
    println(rels)
  }

// TODO Souffle does not support recursive aggregation
//  test("process test 4") {
//    val irModule = Module("PathExample", Language.Datalog, Seq(pathWithDistanceRel, edgeWithDistanceRel, pathColWithDistanceRel))
//    val compiledModule = new CompiledModule:
//      override def name: Name = "PathExample"
//      override def sourceLocation: SourceLocation = ???
//      override def ir: Module = irModule
//    compiledModule.setPipeline(pipeline)
//    val engine = Executor.instantiate(compiledModule)
//    engine.insert(Rel.from("edge", Seq("X", "Y", "D"), Seq(Seq(1, 2, 1), Seq(2, 3, 2), Seq(3, 4, 3))))
//    val rels = engine.readAll()
//    println(rels)
//  }
}
