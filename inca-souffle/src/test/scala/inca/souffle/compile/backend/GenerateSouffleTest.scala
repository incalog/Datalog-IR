package inca.souffle.compile.backend

import inca.ir.*
import inca.ir.Module as irModule
import inca.ir.extension.arithmetic as arith
import inca.ir.extension.data
import inca.ir.execution.{IRExecutor, Relation as Rel}
import inca.ir.extension.aggregate.AggregateColumnArg
import inca.ir.extension.data.{DataDefinition, DataModuleEntry}
import inca.ir.util.SourceLocation
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.extension.{aggregate, aggregateset, block, bool, datamatch, demand, disjunction, impure, not, set, tuple}
import inca.ir.visitors.{BaseIRVisitor, IRVisitor}
import inca.souffle.backend.{Executor, GenerateSouffle}
import inca.souffle.frontend.compile.{CompiledSouffleUnit, SouffleInputHint}
import inca.souffle.syntax.{DirectiveValue, Parser}
import inca.util.FileUtil
import inca.util.compileroptions.CompilerOptions.default
import inca.util.compileroptions.CompilerOptions

import scala.collection.mutable.ListBuffer
import scala.io.Source

class GenerateSouffleTest extends AnyFunSuite:
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
  
  val edgeRel: ExtensionalRelation = ExtensionalRelation("edge", Seq(Param("X", arith.TInt), Param("Y", arith.TInt)))
  val pathRel: Relation = Relation("path", Seq(Param("X", arith.TInt), Param("Y", arith.TInt)),
      Seq(
        Body(Seq(ExtensionalCall("edge", Seq(Var("X"), Var("Y"))))),
        Body(Seq(ExtensionalCall("edge", Seq(Var("X"), Var("Z"))), Call("path", Seq(Var("Z"), Var("Y"))))),
      )
    )
  val nodeRel: Relation = Relation("node", Seq(Param("X", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("X"), Var("Y"))))),
      Body(Seq(ExtensionalCall("edge", Seq(Var("Y"), Var("X")))))
    )
  )
  val notConnectedRel: Relation = Relation("notconnected", Seq(Param("X", arith.TInt), Param("Y", arith.TInt)),
    Seq(
      Body(Seq(Call("node", Seq(Var("X"))), Call("node", Seq(Var("Y"))), Call("path", Seq(Var("X").arg, Var("Y").arg), true)))
    )
  )
  val maxRel: Relation = Relation("test", Seq(Param("X", arith.TInt)), Seq(
    Body(Seq(
      Eq(Var("Y"), arith.IntNum(5)),
      Eq(Var("X"), arith.Max(arith.Add(Var("Y"), arith.IntNum(1)), arith.IntNum(2)))))
  ))
  val minRel: Relation = Relation("test", Seq(Param("X", arith.TInt)), Seq(
    Body(Seq(
      Eq(Var("Y"), arith.IntNum(5)),
      Eq(Var("X"), arith.Min(arith.Add(Var("Y"), arith.IntNum(1)), arith.IntNum(2)))))
  ))
  val absRel: Relation = Relation("test", Seq(Param("X", arith.TInt)), Seq(
    Body(Seq(
      Eq(Var("Y"), arith.IntNum(5)),
      Eq(Var("X"), arith.Abs(Var("Y")))))
  ))
  val natDecl: Seq[DataModuleEntry] = Seq(
    data.DataDefinition("Nat"),
    data.CaseDefinition("Zero", Seq(), data.TData("Nat")),
    data.CaseDefinition("Succ", Seq(data.TData("Nat")), data.TData("Nat"))
  )
  val natRel: Relation = Relation("test", Seq(Param("X", data.TData("Nat"))), Seq(
    Body(Seq(
      Eq(Var("Y"), data.Construct("Succ", Seq(data.Construct("Zero", Seq())))),
      data.Deconstruct(Var("Y"), "Succ", Seq(Var("X"))),
      data.Deconstruct(Var("X"), "Zero", Seq())))
  ))
  
  val edgeWithDistanceRel: ExtensionalRelation = ExtensionalRelation("edge", Seq(Param("X", arith.TInt), Param("Y", arith.TInt), Param("D", arith.TInt)))
  val pathColWithDistanceRel: Relation = Relation("pathCol", Seq(Param("X", arith.TInt), Param("Y", arith.TInt), Param("D", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("X"), Var("Y"), Var("D"))))),
      Body(Seq(ExtensionalCall("edge", Seq(Var("X"), Var("Z"), Var("D1"))), Call("path", Seq(Var("Z"), Var("Y"), Var("D2"))), Eq(Var("D"), arith.Add(Var("D1"), Var("D2"))))),
    )
  )
  val pathWithDistanceRel: Relation = Relation("path", Seq(Param("X", arith.TInt), Param("Y", arith.TInt), Param("D", arith.TInt)),
    Seq(
      Body(Seq(
        Call("pathCol", Seq(Var("X"), Var("Y"), Var("DUMMY"))),
        aggregate.Aggregate(RefByName("pathCol"), Seq(Var("X").arg, Var("Y").arg,  AggregateColumnArg(Var("D"))), arith.ArithmeticAggregationOperator.MinInt))),
    )
  )
  
  val maxTargetNode: Relation = Relation("maxTargetNode", Seq(Param("X", arith.TInt), Param("M", arith.TInt)),
    Seq(
      Body(Seq(
        Call("edge", Seq(Var("X"), Var("DUMMY"))),
        aggregate.Aggregate(RefByName("edge"), Seq(Var("X").arg, AggregateColumnArg(Var("M"))), arith.ArithmeticAggregationOperator.MaxInt))),
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
    //println(prog)
  }
  test("max example") {
    val module = Module("PathExample", Language.Datalog, Seq(maxRel))
    val prog = GenerateSouffle.compileModule(module)
    //println(prog)
  }
  test("min example") {
    val module = Module("PathExample", Language.Datalog, Seq(minRel))
    val prog = GenerateSouffle.compileModule(module)
    //println(prog)
  }
  test("abs example") {
    val module = Module("PathExample", Language.Datalog, Seq(absRel))
    val prog = GenerateSouffle.compileModule(module)
    //println(prog)
  }
  
  test("data example") {
    val module = Module("PathExample", Language.Datalog, natDecl :+ natRel)
    val prog = GenerateSouffle.compileModule(module)
    //println(prog)
  }
  
  test("max aggregation example") {
    val module = Module("MaxExample", Language.Datalog, Seq(edgeRel, maxTargetNode))
    val prog = GenerateSouffle.compileModule(module)
    //println(prog)
  }
  
  test("process test") {
    val irModule = Module("MaxExample", Language.Datalog, Seq(maxRel))
    val compiledModule = new CompiledUnit:
      override def name: Name = "MaxExample"
      override def sourceLocation: SourceLocation = ???
      override val isClosedWorld: Boolean = true
      override def otherUnits: Seq[CompiledUnit] = Seq()
      lazy val irModules: Seq[Module] = Seq(irModule)
      override val compilerOptions: CompilerOptions = CompilerOptions.default
    compiledModule.setPipeline(pipeline)
    println(irModule)
    val engine = Executor().instantiate(compiledModule)
    val rels = engine.readAll()
    println(rels)
  }
  
  test("process test 2") {
    val irModule = Module("PathExample", Language.Datalog, Seq(pathRel, edgeRel))
    val compiledModule = new CompiledUnit:
      override def name: Name = "PathExample"
      override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
      override val isClosedWorld: Boolean = true
      override def otherUnits: Seq[CompiledUnit] = Seq()
      lazy val irModules: Seq[Module] = Seq(irModule)
      override val compilerOptions: CompilerOptions = CompilerOptions.default
    compiledModule.setPipeline(pipeline)
    val engine = Executor().instantiate(compiledModule)
    engine.insert(Rel.from("edge", Seq("X", "Y"), Seq(Seq(1, 2), Seq(2, 3), Seq(3, 4))))
    val rels = engine.readAll()
    //println(rels)
  }

// TODO: We currently do not support aggregation over Extensional Relations
//  test("process test 3") {
//    // Souffle does not support recursive aggregation
//    val irModule = Module("MaxExample", Language.Datalog, Seq(edgeRel, maxTargetNode))
//    val compiledModule = new CompiledUnit:
//      override def name: Name = "MaxExample"
//      override def sourceLocation: SourceLocation = ???
//      override def ir: Module = irModule
//      override val compilerOptions: CompilerOptions = CompilerOptions.default
//    compiledModule.setPipeline(pipeline)
//    val engine = Executor.instantiate(compiledModule)
//    engine.insert(Rel.from("edge", Seq("X", "Y"), Seq(Seq(1, 2), Seq(1, 5), Seq(2, 3), Seq(2, 1), Seq(3, 4), Seq(3, 5), Seq(4, 8))))
//    val rels = engine.readAll()
//    println(rels)
//  }

// TODO Souffle does not support recursive aggregation
//  test("process test 4") {
//    val irModule = Module("PathExample", Language.Datalog, Seq(pathWithDistanceRel, edgeWithDistanceRel, pathColWithDistanceRel))
//    val compiledModule = new CompiledUnit:
//      override def name: Name = "PathExample"
//      override def sourceLocation: SourceLocation = ???
//      override def ir: Module = irModule
//    compiledModule.setPipeline(pipeline)
//    val engine = Executor.instantiate(compiledModule)
//    engine.insert(Rel.from("edge", Seq("X", "Y", "D"), Seq(Seq(1, 2, 1), Seq(2, 3, 2), Seq(3, 4, 3))))
//    val rels = engine.readAll()
//    println(rels)
//  }

  /*test("lowering micro.dl times") {
    val code = Source.fromResource("inca/souffle/doop/micro.dl").getLines().mkString("\n")

    val times = ListBuffer[Long]()
    for (i <- 1 to 10) {
      val compiled = CompiledSouffleUnit.fromSourceCode("micro", code)
      val start = System.nanoTime()
      compiled.lowered
      val end = System.nanoTime()
      times += (end - start) / 1000 / 1000
    }
    val t = times.drop(3)

    println(s"${t.sum / t.size}ms")
  }*/
