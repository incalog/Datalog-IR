package inca.ascent.backend

import inca.ir.extension.arithmetic as arith
import inca.ir.extension.data
import inca.ir.extension.string as s
import inca.ir.execution.{UnitRelation, Relation as Rel}
import inca.ir.util.SourceLocation
import inca.ir.extension.arithmetic.IntNum
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.extension.{aggregate, aggregateset, block, bool, datamatch, demand, disjunction, impure, not, set, tuple}
import inca.ir.visitors.BaseIRVisitor
import inca.ir.extension.aggregate.AggregateColumnArg
import inca.util.compileroptions.CompilerOptions
import inca.ir.extension.data.TData
import inca.ir.typing.IRTypechecker
import inca.ir.*
import inca.ir.term2Arg

import scala.collection.immutable.Seq

class GenerateAscentTest extends AnyFunSuite:
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

  val edgeRel = ExtensionalRelation("edge", Seq(Param("x", arith.TInt), Param("y", arith.TInt)))
  val pathRel = Relation("path", Seq(Param("x", arith.TInt), Param("y", arith.TInt)),
  Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))))),
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("z"))), Call("path", Seq(Var("z"), Var("y"))))),
    )
  )
  val nodeRel = Relation("node", Seq(Param("x", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))))),
      Body(Seq(ExtensionalCall("edge", Seq(Var("y"), Var("x")))))
    )
  )
  val negvalUnary = Relation("negValUnary", Seq(Param("x", arith.TInt)), Seq(
    Body(Seq(ExtensionalCall("n", Seq(Var("x"))), ExtensionalCall("m", Seq(arith.UnOp(Var("x"), "-")))))
  ))
  val valueRel = ExtensionalRelation("n", Seq(Param("x", arith.TInt)))
  val valueRelWrapper = Relation("n_wrap", Seq(Param("x", arith.TInt)), Seq(
    Body(
      Seq(
        ExtensionalCall("n", Seq(Var("x").arg))
      )
    )
  ))
  val value2Rel = ExtensionalRelation("m", Seq(Param("x", arith.TInt)))
  val valueString = ExtensionalRelation("Wort", Seq(Param("x", s.TString)))
  val negRel = Relation("negation", Seq(Param("x", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("n", Seq(Var("x"))), ExtensionalCall("m", Seq(Var("x").arg), neg = true)))
    ))
  val add1 = Relation("add1", Seq(Param("x", arith.TInt), Param("y", arith.TInt), Param("z", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), ExtensionalCall("edge", Seq(arith.Add(Var("x"), Var("y")), Var("z")))))
    ))
  val add2 = Relation("add2", Seq(Param("x", arith.TInt), Param("y", arith.TInt), Param("z", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), Eq(Var("z"), arith.Add(Var("x"), Var("y")))))
    ))
  val sub = Relation("sub", Seq(Param("x", arith.TInt), Param("y", arith.TInt), Param("z", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), Eq(Var("z"), arith.Sub(Var("x"), Var("y")))))
    ))
  val div = Relation("div", Seq(Param("x", arith.TInt), Param("y", arith.TInt), Param("z", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), Eq(Var("z"), arith.Div(Var("x"), Var("y")))))
    ))
  val mul = Relation("mul", Seq(Param("x", arith.TInt), Param("y", arith.TInt), Param("z", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), Eq(Var("z"), arith.Mul(Var("x"), Var("y")))))
    ))
  val rem = Relation("rem", Seq(Param("x", arith.TInt), Param("y", arith.TInt), Param("z", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), Eq(Var("z"), arith.Remainder(Var("x"), Var("y")))))
    ))
  val add3 = Relation("add3", Seq(Param("x", arith.TInt), Param("z", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), Eq(Var("z"), arith.Add(Var("x"), arith.IntNum(3)))))
    ))
  val greater = Relation("Greater", Seq(Param("x", arith.TInt), Param("y", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), arith.BinCompare(Var("x"), Var("y"), ">"))))
  )
  val xGreater3 = Relation("Greater", Seq(Param("x", arith.TInt), Param("y", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), arith.BinCompare(Var("x"), arith.IntNum(3), ">"))))
  )
  val yis5 = Relation("yis5", Seq(Param("x", arith.TInt), Param("y", arith.TInt)),
    Seq(
      Body(Seq(Eq(Var("y"), arith.IntNum(5)), ExtensionalCall("edge", Seq(Var("x"), Var("y")))))
    ))

  val yisx = Relation("yisx", Seq(Param("x", arith.TInt), Param("y", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("z"))), ExtensionalCall("edge", Seq(Var("c"), Var("y"))), Eq(Var("y"), Var("x"))))
    ))

  val cat = Relation("cat", Seq(Param("c", s.TString)),
    Seq(
      Body(Seq(ExtensionalCall("Wort", Seq(Var("x"))), ExtensionalCall("Wort", Seq(Var("y"))), Eq(Var("c"), s.StringConcat(Var("x"), Var("y")))))
    )
  )
  val stringRel = Relation("strwort", Seq(Param("x", s.TString)), Seq(Body(
    Seq(ExtensionalCall("Wort", Seq(Var("x"))))))
  )

  test("test ") {
    val irModule = Module("PathExample", Language.Datalog, Seq(pathRel, edgeRel))
    val compiledModule = new CompiledModule:
      override def name: Name = "PathExample"
      override def compilerOptions: CompilerOptions = CompilerOptions.default
      override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
      override def ir: Module = irModule

    compiledModule.setPipeline(pipeline)
    val engine = Executor.instantiate(compiledModule)
    engine.insert(Rel.from("edge", Seq("x", "y"), Seq(Seq(1, 2), Seq(2, 3), Seq(3, 4))))
    val res = engine.read(UnitRelation("path"))
    assertResult(Set((2, 3), (3, 4), (1, 2), (1, 3), (2, 4), (1, 4)))(res.toSet)
  }

  val floatEdgeRel = ExtensionalRelation("edge", Seq(Param("x", arith.TDouble), Param("y", arith.TDouble)))
  val floatPathRel = Relation("path", Seq(Param("x", arith.TDouble), Param("y", arith.TDouble)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))))),
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("z"))), Call("path", Seq(Var("z"), Var("y"))))),
    )
  )

  val floatAdd = Relation("floatadd", Seq(Param("x", arith.TDouble), Param("y", arith.TDouble), Param("z", arith.TDouble)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), Eq(Var("z"), arith.Add(Var("x"), Var("y"))))),
    )
  )

  val floatSub = Relation("floatsub", Seq(Param("x", arith.TDouble), Param("y", arith.TDouble), Param("z", arith.TDouble)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), Eq(Var("z"), arith.Sub(Var("x"), Var("y"))))),
    )
  )
  val floatMul = Relation("floatmul", Seq(Param("x", arith.TDouble), Param("y", arith.TDouble), Param("z", arith.TDouble)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), Eq(Var("z"), arith.Mul(Var("x"), Var("y"))))),
    )
  )
  val floatDiv = Relation("floatdiv", Seq(Param("x", arith.TDouble), Param("y", arith.TDouble), Param("z", arith.TDouble)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), Eq(Var("z"), arith.Div(Var("x"), Var("y"))))),
    )
  )

  test("test Float path und binops") {
    val irModule = Module("FloatpathExample", Language.Datalog, Seq(floatEdgeRel, floatPathRel, floatAdd, floatSub, floatMul, floatDiv))
    val compiledModule = new CompiledModule:
      override def name: Name = "FloatPathExample"
      override def compilerOptions: CompilerOptions = CompilerOptions.default
      override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
      override def ir: Module = irModule

    compiledModule.setPipeline(pipeline)
    val engine = Executor.instantiate(compiledModule)
    engine.insert(Rel.from("edge", Seq("x", "y"), Seq(Seq(1.13, 2.8), Seq(4.0, 3.0), Seq(2.8, 4.0))))
    val rels = engine.read(UnitRelation("path"))
    assertResult(Set((2.8,4.0), (4.0,3.0), (1.13,4.0), (1.13,3.0), (1.13,2.8), (2.8,3.0)))(rels.toSet)
  }

  val floatLesser = Relation("Lesser", Seq(Param("x", arith.TDouble), Param("y", arith.TDouble)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), arith.BinCompare(Var("x"), Var("y"), "<"))))
  )
  val floatGreaterEqual = Relation("GreaterEqual", Seq(Param("x", arith.TDouble), Param("y", arith.TDouble)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), arith.BinCompare(Var("x"), Var("y"), ">="))))
  )
  val floatLesserEqual = Relation("LesserEqual", Seq(Param("x", arith.TDouble), Param("y", arith.TDouble)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), arith.BinCompare(Var("x"), Var("y"), "<="))))
  )
  val floatNotEqual = Relation("NotEqual", Seq(Param("x", arith.TDouble), Param("y", arith.TDouble)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), Eq(Var("x"), Var("y"), neg = true))))
  )
  val floatEqual = Relation("Equal", Seq(Param("x", arith.TDouble), Param("y", arith.TDouble)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), Eq(Var("x"), Var("y")))))
  )
  val floatGreater = Relation("Greater", Seq(Param("x", arith.TDouble), Param("y", arith.TDouble)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), arith.BinCompare(Var("x"), Var("y"), ">"))))
  )

  test("test float comparison") {
    val irModule = Module("FloatCompareExample", Language.Datalog, Seq(floatEdgeRel, floatEqual, floatNotEqual, floatGreaterEqual, floatGreater, floatLesser, floatLesserEqual))
    val compiledModule = new CompiledModule:
      override def name: Name = "FloatCompareExample"
      override def compilerOptions: CompilerOptions = CompilerOptions.default
      override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
      override def ir: Module = irModule

    compiledModule.setPipeline(pipeline)
    val engine = Executor.instantiate(compiledModule)
    engine.insert(Rel.from("edge", Seq("x", "y"), Seq(Seq(1.13, 2.8), Seq(4.0, 3.0), Seq(2.8, 4.0))))
    val rels = engine.readAll()
    assertResult(7)(rels.size)
  }

  test("test negation") {
    val irModule = Module("NegExample", Language.Datalog, Seq(valueRel, value2Rel, negRel))
    val compiledModule = new CompiledModule:
      override def name: Name = "NegExample"
      override def compilerOptions: CompilerOptions = CompilerOptions.default
      override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
      override def ir: Module = irModule

    compiledModule.setPipeline(pipeline)
    val engine = Executor.instantiate(compiledModule)
    engine.insert(Rel.from("n", Seq("x"), Seq(Seq(1), Seq(2), Seq(3), Seq(6), Seq(7))))
    engine.insert(Rel.from("m", Seq("x"), Seq(Seq(1), Seq(3), Seq(7))))
    val rels = engine.readAll()
    assertResult(3)(rels.size)
    rels.foreach(r => assert(r.size != 0))
  }

  test("test add ") {
    val irModule = Module("AddExample", Language.Datalog, Seq(edgeRel, add1, add2, add3))
    val compiledModule = new CompiledModule:
      override def name: Name = "AddExample"
      override def compilerOptions: CompilerOptions = CompilerOptions.default
      override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
      override def ir: Module = irModule

    compiledModule.setPipeline(pipeline)
    val engine = Executor.instantiate(compiledModule)
    engine.insert(Rel.from("edge", Seq("x", "y"), Seq(Seq(1, 2), Seq(2, 3), Seq(3, 5), Seq(2, 4), Seq(1, 5), Seq(5, 4))))
    val rels = engine.readAll()
    assertResult(4)(rels.size)
    rels.foreach(r => assert(r.size != 0))
  }

  test("test BinOP ") {
    val irModule = Module("BinOPExample", Language.Datalog, Seq(edgeRel, add2, div, sub, mul, rem))
    val compiledModule = new CompiledModule:
      override def name: Name = "BinOPExample"
      override def compilerOptions: CompilerOptions = CompilerOptions.default
      override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
      override def ir: Module = irModule

    compiledModule.setPipeline(pipeline)
    val engine = Executor.instantiate(compiledModule)
    engine.insert(Rel.from("edge", Seq("x", "y"), Seq(Seq(1, 2), Seq(2, 3), Seq(3, 5), Seq(2, 4), Seq(1, 5), Seq(5, 4))))
    val rels = engine.readAll()
    assertResult(6)(rels.size)
    rels.foreach(r => assert(r.size != 0))
  }

  val lesser = Relation("Lesser", Seq(Param("x", arith.TInt), Param("y", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), arith.BinCompare(Var("x"), Var("y"), "<"))))
  )
  val greaterEqual = Relation("GreaterEqual", Seq(Param("x", arith.TInt), Param("y", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), arith.BinCompare(Var("x"), Var("y"), ">="))))
  )
  val lesserEqual = Relation("LesserEqual", Seq(Param("x", arith.TInt), Param("y", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), arith.BinCompare(Var("x"), Var("y"), "<="))))
  )
  val notEqual = Relation("NotEqual", Seq(Param("x", arith.TInt), Param("y", arith.TInt)),
    Seq(
      Body(Seq(ExtensionalCall("edge", Seq(Var("x"), Var("y"))), Eq(Var("x"), Var("y"), neg = true))))
  )

  test("test Comparison ") {
    val irModule = Module("ComparisonExample", Language.Datalog, Seq(edgeRel, greater, lesser, lesserEqual, greaterEqual, notEqual))
    val compiledModule = new CompiledModule:
      override def name: Name = "ComparisonExample"
      override def compilerOptions: CompilerOptions = CompilerOptions.default
      override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
      override def ir: Module = irModule

    compiledModule.setPipeline(pipeline)
    val engine = Executor.instantiate(compiledModule)
    engine.insert(Rel.from("edge", Seq("x", "y"), Seq(Seq(1, 2), Seq(7, 5), Seq(2, 3), Seq(3, 5), Seq(3, 3), Seq(4, 2), Seq(1, 5), Seq(5, 4))))
    val rels = engine.readAll()
    assertResult(6)(rels.size)
    rels.foreach(r => assert(r.size != 0))
  }

  test("test x greater 3 ") {
    val irModule = Module("xgreater3Example", Language.Datalog, Seq(edgeRel, xGreater3))
    val compiledModule = new CompiledModule:
      override def name: Name = "xGreater3Example"
      override def compilerOptions: CompilerOptions = CompilerOptions.default
      override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
      override def ir: Module = irModule

    compiledModule.setPipeline(pipeline)
    val engine = Executor.instantiate(compiledModule)
    engine.insert(Rel.from("edge", Seq("x", "y"), Seq(Seq(1, 2), Seq(7, 5), Seq(2, 3), Seq(3, 5), Seq(4, 2), Seq(1, 5), Seq(5, 4))))
    val rels = engine.readAll()
    assertResult(2)(rels.size)
    rels.foreach(r => assert(r.size != 0))
  }


  test("test yIs5 ") {
    val irModule = Module("yIs5Example", Language.Datalog, Seq(edgeRel, yis5))
    val compiledModule = new CompiledModule:
      override def name: Name = "yIs5Example"
      override def ir: Module = irModule
      override def compilerOptions: CompilerOptions = CompilerOptions.default
      override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation

    compiledModule.setPipeline(pipeline)
    val engine = Executor.instantiate(compiledModule)
    engine.insert(Rel.from("edge", Seq("x", "y"), Seq(Seq(1, 2), Seq(7, 5), Seq(2, 3), Seq(3, 5), Seq(4, 2), Seq(1, 5), Seq(5, 4))))
    val rels = engine.readAll()
    assertResult(2)(rels.size)
    rels.foreach(r => assert(r.size != 0))
  }

  test("test yIsX") {
    val irModule = Module("yIsXExample", Language.Datalog, Seq(edgeRel, yisx))
    val compiledModule = new CompiledModule:
      override def name: Name = "yIsXExample"
      override def compilerOptions: CompilerOptions = CompilerOptions.default
      override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
      override def ir: Module = irModule

    compiledModule.setPipeline(pipeline)
    val engine = Executor.instantiate(compiledModule)
    engine.insert(Rel.from("edge", Seq("x", "y"), Seq(Seq(1, 2), Seq(7, 2), Seq(2, 3), Seq(3, 5), Seq(4, 2), Seq(1, 5), Seq(5, 4))))
    val rels = engine.readAll()
    assertResult(2)(rels.size)
    rels.foreach(r => assert(r.size != 0))
  }

  test("test StringConcat") {
    val irModule = Module("StringConcatExample", Language.Datalog, Seq(valueString, cat))
    val compiledModule = new CompiledModule:
      override def name: Name = "StringConcatExample"
      override def compilerOptions: CompilerOptions = CompilerOptions.default
      override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
      override def ir: Module = irModule

    compiledModule.setPipeline(pipeline)
    val engine = Executor.instantiate(compiledModule)
    engine.insert(Rel.from("Wort", Seq("x"), Seq(Seq("1"), Seq("3"), Seq("7"))))
    print("engine", engine)
    val rels = engine.readAll()
    assertResult(2)(rels.size)
    rels.foreach(r => assert(r.size != 0))
  }

  test("test String ") {
    val irModule = Module("StringExample", Language.Datalog, Seq(valueString, stringRel))
    val compiledModule = new CompiledModule:
      override def name: Name = "StringExample"
      override def ir: Module = irModule
      override def compilerOptions: CompilerOptions = CompilerOptions.default
      override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation

    compiledModule.setPipeline(pipeline)
    val engine = Executor.instantiate(compiledModule)
    engine.insert(Rel.from("Wort", Seq("x"), Seq(Seq("x"), Seq("3"), Seq("7"))))

    val rels = engine.readAll()
    assertResult(2)(rels.size)
    rels.foreach(r => assert(r.size != 0))
  }

  test("ADT test") {
    val listADT = data.DataDefinition(Name("List")) +: Seq(
      data.CaseDefinition(Name("Nil"), Seq(), data.TData(Name("List"))),
      data.CaseDefinition(Name("Cons"), Seq(arith.TInt, data.TData(Name("List"))), data.TData(Name("List")))
    )
    val module = Module("ADTTest", (data.IR.language ++ demand.IR.language.features), listADT ++ Seq(
      Relation("main", Seq(Param("out", arith.TInt)), Seq(
        Body(Seq(
          Call("createADT", Seq(arith.IntNum(10), Var("obj"))),
          data.Deconstruct(Var("obj"), "Cons", Seq(Var("out"), Var("_$1")))
        )),
        Body(Seq(
          Call("createADT", Seq(arith.IntNum(10), Var("obj"))),
          data.Deconstruct(Var("obj"), "Nil", Seq()),
          Eq(Var("out"), arith.IntNum(-1))
        ))

      )),
      Relation("createADT", Seq(Param("inVar", demand.TDemand(arith.TInt)), Param("out", data.TData("List"))), Seq(
        Body(Seq(
          Eq(Var("n"), data.Construct("Nil", Seq())),
          Eq(Var("out"), data.Construct("Cons", Seq(Var("inVar"), Var("n"))))
        ))
      ))
    ))
    val compiledModule = new CompiledModule:
      override def name: Name = "ADTTest"
      override def compilerOptions: CompilerOptions = CompilerOptions.default
      override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
      override def ir: Module = module

    compiledModule.setPipeline(pipeline)
    val engine = Executor.instantiate(compiledModule)
    val rels = engine.readAll()
    assertResult(3)(rels.size)
    rels.foreach(r => assert(r.size != 0))
  }


  val maxRel = Relation("maxRel", Seq(Param("y", arith.TInt)), Seq(
    Body(
      Seq(aggregate.Aggregate(RefByName(Name("n_wrap")), Seq(AggregateColumnArg(Var("y"))), arith.ArithmeticAggregationOperator.MaxInt))
    )
  ))
  val minRel = Relation("minRel", Seq(Param("y", arith.TInt)), Seq(
    Body(
      Seq(aggregate.Aggregate(RefByName(Name("n_wrap")), Seq(AggregateColumnArg(Var("y"))), arith.ArithmeticAggregationOperator.MinInt))
    )
  ))
  val sumRel = Relation("sumRel", Seq(Param("y", arith.TInt)), Seq(
    Body(
      Seq(aggregate.Aggregate(RefByName(Name("n_wrap")), Seq(AggregateColumnArg(Var("y"))), arith.ArithmeticAggregationOperator.SumInt))
    )
  ))
  val countRel = Relation("countRel", Seq(Param("y", arith.TInt)), Seq(
    Body(
      Seq(aggregate.Aggregate(RefByName(Name("n_wrap")), Seq(AggregateColumnArg(Var("y"))), arith.ArithmeticAggregationOperator.Count))
    )
  ))

  test("test Aggregation") {
    val irModule = Module("AggregationExample", Language.Datalog, Seq(valueRel, valueRelWrapper, maxRel, minRel, sumRel, countRel))
    val typechecker = IRTypechecker()
    typechecker.checkProgram(Seq(irModule))
    val compiledModule = new CompiledModule:
      override def name: Name = "AggregationExample"
      override def compilerOptions: CompilerOptions = CompilerOptions.default
      override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
      override def ir: Module = irModule

    compiledModule.setPipeline(pipeline)
    val engine = Executor.instantiate(compiledModule)
    engine.insert(Rel.from("n", Seq("x"), Seq(Seq(1), Seq(31), Seq(4))))

    val rels = engine.readAll()
    assertResult(6)(rels.size)
    rels.foreach(r => assert(r.size != 0))
  }
