package inca.viatra.runtime.aggregate

import inca.foreign.scala.ir.primitive.ScalaMonoDefinition
import inca.ir.execution.{ExecutorEngine, IRExecutor, Relation1, Relation2, Relation3, UnitRelation, Relation as Table}
import inca.ir.extension.arithmetic.{IntNum, TDouble, TInt}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.impure.{Impure, PureHint}
import inca.ir.extension.map.TMap
import inca.ir.extension.mono.ArithmeticMonoDefinition.{Count, CountFrom, MaxInt, SumInt}
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.{BaseIR, Body, Call, Cast, Eq, ExtensionalCall, ExtensionalRelation, Language, Module, ModuleEntry, Name, Param, Relation, TAny, Term, Type, Var, string2name, term2Arg}
import inca.ir.extension.{aggregate, arithmetic, block, bool, data, demand, impure, mono, string}
import inca.ir.extension.mono.{MonoImpurityKind, MonoTypes, NewMono, ReadMono, TMono, WriteMono}
import inca.ir.extension.set.TSet
import inca.ir.extension.tuple.TTuple

import scala.util.Random
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.collection.mutable


class MonoAggregationTest extends AnyFunSuiteLike {
  private val langs : Language = BaseIR.language +
    arithmetic.IR + 
    demand.IR + 
    data.IR + 
    aggregate.IR +
    mono.IR +
    impure.IR +
    block.IR +
    string.IR +
    bool.IR
  
  private def module(relations: ModuleEntry*): Module =
    val mod = Module("M", langs, relations)
    mod


  private def compile(relations: ModuleEntry*): ExecutorEngine =
    val mod = Module("M", langs, relations)
    val compiledMod = CompiledMonoModule(mod)
    compiledMod.setPipeline(CompiledMonoModule.pipeline)
    val exec: IRExecutor = inca.viatra.Executor
    exec.instantiate(compiledMod)

  private lazy val relation1: Relation = Relation(
    "main",
    Seq(
      Param("b", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m"), NewMono(SumInt, Seq(TString), Seq())),
      Eq(Var("b"), ReadMono(Var("m")))
    )))).addHint(PureHint)


  private lazy val mainInput: ExtensionalRelation = ExtensionalRelation(
    Name("main$input"), Seq(Param(Name("id"), TInt))
  )


  test("Test case 1") {
    val engine = compile(relation1)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(0)(res.entries.head)
  }

  private lazy val relation3: Relation = Relation(
    "main",
    Seq(
      Param("b", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m"), NewMono(SumInt, Seq(TString), Seq())),
      WriteMono(Var("m"), IntNum(1), Seq(StringLit("A"))),
      Eq(Var("b"), ReadMono(Var("m")))
    )))).addHint(impure.PureHint)

  test("Test case 2") {
    val engine = compile(relation3)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assertResult(1)(res.entries.head)
  }

  private lazy val relation4: Relation = Relation(
    "main",
    Seq(
      Param("b", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m"), NewMono(SumInt, Seq(TString), Seq())),
      Call("size", Seq(Var("t"), Var("m"))),
      Eq(Var("b"), ReadMono(Var("m")))
    )))).addHint(impure.PureHint)

  private lazy val relation5: Relation = Relation(
    "size",
    Seq(Param("t", TString), Param("m", TDemand(TMono(TInt, TInt, Seq(TString))))),
    Seq(
      Body(Seq(
        ExtensionalCall(Name("leaf"), Seq(Var("t"))),
        WriteMono(Var("m"), IntNum(1), Seq(Var("t")))
      ))
    )
  )

  private lazy val extLeaf: ExtensionalRelation = ExtensionalRelation(
    "leaf", Seq(Param("t", TString))
  )

  private lazy val edbMainInput: Relation1[Seq[Int]] = Relation1("main$input", Seq("id"), Seq(Seq(1)))

  private lazy val edbLeaf: Relation1[Seq[String]] = Relation1("leaf", Seq("t"), Seq(Seq("C"), Seq("D"), Seq("E")))

  private lazy val edbBTree: Relation3[Seq[String], Seq[String], Seq[String]] = Relation3(
    "btree",
    Seq("t", "l", "r"),
    Seq(Seq("A", "B", "C"), Seq("B", "D", "E"))
  )

  test("Test case 3") {
    val engine = compile(relation4, relation5, extLeaf)
    engine.insert(edbLeaf)
    val res = engine.read(UnitRelation("main"))
    assertResult(3)(res.entries.head)
  }


  private lazy val relation6: Relation = Relation(
    "main",
    Seq(
      Param("b1", TInt),
      Param("b2", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m1"), NewMono(SumInt, Seq(TString), Seq())),
      Eq(Var("m2"), NewMono(SumInt, Seq(TString), Seq())),
      Call("size", Seq(Var("t"), Var("m1"))),
      Eq(Var("b1"), ReadMono(Var("m1"))),
      Eq(Var("b2"), ReadMono(Var("m2")))
    )))).addHint(PureHint)

  test("Test case 4") {
    val engine = compile(relation6, relation5, extLeaf)
    engine.insert(edbMainInput)
    engine.insert(edbLeaf)
    val res = engine.read(UnitRelation("main"))
    assertResult((3, 0))(res.entries.head)
  }

  private lazy val extBTree: ExtensionalRelation = ExtensionalRelation(
    "btree", Seq(Param("t", TString), Param("l", TString), Param("r", TString))
  )

  private lazy val relation7: Relation = Relation(
    "size",
    Seq(Param("t", TString), Param("m", TDemand(TMono(TInt, TInt, Seq(TString))))),
    Seq(
      Body(Seq(
        ExtensionalCall(Name("leaf"), Seq(Var("t"))),
        WriteMono(Var("m"), IntNum(1), Seq(Var("t")))
      )),
      Body(Seq(
        ExtensionalCall(Name("btree"), Seq(Var("t"), Var("l"), Var("r"))),
        Call(Name("size"), Seq(Var("l"), Var("m"))),
        Call(Name("size"), Seq(Var("r"), Var("m"))),
        WriteMono(Var("m"), IntNum(1), Seq(Var("t")))
      ))
    )
  )


  test("Test case 5") {
    val engine = compile(relation6, relation7, extLeaf, extBTree)
    engine.insert(edbMainInput)
    engine.insert(edbLeaf)
    engine.insert(edbBTree)
    val res = engine.read(UnitRelation("main"))
    assertResult((5, 0))(res.entries.head)
  }


  private lazy val relation8: Relation = Relation(
    "main",
    Seq(
      Param("b1", TInt),
      Param("b2", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m1"), NewMono(SumInt, Seq(TString), Seq())),
      Eq(Var("m2"), NewMono(MaxInt, Seq(TString), Seq())),
      Call("size", Seq(Var("t"), Var("m1"))),
      Call("size", Seq(Var("t"), Var("m2"))),
      Eq(Var("b1"), ReadMono(Var("m1"))),
      Eq(Var("b2"), ReadMono(Var("m2")))
    )))).addHint(impure.PureHint)

  test("Test case 6") {
    val engine = compile(relation8, relation7, extLeaf, extBTree)
    engine.insert(edbMainInput)
    engine.insert(edbLeaf)
    engine.insert(edbBTree)
    val res = engine.read(UnitRelation("main"))
    assertResult((5, 1))(res.entries.head)
  }

  private val customMono = ScalaMonoDefinition(
    Name("addString"),
    "0.0", // we should be able to typecheck the foreign scala term, e.g. report errors if it was 0.0
    "(st: Double, a: Int) => st + a",
    "(st: Double) => st.toString",
    Seq(),
    MonoTypes(TInt, TDouble, TString)
  )

  private lazy val relationUserDefinedMono: Relation = Relation(
    "main",
    Seq(
      Param("b", TString)
    ),
    Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m"), NewMono(customMono, Seq(TString), Seq())),
      Eq(Var("b"), ReadMono(Var("m")))
    )))).addHint(PureHint)

  test("Test using user-defined mono definition 1") {
    val engine = compile(relationUserDefinedMono)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assertResult("0.0")(res.entries.head)
  }

  // non-standard set mono, probably we need to add a SetSize term?
  // TODO: test polymorphic setmono
  private lazy val SetMono = ScalaMonoDefinition(
    "SetMono",
    initCode = "Set[Any]()",
    addCode = "(st: Set[Any], a: Any) => st + a",
    resultCode = "(st: Set[Any]) => st.size",
    constructorParamTypes = Seq(),
    typ = MonoTypes(TAny, TSet(TAny), TInt)
  )

  // compute the size of graph
  // main(n: TInt) :- m = SetMono, size(m), b = n.result()
  // size(m) :- edge(e1, e2), m <- e1, m <- e2
  private lazy val graphSizeMain = Relation(
    "main",
    Seq(Param("n", TInt)),
    Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m"), NewMono(SetMono, Seq(), Seq())),
      Call("size", Seq(Var("m"))),
      Eq(Var("n"), ReadMono(Var("m")))
    )))
  ).addHint(PureHint)

  private lazy val graphSize = Relation(
    "size",
    Seq(Param("m", TDemand(TMono(TAny, TInt, Seq())))),
    Seq(Body(Seq(
      ExtensionalCall("edge", Seq(Var("e1"), Var("e2"))),
      WriteMono(Var("m"), Cast(Var("e1"), TAny), Seq()),
      WriteMono(Var("m"), Cast(Var("e2"), TAny), Seq()),
    )))
  )

  private lazy val extEdge: ExtensionalRelation = ExtensionalRelation(
    "edge", Seq(Param("e1", TString), Param("e2", TString))
  )

  private lazy val edbEdge: Relation2[Seq[String], Seq[String]] = Relation2(
    "edge",
    Seq("e1", "e2"),
    Seq(
      Seq("6", "2"), Seq("6", "3"), Seq("8", "4"),
      Seq("6", "4"), Seq("9", "5"), Seq("7", "8"),
      Seq("6", "6"), Seq("6", "7"), Seq("8", "9"),
      Seq("2", "9"), Seq("0", "4"), Seq("0", "7"),
      Seq("1", "5"), Seq("5", "9"), Seq("5", "8"),
      Seq("3", "3"), Seq("9", "0"), Seq("1", "0"),
      Seq("2", "5"), Seq("4", "2"), Seq("0", "0"),
      Seq("8", "3"), Seq("9", "3"), Seq("0", "2"),
      Seq("8", "1")
    )
  )


  test("Test set mono") {
    // Problems: should we make collection and aggregation relation pure?
    val engine = compile(graphSizeMain, graphSize, extEdge)
    engine.insert(edbEdge)
    engine.readAll().foreach(res => println(res.asTable))
//    val res = engine.read(UnitRelation("main"))

  }


  private def combination(n: Int): Seq[(Int, Int)] =
    for {i <- 0 until n; j <- 0 until n} yield (i, j)

  private def generateGraph(seed: Int): Unit =
    val comb = combination(seed)
    val set = mutable.Set[Int]()
    val res = mutable.Set[String]()
    for (i <- 0 until 30)
      val k = Random.nextInt(comb.length)
      println("Generate random number " + k)
      if !set.contains(k) then
        val p = s"""Seq("${comb(k)._1}", "${comb(k)._2}")"""
        res += p
        set += k
    println(res.mkString(", "))


  private lazy val multiMapMono = ScalaMonoDefinition(
    name = "MapMono",
    initCode = "Map[String, Set[String]]()",
    addCode =
      """(st: Map[String, Set[String]], a: (String, String)) =>
        |    if st.contains(a._1) then
        |      st + (a._1 -> (st(a._1) + a._2))
        |    else
        |      st + (a._1 -> Set(a._2))
        |""".stripMargin,
    resultCode = "(st : Map[String, Set[String]] => st",
    Seq(),
    typ = MonoTypes(TTuple(Seq(TString, TString)), TMap(TString, TSet(TString)), TMap(TString, TSet(TString)))
  )


  test("test map mono"){

  }

}