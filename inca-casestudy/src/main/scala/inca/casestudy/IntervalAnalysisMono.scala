package inca.casestudy

import inca.casestudy.IntervalAnalysis.{TAdd, TAssign, TExp, TInterval, TNum, TSequence, TSkip, TStmt, TVar, edbNodes, mkIv}
import inca.ir.{Term, string2name, term2Arg, *}
import inca.ir.execution.{Relation1, Relation2, Relation3, Relation4}
import inca.ir.extension.*
import inca.ir.extension.arithmetic.*
import inca.ir.extension.data.*
import inca.ir.extension.demand.*
import inca.ir.extension.mono.ArithmeticMonoDefinition.SumInt
import inca.ir.extension.mono.{MapMonoDefinition, MonoImpurityKind, MonoTypes, NewMono, ReadMono, TMono, WriteMono}
import inca.ir.extension.string.*
import inca.ir.extension.tuple.{Project, TTuple, TupleLit}
import inca.ir.typing.{DependencyGraph, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
import inca.viatra.runtime.EnginePool

import scala.language.implicitConversions
import inca.foreign.scala.ir.primitive.{ConversionElimination, ForeignScalaLowering, ScalaAggregationOperator, ScalaMonoDefinition, ScalaType, Typechecker}
import inca.ir.extension.edbdata.{EdbDataModuleEntry, EdbDeconstruct, EdbFieldDefinition, EdbNodeDefinition, LookupEdbField, LookupEdbType, TEdbNode, TEdbValue}
import inca.ir.extension.map.{MapComprehension, MapLookUp, TMap, IR as mapIR}
import inca.viatra.runtime.context.DataModel
import inca.ir.extension.data.IR as dataIR
import inca.foreign.scala.ir.mono.MonoLowering as MonoScalaLowering
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.{arithmetic as scalaArith, data as scalaData, string as scalaString}
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.edbdata.Link.Parent
import inca.ir.extension.impure.{Impure, MainHint}
import inca.viatra.Executor


object IntervalAnalysisMono:

  def q(name: String): String = s"inca.casestudy.edb.$name"

  val edbNodes = EdbDataModuleEntry.fromNodeMetaInfos(edb.allNodes)

  val TStmt = TEdbNode(q("Stmt"))
  val TSkip = TEdbNode(q("Skip"))
  val TSequence = TEdbNode(q("Sequence"))
  val TAssign = TEdbNode(q("Assign"))
  val TExp = TEdbNode(q("Exp"))
  val TVar = TEdbNode(q("Var"))
  val TNum = TEdbNode(q("Num"))
  val TAdd = TEdbNode(q("Add"))
  val TInterval = TData("Interval")

  val dataDefs = Seq(
    DataDefinition("Interval"),
    CaseDefinition("IV", Seq(TInt, TInt), TInterval),
    CaseDefinition("Top", Seq(), TInterval),
    CaseDefinition("Bot", Seq(), TInterval),
  )

  val dataModel: DataModel = DataModel.from(edb.allNodes:_*)

  val intervalMono = ScalaMonoDefinition(
    "IntervalMono",
    initCode = "Bot()",
    addCode =
      """(st: Interval, a: Interval) => (st, a) match {
        |    case (Bot(), _) => a
        |    case (Top(), _) => Top()
        |    case (_, Top()) => Top()
        |    case (IV(l1, l2), IV(l3, l4)) => IV(Math.min(l1, l3), Math.max(l2, l4))
        |}""".stripMargin,
    resultCode = "(st: Interval) => st", // TODO: We could widen here
    constructorParamTypes = Seq(),
    typ = MonoTypes(TInterval, ScalaType("Interval"), ScalaType("Interval"))
  )
  val mapMono = MapMonoDefinition(TStmt, MapMonoDefinition(TString, intervalMono))
  val TMonoMap = TMono(TTuple(Seq(TStmt, TTuple(Seq(TString, TInterval)))), TMap(TStmt, TMap(TString, ScalaType("Interval"))), Seq())


  private def makeTp[T](K: Seq[T] => T, ts: T*): T =
    if ts.size == 1 then ts.head
    else if ts.size == 2 then K(ts.toSeq)
    else K(Seq(ts.head, makeTp(K, ts.tail: _*)))

  def nmapLookUp(map: Term, keys: Seq[Term]): Term =
    if keys.size == 1 then MapLookUp(map, keys.head)
    else if keys.size > 1 then MapLookUp(nmapLookUp(map, keys.dropRight(1)), keys.last)
    else throw IllegalAccessError(s"$keys is an empty list")

  def mkIv(l: Term, r: Term): Term = Construct("IV", Seq(l, r))


  val initStmt = Relation("initStmt", Seq(
    Param("stmt", TStmt),
    Param("out", TStmt)
  ), Seq(
    Body(Seq(
      //EdbDeconstruct(Var("stmt"), "Sequence", "s1" -> Var("s1")),
      Eq(Var("stmt"), LookupEdbType(TSequence)),
      Eq(Var("s1"), LookupEdbField(Cast(Var("stmt"), TSequence), "s1")),
      Call("initStmt", Seq(Var("s1"), Var("out")))
    )),
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TSkip)),
      Eq(Var("out"), Var("stmt"))
    )),
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TAssign)),
      Eq(Var("out"), Var("stmt"))
    )),
  ))

  val finalStmt = Relation("finalStmt", Seq(
    Param("stmt", TStmt),
    Param("out", TStmt)
  ), Seq(
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TSequence)),
      Eq(Var("s2"), LookupEdbField(Cast(Var("stmt"), TSequence), "s2")),
      Call("finalStmt", Seq(Var("s2"), Var("out")))
    )),
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TSkip)),
      Eq(Var("out"), Var("stmt"))
    )),
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TAssign)),
      Eq(Var("out"), Var("stmt"))
    )),
  ))

  val cflow = Relation("cflow", Seq(
    Param("from", TStmt),
    Param("to", TStmt)
  ), Seq(
    Body(
      Seq(
        Eq(Var("seqs"), LookupEdbType(TSequence)),
        Eq(Var("s1"), LookupEdbField(Cast(Var("seqs"), TSequence), "s1")),
        Eq(Var("s2"), LookupEdbField(Cast(Var("seqs"), TSequence), "s2")),
        Call("finalStmt", Seq(Var("s1"), Var("from"))),
        Call("initStmt", Seq(Var("s2"), Var("to"))),
      )
    )
  ))

  /*val allVars = Relation("allVars", Seq(
    Param("name", TString)
  ), Seq(
    Body(Seq(
      Eq(Var("s"), LookupEdbType(TAssign)),
      Eq(Var("name"), Cast(LookupEdbField(Cast(Var("s"), TAssign), "name"), TString))
    ))
  ))*/

  /*val parentOf = Relation("parentOf", Seq(
    Param("exp", TExp),
    Param("stmt", TStmt),
  ), Seq(
    Body(Seq(
      Eq(Var("exp"), LookupEdbType(TExp)),
      Eq(Var("_p"), LookupEdbField(Var("exp"), Parent)),
      Eq(Cast(Var("_p"), TStmt), LookupEdbType(TStmt)),
      Eq(Var("stmt"), Cast(Var("_p"), TStmt)),
    )),
    Body(Seq(
      Eq(Var("exp"), LookupEdbType(TExp)),
      Eq(Var("_p"), LookupEdbField(Var("exp"), Parent)),
      Eq(Cast(Var("_p"), TExp), LookupEdbType(TExp)),
      Call("parentOf", Seq(Cast(Var("_p"), TExp), Var("stmt")))
    ))
  ))*/

  val aeval = Relation("aeval",
    Seq(
      Param("m", TDemand(TMap(TString, TInterval))),
      Param("exp", TDemand(TExp)),
      Param("iv", TInterval)
    ),
    Seq(
      Body(Seq(
        //Eq(Var("exp"), LookupEdbType(TVar)),
        Eq(Var("_name"), LookupEdbField(Cast(Var("exp"), TVar), "name")),
        Eq(Var("name"), Cast(Var("_name"), TString)),
        //Call("intervalBefore", Seq(Var("stmt"), Var("name"), Var("iv")))
        Eq(Var("_iv"), MapLookUp(Var("m"), Var("name"))),
        Eq(Var("iv"), Cast(Var("_iv"), TInterval))
      )),
      Body(Seq(
        //Eq(Var("exp"), LookupEdbType(TNum)),
        Eq(Var("n"), LookupEdbField(Cast(Var("exp"), TNum), "value")),
        Eq(Var("iv"), mkIv(Cast(Var("n"), TInt), Cast(Var("n"), TInt)))
      )),
      Body(Seq(
        //Eq(Var("exp"), LookupEdbType(TAdd)),
        Eq(Var("lhs"), LookupEdbField(Cast(Var("exp"), TAdd), "lhs")),
        Eq(Var("rhs"), LookupEdbField(Cast(Var("exp"), TAdd), "rhs")),
        Call("aeval", Seq(Var("m"), Var("lhs"), Var("iv1"))),
        Call("aeval", Seq(Var("m"), Var("rhs"), Var("iv2"))),
        Deconstruct(Var("iv1"), "IV", Seq(Var("l1"), Var("l2"))),
        Deconstruct(Var("iv2"), "IV", Seq(Var("l3"), Var("l4"))),
        Eq(Var("iv"), mkIv(Add(Var("l1"), Var("l3")), Add(Var("l2"), Var("l4"))))
      ))
    )
  )

  val main = Relation("main", Seq(
    Param("after", TMonoMap),
  ), Seq(
    Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("before"), NewMono(mapMono)),
      Eq(Var("after"), NewMono(mapMono)),
      Call("interval", Seq(Var("before"), Var("after")))
    ))
  )).addHint(MainHint)

  val interval = Relation("interval", Seq(
    Param("before", TDemand(TMonoMap)),
    Param("after", TDemand(TMonoMap)),
  ), Seq(
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TAssign)),
      Eq(Var("exp"), LookupEdbField(Cast(Var("s"), TAssign), "exp")),
      Eq(Var("_name"), LookupEdbField(Cast(Var("s"), TAssign), "name")),
      Eq(Var("name"), Cast(Var("_name"), TString)),
      Call("predecessorIntervals", Seq(Var("stmt"), Var("before"))),
      Eq(Var("mp"), ReadMono(Var("before"))),
      Eq(Var("varIvMap"), Cast(MapLookUp(Var("mp"), Var("stmt")), TMap(TString, TInterval))),
      Call("aeval", Seq(Var("varIvMap"), Var("exp"), Var("iv"))),
      WriteMono(Var("after"), TupleLit(Seq(Var("stmt"), TupleLit(Seq(Cast(Var("name"), TString), Var("iv"))))), Seq())
    )),
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TSkip)),
      Call("allVars", Seq(Var("name"))),
      Call("predecessorIntervals", Seq(Var("stmt"), Var("before"))),
      Eq(Var("mp"), ReadMono(Var("before"))),
      Eq(Var("_iv"), nmapLookUp(Var("mp"), Seq(Var("stmt"), Var("name")))),
      Eq(Var("iv"), Cast(Var("_iv"), TInterval)),
      WriteMono(Var("after"), TupleLit(Seq(Var("stmt"), TupleLit(Seq(Var("name"), Var("iv"))))))
    ))
  ))

  val allVars = Relation("allVars", Seq(
    Param("name", TString)
  ), Seq(
    Body(Seq(
      Eq(Var("s"), LookupEdbType(TAssign)),
      Eq(Var("name"), Cast(LookupEdbField(Cast(Var("s"), TAssign), "name"), TString))
    ))
  ))

  val predecessorIntervals = Relation("predecessorIntervals", Seq(
    Param("stmt", TDemand(TStmt)),
    Param("before", TDemand(TMonoMap)),
  ), Seq(Body(Seq(
    Call("cflow", Seq(Var("pred"), Var("stmt"))),
    Call("predecessorIntervals", Seq(Var("pred"), Var("before"))),
    Call("allVars", Seq(Var("name"))),
    Eq(Var("mp"), ReadMono(Var("before"))),
    Eq(Var("_iv"), nmapLookUp(Var("mp"), Seq(Var("pred"), Var("name")))),
    Eq(Var("iv"), Cast(Var("_iv"), TInterval)),
    WriteMono(Var("before"), TupleLit(Seq(Var("stmt"), TupleLit(Seq(Var("name"), Var("iv"))))), Seq())
  ))
  ))

  val mod = Module("IntervalAnalysis", BaseIR.language + arithmetic.IR + dataIR + mapIR + demand.IR + string.IR + edbdata.IR + mono.IR + tuple.IR + impure.IR,
    edbNodes ++ dataDefs ++ Seq(
      allVars,
      //parentOf,
      cflow,
      initStmt,
      finalStmt,
      aeval,
      predecessorIntervals,
      main,
      interval
    )
  )



  def compiled = new CompiledModule:
    override def name: Name = "IntervalAnalysis"
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
    override def ir: Module = mod
    override def compilerOptions: CompilerOptions =
      val op = CompilerOptions.default
      op.irLogging.logModule = true
      op.irLogging.logLowerings = true
      op


    private trait demandLowering extends demand.Lowering with primitive.Visitor
    private trait blockLowering extends block.Lowering with primitive.Visitor
    private trait scalaLowering extends primitive.ScalaLowering
      with scalaArith.ScalaLowering
      with scalaData.ScalaLowering
      with scalaString.ScalaLowering
    override def typechecker = new IRTypechecker with Typechecker {}

    setPipeline(List(
      () => new mono.Lowering(true) {},
      () => new MonoScalaLowering {},
      () => new ConversionElimination {},
      () => new impure.Lowering {},
      () => new bool.Lowering {},
      () => new set.Lowering {},
      () => new map.Lowering {},
      () => new blockLowering {},
      () => new blockLowering {},
      () => new disjunction.Lowering {},
      () => new not.Lowering {},
      () => new demandLowering {},
      () => new tuple.Lowering {}
    ))

  @main def check2() = {
    println(mod)
    try
      compiled.checked

    val exec = new Executor()
    val engine = exec.instantiate(compiled, dataModel)


    val a1 = edb.Assign(
      "x", edb.Num(4)
    )
    val a2 = edb.Assign(
      "y", edb.Add(edb.Num(5), edb.Var("x"))
    )
    val s = edb.Sequence(a1, a2)

    println(s"Loading $s")
    s.loadEdits.print()
    engine.feed.processEditScript(s.loadEdits)
    engine.readAll().map(_.asTable).foreach(println)
  }