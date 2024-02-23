package inca.casestudy.interval

import inca.casestudy.interval.edb
import inca.ir.{Term, string2name, term2Arg, *}
import inca.ir.execution.{Relation1, Relation2, Relation3, Relation4, UnitRelation}
import inca.ir.extension.*
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
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
import inca.ir.extension.disjunction.DisjunctionAlternative
import inca.ir.extension.edbdata.{EdbDataModuleEntry, EdbDeconstruct, EdbFieldDefinition, EdbNodeDefinition, LookupEdbField, LookupEdbType, TEdbNode, TEdbValue}
import inca.ir.extension.map.{MapComprehension, MapLookUp, TMap, IR as mapIR}
import inca.viatra.runtime.context.DataModel
import inca.ir.extension.data.IR as dataIR
import inca.foreign.scala.ir.mono.MonoLowering as MonoScalaLowering
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.{arithmetic as scalaArith, data as scalaData, string as scalaString}
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.disjunction.{Disjunction, DisjunctionAlternative}
import inca.ir.extension.edbdata.Link.Parent
import inca.ir.extension.impure.{Impure, MainHint}
import inca.viatra.Executor
import inca.viatra.runtime.EnginePool
import inca.viatra.runtime.context.DataModel
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory

import scala.language.implicitConversions


object IntervalAnalysisMono:

  def q(name: String): String = s"inca.casestudy.interval.edb.$name"

  val edbNodes: Seq[EdbDataModuleEntry] = EdbDataModuleEntry.fromNodeMetaInfos(edb.allNodes)

  val TStmt: TEdbNode = TEdbNode(q("Stmt"))
  val TSkip: TEdbNode = TEdbNode(q("Skip"))
  val TSequence: TEdbNode = TEdbNode(q("Sequence"))
  val TAssign: TEdbNode = TEdbNode(q("Assign"))
  val TWhile: TEdbNode = TEdbNode(q("While"))
  val TExp: TEdbNode = TEdbNode(q("Exp"))
  val TVar: TEdbNode = TEdbNode(q("Var"))
  val TNum: TEdbNode = TEdbNode(q("Num"))
  val TAdd: TEdbNode = TEdbNode(q("Add"))
  val TGT: TEdbNode = TEdbNode(q("GT"))
  val TInterval: TData = TData("Interval")
  // We need this, because casting a map makes the lookup fail somehow ??
  private val TScalaInterval = ScalaType("Interval")

  val dataDefs: Seq[DataModuleEntry] = Seq(
    DataDefinition("Interval"),
    CaseDefinition("IV", Seq(TInt, TInt), TInterval),
    CaseDefinition("Top", Seq(), TInterval),
    CaseDefinition("Bot", Seq(), TInterval),
    CaseDefinition("BTrue", Seq(), TInterval),
    CaseDefinition("BFalse", Seq(), TInterval),
  )

  val dataModel: DataModel = DataModel.from(edb.allNodes:_*)

  private val intervalMono = ScalaMonoDefinition(
    "IntervalMono",
    initCode = "Bot()",
    addCode = """(st: Interval, a: Interval) => (st, a) match {
                |    case (Bot(), _) => a
                |    case (Top(), _) => Top()
                |    case (_, Top()) => Top()
                |    case (BTrue(), BTrue()) => BTrue()
                |    case (BFalse(), BFalse()) => BFalse()
                |    case (IV(l1, l2), IV(l3, l4)) =>
                |      val l = l1.min(l3)
                |      val h = l2.max(l4)
                |      if ((h - l).abs <= 2) then IV(l, h) else Top()
                |    case _ => Top()
                |}""".stripMargin,
    resultCode = "(st: Interval) => st", // TODO: We could widen here
    constructorParamTypes = Seq(),
    typ = MonoTypes(TInterval, ScalaType("Interval"), ScalaType("Interval"))
  )
  private val mapMono = MapMonoDefinition(TStmt, MapMonoDefinition(TString, intervalMono))
  private val TMonoMap = TMono(TTuple(Seq(TStmt, TTuple(Seq(TString, TInterval)))), TMap(TStmt, TMap(TString, ScalaType("Interval"))), Seq())


  private def makeTp[T](K: Seq[T] => T, ts: T*): T =
    if ts.size == 1 then ts.head
    else if ts.size == 2 then K(ts.toSeq)
    else K(Seq(ts.head, makeTp(K, ts.tail: _*)))

  def nmapLookUp(map: Term, keys: Seq[Term]): Term =
    if keys.size == 1 then MapLookUp(map, keys.head)
    else if keys.size > 1 then MapLookUp(nmapLookUp(map, keys.dropRight(1)), keys.last)
    else throw IllegalAccessError(s"$keys is an empty list")

  def mkIv(l: Term, r: Term): Term = Construct("IV", Seq(l, r))


  val initStmt: Relation = Relation("initStmt", Seq(
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
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TWhile)),
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
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TWhile)),
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
    ),
    Body(
      Seq(
        Eq(Var("whl"), LookupEdbType(TWhile)),
        Eq(Var("cond"), LookupEdbField(Cast(Var("whl"), TWhile), "cond")),
        Eq(Var("body"), LookupEdbField(Cast(Var("whl"), TWhile), "body")),
        Eq(Var("whl"), Var("from")),
        Call("initStmt", Seq(Var("body"), Var("to"))),
      )
    ),
    Body(
      Seq(
        Eq(Var("whl"), LookupEdbType(TWhile)),
        Eq(Var("cond"), LookupEdbField(Cast(Var("whl"), TWhile), "cond")),
        Eq(Var("body"), LookupEdbField(Cast(Var("whl"), TWhile), "body")),
        Call("finalStmt", Seq(Var("body"), Var("from"))),
        Eq(Var("whl"), Var("to"))
      )
    )
  ))

  val aeval = Relation("aeval",
    Seq(
      Param("m", TDemand(TMap(TString, TScalaInterval))),
      Param("exp", TDemand(TExp)),
      Param("iv", TInterval)
    ),
    Seq(
      Body(Seq(
        Eq(Var("_name"), LookupEdbField(Cast(Var("exp"), TVar), "name")),
        Eq(Var("name"), Cast(Var("_name"), TString)),
        Eq(Var("_iv"), MapLookUp(Var("m"), Var("name"))),
        Eq(Var("iv"), Cast(Var("_iv"), TInterval))
      )),
      Body(Seq(
        Eq(Var("n"), LookupEdbField(Cast(Var("exp"), TNum), "value")),
        Eq(Var("iv"), mkIv(Cast(Var("n"), TInt), Cast(Var("n"), TInt)))
      )),
      Body(Seq(
        Eq(Var("lhs"), LookupEdbField(Cast(Var("exp"), TAdd), "lhs")),
        Eq(Var("rhs"), LookupEdbField(Cast(Var("exp"), TAdd), "rhs")),
        Call("aeval", Seq(Var("m"), Var("lhs"), Var("iv1"))),
        Call("aeval", Seq(Var("m"), Var("rhs"), Var("iv2"))),
        Disjunction(Seq(
          DisjunctionAlternative(
            Deconstruct(Var("iv1"), "IV", Seq(Var("l1"), Var("l2"))),
            Deconstruct(Var("iv2"), "IV", Seq(Var("l3"), Var("l4"))),
            Eq(Var("iv"), mkIv(Add(Var("l1"), Var("l3")), Add(Var("l2"), Var("l4"))))
          ),
          DisjunctionAlternative(
            Deconstruct(Var("iv1"), "Top", Seq()),
            Eq(Var("iv"), Construct("Top", Seq()))
          ),
          DisjunctionAlternative(
            Deconstruct(Var("iv2"), "Top", Seq()),
            Eq(Var("iv"), Construct("Top", Seq()))
          ),
        )),
      )),
      Body(Seq(
        Eq(Var("lhs"), LookupEdbField(Cast(Var("exp"), TGT), "lhs")),
        Eq(Var("rhs"), LookupEdbField(Cast(Var("exp"), TGT), "rhs")),
        Call("aeval", Seq(Var("m"), Var("lhs"), Var("iv1"))),
        Call("aeval", Seq(Var("m"), Var("rhs"), Var("iv2"))),
        Deconstruct(Var("iv1"), "IV", Seq(Var("l1"), Var("h1"))),
        Deconstruct(Var("iv2"), "IV", Seq(Var("l2"), Var("h2"))),
        Disjunction(Seq(
          DisjunctionAlternative(
            GT(Var("l1"), Var("h2")),
            Eq(Var("iv"), Construct("BTrue", Seq()))
          ),
          DisjunctionAlternative(
            GT(Var("l2"), Var("h1")),
            Eq(Var("iv"), Construct("BFalse", Seq()))
          ),
          DisjunctionAlternative(
            LE(Var("l1"), Var("h2")),
            LE(Var("l2"), Var("h1")),
            Eq(Var("iv"), Construct("Top", Seq()))
          ),
        ))
      )),
      Body(Seq(
        Eq(Var("lhs"), LookupEdbField(Cast(Var("exp"), TGT), "lhs")),
        Eq(Var("rhs"), LookupEdbField(Cast(Var("exp"), TGT), "rhs")),
        Call("aeval", Seq(Var("m"), Var("lhs"), Var("iv1"))),
        Call("aeval", Seq(Var("m"), Var("rhs"), Var("iv2"))),
        Disjunction(Seq(
          DisjunctionAlternative(
            Deconstruct(Var("iv1"), "Top", Seq()),
          ),
          DisjunctionAlternative(
            Deconstruct(Var("iv2"), "Top", Seq()),
          ),
        )),
        Eq(Var("iv"), Construct("Top", Seq())),
      )),
    )
  )

  val main = Relation("main", Seq(
    Param("stmt", TStmt),
    Param("v", TString),
    Param("iv", TInterval),
    //Param("m", TMap(TString, TScalaInterval))
  ), Seq(
    Body(Seq(
      // Create mono maps
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("before"), NewMono(mapMono)),
      Eq(Var("after"), NewMono(mapMono)),

      // initial call to interval with head statement
      Eq(Var("head"), LookupEdbType(TStmt)),
      Call("cflow", Seq(WildcardArg(), Var("head").arg), true),
      Call("interval", Seq(Var("head"), Var("before"), Var("after"))),

      // output
      Disjunction(Seq(
        DisjunctionAlternative(
          Eq(Var("stmt"), LookupEdbType(TAssign))
        ),
        DisjunctionAlternative(
          Eq(Var("stmt"), LookupEdbType(TWhile))
        ),
      )),
      Eq(Var("mp"), ReadMono(Var("after"))),
      Call("allVars", Seq(Var("v"))),
      Eq(Var("iv"), Cast(
        nmapLookUp(Var("mp"), Seq(Var("stmt"), Var("v"))),
        TInterval
      ))
      // Comment this in to verify that we only read from one map. Is it the double aggregation bug again ?
      //Eq(Var("m"), MapLookUp(Var("mp"), Var("stmt"))),
      //Eq(Var("iv"), Cast(MapLookUp(Var("m"), Var("v")), TInterval)),
    ))
  )).addHint(MainHint)

  val assignToVar = Relation("assignToVar", Seq(Param("stmt", TStmt), Param("v", TString)), Seq(
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TAssign)),
      Eq(Var("_name"), LookupEdbField(Cast(Var("stmt"), TAssign), "name")),
      Eq(Var("v"), Cast(Var("_name"), TString)),
    )),
    /*Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TAssign)),
      Eq(Var("_name"), LookupEdbField(Cast(Var("stmt"), TAssign), "name")),
      Call("allVars", Seq(Var("v"))),
      Eq(Var("v"), Cast(Var("_name"), TString), true),
    )),*/
//    Body(Seq(
//      Disjunction(Seq(
//        DisjunctionAlternative(
//          Eq(Var("stmt"), LookupEdbType(TWhile)),
//        ),
//        DisjunctionAlternative(
//          Eq(Var("stmt"), LookupEdbType(TSkip)),
//        ),
//      )),
//      Call("allVars", Seq(Var("v"))),
//    )),

  ))

  /*
  30c x = 1
  4be while x > 1
  bdc   x = -1
   */

  val interval = Relation("interval", Seq(
    Param("stmt", TDemand(TStmt)),
    Param("before", TDemand(TMonoMap)),
    Param("after", TDemand(TMonoMap)),
  ), Seq(
    Body(Seq(
      // Check if the stmt is an assignment to variable `name`
      Call("assignToVar", Seq(Var("stmt"), Var("v"))),
      Eq(Var("exp"), LookupEdbField(Cast(Var("stmt"), TAssign), "exp")),
      // Update the after map
      Eq(Var("mp"), ReadMono(Var("before"))),
      Eq(Var("varIvMap"), MapLookUp(Var("mp"), Var("stmt"))),
      Call("aeval", Seq(Var("varIvMap"), Var("exp"), Var("iv"))),
      WriteMono(Var("after"), TupleLit(Seq(Var("stmt"), TupleLit(Seq(Var("v"), Var("iv"))))), Seq()),
      Call("transfer", Seq(Var("stmt"), Var("before"), Var("after")))
    )),
    Body(Seq(
      // Check if the assignment stmt does not assign value to `name`
      Call("allVars", Seq(Var("v"))),
      Call("assignToVar", Seq(Var("stmt").arg, Var("v").arg), true),
      // Update the after map
      Eq(Var("mp"), ReadMono(Var("before"))),
      Eq(Var("iv"), Cast(nmapLookUp(Var("mp"), Seq(Var("stmt"), Var("v"))), TInterval)),
      WriteMono(Var("after"), TupleLit(Seq(Var("stmt"), TupleLit(Seq(Var("v"), Var("iv"))))), Seq()),
      Call("transfer", Seq(Var("stmt"), Var("before"), Var("after")))
    ))
  ))

  val transfer = Relation("transfer", Seq(
    Param("stmt", TDemand(TStmt)),
    Param("before", TDemand(TMonoMap)),
    Param("after", TDemand(TMonoMap)),
  ), Seq(
    Body(Seq(
      Call("cflow", Seq(Var("stmt"), Var("succ"))),
      Call("allVars", Seq(Var("vn"))),
      Eq(Var("mp"), ReadMono(Var("after"))),
      Eq(Var("iv"), Cast(nmapLookUp(Var("mp"), Seq(Var("stmt"), Var("vn"))), TInterval)),
      WriteMono(Var("before"), TupleLit(Seq(Var("succ"), TupleLit(Seq(Var("vn"), Var("iv"))))), Seq()),
    )),
    Body(Seq(
      Call("cflow", Seq(Var("stmt").arg, WildcardArg()), true),
    )),
    Body(Seq(
      Call("cflow", Seq(Var("stmt"), Var("succ"))),
      Call("interval", Seq(Var("succ"), Var("before"), Var("after")))
    ))
  ))

  val allVars = Relation("allVars", Seq(
    Param("name", TString)
  ), Seq(
    Body(Seq(
      Eq(Var("s"), LookupEdbType(TAssign)),
      Eq(Var("_name"), LookupEdbField(Cast(Var("s"), TAssign), "name")),
      Eq(Var("name"), Cast(Var("_name"), TString))
    ))
  ))

  val mod = Module("IntervalAnalysis", BaseIR.language + arithmetic.IR + dataIR + mapIR + demand.IR + string.IR + edbdata.IR + mono.IR + tuple.IR + impure.IR,
    edbNodes ++ dataDefs ++ Seq(
      allVars,
      cflow,
      initStmt,
      finalStmt,
      aeval,
      transfer,
      main,
      interval,
      assignToVar
    )
  )



  def compiled(opt: Boolean) = new CompiledModule:
    override def name: Name = "IntervalAnalysis"
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
    override def ir: Module = mod
    override def compilerOptions: CompilerOptions =
      val op = CompilerOptions.default
      op.irLogging.logModule = false
      op.irLogging.logLowerings = true
      val viatraLogging = op("viatra_logging")
      viatraLogging.update("module", false)
      viatraLogging.update("lowerings", false)
      val viatraOptions = op("viatra_options")
      viatraOptions.update("apply_double_aggregation_rewrite", true)
      op


    private trait demandLowering extends demand.Lowering with primitive.Visitor
    private trait blockLowering extends block.Lowering with primitive.Visitor
    private trait scalaLowering extends primitive.ScalaLowering
      with scalaArith.ScalaLowering
      with scalaData.ScalaLowering
      with scalaString.ScalaLowering
    override def typechecker = new IRTypechecker with Typechecker {}

    setPipeline(List(
      () => new mono.Lowering(opt) {},
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
    val exec = new Executor()
    //val exec = new Executor(DRedReteBackendFactory.INSTANCE)
    val engine = exec.instantiate(compiled(true), dataModel)


    val a1 = edb.Assign(
      "x", edb.Num(4)
    )
    val a2 = edb.Assign(
      "y", edb.Add(edb.Num(5), edb.Var("x"))
    )
    val a3 = edb.Assign(
      "z", edb.Num(2)
    )
    val s = edb.Sequence(edb.Sequence(a1, a2), a3)

    println(s"Loading $s")
    s.loadEdits.print()
    engine.feed.processEditScript(s.loadEdits)
    engine.readAll().map(_.asTable).foreach(println)
    println(s.toStringWithURI)
    /*println(engine.read(UnitRelation("main")).asTable)
    println(engine.read(UnitRelation("interval")).asTable)
    println(engine.read(UnitRelation("transfer")).asTable)
    println(engine.read(UnitRelation("aeval")).asTable)*/
  }

  @main def checkWhile2 = {
    val exec = new Executor()
    //val exec = new Executor(DRedReteBackendFactory.INSTANCE)
    val engine = exec.instantiate(compiled(true), dataModel)


    val a1 = edb.Assign(
      "x", edb.Num(1)
    )

    val a2 = edb.While(
      edb.GT(edb.Var("x"), edb.Num(0)),
      edb.Assign("x", edb.Num(-1))
    )

    val s = edb.Sequence(a1, a2)

    println(s"Loading $s")
    s.loadEdits.print()
    engine.feed.processEditScript(s.loadEdits)
    engine.readAll().map(_.asTable).foreach(println)
    println(s.toStringWithURI)
  }