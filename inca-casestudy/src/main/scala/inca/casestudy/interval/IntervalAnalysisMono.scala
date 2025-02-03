package inca.casestudy.interval

import inca.casestudy.interval.Benchmark.nestedWhileProgram
import inca.casestudy.interval.edb
import inca.casestudy.util.Util.{collectGarbage, toCSV}
import inca.ir.{Term, string2name, term2Arg, *}
import inca.ir.execution.{Relation1, Relation2, Relation3, Relation4, UnitRelation}
import inca.ir.extension.*
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.arithmetic.*
import inca.ir.extension.data.*
import inca.ir.extension.demand.*
import inca.ir.extension.mono.ArithmeticMonoDefinition.SumInt
import inca.ir.extension.mono.{MapMonoDefinition, MonoImpurityKind, MonoTypes, NewMono, NewMonoFor, ReadMono, TMono, WriteMono}
import inca.ir.extension.string.*
import inca.ir.extension.tuple.{Project, TTuple, TupleLit}
import inca.ir.typing.{DependencyGraph, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
import inca.viatra.runtime.EnginePool

import scala.language.implicitConversions
import inca.foreign.scala.ir.primitive.{ConversionElimination, ForeignScalaLowering, ScalaAggregationOperator, ScalaMonoDefinition, ScalaType, Typechecker}
import inca.ir.extension.disjunction.DisjunctionAlternative
import inca.ir.extension.edbdata.{EdbDataModuleEntry, EdbDeconstruct, EdbFieldDefinition, EdbNodeDefinition, LookupEdbField, LookupEdbType, NotInEdbType, TEdbNode, TEdbValue}
import inca.ir.extension.map.{MapComprehension, MapContains, MapLit, MapLookUp, TMap, IR as mapIR}
import inca.viatra.runtime.context.DataModel
import inca.ir.extension.data.IR as dataIR
import inca.foreign.scala.ir.mono.MonoLowering as MonoScalaLowering
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.primitive.ScalaInca.cleanString
import inca.foreign.scala.ir.{arithmetic as scalaArith, data as scalaData, string as scalaString}
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.disjunction.{Disjunction, DisjunctionAlternative}
import inca.ir.extension.edbdata.Link.Parent
import inca.ir.extension.impure.Impure
import inca.util.CSVUtil.csvToString
import inca.util.FileUtil
import inca.viatra.backend.Executor
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
  val TExit: TEdbNode = TEdbNode(q("Exit"))
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

  val dataModel: DataModel = DataModel.from(edb.allNodes: _*)

  private val intervalMono = ScalaMonoDefinition(
    "IntervalMono",
    initCode = "Bot()",
    addCode =
      """(st: Interval, a: Interval) => (st, a) match {
        |    case (Bot(), _) => a
        |    case (_,Bot()) => st
        |    case (Top(), _) => Top()
        |    case (_, Top()) => Top()
        |    case (BTrue(), BTrue()) => BTrue()
        |    case (BFalse(), BFalse()) => BFalse()
        |    case (IV(l1, l2), IV(l3, l4)) =>
        |      val l = l1.min(l3)
        |      val h = l2.max(l4)
        |      if ((h - l).abs <= 5) then IV(l, h) else Top()
        |    case _ => Top()
        |}""".stripMargin,
    resultCode = "(st: Interval) => st", // TODO: We could widen here
    combineCode =
      """(st: Interval, a: Interval) => (st, a) match {
        |    case (Bot(), _) => a
        |    case (_,Bot()) => st
        |    case (Top(), _) => Top()
        |    case (_, Top()) => Top()
        |    case (BTrue(), BTrue()) => BTrue()
        |    case (BFalse(), BFalse()) => BFalse()
        |    case (IV(l1, l2), IV(l3, l4)) =>
        |      val l = l1.min(l3)
        |      val h = l2.max(l4)
        |      if ((h - l).abs <= 5) then IV(l, h) else Top()
        |    case _ => Top()
        |}""".stripMargin,
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
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TExit)),
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
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TExit)),
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
        Eq(Var("var"), LookupEdbField(Cast(Var("exp"), TVar), "name")),
        Eq(Var("name"), Cast(Var("var"), TString)),
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
    Param("exit", TStmt),
    Param("x", TString),
    Param("x_iv", TInterval),
    //    Param("m", TMap(TString, TScalaInterval)),
    //    Param("mp", mapMono.monoType(Seq()).output)
  ), Seq(
    Body(Seq(
      // output
      Eq(Var("exit"), LookupEdbType(TExit)),
      Call("traverse", Seq(Var("exit"), Var("before"))),
      Eq(Var("mp"), ReadMono(Var("before"))),

      Eq(Var("m"), MapLookUp(Var("mp"), Var("exit"))),
      Call("allVars", Seq(Var("x"))),
      Eq(Var("x_iv"), Cast(MapLookUp(Var("m"), Var("x")), TInterval)),
    ))
  )).addHint(MainHint)

  val transfer = Relation("transfer", Seq(
    Param("stmt", TStmt),
    //    Param("x", TString),
    //    Param("x_iv", TInterval)
  ), Seq(
    Body(Seq(
      Call("traverse", Seq(Var("stmt"), Var("before"))),
      // Check if the stmt is an assignment to variable `name`
      Eq(Var("stmt"), LookupEdbType(TAssign)),
      Eq(Var("x"), Cast(LookupEdbField(Cast(Var("stmt"), TAssign), "name"), TString)),
      Eq(Var("exp"), LookupEdbField(Cast(Var("stmt"), TAssign), "exp")),

      // compute new value of x
      Disjunction(
        Seq(
          Eq(Var("mp"), ReadMono(Var("before"))),
          Eq(Var("varIvMap"), MapLookUp(Var("mp"), Var("stmt"))),
        ),
        Seq(
          Eq(Var("varIvMap"), Cast(MapLit.empty, TMap(TString, TScalaInterval))),
        )
      ),
      Call("aeval", Seq(Var("varIvMap"), Var("exp"), Var("x_iv"))),

      // Update the map before the successor
      Call("cflow", Seq(Var("stmt"), Var("succ"))),
      WriteMono(Var("before"), TupleLit(Seq(Var("succ"), TupleLit(Seq(Var("x"), Var("x_iv"))))), Seq())
    )),
    Body(Seq(
      Call("traverse", Seq(Var("stmt"), Var("before"))),

      // Check if the assignment stmt does not assign value to `name`
      Disjunction(
        Seq(
          Eq(Var("stmt"), LookupEdbType(TAssign)),
          Eq(Var("name"), Cast(LookupEdbField(Cast(Var("stmt"), TAssign), "name"), TString)),
        )
        , Seq(
          Eq(Var("stmt"), LookupEdbType(TStmt)),
          NotInEdbType(Var("stmt"), TAssign),
          Eq(Var("name"), StringLit(""))
        )
      ),

      // Fetch value of x
      Eq(Var("mp"), ReadMono(Var("before"))),
      Eq(Var("varIvMap"), MapLookUp(Var("mp"), Var("stmt"))),
      Call("allVars", Seq(Var("x"))),
      Eq(Var("name"), Var("x"), neg = true),
      Eq(Var("x_iv"), Cast(MapLookUp(Var("varIvMap"), Var("x")), TInterval)),

      // Propagate to map before successor
      Call("cflow", Seq(Var("stmt"), Var("succ"))),
      WriteMono(Var("before"), TupleLit(Seq(Var("succ"), TupleLit(Seq(Var("x"), Var("x_iv"))))), Seq())
    ))
  ))

  val traverse = Relation("traverse", Seq(
    Param("stmt", TStmt),
    Param("before", TMonoMap)
  ), Seq(
    Body(Seq(
      // first statement
      Eq(Var("stmt"), LookupEdbType(TStmt)),
      Call("cflow", Seq(Var("stmt").arg, WildcardArg()), neg = false), // has cflow
      Call("cflow", Seq(WildcardArg(), Var("stmt").arg), neg = true), // but no predecessor
      Eq(Var("before"), NewMonoFor(mapMono, Seq(), Seq(), Seq(Var("stmt"))))
    )),
    Body(Seq(
      // subsequent statements
      Call("cflow", Seq(Var("pred"), Var("stmt"))),
      Call("traverse", Seq(Var("pred"), Var("before")))
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
      traverse,
      main,
      transfer,
      //      assignToVar
    )
  )


  def compiled(opt: Boolean) = new CompiledUnit:
    override def name: Name = "IntervalAnalysis"

    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation

    override val irModules: Seq[Module] = Seq(mod)

    override val isClosedWorld: Boolean = true

    override val otherUnits: Seq[CompiledUnit] = Seq()

    override def compilerOptions: CompilerOptions =
      val op = CompilerOptions.default
      op.irLogging.logModule = false
      op.irLogging.logLowerings = false
      val viatraLogging = op("viatra_logging")
      viatraLogging.update("module", false)
      viatraLogging.update("lowerings", false)
      val viatraOptions = op("viatra_options")
      viatraOptions.update("apply_double_aggregation_rewrite", false)
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
    //    val exec = new Executor()
    val exec = new Executor(DRedReteBackendFactory.INSTANCE)
    val engine = exec.instantiate(compiled(false), dataModel)


    val a1 = edb.Assign(
      "x", edb.Num(4)
    )
    val a2 = edb.Assign(
      "y", edb.Var("x")
    )
    val a3 = edb.Assign(
      "z1", edb.Num(2)
    )
    val a4 = edb.Assign(
      "z2", edb.Num(1)
    )
    //    val s = edb.Sequence(edb.Sequence(a1, a2), a3)
    val s = edb.Sequence(edb.Sequence(edb.Sequence(a1, a2), edb.Sequence(a3, a4)), edb.Exit())
    //    val s = edb.Sequence(a1, a2)

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

  @main def check2NoOpt() = {
    //    val exec = new Executor()
    val exec = new Executor(DRedReteBackendFactory.INSTANCE)
    val module = compiled(false)
    println(module.compiled)
    val engine = exec.instantiate(module, dataModel)

    val a1 = edb.Assign(
      "x", edb.Num(4)
    )
    val a2 = edb.Assign(
      "y", edb.Var("x")
    )
    val a3 = edb.Assign(
      "z1", edb.Num(2)
    )
    val a4 = edb.Assign(
      "z2", edb.Num(1)
    )
    //    val s = edb.Sequence(edb.Sequence(a1, a2), a3)
    val s = edb.Sequence(edb.Sequence(edb.Sequence(a1, a2), edb.Sequence(a3, a4)), edb.Exit())
    //    val s = edb.Sequence(a1, a2)

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
    val exec = new Executor(DRedReteBackendFactory.INSTANCE)
    //val exec = new Executor(DRedReteBackendFactory.INSTANCE)
    val engine = exec.instantiate(compiled(true), dataModel)


    val a1 = edb.Assign(
      "x", edb.Num(1)
    )

    val a2 = edb.While(
      edb.GT(edb.Var("x"), edb.Num(0)),
      edb.Assign("x", edb.Add(edb.Var("x"), edb.Num(-1)))
    )

    val s = edb.Sequence(edb.Sequence(a1, a2), edb.Exit())

    println(s"Loading $s")
    s.loadEdits.print()
    engine.feed.processEditScript(s.loadEdits)
    engine.readAll().map(_.asTable).foreach(println)
    println(s.toStringWithURI)
  }

  @main def checkWhile2NoOpt = {
    val exec = new Executor(DRedReteBackendFactory.INSTANCE)
    //val exec = new Executor(DRedReteBackendFactory.INSTANCE)
    val engine = exec.instantiate(compiled(false), dataModel)


    val a1 = edb.Assign(
      "x", edb.Num(1)
    )

    val a2 = edb.While(
      edb.GT(edb.Var("x"), edb.Num(0)),
      edb.Assign("x", edb.Add(edb.Var("x"), edb.Num(-1)))
    )

    val s = edb.Sequence(edb.Sequence(a1, a2), edb.Exit())

    println(s"Loading $s")
    s.loadEdits.print()
    engine.feed.processEditScript(s.loadEdits)
    engine.readAll().map(_.asTable).foreach(println)
    println(s.toStringWithURI)
  }

  @main def checkBigWhileStable = {
    val exec = new Executor(DRedReteBackendFactory.INSTANCE)
    //val exec = new Executor(DRedReteBackendFactory.INSTANCE)
    val engine = exec.instantiate(compiled(true), dataModel)

    val nestings = 50
    val repetitions = 50

    val a1 = edb.Assign(
      "x", edb.Num(1)
    )

    def nestedWhile(levels: Int): edb.Stmt =
      if (levels == 0)
        edb.Sequence(
          edb.Assign("x", edb.Add(edb.Var("x"), edb.Num(-1))),
          edb.Assign("x", edb.Add(edb.Var("x"), edb.Num(1)))
        )
      else
        edb.While(
          edb.GT(edb.Var("x"), edb.Num(0)),
          nestedWhile(levels - 1)
        )

    def sequence(s: () => edb.Stmt, counts: Int): edb.Stmt =
      if (counts == 0)
        s()
      else
        edb.Sequence(s(), sequence(s, counts - 1))

    val s = edb.Sequence(
      edb.Assign("x", edb.Num(1)),
      edb.Sequence(
        sequence(() => nestedWhile(nestings), repetitions),
        edb.Exit()))

    val startLoad = System.nanoTime()
    engine.feed.processEditScript(s.loadEdits)
    val endLoad = System.nanoTime()
    val loadTimeMs = (endLoad - startLoad) / 1000000

    val startProp = System.nanoTime()
    val spec = engine.module.patterns(cleanString("main"))()
    val matcher = spec.getMatcher(engine.engine)
    val endProp = System.nanoTime()
    val propTimeMs = (endProp - startProp) / 1000000


    val mainRel = engine.read(Relation3("main", Seq("exit", "x", "x_iv"), Seq()))
    println(mainRel.asTable)
    val cflowRel = engine.read(Relation2("cflow", Seq("from", "to"), Seq()))
    println(s"${cflowRel.size} cflow entries")

    println(s"Load time ${loadTimeMs}ms")
    println(s"Propagation time ${propTimeMs}ms")
  }

  @main def checkBigWhileStableNoOpt = {
    val exec = new Executor(DRedReteBackendFactory.INSTANCE)
    //val exec = new Executor(DRedReteBackendFactory.INSTANCE)
    val engine = exec.instantiate(compiled(false), dataModel)

    val nestings = 50
    val repetitions = 50

    val a1 = edb.Assign(
      "x", edb.Num(1)
    )

    def nestedWhile(levels: Int): edb.Stmt =
      if (levels == 0)
        edb.Sequence(
          edb.Assign("x", edb.Add(edb.Var("x"), edb.Num(-1))),
          edb.Assign("x", edb.Add(edb.Var("x"), edb.Num(1)))
        )
      else
        edb.While(
          edb.GT(edb.Var("x"), edb.Num(0)),
          nestedWhile(levels - 1)
        )

    def sequence(s: () => edb.Stmt, counts: Int): edb.Stmt =
      if (counts == 0)
        s()
      else
        edb.Sequence(s(), sequence(s, counts - 1))

    val s = edb.Sequence(
      edb.Assign("x", edb.Num(1)),
      edb.Sequence(
        sequence(() => nestedWhile(nestings), repetitions),
        edb.Exit()))

    val startLoad = System.nanoTime()
    engine.feed.processEditScript(s.loadEdits)
    val endLoad = System.nanoTime()
    val loadTimeMs = (endLoad - startLoad) / 1000000

    val startProp = System.nanoTime()
    val spec = engine.module.patterns(cleanString("main"))()
    val matcher = spec.getMatcher(engine.engine)
    val endProp = System.nanoTime()
    val propTimeMs = (endProp - startProp) / 1000000


    val mainRel = engine.read(Relation3("main", Seq("exit", "x", "x_iv"), Seq()))
    println(mainRel.asTable)
    val cflowRel = engine.read(Relation2("cflow", Seq("from", "to"), Seq()))
    println(s"${cflowRel.size} cflow entries")

    println(s"Load time ${loadTimeMs}ms")
    println(s"Propagation time ${propTimeMs}ms")
  }

  @main def checkBigWhile = {
    val exec = new Executor(DRedReteBackendFactory.INSTANCE)
    //val exec = new Executor(DRedReteBackendFactory.INSTANCE)
    val engine = exec.instantiate(compiled(false), dataModel)

    val nestings = 100
    val repetitions = 100

    val a1 = edb.Assign(
      "x", edb.Num(1)
    )

    def nestedWhile(levels: Int): edb.Stmt =
      if (levels == 0)
        edb.Sequence(
          edb.Assign("x", edb.Add(edb.Var("x"), edb.Num(-1))),
          edb.Assign("x", edb.Add(edb.Var("x"), edb.Num(2)))
        )
      else
        edb.While(
          edb.GT(edb.Var("x"), edb.Num(0)),
          nestedWhile(levels - 1)
        )

    def sequence(s: () => edb.Stmt, counts: Int): edb.Stmt =
      if (counts == 0)
        s()
      else
        edb.Sequence(s(), sequence(s, counts - 1))

    val s = edb.Sequence(
      edb.Assign("x", edb.Num(1)),
      edb.Sequence(
        sequence(() => nestedWhile(nestings), repetitions),
        edb.Exit()))

    val startLoad = System.nanoTime()
    engine.feed.processEditScript(s.loadEdits)
    val endLoad = System.nanoTime()
    val loadTimeMs = (endLoad - startLoad) / 1000000

    val startProp = System.nanoTime()
    val spec = engine.module.patterns(cleanString("main"))()
    val matcher = spec.getMatcher(engine.engine)
    val endProp = System.nanoTime()
    val propTimeMs = (endProp - startProp) / 1000000


    val mainRel = engine.read(Relation3("main", Seq("exit", "x", "x_iv"), Seq()))
    println(mainRel.asTable)
    val cflowRel = engine.read(Relation2("cflow", Seq("from", "to"), Seq()))
    println(s"${cflowRel.size} cflow entries")

    println(s"Load time ${loadTimeMs}ms")
    println(s"Propagation time ${propTimeMs}ms")
  }

  @main def checkBigWhileUnstableNoOpt = {
    val exec = new Executor(DRedReteBackendFactory.INSTANCE)
    //val exec = new Executor(DRedReteBackendFactory.INSTANCE)
    val engine = exec.instantiate(compiled(false), dataModel)

    val nestings = 100
    val repetitions = 100

    val s = nestedWhileProgram(nestings, repetitions)

    val startLoad = System.nanoTime()
    engine.feed.processEditScript(s.loadEdits)
    val endLoad = System.nanoTime()
    val loadTimeMs = (endLoad - startLoad) / 1000000

    val startProp = System.nanoTime()
    val spec = engine.module.patterns(cleanString("main"))()
    val matcher = spec.getMatcher(engine.engine)
    val endProp = System.nanoTime()
    val propTimeMs = (endProp - startProp) / 1000000


    val mainRel = engine.read(Relation3("main", Seq("exit", "x", "x_iv"), Seq()))
    println(mainRel.asTable)
    val cflowRel = engine.read(Relation2("cflow", Seq("from", "to"), Seq()))
    println(s"${cflowRel.size} cflow entries")

    println(s"Load time ${loadTimeMs}ms")
    println(s"Propagation time ${propTimeMs}ms")
  }

  @main def measureBigWhile2 = {
    val resultPath = "benchmark/mono"
    val opt = false // opt = false is waaaayyy to slow
    val suffix = if opt then "_opt" else ""

    val warmups = 3
    val runs = 5

    val start = 10
    val maxRep = 50
    val step = 5
    val nestings = 5

    val measurements = for (reps <- Range.inclusive(start, maxRep, step)) yield {
      // input program
      val s = nestedWhileProgram(nestings, reps)

      var cflowSize = 0
      // Stats
      {
        val engine = new Executor(DRedReteBackendFactory.INSTANCE).instantiate(compiled(opt), dataModel)
        engine.feed.processEditScript(s.loadEdits)

        val rels = engine.readAll()
        val stats = ("total" -> IndexedSeq(rels.map(_.size).sum.toLong)) +: engine.readAll().map { r =>
          r.name -> IndexedSeq(r.size.toLong)
        }
        val cflowRel = engine.read(Relation2("cflow", Seq("from", "to"), Seq()))
        cflowSize = cflowRel.size

        FileUtil.writeFile(s"$resultPath/interval/Interval_Mono${suffix}_${cflowSize}_stats.csv", csvToString(toCSV(stats)))
      }

      assert(cflowSize != 0)

      collectGarbage()

      // Warmup
      for (k <- Range.inclusive(1, warmups)) {
        val engine = new Executor(DRedReteBackendFactory.INSTANCE).instantiate(compiled(opt), dataModel)
        engine.feed.processEditScript(s.loadEdits)
        engine.measure(Relation3("main", Seq("exit", "x", "x_iv"), Seq()))

        collectGarbage()
      }

      // Measurement run
      cflowSize.toString -> (for (j <- Range.inclusive(1, runs)) yield {
        collectGarbage()

        val engine = new Executor(DRedReteBackendFactory.INSTANCE).instantiate(compiled(opt), dataModel)

        val startLoad = System.nanoTime()
        engine.feed.processEditScript(s.loadEdits)
        val endLoad = System.nanoTime()
        val loadTime = endLoad - startLoad

        val propTime = engine.measure(Relation3("main", Seq("exit", "x", "x_iv"), Seq()))
        val cflowRel = engine.read(Relation2("cflow", Seq("from", "to"), Seq()))

        println(s"${cflowRel.size} cflow entries")
        println(s"Load time ${loadTime / 1000000}ms")
        println(s"Propagation time ${propTime / 1000000}ms")
        println()

        propTime
      })
    }

    FileUtil.writeFile(s"$resultPath/interval/Interval_Mono$suffix.csv", csvToString(toCSV(measurements)))
  }