package inca.casestudy.interval

import inca.casestudy.interval
import inca.casestudy.interval.IntervalAnalysisMono.TAssign
import inca.casestudy.interval.edb.{Assign, Num, Sequence, While}
import inca.foreign.scala.ir.primitive.{IR, Typechecker, *}
import inca.foreign.scala.ir.{primitive, arithmetic as scalaArith, data as scalaData, string as scalaString}
import inca.ir.execution.{Relation1, Relation2, Relation3, Relation4}
import inca.ir.extension.*
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.arithmetic.*
import inca.ir.extension.data.{IR as dataIR, *}
import inca.ir.extension.demand.*
import inca.ir.extension.disjunction.{Disjunction, DisjunctionAlternative}
import inca.ir.extension.edbdata.*
import inca.ir.extension.edbdata.Link.Parent
import inca.ir.extension.impure.Impure
import inca.ir.extension.map.{MapComprehension, MapLookUp, TMap, IR as mapIR}
import inca.ir.extension.string.*
import inca.ir.extension.tuple.{Project, TTuple, TupleLit}
import inca.ir.typing.{DependencyGraph, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.ir.{Term, string2name, term2Arg, *}
import inca.util.compileroptions.CompilerOptions
import inca.viatra.Executor
import inca.viatra.runtime.EnginePool
import inca.viatra.runtime.context.DataModel
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory

import scala.language.implicitConversions


object IntervalAnalysis:

  def q(name: String): String = s"inca.casestudy.interval.edb.$name"

  val edbNodes = EdbDataModuleEntry.fromNodeMetaInfos(edb.allNodes)
  edbNodes.foreach(println(_))

  val TStmt = TEdbNode(q("Stmt"))
  val TSkip = TEdbNode(q("Skip"))
  val TSequence = TEdbNode(q("Sequence"))
  val TAssign = TEdbNode(q("Assign"))
  val TWhile = TEdbNode(q("While"))
  val TExp = TEdbNode(q("Exp"))
  val TVar = TEdbNode(q("Var"))
  val TNum = TEdbNode(q("Num"))
  val TAdd = TEdbNode(q("Add"))
  val TGT = TEdbNode(q("GT"))
  val TInterval = TData("Interval")

  val dataDefs = Seq(
    DataDefinition("Interval"),
    CaseDefinition("IV", Seq(TInt, TInt), TInterval),
    CaseDefinition("Top", Seq(), TInterval),
    CaseDefinition("Bot", Seq(), TInterval),
    CaseDefinition("BTrue", Seq(), TInterval),
    CaseDefinition("BFalse", Seq(), TInterval),
  )

  val dataModel: DataModel = DataModel.from(edb.allNodes:_*)

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


  val allVars = Relation("allVars", Seq(
    Param("name", TString)
  ), Seq(
    Body(Seq(
      Eq(Var("s"), LookupEdbType(TAssign)),
      Eq(Var("_name"), LookupEdbField(Cast(Var("s"), TAssign), "name")),
      Eq(Var("name"), Cast(Var("_name"), TString))
    ))
  ))

  val parentOf = Relation("parentOf", Seq(
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
  ))

  val aeval = Relation("aeval",
    Seq(
      Param("stmt", TStmt),
      Param("exp", TExp),
      Param("iv", TInterval)
    ),
    Seq(
      Body(Seq(
        Eq(Var("exp"), LookupEdbType(TVar)),
        Call("parentOf", Seq(Var("exp"), Var("stmt"))),
        Eq(Var("_name"), LookupEdbField(Cast(Var("exp"), TVar), "name")),
        Eq(Var("name"), Cast(Var("_name"), TString)),
        Call("intervalBefore", Seq(Var("stmt"), Var("name"), Var("iv")))
      )),
      Body(Seq(
        Eq(Var("exp"), LookupEdbType(TNum)),
        Call("parentOf", Seq(Var("exp"), Var("stmt"))),
        Eq(Var("n"), LookupEdbField(Cast(Var("exp"), TNum), "value")),
        Eq(Var("iv"), mkIv(Cast(Var("n"), TInt), Cast(Var("n"), TInt)))
      )),
      Body(Seq(
        Eq(Var("exp"), LookupEdbType(TAdd)),
        Eq(Var("lhs"), LookupEdbField(Cast(Var("exp"), TAdd), "lhs")),
        Eq(Var("rhs"), LookupEdbField(Cast(Var("exp"), TAdd), "rhs")),
        Call("aeval", Seq(Var("stmt"), Var("lhs"), Var("iv1"))),
        Call("aeval", Seq(Var("stmt"), Var("rhs"), Var("iv2"))),
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
        Eq(Var("exp"), LookupEdbType(TGT)),
        Eq(Var("lhs"), LookupEdbField(Cast(Var("exp"), TGT), "lhs")),
        Eq(Var("rhs"), LookupEdbField(Cast(Var("exp"), TGT), "rhs")),
        Call("aeval", Seq(Var("stmt"), Var("lhs"), Var("iv1"))),
        Call("aeval", Seq(Var("stmt"), Var("rhs"), Var("iv2"))),
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
        Eq(Var("exp"), LookupEdbType(TGT)),
        Eq(Var("lhs"), LookupEdbField(Cast(Var("exp"), TGT), "lhs")),
        Eq(Var("rhs"), LookupEdbField(Cast(Var("exp"), TGT), "rhs")),
        Call("aeval", Seq(Var("stmt"), Var("lhs"), Var("iv1"))),
        Call("aeval", Seq(Var("stmt"), Var("rhs"), Var("iv2"))),
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

  val intervalOp = ScalaAggregationOperator(
    "JoinInterval",
    ScalaType("Interval"),
    "Bot()",
    addCode =
      """(st: Interval, a: Interval) => (st, a) match {
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
        |}""".stripMargin
  )

  val _intervalAfter = Relation("_intervalAfter",
    Seq(
      Param("stmt", TStmt),
      Param("v", TString),
      Param("iv", TInterval),
    ),
    Seq(
      Body(Seq(
        Eq(Var("stmt"), LookupEdbType(TAssign)),
        Eq(Var("_v"), LookupEdbField(Cast(Var("s"), TAssign), "name")),
        Eq(Var("v"), Cast(Var("_v"), TString)),
        Eq(Var("e"), LookupEdbField(Cast(Var("s"), TAssign), "exp")),
        Call("aeval", Seq(Var("stmt"), Var("e"), Var("iv")))
      )),
      Body(Seq(
        Eq(Var("stmt"), LookupEdbType(TAssign)),
        Eq(Var("_name"), LookupEdbField(Cast(Var("s"), TAssign), "name")),
        Eq(Var("name"), Cast(Var("_name"), TString)),
        Call("allVars", Seq(Var("v"))),
        Eq(Var("v"), Var("name"), true),
        Call("intervalBefore", Seq(Var("stmt"), Var("v"), Var("iv")))
      )),
      Body(Seq(
        Eq(Var("stmt"), LookupEdbType(TSkip)),
        Call("intervalBefore", Seq(Var("stmt"), Var("v"), Var("iv")))
      )),
      Body(Seq(
        Eq(Var("stmt"), LookupEdbType(TWhile)),
        Call("intervalBefore", Seq(Var("stmt"), Var("v"), Var("iv")))
      ))
    )
  )

  val intervalAfter = Relation("intervalAfter",
    Seq(
      Param("stmt", TStmt),
      Param("v", TString),
      Param("iv", TInterval),
    ),
    Seq(
      Body(Seq(
        Call("_intervalAfter", Seq(Var("stmt").arg, Var("v").arg, WildcardArg())),
        Aggregate(RefByName("_intervalAfter"), Seq(Var("stmt").arg, Var("v").arg, AggregateColumnArg(Var("_iv"))), intervalOp),
        Eq(Var("iv"), Cast(Var("_iv"), TInterval))
      ))
    ))

  val predecessorIntervals = Relation("predecessorIntervals",
    Seq(
      Param("stmt", TStmt),
      Param("v", TString),
      Param("pred", TStmt),
      Param("iv", TInterval),
    ),
    Seq(
      Body(Seq(
        Call("cflow", Seq(Var("pred"), Var("stmt"))),
        Call("_intervalAfter", Seq(Var("pred"), Var("v"), Var("iv")))
      ))
    )
  )

  val intervalBefore = Relation("intervalBefore",
    Seq(
      Param("stmt", TStmt),
      Param("v", TString),
      Param("iv", TInterval),
    ),
    Seq(
      Body(Seq(
        //Eq(Var("stmt"), LookupEdbType(TStmt)),
        //Call("allVars", Seq(Var("v"))),
        Call("predecessorIntervals", Seq(Var("stmt").arg, Var("v").arg, WildcardArg(), WildcardArg())),
        Aggregate(RefByName("predecessorIntervals"), Seq(Var("stmt").arg, Var("v").arg, WildcardArg(), AggregateColumnArg(Var("_iv"))), intervalOp),
        Eq(Var("iv"), Cast(Var("_iv"), TInterval))
      ))
    )
  )


  val mod = Module("IntervalAnalysis", BaseIR.language + arithmetic.IR + dataIR + mapIR + demand.IR + string.IR + edbdata.IR + disjunction.IR,
    edbNodes ++ dataDefs ++ Seq(
      allVars,
      parentOf,
      cflow,
      initStmt,
      finalStmt,
      aeval,
      predecessorIntervals,
      intervalAfter,
      _intervalAfter,
      intervalBefore
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
      // Does not work, because cycles are detected incorrectly
      //val viatraOptions = op("viatra_options")
      //viatraOptions.update("apply_double_aggregation_rewrite", true)
      op

    private trait demandLowering extends demand.Lowering with primitive.Visitor
    private trait blockLowering extends block.Lowering with primitive.Visitor
    private trait scalaLowering extends primitive.ScalaLowering
      with scalaArith.ScalaLowering
      with scalaData.ScalaLowering
      with scalaString.ScalaLowering
    override def typechecker = new IRTypechecker with Typechecker {}

    setPipeline(List(
      () => new disjunction.Lowering {}
    ))

  @main def check1 = {
    //println(mod)
    try
      compiled.checked

    val exec = new Executor()
    val engine = exec.instantiate(compiled, dataModel)


    val a1 = Assign(
      "x", Num(4)
    )
    val a2 = interval.edb.Assign(
      "y", interval.edb.Add(interval.edb.Num(5), interval.edb.Var("x"))
    )
    val a3 = interval.edb.Assign(
      "x", interval.edb.Num(2)
    )
    val s = Sequence(interval.edb.Sequence(a1, a2), a3)

    println(s"Loading $s")
    s.loadEdits.print()
    engine.feed.processEditScript(s.loadEdits)
    engine.readAll().map(_.asTable).foreach(println)
  }

  @main def checkWhileAgg = {
    //println(mod)
    try
      compiled.checked

    val exec = new Executor()
    //val exec = new Executor(DRedReteBackendFactory.INSTANCE)
    val engine = exec.instantiate(compiled, dataModel)


    val a1 = interval.edb.Assign(
      "x", interval.edb.Num(1)
    )

    val a2 = While(
      interval.edb.GT(interval.edb.Var("x"), interval.edb.Num(0)),
      edb.Assign("x", edb.Add(edb.Var("x"), edb.Num(-1)))
    )

    val s = interval.edb.Sequence(a1, a2)

    println(s"Loading $s")
    s.loadEdits.print()
    engine.feed.processEditScript(s.loadEdits)
    engine.readAll().map(_.asTable).foreach(println)
    println(s.toStringWithURI)
  }
