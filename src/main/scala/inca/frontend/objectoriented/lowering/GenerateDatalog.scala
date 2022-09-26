package inca.frontend.objectoriented.lowering

import inca.backend.hints.{MagicSetHints, ObjectHints, OptimizationHints}
import inca.backend.ir.Datalog
import inca.backend.transform.magic.demand.DemandTransformation.demandPatternPrefix
import inca.frontend.functional.core.DataDef
import inca.frontend.objectoriented.core.{TNull, _}
import inca.frontend.objectoriented.lowering.GenerateDatalog._
import inca.runtime.aggregate.Aggregation
import inca.runtime.data.ObjectID
import inca.util.Scala.{symbolOf, typeOf}
import inca.util.{Gensym, Scala, TupleOps}

import scala.:+
import scala.annotation.tailrec
import scala.collection.immutable.MultiDict
import scala.collection.mutable.ListBuffer
import scala.meta.Term
import scala.meta.quasiquotes._

object GenerateDatalog {
  private val sep: String = "$"
  private val internalPrefix: String = "_" + sep

  val castPatName: String       = internalPrefix + "cast"
  val equalsPatName: String     = internalPrefix + "equals"
  val instanceOfPatName: String = internalPrefix + "instanceOf"

  def dispatchPatName(methodNameWithSignature: String): String = s"${internalPrefix}dispatch_${methodNameWithSignature}"
  def constructorPatName(className: String): String = className

  def transformModule(module: Module): Datalog.Module =
    new GenerateDatalog(module).transModule()

  def transformModules(modules: Seq[Module]): Seq[Datalog.Module] =
    modules.map(transformModule)
}

// testTimestampsAndAggregation
case class MaxAgg(upperBound: Int) extends Aggregation[Int] {
  override val name: String = "max"
  override def init: Int = Int.MinValue
  override def join(v1: Int, v2: Int): Int = if (v1.max(v2) <= upperBound) v1.max(v2) else 0
  //override def unjoin(v1: Int, v2: Int): Int = v1 - v2
  override val isAssociative: Boolean = true
  override val isCommutative: Boolean = true
  override val hasUnjoin: Boolean = false
}

// testTimestampsAndAggregationWithEqual override

case class MaxAggWithTimestamp() extends Aggregation[Timestamp] {
  override val name: String = "max"
  override def init: Timestamp = Timestamp(Int.MinValue, isQuery = false)
  override def join(v1: Timestamp, v2: Timestamp): Timestamp = if (v1.value > v2.value) v1 else v2
  //override def unjoin(v1: Int, v2: Int): Int = v1 - v2
  override val isAssociative: Boolean = true
  override val isCommutative: Boolean = true
  override val hasUnjoin: Boolean = false
}

case class Timestamp(val value: Int, isQuery: Boolean) {
  /*override def equals(obj: Any): Boolean = {
    val isTimestamp = obj.isInstanceOf[Timestamp]
    if (isTimestamp) {
      println("Compare with ts: ", this, obj)
      val that = obj.asInstanceOf[Timestamp]
      if (this.isQuery) {
        that.value <= value
      } else if (that.isQuery) {
        value <= that.value
      } else {
        value == that.value
      }
    } else {
      false
    }
  }*/

  override def hashCode: Int = {
    println("hashcode: ", this, value)
    value
  }
}

object Timestamp {
  def apply(value: Int, isQuery: Boolean): Timestamp = {
    new Timestamp(value, isQuery)
  }
}


class GenerateDatalog(module: Module) {

  private val gensym: Gensym = new Gensym(Iterable.empty)

  private val generatedPatterns = ListBuffer[Datalog.Pattern]()

  def testRaw(): Unit = {
    val mainPat = Datalog.Pattern(None, "main", Seq(
      Datalog.Param("obj", GP_URI)
    ), Seq(
      Datalog.Body(Seq(
        Datalog.Call("A$constr$0", Seq(Datalog.Var("_$0"), Datalog.IntConstant(5))),
        Datalog.Call("A$constr$0", Seq(Datalog.Var("_$1"), Datalog.IntConstant(5))),
        Datalog.Call("A$constr$1", Seq(Datalog.Var("_$2"))),
        Datalog.Call("A", Seq(Datalog.Var("obj")))
          .addHint(MagicSetHints.IgnoreCall, MagicSetHints.FixedAdornment(Seq(false)))
      ))
    )).addHint(ObjectHints.AllocationRoot)
      .addHint(MagicSetHints.Main(Seq(false)))

    val thisVar = Datalog.Var("this")
    val constrScalaFun = Term.Function(Nil, q"""$oOID("A")""")
    val tmpCons = Datalog.Computed(thisVar, Datalog.Evaluation(Seq(), GP_URI, Scala(constrScalaFun)))

    val objPat = Datalog.Pattern(None, "A", Seq(Datalog.Param("this", GP_URI)), Seq(
      Datalog.Body(Seq(
        tmpCons
      ))
    )).addHint(ObjectHints.Allocation)
      .addHint(OptimizationHints.NoInline)

    val objAttrPat = Datalog.Pattern(None, "A$$x", Seq(
      Datalog.Param("this", GP_URI),
      Datalog.Param("x", Datalog.TScalaInt)
    ), Seq(
      Datalog.Body(Seq())
    )).addHint(OptimizationHints.NoInline)

    val objConstrPat = Datalog.Pattern(None, "A$constr$0", Seq(
      Datalog.Param("this", GP_URI),
      Datalog.Param("x", Datalog.TScalaInt)
    ), Seq(
      Datalog.Body(Seq(
        Datalog.Call("A", Seq(thisVar)), Datalog.Call("A$$x", Seq(thisVar, Datalog.Var("x")))
      ))
    ))

    val objConstrPat2 = Datalog.Pattern(None, "A$constr$1", Seq(
      Datalog.Param("this", GP_URI),
    ), Seq(
      Datalog.Body(Seq(
        Datalog.Call("A", Seq(thisVar)), Datalog.Call("A$$x", Seq(thisVar, Datalog.IntConstant(1)))
      ))
    ))

    generatedPatterns ++= Seq(mainPat, objPat, objConstrPat, objConstrPat2, objAttrPat)
  }

  def testAttributeManagedByScala(): Unit = {
    val thisVar = Datalog.Var("this")
    val xVar = Datalog.Var("x")

    def newTsCount(count: Int) = Datalog.Var("tsCount$" + count.toString)
    def newXVar(count: Int) = Datalog.Var("x$" + count.toString)

    def getAttrXForTsComp(objVar: Datalog.Var, ts: Datalog.Var, outVar: Datalog.Var): Datalog.Computed = {
      val objTerm = Term.Name("obj")
      val tsTerm = Term.Name("ts")
      val tsParam = Term.Param(Nil, tsTerm, Some(TScalaInt.asScala), None)
      val objParam = Term.Param(Nil, objTerm, Some(GP_URI.asScala), None)

      Datalog.Computed(
        outVar,
        Datalog.Evaluation(
          Seq(
            objVar -> GP_URI,
            ts -> Datalog.TScalaInt
          ),
          Datalog.TScalaInt, // We should make this dynamic for arbitrary attributs
          Scala(q"""($objParam, $tsParam) => $objTerm.getAttr("x", $tsTerm)""")
        )
      )
    }

    def setAttrXForTsComp(objVar: Datalog.Var, value: Datalog.Var, ts: Datalog.Var, outVar: Datalog.Var): Datalog.Computed = {
      val tsTerm = Term.Name("ts")
      val tsParam = Term.Param(Nil, tsTerm, Some(TScalaInt.asScala), None)
      val valueTerm = Term.Name("value")
      val valueParam = Term.Param(Nil, valueTerm, Some(TScalaInt.asScala), None)
      val objTerm = Term.Name("obj")
      val objParam = Term.Param(Nil, objTerm, Some(GP_URI.asScala), None)

      Datalog.Computed(
        outVar,
        Datalog.Evaluation(
          Seq(
            objVar -> GP_URI,
            value -> Datalog.TScalaInt,
            ts -> Datalog.TScalaInt // make this variable
          ),
          Datalog.TScalaInt, // We should make this dynamic for arbitrary attributs
          Scala(q"""($objParam, $valueParam, $tsParam) => $objTerm.setAttr("x", $valueTerm, $tsTerm)""")
        )
      )
    }

    val mainPat = Datalog.Pattern(None, "main", Seq(
      //Datalog.Param("obj", GP_URI),
      Datalog.Param("a3_xInit", Datalog.TScalaInt),
      Datalog.Param("a3_xMod", Datalog.TScalaInt),
      Datalog.Param("a1_xInit", Datalog.TScalaInt),
      Datalog.Param("a1_xMod", Datalog.TScalaInt),
    ), Seq(
      Datalog.Body(Seq(
        // Init the tsCount
        Datalog.Eq(newTsCount(0), Datalog.IntConstant(0)),

        Datalog.Call("A$constr$0", Seq(Datalog.Var("a1"), Datalog.IntConstant(5), newTsCount(0), newTsCount(1))),
        //Datalog.Call("A$constr$0", Seq(Datalog.Var("a2"), Datalog.IntConstant(3))),
        Datalog.Call("A$constr$1", Seq(Datalog.Var("a3"), newTsCount(1), newTsCount(2))),

        // Read value of attr x
        getAttrXForTsComp(Datalog.Var("a3"), newTsCount(2), Datalog.Var("a3_xInit")),

        getAttrXForTsComp(Datalog.Var("a1"), newTsCount(2), Datalog.Var("a1_xInit")),

        // Change attribute
        Datalog.Eq(newXVar(0), Datalog.IntConstant(9)),
        setAttrXForTsComp(Datalog.Var("a3"), newXVar(0), newTsCount(2), newTsCount(3)),

        Datalog.Eq(newXVar(1), Datalog.IntConstant(12)),
        setAttrXForTsComp(Datalog.Var("a1"), newXVar(1), newTsCount(3), newTsCount(4)),

        // Read attribute again
        getAttrXForTsComp(Datalog.Var("a3"), newTsCount(4), Datalog.Var("a3_xMod")),

        getAttrXForTsComp(Datalog.Var("a1"), newTsCount(4), Datalog.Var("a1_xMod")),
      ))
    )).addHint(ObjectHints.AllocationRoot)
      .addHint(MagicSetHints.Main(Seq(false, false)))

    val constrScalaFun = Term.Function(Nil, q"""$oOID("A")""")
    val tmpCons = Datalog.Computed(thisVar, Datalog.Evaluation(Seq(), GP_URI, Scala(constrScalaFun)))

    val objPat = Datalog.Pattern(None, "A", Seq(Datalog.Param("this", GP_URI)), Seq(
      Datalog.Body(Seq(
        tmpCons
      ))
    )).addHint(ObjectHints.Allocation)
      .addHint(OptimizationHints.NoInline)

    val objConstrPat = Datalog.Pattern(None, "A$constr$0", Seq(
      Datalog.Param("this", GP_URI),
      Datalog.Param("x", Datalog.TScalaInt),
      Datalog.Param("tsIn", Datalog.TScalaInt),
      Datalog.Param("tsOut", Datalog.TScalaInt)
    ), Seq(
      Datalog.Body(Seq(
        Datalog.Call("A", Seq(thisVar)),
        setAttrXForTsComp(thisVar, xVar, Datalog.Var("tsIn"), Datalog.Var("tsOut")),
      ))
    ))

    val objConstrPat2 = Datalog.Pattern(None, "A$constr$1", Seq(
      Datalog.Param("this", GP_URI),
      Datalog.Param("tsIn", Datalog.TScalaInt),
      Datalog.Param("tsOut", Datalog.TScalaInt)
    ), Seq(
      Datalog.Body(Seq(
        Datalog.Call("A", Seq(thisVar)),
        Datalog.Eq(xVar, Datalog.IntConstant(1)),
        setAttrXForTsComp(thisVar, xVar, Datalog.Var("tsIn"), Datalog.Var("tsOut")),
      ))
    ))

    generatedPatterns ++= Seq(mainPat, objPat, objConstrPat, objConstrPat2) // objGetAttrPat, objSetAttrPat)
  }

  def testCounterManagedByScala(): Unit = {
    val thisVar = Datalog.Var("this")
    val xVar = Datalog.Var("x")
    val tsVar = Datalog.Var("ts")
    val tsCount = Datalog.Var("tsCount")

    def newTsCount(count: Int) = Datalog.Var("tsCount$" + count.toString)

    def getTsForAttrXComp(objVar: Datalog.Var, tsCount: Datalog.Var, outVar: Datalog.Var) = {
      val objTerm = Term.Name("obj")
      val tsCountTerm = Term.Name("tsCount")
      val tsCountParam = Term.Param(Nil, tsCountTerm, Some(TScalaInt.asScala), None)
      val objParam = Term.Param(Nil, objTerm, Some(GP_URI.asScala), None)

      Datalog.Computed(
        outVar,
        Datalog.Evaluation(
          Seq(
            objVar -> GP_URI,
            tsCount -> Datalog.TScalaInt
          ),
          Datalog.TScalaInt,
          Scala(q"""($objParam, $tsCountParam) => $objTerm.getTs("x", $tsCountTerm)""")
        )
      )
    }

    def inc(inVar: Datalog.Var, outVar: Datalog.Var) = {
      val inTerm = Term.Name("inV")
      val inParam = Term.Param(Nil, inTerm, Some(TScalaInt.asScala), None)
      Datalog.Computed(
        outVar,
        Datalog.Evaluation(
          Seq(
            inVar -> Datalog.TScalaInt
          ),
          Datalog.TScalaInt,
          Scala(q"""($inParam) => $inTerm + 1""")
        )
      )
    }

    val mainPat = Datalog.Pattern(None, "main", Seq(
      //Datalog.Param("obj", GP_URI),
      Datalog.Param("xInit", Datalog.TScalaInt),
      //Datalog.Param("xMod", Datalog.TScalaInt),
    ), Seq(
      Datalog.Body(Seq(
        // Init the tsCount
        Datalog.Eq(tsCount, Datalog.IntConstant(0)),

        //Datalog.Call("A$constr$0", Seq(Datalog.Var("a1"), Datalog.IntConstant(5))),
        //Datalog.Call("A$constr$0", Seq(Datalog.Var("a2"), Datalog.IntConstant(3))),
        Datalog.Call("A$constr$1", Seq(Datalog.Var("a3"), tsCount, newTsCount(0))),

        // Read value of attr x
        getTsForAttrXComp(Datalog.Var("a3"), tsCount, tsVar),
        Datalog.Call("A$$x", Seq(Datalog.Var("a3"), Datalog.Var("xInit"), tsVar))
          .addHint(MagicSetHints.IgnoreCall, MagicSetHints.FixedAdornment(Seq(true, true, true))),

        // Change value of attr x
        // get a new timestamp for the increased counter
        getTsForAttrXComp(Datalog.Var("a3"), newTsCount(0), tsVar),
        /*Datalog.Call("A$$x", Seq(Datalog.Var("a3"), Datalog.IntConstant(10), tsVar)),*/

        // Read value of attr x
        /*getTsForAttrXComp(Datalog.Var("a3"), tsVar),
        Datalog.Call("A$$x", Seq(Datalog.Var("a3"), Datalog.Var("xMod"), tsVar))
          .addHint(MagicSetHints.IgnoreCall, MagicSetHints.FixedAdornment(Seq(true, true, true))),*/

        /*Datalog.Call("A", Seq(Datalog.Var("obj")))
          .addHint(MagicSetHints.IgnoreCall, MagicSetHints.FixedAdornment(Seq(false)))*/
      ))
    )).addHint(ObjectHints.AllocationRoot)
      .addHint(MagicSetHints.Main(Seq(false, false)))

    val constrScalaFun = Term.Function(Nil, q"""$oOID("A")""")
    val tmpCons = Datalog.Computed(thisVar, Datalog.Evaluation(Seq(), GP_URI, Scala(constrScalaFun)))

    val objPat = Datalog.Pattern(None, "A", Seq(Datalog.Param("this", GP_URI)), Seq(
      Datalog.Body(Seq(
        tmpCons
      ))
    )).addHint(ObjectHints.Allocation)
      .addHint(OptimizationHints.NoInline)

    val objAttrPat = Datalog.Pattern(None, "A$$x", Seq(
      Datalog.Param("this", GP_URI),
      Datalog.Param("x", Datalog.TScalaInt),
      Datalog.Param("ts", Datalog.TScalaInt)
    ), Seq(
      Datalog.Body(Seq())
    )).addHint(OptimizationHints.NoInline)

    /*val objSetAttrPat = Datalog.Pattern(None, "A$set_x", Seq(
      Datalog.Param("this", GP_URI),
      Datalog.Param("x", Datalog.TScalaInt),
      //Datalog.Param("ts", Datalog.TScalaInt),
    ), Seq(
      Datalog.Body(Seq())
    )).addHint(MagicSetHints.NoInputRelation)*/

    val objConstrPat = Datalog.Pattern(None, "A$constr$0", Seq(
      Datalog.Param("this", GP_URI),
      Datalog.Param("x", Datalog.TScalaInt),
      Datalog.Param("tsIn", Datalog.TScalaInt),
      Datalog.Param("tsOut", Datalog.TScalaInt)
    ), Seq(
      Datalog.Body(Seq(
        Datalog.Call("A", Seq(thisVar)),
        getTsForAttrXComp(thisVar, Datalog.Var("tsIn"), tsVar),
        inc(Datalog.Var("tsIn"), Datalog.Var("tsOut")),
        Datalog.Call("A$$x", Seq(thisVar, Datalog.Var("x"), tsVar))
      ))
    ))

    val objConstrPat2 = Datalog.Pattern(None, "A$constr$1", Seq(
      Datalog.Param("this", GP_URI),
      Datalog.Param("tsIn", Datalog.TScalaInt),
      Datalog.Param("tsOut", Datalog.TScalaInt)
    ), Seq(
      Datalog.Body(Seq(
        Datalog.Call("A", Seq(thisVar)),
        Datalog.Eq(xVar, Datalog.IntConstant(1)),
        getTsForAttrXComp(thisVar, Datalog.Var("tsIn"), tsVar),
        inc(Datalog.Var("tsIn"), Datalog.Var("tsOut")),
        Datalog.Call("A$$x", Seq(thisVar, Datalog.Var("x"), tsVar)),
      ))
    ))

    generatedPatterns ++= Seq(mainPat, objPat, objConstrPat, objConstrPat2, objAttrPat) // , objGetAttrPat, objSetAttrPat)
  }

  def testTimestampsAndAggregation(): Unit = gensym.scoped {
    val thisVar = Datalog.Var("this")

    def max(patName: String, objVar: Datalog.Var, outVar: Datalog.Var, maxTs: Datalog.Var): Datalog.Computed = {
      Datalog.Computed(
        outVar,
        Datalog.CustomAggregation(
          Datalog.TScalaInt,
          Some("Maximum aggregation"),
          Scala(q"""new inca.frontend.objectoriented.lowering.MaxAgg(1000)"""),
          patName,
          Seq(objVar, Datalog.Var(gensym.fresh("_")), Datalog.Var(gensym.fresh("_"))),
          2
        )
      )
    }

    def inc(inVar: Datalog.Var, outVar: Datalog.Var) = {
      val inArg = Term.Name("inArg")
      val inParam = Term.Param(Nil, inArg, Some(Datalog.TScalaInt.asScala), None)
      Datalog.Computed(
        outVar,
        Datalog.Evaluation(
          Seq(inVar -> Datalog.TScalaInt),
          Datalog.TScalaInt,
          Scala(q"($inParam) => $inArg + 1")
        )
      )
    }

    // Idee: ts einfach immer erhöhen wie alloc count. Nur beim Lesen das max ermitteln !

    val mainPat = Datalog.Pattern(None, "main", Seq(
      //Datalog.Param("obj", GP_URI)
      Datalog.Param("ts1", Datalog.TScalaInt),
      Datalog.Param("x1", Datalog.TScalaInt),
      Datalog.Param("ts2", Datalog.TScalaInt),
      Datalog.Param("x2", Datalog.TScalaInt)
    ), Seq(
      Datalog.Body(Seq(
        Datalog.Call("A$constr$0", Seq(Datalog.Var("a1"), Datalog.IntConstant(3))),
        //Datalog.Call("A$constr$0", Seq(Datalog.Var("a2"), Datalog.IntConstant(5))),
        //Datalog.Call("A$constr$1", Seq(Datalog.Var("a2"))),

        // Set
        /*max("A$$x", Datalog.Var("a1"), Datalog.Var("ts1"))
          .addHint(MagicSetHints.IgnoreCall, MagicSetHints.FixedAdornment(Seq(true, true, true))),*/
        Datalog.Eq(Datalog.Var("argTs1"), Datalog.IntConstant(1)),
        Datalog.Call("A$$x", Seq(Datalog.Var("a1"), Datalog.IntConstant(7), Datalog.Var("argTs1"))),
        //Datalog.Call("A$$x", Seq(Datalog.Var("a2"), Datalog.IntConstant(3), Datalog.IntConstant(1))),
        //Datalog.Call("A$$x", Seq(Datalog.Var("a1"), Datalog.IntConstant(8), Datalog.IntConstant(2))),

        // Get
        max("A$$x", Datalog.Var("a1"), Datalog.Var("ts1"), Datalog.Var("argTs1"))
          .addHint(MagicSetHints.IgnoreCall, MagicSetHints.FixedAdornment(Seq(true, true, true))),
        Datalog.Call("A$$x", Seq(Datalog.Var("a1"), Datalog.Var("x1"), Datalog.Var("ts1")))
          .addHint(MagicSetHints.IgnoreCall, MagicSetHints.FixedAdornment(Seq(true, true, true))),

        // Set 2
        //Datalog.Eq(Datalog.Var("argTs2"), Datalog.IntConstant(4)),
        inc(Datalog.Var("argTs1"), Datalog.Var("argTs2")),
        Datalog.Call("A$$x", Seq(Datalog.Var("a1"), Datalog.IntConstant(11), Datalog.Var("argTs2"))),

        // Get 2
        max("A$$x", Datalog.Var("a1"), Datalog.Var("ts2"), Datalog.Var("argTs2"))
          .addHint(MagicSetHints.IgnoreCall, MagicSetHints.FixedAdornment(Seq(true, true, true))),
        Datalog.Call("A$$x", Seq(Datalog.Var("a1"), Datalog.Var("x2"), Datalog.Var("ts2")))
          .addHint(MagicSetHints.IgnoreCall, MagicSetHints.FixedAdornment(Seq(true, true, true)))

        /*Datalog.Call("A", Seq(Datalog.Var("obj")))
          .addHint(MagicSetHints.IgnoreCall, MagicSetHints.FixedAdornment(Seq(false)))*/
      ))
    )).addHint(ObjectHints.AllocationRoot)
      .addHint(MagicSetHints.Main(Seq(false)))

    val objPat = Datalog.Pattern(None, "A", Seq(Datalog.Param("this", GP_URI)), Seq(
      Datalog.Body(Seq(
        Datalog.Computed(thisVar, Datalog.Evaluation(Seq(), GP_URI, Scala(Term.Function(Nil, q"""$oOID("A")"""))))
      ))
    )).addHint(ObjectHints.Allocation)
      .addHint(OptimizationHints.NoInline)

    val objAttrPat = Datalog.Pattern(None, "A$$x", Seq(
      Datalog.Param("this", GP_URI),
      Datalog.Param("x", Datalog.TScalaInt),
      Datalog.Param("ts", Datalog.TScalaInt)
    ), Seq(
      Datalog.Body(Seq())
    )).addHint(OptimizationHints.NoInline)

    val objConstrPat = Datalog.Pattern(None, "A$constr$0", Seq(
      Datalog.Param("this", GP_URI),
      Datalog.Param("x", Datalog.TScalaInt)
    ), Seq(
      Datalog.Body(Seq(
        Datalog.Call("A", Seq(thisVar)), Datalog.Call("A$$x", Seq(thisVar, Datalog.Var("x"), Datalog.IntConstant(0)))
      ))
    ))

    val objConstrPat2 = Datalog.Pattern(None, "A$constr$1", Seq(
      Datalog.Param("this", GP_URI),
    ), Seq(
      Datalog.Body(Seq(
        Datalog.Call("A", Seq(thisVar)), Datalog.Call("A$$x", Seq(thisVar, Datalog.IntConstant(1), Datalog.IntConstant(0)))
      ))
    ))

    generatedPatterns ++= Seq(mainPat, objPat, objConstrPat, objConstrPat2, objAttrPat)
  }

  def test(): Unit = gensym.scoped {
    val thisVar = Datalog.Var("this")

    def max(patName: String, objVar: Datalog.Var, outVar: Datalog.Var, queryTs: Datalog.Var): Datalog.Computed = {
      Datalog.Computed(
        outVar,
        Datalog.CustomAggregation(
          GP_TS,
          Some("Maximum aggregation"),
          Scala(q"""new inca.frontend.objectoriented.lowering.MaxAggWithTimestamp()"""),
          patName,
          Seq(objVar, Datalog.Var(gensym.fresh("_")), queryTs),
          2
        )
      )
    }

    def inc(inVar: Datalog.Var, outVar: Datalog.Var) = {
      val inArg = Term.Name("inArg")
      val inParam = Term.Param(Nil, inArg, Some(Datalog.TScalaInt.asScala), None)
      Datalog.Computed(
        outVar,
        Datalog.Evaluation(
          Seq(inVar -> Datalog.TScalaInt),
          Datalog.TScalaInt,
          Scala(q"($inParam) => $inArg + 1")
        )
      )
    }

    def GP_TS: Datalog.TScala = Datalog.TScala(Scala(typeOf[Timestamp]))

    val oTimestamp = symbolOf(Timestamp)

    def createTs(inVar: Datalog.Var, outVar: Datalog.Var): Datalog.Computed = {
      val inArg = Term.Name("tsArg")
      val inParam = Term.Param(Nil, inArg, Some(Datalog.TScalaInt.asScala), None)
      Datalog.Computed(
        outVar,
        Datalog.Evaluation(
          Seq(inVar -> Datalog.TScalaInt),
          GP_TS,
          Scala(q"($inParam) => $oTimestamp($inArg, false)")
        )
      )
    }

    def createQueryTs(inVar: Datalog.Var, outVar: Datalog.Var): Datalog.Computed = {
      val inArg = Term.Name("tsArg")
      val inParam = Term.Param(Nil, inArg, Some(Datalog.TScalaInt.asScala), None)
      Datalog.Computed(
        outVar,
        Datalog.Evaluation(
          Seq(inVar -> Datalog.TScalaInt),
          GP_TS,
          Scala(q"($inParam) => $oTimestamp($inArg, true)")
        )
      )
    }

    // Idee: ts einfach immer erhöhen wie alloc count. Nur beim Lesen das max ermitteln !

    val mainPat = Datalog.Pattern(None, "main", Seq(
      //Datalog.Param("obj", GP_URI)
      Datalog.Param("_mTs1", GP_TS),
      Datalog.Param("x1", Datalog.TScalaInt),
      Datalog.Param("_mTs2", GP_TS),
      Datalog.Param("x2", Datalog.TScalaInt)
    ), Seq(
      Datalog.Body(Seq(
        Datalog.Call("A$constr$0", Seq(Datalog.Var("a1"), Datalog.IntConstant(3))),
        Datalog.Call("A$constr$0", Seq(Datalog.Var("a2"), Datalog.IntConstant(5))),
        //Datalog.Call("A$constr$1", Seq(Datalog.Var("a2"))),

        // Set
        /*max("A$$x", Datalog.Var("a1"), Datalog.Var("ts1"))
          .addHint(MagicSetHints.IgnoreCall, MagicSetHints.FixedAdornment(Seq(true, true, true))),*/
        Datalog.Eq(Datalog.Var("argTs1"), Datalog.IntConstant(1)),
        createTs(Datalog.Var("argTs1"), Datalog.Var("_ts1")),
        Datalog.Call("A$$x", Seq(Datalog.Var("a1"), Datalog.IntConstant(7), Datalog.Var("_ts1"))),
        //Datalog.Call("A$$x", Seq(Datalog.Var("a2"), Datalog.IntConstant(3), Datalog.IntConstant(1))),
        //Datalog.Call("A$$x", Seq(Datalog.Var("a1"), Datalog.IntConstant(8), Datalog.IntConstant(2))),

        // Get
        inc(Datalog.Var("argTs1"), Datalog.Var("argTs1_inc")),
        createQueryTs(Datalog.Var("argTs1_inc"), Datalog.Var("_qTs1")),
        max("A$$x", Datalog.Var("a1"), Datalog.Var("_mTs1"),  Datalog.Var("_qTs1"))
          .addHint(MagicSetHints.IgnoreCall, MagicSetHints.FixedAdornment(Seq(true, true, true))),
        Datalog.Call("A$$x", Seq(Datalog.Var("a1"), Datalog.Var("x1"), Datalog.Var("_mTs1")))
          .addHint(MagicSetHints.IgnoreCall, MagicSetHints.FixedAdornment(Seq(true, true, true))),

        // Set 2
        //Datalog.Eq(Datalog.Var("argTs2"), Datalog.IntConstant(4)),
        inc(Datalog.Var("argTs1"), Datalog.Var("argTs2")),
        createTs(Datalog.Var("argTs2"), Datalog.Var("_ts2")),
        Datalog.Call("A$$x", Seq(Datalog.Var("a1"), Datalog.IntConstant(11), Datalog.Var("_ts2"))),

        // Get 2
        createQueryTs(Datalog.Var("argTs2"), Datalog.Var("_qTs2")),
        max("A$$x", Datalog.Var("a1"), Datalog.Var("_mTs2"), Datalog.Var("_qTs2"))
          .addHint(MagicSetHints.IgnoreCall, MagicSetHints.FixedAdornment(Seq(true, true, true))),
        Datalog.Call("A$$x", Seq(Datalog.Var("a1"), Datalog.Var("x2"), Datalog.Var("_mTs2")))
          .addHint(MagicSetHints.IgnoreCall, MagicSetHints.FixedAdornment(Seq(true, true, true)))

        /*Datalog.Call("A", Seq(Datalog.Var("obj")))
          .addHint(MagicSetHints.IgnoreCall, MagicSetHints.FixedAdornment(Seq(false)))*/
      ))
    )).addHint(ObjectHints.AllocationRoot)
      .addHint(MagicSetHints.Main(Seq(false)))

    val objPat = Datalog.Pattern(None, "A", Seq(Datalog.Param("this", GP_URI)), Seq(
      Datalog.Body(Seq(
        Datalog.Computed(thisVar, Datalog.Evaluation(Seq(), GP_URI, Scala(Term.Function(Nil, q"""$oOID("A")"""))))
      ))
    )).addHint(ObjectHints.Allocation)
      .addHint(OptimizationHints.NoInline)

    val objAttrPat = Datalog.Pattern(None, "A$$x", Seq(
      Datalog.Param("this", GP_URI),
      Datalog.Param("x", Datalog.TScalaInt),
      Datalog.Param("ts", GP_TS)
    ), Seq(
      Datalog.Body(Seq())
    )).addHint(OptimizationHints.NoInline)

    val objConstrPat = Datalog.Pattern(None, "A$constr$0", Seq(
      Datalog.Param("this", GP_URI),
      Datalog.Param("x", Datalog.TScalaInt)
    ), Seq(
      Datalog.Body(Seq(
        Datalog.Eq(Datalog.Var("ts"), Datalog.IntConstant(0)),
        createTs(Datalog.Var("ts"), Datalog.Var("_ts")),
        Datalog.Call("A", Seq(thisVar)), Datalog.Call("A$$x", Seq(thisVar, Datalog.Var("x"), Datalog.Var("_ts")))
      ))
    ))

    val objConstrPat2 = Datalog.Pattern(None, "A$constr$1", Seq(
      Datalog.Param("this", GP_URI),
    ), Seq(
      Datalog.Body(Seq(
        Datalog.Eq(Datalog.Var("ts"), Datalog.IntConstant(0)),
        createTs(Datalog.Var("ts"), Datalog.Var("_ts")),
        Datalog.Call("A", Seq(thisVar)), Datalog.Call("A$$x", Seq(thisVar, Datalog.IntConstant(1), Datalog.Var("_ts")))
      ))
    ))

    generatedPatterns ++= Seq(mainPat, objPat, objConstrPat, objConstrPat2, objAttrPat)
  }

  def transModule(): Datalog.Module = {
    val Module(name, imports, classes) = module
    gensym.register(module.usedModuleNames.map(_.raw))
    gensym.register(module.usedClassNames.map(_.raw))

    testAttributeManagedByScala()

    /*generatedPatterns += transNull()
    generatedPatterns += transInstanceOf()
    generatedPatterns += transCast()
    generatedPatterns += transEquals()
    generatedPatterns ++= transDynamicDispatch(classes)
    generatedPatterns ++= classes.flatMap(transClass)*/

    Datalog.Module(
      name.raw,
      imports.map(_.name.raw),
      generatedPatterns.toList,
      Seq()
    )
  }

  /**
   * Guard that ensures that an object of the class with the id specified in the var `this` exists.
   * @param classDef the class to check for
   * @param neg      negate the guard, that means no object with the id exists
   * @return         Datalog.Call to check the existence
   */
  private def guard(classDef: ClassDef, neg: Boolean = false): Datalog.Call = {
    val thisVar = Datalog.Var("this")
    val fieldVars = classDef.fields.map(_ => Datalog.Var(gensym.fresh("_")))
    Datalog.Call(classDef.name.raw, thisVar +: fieldVars, neg = neg)
      .addHint(MagicSetHints.IgnoreCall)
      .addHint(MagicSetHints.FixedAdornment(false +: fieldVars.map(_ => true)))
  }

  private def transDynamicDispatch(classes: Seq[ClassDef]): Seq[Datalog.Pattern] = {
    /*
     * Collect all methods implemented by a class. This function traverses all parent classes and stores
     * a mapping func.name${hash} -> (classDef, methodDef) where classDef is the class itself or the parent class
     * where the method is last overwritten.
     */
    def collectMethods(classDef: ClassDef): Map[String, (ClassDef, MethodDef)] = {
      val methods = classDef.content.flatMap {
        case m :MethodDef if !m.annos.contains(MainAnnotation) => Seq(m.name + sep + m.paramSignature -> (classDef, m))
        case _ => None
      }.toMap

      val parentMethods = classDef.parentClassRefs.flatMap { ref =>
        val parentClassDef = ref.target.getOrElse(throw new RuntimeException(s"Unresolved class ${ref.name.raw}"))
        collectMethods(parentClassDef)
      }.toMap
      parentMethods ++ methods
    }

    val params = classes.flatMap(collectMethods).map {
      case (sig, (c, m)) =>
        sig ->
          (Datalog.Param("this", transType(c.typ))
            +: m.params.map(p => Datalog.Param(p.name.raw, transType(p.typ)))
            :+ Datalog.Param("out", transType(m.outType)))
    }.toMap

    val bodies = MultiDict.from(classes.flatMap { cls =>
      collectMethods(cls).map {
        case (sig, (c, m)) =>
          val methodParams = params(sig).map(p => Datalog.Var(p.name))
          val methodCall = Datalog.Call(c.name.raw + sep + m.name, methodParams)
          sig -> Datalog.Body(Seq(guard(cls), methodCall))
      }.toSeq
    })

    params.map { case (sig, params) =>
      Datalog.Pattern(None, dispatchPatName(sig), params, bodies.get(sig).toSeq)
    }.toSeq
  }

  private def transNull(): Datalog.Pattern = gensym.scoped {
    val outParam = Datalog.Param("this", transType(TNull))
    val thisVar = Datalog.Var("this")
    val constrScalaFun = Term.Function(Nil, q"""$oOID("Null", -1)""")
    val tmpCons = Datalog.Computed(thisVar, Datalog.Evaluation(Seq(), transType(TNull), Scala(constrScalaFun)))

    Datalog.Pattern(None, "Null", Seq(outParam), Seq(
      Datalog.Body(Seq(tmpCons))
    ))
  }

  private def getObjectAttribute(obj: Datalog.Var, attribute: String, outVar: Datalog.Var, outType: Datalog.Type): Datalog.Computed = {
    val compAttr = Term.Name(attribute)
    val compArg = Term.Name("obj")
    val compParam = Term.Param(Nil, compArg, Some(GP_URI.asScala), None)
    Datalog.Computed(
      outVar, Datalog.Evaluation(Seq(obj -> GP_URI), outType, Scala(q"($compParam) => $compArg.$compAttr")
      )
    )
  }

  private def getObjectTyp(obj: Datalog.Var, outVar: Datalog.Var): Datalog.Computed = {
    getObjectAttribute(obj, "typ", outVar, Datalog.TScalaString)
  }

  private def getObjectId(obj: Datalog.Var, outVar: Datalog.Var): Datalog.Computed = {
    getObjectAttribute(obj, "allocId", outVar, Datalog.TScalaInt)
  }

  private def transEquals(): Datalog.Pattern = gensym.scoped {
    // TODO: Change this to scala quality of obj, aka. out = `obj1 == obj2`
    val params = Seq(
      Datalog.Param("obj1", GP_URI),
      Datalog.Param("obj2", GP_URI),
      Datalog.Param("out", Datalog.TScalaBoolean)
    )

    val obj1Var = Datalog.Var("obj1")
    val obj1Typ = Datalog.Var("ty1")
    val obj1Id = Datalog.Var("id1")
    val obj1TypComp = getObjectTyp(obj1Var, obj1Typ)
    val obj1IdComp = getObjectId(obj1Var, obj1Id)

    val obj2Var = Datalog.Var("obj2")
    val obj2Typ = Datalog.Var("ty2")
    val obj2Id = Datalog.Var("id2")
    val obj2TypComp = getObjectTyp(obj2Var, obj2Typ)
    val obj2IdComp = getObjectId(obj2Var, obj2Id)

    val outVar = Datalog.Var("out")
    val outTrue = Datalog.Eq(outVar, Datalog.True)
    val outFalse = Datalog.Eq(outVar, Datalog.False)

    val bodies = Seq(
      Datalog.Body(Seq(
        obj1TypComp, obj2TypComp, Datalog.Eq(obj1Typ, obj2Typ),
        obj1IdComp, obj2IdComp, Datalog.Eq(obj1Id, obj2Id), outTrue
      )),
      Datalog.Body(Seq(
        obj1TypComp, obj2TypComp, Datalog.Neq(obj1Typ, obj2Typ),
        obj1IdComp, obj2IdComp, Datalog.Eq(obj1Id, obj2Id), outFalse
      )),
      Datalog.Body(Seq(
        obj1TypComp, obj2TypComp, Datalog.Eq(obj1Typ, obj2Typ),
        obj1IdComp, obj2IdComp, Datalog.Neq(obj1Id, obj2Id), outFalse
      )),
      Datalog.Body(Seq(
        obj1TypComp, obj2TypComp, Datalog.Neq(obj1Typ, obj2Typ),
        obj1IdComp, obj2IdComp, Datalog.Neq(obj1Id, obj2Id), outFalse
      ))
    )

    Datalog.Pattern(None, equalsPatName, params, bodies)
  }

  private def transInstanceOf(): Datalog.Pattern = gensym.scoped {
    val params = Seq(
      Datalog.Param("this", GP_URI),
      Datalog.Param("t", Datalog.TScalaString),
      Datalog.Param("out", Datalog.TScalaBoolean)
    )

    val tyVar = Datalog.Var("ty")
    val tyComp = getObjectTyp(Datalog.Var("this"), tyVar)

    val outVar = Datalog.Var("out")
    val tyParamVar = Datalog.Var("t")
    val outTrue = Datalog.Eq(outVar, Datalog.True)
    val outFalse = Datalog.Eq(outVar, Datalog.False)

    val isSubtype = Datalog.ExtensionalCall("subtype", Seq(tyVar, tyParamVar))
    // TODO: If negation of ExtensionalCall is implemented this can be changed to !isSubtype
    val notIsSubtype = Datalog.ExtensionalCall("not#subtype", Seq(tyVar, tyParamVar))

    val bodies = Seq(
      Datalog.Body(Seq(tyComp, isSubtype, outTrue)),
      Datalog.Body(Seq(tyComp, notIsSubtype, outFalse)),
    )
    Datalog.Pattern(None, instanceOfPatName, params, bodies)
  }

  private def transCast(): Datalog.Pattern = gensym.scoped {
    val params = Seq(
      Datalog.Param("this", GP_URI),
      Datalog.Param("t", Datalog.TScalaString)
    )

    val tyVar = Datalog.Var("ty")
    val tyComp = getObjectTyp(Datalog.Var("this"), tyVar)

    val tyParamVar = Datalog.Var("t")
    val isSubtype = Datalog.ExtensionalCall("subtype", Seq(tyVar, tyParamVar))

    val bodies = Seq(
      Datalog.Body(Seq(tyComp, isSubtype)),
    )
    Datalog.Pattern(None, castPatName, params, bodies)
      .addHint(OptimizationHints.NoInline, OptimizationHints.NoInlineInput)
  }

  private def transClass(classDef: ClassDef): Seq[Datalog.Pattern] = {
    val constPat = transDefaultConstructor(classDef)
    val methodPats = classDef.methods.map(m => transMethod(classDef, m))
    constPat +: methodPats
  }

  val oOID: meta.Term = symbolOf(ObjectID)
  val tyOID: meta.Type = typeOf[ObjectID]

  private def transDefaultConstructor(classDef: ClassDef): Datalog.Pattern = gensym.scoped {
    val thisVar = Datalog.Var("this")
    val constrScalaFun = Term.Function(Nil, q"""$oOID(${classDef.name.raw})""")
    val tmpCons = Datalog.Computed(thisVar, Datalog.Evaluation(Seq(), transType(classDef.typ), Scala(constrScalaFun)))

    val thisParam = Datalog.Param("this", transType(classDef.typ))
    val params = classDef.fields.map(f => Datalog.Param(f.name.raw, transType(f.typ)))

    Datalog.Pattern(transVis(classDef.vis), constructorPatName(classDef.name.raw), thisParam +: params, Seq(
      Datalog.Body(Seq(tmpCons))
    )).addHint(OptimizationHints.NoInline)
      .addHint(ObjectHints.Allocation)
  }

  private def transMethod(classDef: ClassDef, methodDef: MethodDef): Datalog.Pattern = gensym.scoped {
    gensym.register(methodDef.vars.keys.map(_.raw) + "this")

    val qualifiedName = classDef.name + sep + methodDef.name.raw

    val thisParam = Datalog.Param("this", transType(classDef.typ))
    val argParams = methodDef.params.map { case Param(name, typ) =>
      Datalog.Param(name.raw, transType(typ))
    }
    val returnParams =
      if (methodDef.returnsUnit)
        Seq()
      else
        Seq(Datalog.Param(gensym.fresh("return"), transType(methodDef.outType)))

    val bodyRes = transStatements(methodDef.body)
    val bodies = for ((optReturn, cons) <- bodyRes) yield {
      if (!methodDef.returnsUnit && optReturn.isEmpty)
        throw new IllegalStateException(s"Method ${classDef.name}.${methodDef.name} must call return")
      val returnCons = returnParams.zip(optReturn.getOrElse(Seq())).map { case (p, t) =>
        Datalog.Eq(Datalog.Var(p.name), t)
      }
      Datalog.Body(cons ++ returnCons)
    }

    if (methodDef.isMain)
      Datalog.Pattern(transVis(methodDef.vis), qualifiedName, argParams ++ returnParams,  bodies)
        .addHint(MagicSetHints.Main(argParams.map(_ => true) ++ returnParams.map(_ => false)))
        .addHint(ObjectHints.AllocationRoot)
    else
      Datalog.Pattern(transVis(methodDef.vis), qualifiedName, thisParam +: (argParams ++ returnParams), bodies)
  }

  type Constraints = Seq[Datalog.Atom]
  type Tuple = Seq[Datalog.Term]
  type Alternatives[A] = Seq[A]

  type ExpRes = Alternatives[(Tuple, Constraints)]
  type StmRes = Alternatives[(Option[Tuple], Constraints)]

  private def transStatements(stmts: Seq[Statement]): StmRes = stmts match {
    case Nil => Seq((None, Seq()))
    case s::rest =>
      val alternatives = for ((sReturn, sConstraints) <- transStatement(s)) yield {
        if (sReturn.isDefined)
          Seq((sReturn, sConstraints))
        else
          transStatements(rest).map(res => (res._1, sConstraints ++ res._2))
      }
      alternatives.flatten
  }

  private def transStatement(stmt: Statement): StmRes = stmt match {
    case ExprStmt(expression) =>
      for ((_, cons) <- transExpression(expression))
        yield (None, cons)

    case ReturnStmt(expression) =>
      for ((tup, cons) <- transExpression(expression))
        yield (Some(tup), cons)

    case VarDeclareStmt(name, _, Some(expression), true) =>
      for ((Seq(term), cons) <- transExpression(expression))
        yield (None, cons :+ Datalog.Eq(Datalog.Var(name.raw), term))

    case IfStmt(cnd, thn, els) =>
      val cndTrans = transExpression(cnd)
      val thnTrans = transStatements(thn)
      val elsTrans = transStatements(els)
      val thnRes: StmRes =
        for ((Seq(cndTerm), cndCons) <- cndTrans;
             (thnTerm, thnCons) <- thnTrans)
        yield (thnTerm, cndCons ++ Seq(Datalog.Eq(cndTerm, Datalog.True)) ++ thnCons)
      val elsRes: StmRes =
        for ((Seq(cndTerm), cndCons) <- cndTrans;
             (elsTerm, elsCons) <- elsTrans)
        yield (elsTerm, cndCons ++ Seq(Datalog.Eq(cndTerm, Datalog.False)) ++ elsCons)
      thnRes ++ elsRes
    /*case FieldAssignStmt(recv, name, expression) => ???
    case VarAssignStmt(targetName, expression) => ???
    */
    case _ =>
      throw new RuntimeException("Mutability is no yet supported")
  }

  private def transExpression(expression: Expression): ExpRes = expression match {
    case VarReadExpr(name) =>
      val expTyp = expression.typ.getOrElse(throw new IllegalArgumentException(s"Untyped expression $expression"))
      Seq((flattenVars(name, expTyp).map(_._1), Seq()))

    case FieldReadExpr(recv, targetName) =>
      // TODO: We might use the fieldDef target here instead to allow inheritance of attributes
      val classType = recv.typ.getOrElse(throw new IllegalArgumentException(s"Untyped expression $expression")) match {
        case t: TClass => t
        case _ => throw new IllegalArgumentException(s"Illegal field lookup on expression $expression")
      }
      val classDef = classType.ref.target.getOrElse(throw new IllegalArgumentException(s"Unresolved class $classType"))

      for ((Seq(term), cons) <- transExpression(recv)) yield {
        val fieldReadVar = Datalog.Var(gensym.fresh("fieldRead"))
        val fieldVars = classDef.fields.map(f =>
          if (f.name == targetName)
            fieldReadVar
          else {
            // Using _ more than once is considered the same variable! Use gensym fresh
            Datalog.Var(gensym.fresh("_"))
          }
        )
        val fieldReadCall = Datalog.Call(classType.ref.name.raw, term +: fieldVars)
          .addHint(MagicSetHints.IgnoreCall)
          .addHint(MagicSetHints.FixedAdornment(false +: fieldVars.map(_ => true)))
        (Seq(fieldReadVar), cons :+ fieldReadCall)
      }

    case ConstructorExpr(classRef, args) =>
      val constructedVar = Datalog.Var(gensym.fresh("new"))
      val argRes = args.map(e => transExpression(e))
      val constName = constructorPatName(classRef.name.raw)

      // create single call constraint when no arguments passed
      if (argRes.isEmpty)
        return Seq((Seq(constructedVar), Seq(Datalog.Call(constName, Seq(constructedVar)))))

      for (tups <- TupleOps.cartesianProduct(argRes)) yield {
        val (argTerms, argCons) = tups.unzip
        (Seq(constructedVar), argCons.flatten ++ Seq(Datalog.Call(constName, constructedVar +: argTerms.flatten)))
      }

    case methodCallExp@MethodCallExpr(recv, fun, args) =>
      val outVar = Datalog.Var(gensym.fresh("methodCall"))
      val argRes = args.map(e => transExpression(e))

      val methodDef = methodCallExp.target.getOrElse(throw new IllegalArgumentException(s"Unresolved method $methodCallExp"))
      val qualifiedName = dispatchPatName(methodDef.name + sep + methodDef.paramSignature)

      val transRecv = for ((Seq(term), cons) <- transExpression(recv)) yield {
        if (argRes.isEmpty)
          return Seq((Seq(outVar), cons :+ Datalog.Call(qualifiedName, term +: Seq(outVar))))
        for (tups <- TupleOps.cartesianProduct(argRes)) yield {
          val (argTerms, argCons) = tups.unzip
          (Seq(outVar), cons ++ argCons.flatten ++ Seq(Datalog.Call(qualifiedName, term +: argTerms.flatten :+ outVar)))
        }
      }
      transRecv.flatten

    case TypeCastExpr(recv, toTyp) =>
      for ((Seq(eTerm), eCons) <- transExpression(recv)) yield {
        val castCall = Datalog.Call(castPatName, Seq(eTerm, Datalog.StringConstant(toTyp.toString)))
        (Seq(eTerm), eCons :+ castCall)
      }

    case InstanceOfExpr(recv, ofTyp) =>
      for ((Seq(eTerm), eCons) <- transExpression(recv)) yield {
        val outVar = Datalog.Var(gensym.fresh("isInstance"))
        val instanceOfCall = Datalog.Call(instanceOfPatName, Seq(eTerm, Datalog.StringConstant(ofTyp.toString), outVar))
        (Seq(outVar), eCons :+ instanceOfCall)
      }

    case EqualsExpr(obj1, obj2) =>
      val transExps = Seq(transExpression(obj1), transExpression(obj2))
      for (tups <- TupleOps.cartesianProduct(transExps)) yield {
        val (eTerms, eCons) = tups.unzip
        val outVar = Datalog.Var(gensym.fresh("isEqual"))
        val equalsCall = Datalog.Call(equalsPatName, eTerms.flatten :+ outVar)
        (Seq(outVar), eCons.flatten ++ Seq(equalsCall))
      }

    case NullExpr() =>
      val nullVar = Datalog.Var(gensym.fresh("null"))
      val nullConstrCall = Datalog.Call("Null", Seq(nullVar))
      Seq((Seq(nullVar), Seq(nullConstrCall)))

    /*
    case SuperExpr(args) => ???
    case TupleExpr(exps) => ???*/
    case BaseLitExpr(code) =>
      import scala.meta._
      val evalOut = Datalog.Var(gensym.fresh("lit"))
      val resType = expression.typ.getOrElse(throw new IllegalStateException("Cannot compile untyped Eval"))
      val funCode = q"() => ${code.tree}"
      val evalConstraint = Datalog.Computed(evalOut, Datalog.Evaluation(Seq(), transType(resType), Scala(funCode)))
      Seq((Seq(evalOut), Seq(evalConstraint)))

    case BaseApplyUnaryExpr(op, exp) =>
      import scala.meta.quasiquotes._
      val expParam = {
        val typ = exp.typ.getOrElse(throw new IllegalStateException(s"Cannot compile call to $op with untyped argument $exp"))
        param"exp: ${typ.asScala}"
      }

      val unary = meta.Term.ApplyUnary(op.tree, meta.Term.Name("exp"))
      val funCode = q"($expParam) => $unary"
      val resType = exp.typ.getOrElse(throw new IllegalStateException("Cannot compile untyped base infix application"))

      val expRes = transExpression(exp)
      val evalOut = Datalog.Var(gensym.fresh("eval"))
      for ((Seq(expTerm), expCons) <- expRes) yield {
        val evalConstraint = Datalog.Computed(evalOut,
          Datalog.Evaluation(Seq(expTerm -> transType(exp.typ.get)),
            transType(resType), Scala(funCode)))
        (Seq(evalOut), expCons ++ Seq(evalConstraint))
      }

    case BaseApplyMethodExpr(recv, method, args) =>
      import scala.meta._

      val paramsTyped = (recv +: args.getOrElse(Seq())).zipWithIndex.map { case (arg, ix) =>
        val argTyp = arg.typ.getOrElse(throw new IllegalStateException(s"Cannot compile call of ${recv.prettyprint("")}.$method with untyped argument/reciever $arg"))
        val paramName = gensym.fresh(s"arg$ix")
        param"${Term.Name(paramName)}: ${argTyp.asScala}"
      }.toList
      val scalaArgs = paramsTyped.map(p => Term.Name(p.name.value))
      val methodName = Term.Name(method.raw)
      val funCode =
        if (args.isEmpty)
          q"(..$paramsTyped) => ${scalaArgs.head}.${methodName}"
        else
          q"(..$paramsTyped) => ${scalaArgs.head}.${methodName}(..${scalaArgs.tail})"
      val resType = expression.typ.getOrElse(throw new IllegalStateException("Cannot compile untyped Eval"))

      val recvRes = transExpression(recv)
      val argRes = args.getOrElse(Seq()).map(e => transExpression(e))
      val evalOut = Datalog.Var(gensym.fresh("eval"))
      for (tups <- TupleOps.cartesianProduct(recvRes +: argRes)) yield {
        val (argTermss, argCons) = tups.unzip
        val flatArgTerms = argTermss.zip(recv +: args.getOrElse(Nil)).map {
          case (t :: Nil, arg) => (t, transType(arg.typ.get))
          case (_, arg) => throw new IllegalArgumentException(s"Cannot pass tuple argument $arg to ${recv.prettyprint("")}.$method")
        }
        val evalConstraint = Datalog.Computed(evalOut, Datalog.Evaluation(flatArgTerms, transType(resType), Scala(funCode)))
        (Seq(evalOut), argCons.flatten :+ evalConstraint)
      }

    case BaseApplyExpr(fun, args) =>
      import scala.meta._
      val paramsTyped = args.zipWithIndex.map { case (arg, ix) =>
        val argTyp = arg.typ.getOrElse(throw new IllegalStateException(s"Cannot compile call to $fun with untyped argument $arg"))
        val paramName = gensym.fresh(s"arg$ix")
        param"${Term.Name(paramName)}: ${argTyp.asScala}"
      }.toList
      val scalaArgs: List[meta.Term] = paramsTyped.map(p => Term.Name(p.name.value))
      val funCode = q"(..$paramsTyped) => ${fun.tree}(..$scalaArgs)"
      val resType = expression.typ.getOrElse(throw new IllegalStateException("Cannot compile untyped Eval"))

      val argRes = args.map(e => transExpression(e))
      val evalOut = Datalog.Var(gensym.fresh("eval"))
      for (tups <- TupleOps.cartesianProduct(argRes)) yield {
        val (argTermss, argCons) = tups.unzip
        val flatArgTerms = argTermss.zip(args).map {
          case (Nil, arg) => throw new IllegalArgumentException(s"Cannot pass empty argument $arg to $fun")
          case (t :: Nil, arg) => (t, transType(arg.typ.get))
          case (_, arg) => throw new IllegalArgumentException(s"Cannot pass tuple argument $arg to $fun")
        }
        val evalConstraint = Datalog.Computed(evalOut, Datalog.Evaluation(flatArgTerms, transType(resType), Scala(funCode)))
        (Seq(evalOut), argCons.flatten :+ evalConstraint)
      }

    /*case BaseApplyInfixExpr(left, op, right)
      if op.tree.value == "++" && left.typ.exists(_.isInstanceOf[TSet]) && right.typ.exists(_.isInstanceOf[TSet]) =>
      transExpression(left) ++ transExpression(right)*/

    /*case BaseApplyInfixExpr(left, op, right)
      if op.tree.value == "&" && left.typ.exists(_.isInstanceOf[TSet]) && right.typ.exists(_.isInstanceOf[TSet]) =>
      val transLeft = transExpression(left)
      val transRight = transExpression(right)

      // create substitution: replace every bound variable in right with freshly generated variable to avoid unwanted nameclashes after merging constraints from left and right
      val boundNamesInRight = right.vars.keys.map(_.name).toSet -- right.freevars.map(_.name.name)
      val freshVarsInRight = boundNamesInRight.map { n => Datalog.Var(gensym.fresh(n)) }
      val boundVarsInRight = boundNamesInRight.map(Datalog.Var)
      val subst = Substitute.fromMap(boundVarsInRight.zip(freshVarsInRight).toMap)

      for ((leftTerms, leftCons) <- transLeft;
           (rightTerms, rightCons) <- transRight) yield {
        // apply substitution created above
        val renamedRightTerms = rightTerms.map(subst.substTerm)
        val renamedRightCons = rightCons.map(subst.substAtom)

        // generate equality constraints to force that constraints of left and right have to hold (X intersect Y implemented as X AND Y)
        val eqTerms = leftTerms.zip(renamedRightTerms).map { case (l, r) => Datalog.Eq(l, r) }
        (leftTerms, leftCons ++ renamedRightCons ++ eqTerms)
      }*/

    case BaseApplyInfixExpr(left, op, right) =>
      import scala.meta.quasiquotes._
      val leftParam = {
        val typ = left.typ.getOrElse(throw new IllegalStateException(s"Cannot compile call to $op with untyped argument $left"))
        param"left: ${typ.asScala}"
      }
      val rightParam = {
        val typ = right.typ.getOrElse(throw new IllegalStateException(s"Cannot compile call to $op with untyped argument $right"))
        param"right: ${typ.asScala}"
      }
      val funCode = q"($leftParam, $rightParam) => left ${op.tree} right"
      val resType = expression.typ.getOrElse(throw new IllegalStateException("Cannot compile untyped base infix application"))

      val leftRes = transExpression(left)
      val rightRes = transExpression(right)
      val evalOut = Datalog.Var(gensym.fresh("eval"))
      for ((Seq(leftTerm), leftCons) <- leftRes;
           (Seq(rightTerm), rightCons) <- rightRes) yield {
        val evalConstraint = Datalog.Computed(evalOut,
          Datalog.Evaluation(Seq(leftTerm -> transType(left.typ.get), rightTerm -> transType(right.typ.get)),
            transType(resType), Scala(funCode)))
        (Seq(evalOut), leftCons ++ rightCons ++ Seq(evalConstraint))
      }

    case stmt =>
      throw new RuntimeException(s"Statement not supported $stmt")
  }

  private def flattenParam(name: String, typ: Type, genFresh: Boolean): Seq[Datalog.Param] = typ match {
    case TTuple(tys) => tys.zipWithIndex.flatMap { case (ty, ix) =>
      flattenParam(name + "_" + ix, ty, genFresh = true)
    }
    case _ =>
      val v = if (genFresh) gensym.fresh(name) else name
      Seq(Datalog.Param(v, transType(typ)))
  }

  private def flattenVars(x: Name, ty: Type): Seq[(Datalog.Var, Datalog.Type)] = ty match {
    /*case TTuple(ts) =>
      ts.zipWithIndex.map { case (ty, ix) => Datalog.Var(x.name + "$_" + ix) -> transType(ty) }
      tupleParams.get(x.name) match {
        case Some(vars) =>
          ts.zip(vars).map { case (ty, v) => Datalog.Var(v) -> transType(ty) }
        case None =>
          ts.zipWithIndex.map { case (ty, ix) => Datalog.Var(x.name + "$_" + ix) -> transType(ty) }
      }*/
    case ty =>
      Seq(Datalog.Var(x.raw) -> transType(ty))
  }

  def GP_URI: Datalog.TScala = Datalog.TScala(Scala(typeOf[ObjectID]))

  private def transVis(vis: Option[Visibility]): Option[Datalog.Visibility] =
    vis.map { case Private => Datalog.Private }

  //@tailrec
  private def transType(typ: Type): Datalog.Type = typ match {
    case TAny => Datalog.TAny
    case TNull => GP_URI
    case TClass(_) => GP_URI
    case TScala(ty) => Datalog.TScala(ty)
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
  }
}