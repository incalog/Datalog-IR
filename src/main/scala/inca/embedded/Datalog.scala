package inca.embedded
import inca.backend.hints.{Hint, MagicSetHints}
import inca.backend.transform.magic.demand.{DemandTransformation, DeriveDemandPatterns}
import inca.compiler
import inca.frontend.constraint.compiler.ConstraintOptions
import inca.runtime.context.{DataModel, QueryScope}
import inca.runtime.{EnginePool, Query}
import inca.util.Scala
import org.eclipse.viatra.query.runtime.api.IMatchUpdateListener
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import inca.backend.ir.{Datalog => ir}

import scala.jdk.CollectionConverters._

trait Datalog {
  type Mod
  type Pat
  type Bod
  type Ato
  type Trm
  type Typ
  type Agg

  def module(name: String, patterns: List[Pat]): Mod
  def pattern(name: String, params: List[(String, Typ)], bodies: List[Bod], hints: Set[Hint] = Set()): Pat
  def body(atoms: List[Ato]): Bod

  // types
  def tany: Typ
  def tbool: Typ
  def tint: Typ
  def tdouble: Typ
  def tstring: Typ

  // atoms
  def eq(t1: Trm, t2: Trm): Ato
  def neq(t1: Trm, t2: Trm): Ato
  def query(rel: String, args: List[Trm], extensional: Boolean = false): Ato
  def aggregate(res: Trm, ty: Typ, agg: Agg, rel: String, args: List[Trm], aggColumn: Int): Ato
  def op(res: Trm, lhs: Trm, lty: Typ, op: String, rhs: Trm, rty: Typ): Ato

  // terms
  def va(name: String): Trm
  def bool(b: Boolean): Trm
  def int(i: Int): Trm
  def double(d: Double): Trm
  def string(s: String): Trm
}

trait DatalogReplay extends Datalog {
  import meta.quasiquotes._

  override type Agg = meta.Term

  def replay(m: ir.Module): Mod =
    module(m.name, m.pats.map(replay).toList)
  def replay(p: ir.Pattern): Pat =
    pattern(p.name, p.params.map(p => p.name -> replay(p.typ)).toList, p.bodies.map(replay).toList, p.hints.values.toSet)
  def replay(b: ir.Body): Bod =
    body(b.atoms.map(replay).toList)
  def replay(a: ir.Atom): Ato = a match {
    case ir.Call(name, args, false, false) => query(name, args.map(replay).toList, extensional = false)
    case ir.ExtensionalCall(name, args, false) => query(name, args.map(replay).toList, extensional = true)
    case ir.Compare(ir.EqComparator, lhs, rhs) => eq(replay(lhs), replay(rhs))
    case ir.Compare(ir.NeqComparator, lhs, rhs) => neq(replay(lhs), replay(rhs))
    case ir.Computed(lhs, ir.Evaluation(Seq((l, tyl), (r, tyr)), _, Scala(q"(x: $_, y: $_) => x $oper y"))) if tyl == tyr =>
      op(replay(lhs), replay(l), replay(tyl), oper.value, replay(r), replay(tyr))
    case ir.Computed(lhs, ir.CustomAggregation(ty, _, Scala(agg), name, args, col)) =>
      aggregate(replay(lhs), replay(ty), agg, name, args.map(replay).toList, col)
    case _ => throw new UnsupportedOperationException(s"Cannot replay $a")
  }
  def replay(t: ir.Term): Trm = t match {
    case ir.Var(name) => va(name)
    case ir.Constant(lit) => lit match {
      case ir.base.IntLiteral(v) => int(v)
      case ir.base.LongLiteral(v) => int(v.toInt)
      case ir.base.DoubleLiteral(v) => double(v)
      case ir.base.StringLiteral(v) => string(v)
      case ir.base.BooleanLiteral(v) => bool(v)
    }
  }
  def replay(t: ir.Type): Typ = t match {
    case ir.TAny => tany
    case ir.base.TScalaBoolean => tbool
    case ir.base.TScalaInt => tint
    case ir.base.TScalaDouble => tdouble
    case ir.base.TScalaString => tstring
    case _ => throw new UnsupportedOperationException(s"Cannot replay $t")
  }
}

trait DatalogOperatorType extends Datalog {
  def operatorType(op: String, lhs: Typ, rhs: Typ): Typ = {
    if (lhs == tbool && rhs == tbool) op match {
      case "==" | "!=" | "&&" | "||" => tbool
      case _ => throw new IllegalArgumentException(s"Unknown operator $op")
    } else if (lhs == tint && rhs == tint) op match {
      case "==" | "!=" | "<" | "<=" | ">" | ">=" => tbool
      case "+" | "-" | "*" | "/" => tint
      case _ => throw new IllegalArgumentException(s"Unknown operator $op")
    } else if (lhs == tdouble && rhs == tdouble) op match {
      case "==" | "!=" | "<" | "<=" | ">" | ">=" => tbool
      case "+" | "-" | "*" | "/" => tdouble
      case _ => throw new IllegalArgumentException(s"Unknown operator $op")
    } else if (lhs == tstring && rhs == tstring) op match {
      case "==" | "!=" => tbool
      case "+" => tstring
      case _ => throw new IllegalArgumentException(s"Unknown operator $op")
    } else {
      throw new IllegalArgumentException(s"Type error: Expected numeric or string, but got $lhs and $rhs")
    }
  }
}

trait DatalogPatternAST extends Datalog {
  import meta.quasiquotes._

  type Pat = ir.Pattern
  type Bod = ir.Body
  type Ato = ir.Atom
  type Trm = ir.Term
  type Typ = ir.Type
  type Agg = meta.Term

  override def pattern(name: String, params: List[(String, ir.Type)], bodies: List[ir.Body], hints: Set[Hint]): ir.Pattern = {
    val pat = ir.Pattern(None, name, params.map { case (x, t) => ir.Param(x, t) }, bodies)
    pat.addHint(hints.toSeq:_*)
    pat
  }

  override def body(atoms: List[ir.Atom]): ir.Body =
    ir.Body(atoms)

  override def tany: ir.Type = ir.TAny
  override def tbool: ir.Type = ir.base.TScalaBoolean
  override def tint: ir.Type = ir.base.TScalaInt
  override def tdouble: ir.Type = ir.base.TScalaDouble
  override def tstring: ir.Type = ir.base.TScalaString

  override def eq(t1: ir.Term, t2: ir.Term): ir.Atom = ir.Compare(ir.EqComparator, t1, t2)
  override def neq(t1: ir.Term, t2: ir.Term): ir.Atom = ir.Compare(ir.NeqComparator, t1, t2)
  override def query(rel: String, args: List[ir.Term], extensional: Boolean): ir.Atom =
    if (extensional)
      ir.ExtensionalCall(rel, args)
    else
      ir.Call(rel, args)
  override def aggregate(res: ir.Term, ty: ir.Type, agg: meta.Term, rel: String, args: List[ir.Term], aggColumn: Int): ir.Atom = {
    ir.Computed(res, ir.CustomAggregation(ty, None, Scala(agg), rel, args, aggColumn))
  }
  override def op(res: ir.Term, lhs: ir.Term, lty: ir.Type, op: String, rhs: ir.Term, rty: ir.Type): ir.Atom = {
    val args = Seq((lhs, lty), (rhs, rty))
    val code = q"(x: ${ir.base.typeAsScala(lty)}, y: ${ir.base.typeAsScala(rty)}) => x ${meta.Term.Name(op)} y"
    ir.Computed(res, ir.Evaluation(args, tany, Scala(code)))
  }

  override def va(name: String): ir.Term = ir.Var(name)
  override def bool(b: Boolean): ir.Term = ir.Constant(ir.base.BooleanLiteral(b))
  override def int(i: Int): ir.Term = ir.Constant(ir.base.IntLiteral(i))
  override def double(d: Double): ir.Term = ir.Constant(ir.base.DoubleLiteral(d))
  override def string(s: String): ir.Term = ir.Constant(ir.base.StringLiteral(s))
}

trait DatalogModuleAST extends DatalogPatternAST {

  override type Mod = ir.Module
  override def module(name: String, patterns: List[Pat]): ir.Module =
    ir.Module(name, Seq(), patterns, Seq())
}

trait DatalogEval extends Datalog with DatalogPatternAST {

  type Tuples = Set[Seq[Any]]
  type EDB = Map[String, Tuples]
  type IDB = String => Tuples
  override type Mod = EDB => IDB

  override def module(name: String, patterns: List[ir.Pattern]): EDB => IDB = {
    val module = ir.Module(name, Seq(), patterns, Seq())
    val dataModel = new DataModel()
    val options = ConstraintOptions()
    val compiled = compiler.Compiler.compileGP(module, dataModel, options)
    val psystem = compiled.psystemModule

    edb => {
      val scope = new QueryScope(dataModel)
      val feed = EnginePool.loadDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
      for ((rel, tups) <- edb; tup <- tups)
        feed.insert(rel, Tuples.flatTupleOf(tup:_*))

      name => {
        val querySpec = psystem.patterns.getOrElse(name, throw new IllegalArgumentException(s"Pattern $name undefined in module ${module.name}."))
        val matcher = EnginePool.loadQuery(querySpec(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
        matcher.getAllMatches.asScala.map(m => m.toArray.toSeq).toSet
      }
    }
  }
}

trait DatalogEvalIncremental extends Datalog with DatalogPatternAST {

  type Tuples = Set[Seq[Any]]
  type Modify = (String, Seq[Any], Boolean) => Unit
  type Observe = (String, (Seq[Any], Boolean) => Unit) => Unit
  case class IncDB(modify: Modify, addObserver: Observe)
  override type Mod = IncDB

  override def module(name: String, patterns: List[ir.Pattern]): IncDB = {
    val module = ir.Module(name, Seq(), patterns, Seq())
    val dataModel = new DataModel()
    val options = ConstraintOptions()
    val compiled = compiler.Compiler.compileGP(module, dataModel, options)
    val psystem = compiled.psystemModule

    val scope = new QueryScope(dataModel)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

    val modify: Modify = (rel, tup, insert) =>
      if (insert)
        feed.insert(rel, Tuples.flatTupleOf(tup:_*))
      else
        feed.delete(rel, Tuples.flatTupleOf(tup:_*))
    val listen: Observe = (rel, listener) => {
      val querySpec = psystem.patterns.getOrElse(rel, throw new IllegalArgumentException(s"Pattern $rel undefined in module ${module.name}."))
      engine.addMatchUpdateListener(
        engine.getMatcher(querySpec()),
        new IMatchUpdateListener[Query.Match] {
          override def notifyAppearance(mtch: Query.Match): Unit =
            listener(mtch.toArray.toSeq, true)
          override def notifyDisappearance(mtch: Query.Match): Unit =
            listener(mtch.toArray.toSeq, false)
        },
        false
      )
    }
    IncDB(modify, listen)
  }
}

trait DatalogDemandTransformed extends Datalog with DatalogPatternAST {

  val target: Datalog with DatalogReplay

  override type Mod = Set[String] => target.Mod

  override def module(name: String, patterns: List[ir.Pattern]): Set[String] => target.Mod = mains => {
    patterns.foreach { p =>
      if (mains.contains(p.name)) {
        if (p.hasHint(MagicSetHints.FixedAdornmentKey)) {
          val h = p.getHint(MagicSetHints.FixedAdornmentKey).get.asInstanceOf[MagicSetHints.FixedAdornment]
          p.addHint(MagicSetHints.Main(h.adorn))
        } else {
          throw new IllegalArgumentException(s"Cannot use ${p.name} as main function because its adornment is unknown")
        }
      }
    }
    val module = ir.Module(name, Seq(), patterns, Seq())
    val adorned = DeriveDemandPatterns.transformer(null).transformModule(module)
    val demanded = DemandTransformation.transformer(null).transformModule(adorned)
    target.replay(demanded)
  }
}

object DatalogTest extends App {

  def path(datalog: Datalog): datalog.Mod = {
    import datalog._
    module("Path", List(
      pattern("path", List(("x", tany), ("y", tany)), List(
        body(List(
          query("edge", List(va("x"), va("y")), extensional = true)
        )),
        body(List(
          query("edge", List(va("x"), va("z")), extensional = true),
          query("path", List(va("z"), va("y")))
        ))
      ))
    ))
  }

  val eval = new DatalogEval {}

  val edb0: eval.EDB = Map()
  val edb1: eval.EDB = Map(
    "edge" -> Set(
      Seq("a", "b"),
      Seq("b", "a"),
      Seq("a", "c"),
      Seq("c", "d")
    )
  )
  val edb2: eval.EDB = Map(
    "edge" -> Set(
      Seq("a", "b"),
      Seq("b", "c"),
      Seq("c", "d"),
      Seq("d", "a"),
      Seq("d", "e"),
      Seq("e", "f")
    )
  )

  val edb3: eval.EDB = Map(
    "edge" -> {
      (for (i <- 1 to 100) yield Seq(s"n$i", s"n${i + 1}")) :+ Seq("n100", "n1")
    }.toSet
  )

  private val pathEval = path(eval)

  println("path edb0 = " + pathEval(edb0)("path"))
  println("path edb1 = " + pathEval(edb1)("path"))
  println("path edb0 = " + pathEval(edb0)("path"))
  println("path edb2 = " + pathEval(edb2)("path"))

  val path3 = pathEval(edb3)
  println("path edb3 = " + path3("path").size + " tuples")
  println("path edb3 = " + path3("path").size + " tuples")


  // incremental
  val incremental = new DatalogEvalIncremental {}
  import incremental.IncDB

  def modifyEDB(edb: eval.EDB, modify: incremental.Modify, insert: Boolean): Unit = {
    for ((rel, tups) <- edb; tup <- tups)
      modify(rel, tup, insert)
  }

  private val IncDB(pathModify, pathObserve) = path(incremental)
  pathObserve("path", (tup, inserted) => println((if (inserted) "insert " else "delete ") + tup))

  println(s"### insert EDB0 ###")
  modifyEDB(edb0, pathModify, insert = true)
  println(s"### delete EDB0 ###")
  modifyEDB(edb0, pathModify, insert = false)
  println(s"### insert EDB1 ###")
  modifyEDB(edb1, pathModify, insert = true)
  println(s"### delete EDB1 ###")
  modifyEDB(edb1, pathModify, insert = false)
  println(s"### insert EDB2 ###")
  modifyEDB(edb2, pathModify, insert = true)
  println(s"### delete EDB2 ###")
  modifyEDB(edb2, pathModify, insert = false)

  println(s"### insert EDB1 ###")
  modifyEDB(edb1, pathModify, insert = true)
  println(s"### delete (a,b) ###")
  pathModify("edge", Seq("a", "b"), false)

}
