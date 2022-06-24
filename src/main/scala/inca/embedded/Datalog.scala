package inca.embedded
import inca.compiler
import inca.frontend.constraint.compiler.ConstraintOptions
import inca.runtime.{EnginePool, Query}
import inca.runtime.context.{DataModel, QueryScope}
import inca.util.Scala
import org.eclipse.viatra.query.runtime.api.IMatchUpdateListener
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

import scala.+:
import scala.jdk.CollectionConverters._

trait Datalog {
  type Mod
  type Pat
  type Bod
  type Ato
  type Trm
  type Typ

  def module(name: String, patterns: List[Pat]): Mod
  def pattern(name: String, params: List[(String, Typ)], bodies: List[Bod]): Pat
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
  def op(res: Trm, lhs: Trm, op: String, rhs: Trm): Ato

  // terms
  def va(name: String): Trm
  def bool(b: Boolean): Trm
  def int(i: Int): Trm
  def double(d: Double): Trm
  def string(s: String): Trm
}

trait DatalogPatternAST extends Datalog {
  import inca.backend.ir.{Datalog => ir}
  import meta.quasiquotes._

  type Pat = ir.Pattern
  type Bod = ir.Body
  type Ato = ir.Atom
  type Trm = ir.Term
  type Typ = ir.Type

  override def pattern(name: String, params: List[(String, ir.Type)], bodies: List[ir.Body]): ir.Pattern =
    ir.Pattern(None, name, params.map { case (x, t) => ir.Param(x, t) }, bodies)
  override def body(atoms: List[ir.Atom]): ir.Body =
    ir.Body(atoms)

  override def tany: ir.Type = ir.TAny
  override def tbool: ir.Type = ir.TScalaBoolean
  override def tint: ir.Type = ir.TScalaInt
  override def tdouble: ir.Type = ir.TScalaDouble
  override def tstring: ir.Type = ir.TScalaString

  override def eq(t1: ir.Term, t2: ir.Term): ir.Atom = ir.Compare(ir.EqComparator, t1, t2)
  override def neq(t1: ir.Term, t2: ir.Term): ir.Atom = ir.Compare(ir.NeqComparator, t1, t2)
  override def query(rel: String, args: List[ir.Term], extensional: Boolean): ir.Atom =
    if (extensional)
      ir.ExtensionalCall(rel, args)
    else
      ir.Call(rel, args)
  def op(res: ir.Term, lhs: ir.Term, op: String, rhs: ir.Term): ir.Atom = {
    val args = Seq((lhs, tany), (rhs, tany))
    val code = q"(x: Any, y: Any) => x ${meta.Term.Name(op)} y"
    ir.Computed(res, ir.Evaluation(args, tany, Scala(code)))
  }

  override def va(name: String): ir.Term = ir.Var(name)
  override def bool(b: Boolean): ir.Term = ir.Constant(ir.BooleanLiteral(b))
  override def int(i: Int): ir.Term = ir.Constant(ir.IntLiteral(i))
  override def double(d: Double): ir.Term = ir.Constant(ir.DoubleLiteral(d))
  override def string(s: String): ir.Term = ir.Constant(ir.StringLiteral(s))
}

object DatalogModuleAST extends DatalogPatternAST {
  import inca.backend.ir.{Datalog => ir}

  override type Mod = ir.Module
  override def module(name: String, patterns: List[Pat]): ir.Module =
    ir.Module(name, Seq(), patterns, Seq())
}

object DatalogEval extends Datalog with DatalogPatternAST {
  import inca.backend.ir.{Datalog => ir}

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

object DatalogEvalIncremental extends Datalog with DatalogPatternAST {
  import inca.backend.ir.{Datalog => ir}

  type Tuples = Set[Seq[Any]]
  type Modify = (String, Seq[Any], Boolean) => Unit
  type Delete = (String, Seq[Any]) => Unit
  type Observe = (String, (Seq[Any], Boolean) => Unit) => Unit
  override type Mod = (Modify, Observe)

  override def module(name: String, patterns: List[ir.Pattern]): (Modify, Observe) = {
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
    (modify, listen)
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

  val edb0: DatalogEval.EDB = Map()
  val edb1: DatalogEval.EDB = Map(
    "edge" -> Set(
      Seq("a", "b"),
      Seq("b", "a"),
      Seq("a", "c"),
      Seq("c", "d")
    )
  )
  val edb2: DatalogEval.EDB = Map(
    "edge" -> Set(
      Seq("a", "b"),
      Seq("b", "c"),
      Seq("c", "d"),
      Seq("d", "a"),
      Seq("d", "e"),
      Seq("e", "f")
    )
  )

  val edb3: DatalogEval.EDB = Map(
    "edge" -> {
      (for (i <- 1 to 100) yield Seq(s"n$i", s"n${i + 1}")) :+ Seq("n100", "n1")
    }.toSet
  )

  private val pathEval = path(DatalogEval)

  println("path edb0 = " + pathEval(edb0)("path"))
  println("path edb1 = " + pathEval(edb1)("path"))
  println("path edb0 = " + pathEval(edb0)("path"))
  println("path edb2 = " + pathEval(edb2)("path"))

  val path3 = pathEval(edb3)
  println("path edb3 = " + path3("path").size + " tuples")
  println("path edb3 = " + path3("path").size + " tuples")


  // incremental

  def modifyEDB(edb: DatalogEval.EDB, modify: DatalogEvalIncremental.Modify, insert: Boolean): Unit = {
    for ((rel, tups) <- edb; tup <- tups)
      modify(rel, tup, insert)
  }

  private val (pathModify, pathObserve) = path(DatalogEvalIncremental)
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
