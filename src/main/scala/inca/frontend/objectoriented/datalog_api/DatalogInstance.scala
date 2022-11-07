package inca.frontend.objectoriented.datalog_api

import inca.compiler.{CompiledDatalogModule, CompiledModule}
import inca.frontend.objectoriented.core.TypeCastExpr
import inca.frontend.objectoriented.datalog_api.datalog_api.RelationName
import inca.runtime.Query
import inca.runtime.Query.Specification
import inca.runtime.db.Database
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.{Tuple, Tuples}
import truechange.EditScript

import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.meta.Term

package object datalog_api {
  type RelationName = String
}

object Relation {
  def from(arity: Int, relName: RelationName, matches: Iterable[Query.Match]): Relation = arity match {
    case 0 => UnitRelation(relName)
    case 1 => Relation1(relName, matches)
    case 2 => Relation2(relName, matches)
  }
}

// TODO: Parameter names
trait Relation {
  type Tuple
  def name: RelationName
  def size: Int
  //def columns: Seq[String]
  def entries: Iterable[Tuple]
  def toSet: Set[Tuple] = entries.toSet
  def flattenEntry(entry: Tuple): Seq[AnyRef]

  override def toString: RelationName = s"${getClass.getSimpleName}($name, $size, $toSet)"
}

case class UnitRelation(name: RelationName) extends Relation {
  type Tuple = Unit
  lazy val size: Int = 0
  //def columns: Seq[String] = Seq.empty
  lazy val entries: Iterable[Tuple] = Iterable.empty
  def flattenEntry(entry: Tuple): Seq[AnyRef] = Seq.empty
}

case class Relation1[A <: AnyRef](name: RelationName, matches: Iterable[Query.Match]) extends Relation {
  type Tuple = A
  lazy val size: Int = matches.size
  //def columns: Seq[String] =
  lazy val entries: Iterable[Tuple] = matches.map(_.toArray.head.asInstanceOf[A])
  def flattenEntry(entry: Tuple): Seq[AnyRef] = Seq(entry)
}

case class Relation2[A1 <: AnyRef, A2 <: AnyRef](name: RelationName, matches: Iterable[Query.Match]) extends Relation {
  type Tuple = (A1, A2)
  lazy val size: Int = matches.size
  //def columns: Seq[String] =
  lazy val entries: Iterable[Tuple] = matches.map { m =>
    val entry = m.toArray
    (entry(0).asInstanceOf[A1], entry(1).asInstanceOf[A2])
  }
  def flattenEntry(entry: Tuple): Seq[AnyRef] = Seq(entry._1, entry._2)
}
case class Relation3[A1 <: AnyRef, A2 <: AnyRef, A3 <: AnyRef](name: RelationName, matches: Iterable[Query.Match])
  extends Relation
  {
  type Tuple = (A1, A2, A3)
  lazy val size: Int = matches.size
  //def columns: Seq[String] =
  lazy val entries: Iterable[Tuple] = matches.map { m =>
    val entry = m.toArray
    (entry(0).asInstanceOf[A1], entry(1).asInstanceOf[A2], entry(2).asInstanceOf[A3])
  }
  def flattenEntry(entry: Tuple): Seq[AnyRef] = Seq(entry._1, entry._2, entry._3)
}

trait Operation
case class Insertion(name: String, tuple: Tuple) extends Operation
case class Deletion(name: String, tuple: Tuple) extends Operation

// trait Input {
//  def edits: Option[EditScript]
//  def inputs: Seq[Operation]
// }
// ...
// feed.processEditScript(edits)
// inputs.foreach { o => feed.insert(o.name, o.tuple) }

// case class PatternInput(pat: String, tuple: Tuple, edits: Option[EditScript]) extends Input {
//   override def inputs: Seq[Operation] = Seq(Insertion(demandPatternExtensionalPrefix + pat, tuple))
// }

case class EDBInput(insertions: Seq[Relation], deletions: Seq[Relation], edits: Option[EditScript] = None) extends Input {
  override def inputs: Seq[Operation] =
    insertions.flatMap { rel =>
      rel.entries.map(tuple => Insertion(rel.name, Tuples.flatTupleOf(rel.flattenEntry(tuple))))
    } ++
    deletions.flatMap { rel =>
      rel.entries.map(tuple => Insertion(rel.name, Tuples.flatTupleOf(rel.flattenEntry(tuple))))
    }
}

// Constraint:
// def execute[T <: Diffable](tree: T, pat: String, input: Tuple = null)
//  PatternInput(pat, input, Some(tree.loadEdits))

// Functional:
// def executeInput(main: String, args: Seq[meta.Term])
//  val (es, tuple) = input(args)
//  PatternInput(main, input, Some(es))

// TODO: Edit scripts
trait EDBChange {
  def es: EditScript
  def insertions: Map[RelationName, Relation]
  def deletions: Map[RelationName, Relation]
}

trait DatalogInstance {
  protected def engine: AdvancedViatraQueryEngine
  protected def feed: Database
  protected def compiled: CompiledModule

  protected def update(edb: EDBChange): Unit = engine.delayUpdatePropagation { () =>
    edb.insertions.foreach { case (name, relation) =>
      relation.entries.foreach { tuple =>
        feed.insert(name, Tuples.flatTupleOf(relation.flattenEntry(tuple)))
      }
    }
    edb.deletions.foreach { case (name, relation) =>
      relation.entries.foreach { tuple =>
        feed.delete(name, Tuples.flatTupleOf(relation.flattenEntry(tuple)))
      }
    }
  }
  protected def query(relName: RelationName)(input: Relation = UnitRelation(relName)): Relation = {
    val specification = compiled.psystemModule.patterns(relName)()
    val matcher = specification.getMatcher(engine)
    // TODO: Parameter names
    val arity = matcher.getParameterNames.size()
    val output = {
      if (input.entries.nonEmpty)
        input.entries.flatMap { t =>
          val inputMatch = toQueryMatch(input.flattenEntry(t), specification)
          matcher.getAllMatches(inputMatch).asScala
        }
      else
        matcher.getAllMatches().asScala
    }
    Relation.from(arity, relName, output)
  }

  def toQueryMatch(values: Seq[AnyRef], spec: Specification): Query.Match =
    Query.Match(spec, values.toArray, isMutable = false)
}

object Test {
  // Functional
  val inst = FunctionalInstance(mod)
  inst.execute("main", Seq(q"1", q"2", q"""Add(Var("x"), 12)"""))
  // Object Oriented
  val inst = OOInstance(mod)
  inst.execute("main", Seq(q"1", q"2", q"""new X(12, 3)"""))
  // Constaint
  val inst = ConstraintInstance(mod)
  //
  // @diffable trait Exp extends Diffable
  // case class Add(lhs: Exp, rhs: Exp)
  val tree = Add(Var("x"), 12)
  inst.execute("main", tree, edbInserts/Deletions)
  // IR
  val inst = IRInstance(mod)
  inst.execute("main", edbInserts/Deletions)
  // Souffle
  val inst = SouffleInstance(mod)
  inst.execute("main", edbInserts/Deletions OR Editscript)

  val inst = FunctionalInstance(mod)
  inst.
}

trait DatalogInstance[I <: Input, R <: Runner] {
  def engine: AdvancedViatraQueryEngine
  def database: Database

  protected def updateInput(input: I): Unit = {
    val edbChange = input.translate
    engine.delayUpdatePropagation { () =>
      database.processEditScript(edbChange.es)
      edbChange.insertions.foreach { case (name, rel) =>
        rel.entries.foreach {
          database.insert(name, rel.flattenEntry(_))
        }
      }
      edbChange.deletions.foreach { case (name, rel) =>
        rel.entries.foreach {
          database.delete(name, rel.flattenEntry(_))
        }
      }
    }
  }
  protected def query(name: String, rel: Relation): Runner = {
    // get spec
    // get matcher
    // get result
  }
}
trait Runner[I <: Input] {
  def updateInput(input: I): Unit
}
class FunctionalRunner(instance: FunctionalInstance[FunctionalInput, FunctionalRunner]) extends Runner {
  def run(name: String, args: Seq[meta.Scala.Term]): Relation
}
class ObjectOrientedRunner(instance: ObjectOrientedInstance[ObjectOrientedInput, ObjectOrientedRunner]) extends Runner {
  def run(name: String, args: Seq[meta.Scala.Term]): Relation = {
    if (typeCastError) throw TypeCastEx
    else rel
  }
}
class IRRunner(instance: DatalogInstance[IRInput, IRRunner]) extends Runner {
  def run(name: String, rel: Relation): Relation
}
//trait EDBChange {
//  def es: EditScript
//  def insertions: Seq[Insertion]
//  def deletions: Seq[Insertion]
//}
trait Input {
  def translate: EDBChange
}
case class FunctionalInput() extends Input
case class ObjectOrientedInput() extends Input
class FuntionalInstance(...) extends DatalogInstance[FunctionalInput] {

}

// x, y, z
// 1, 2, 3

// 1, 2, 3
// 2, 3, 5
// 7, 3, 9
inst.changeEDB()
inst.changeEDB()
inst.query()

// we need compiled module for these steps
//   engine
//   database
//   queryscope

// oo
// engine
// db
// queryscope
// fill db with inheritance hierarchy
// create runner r
// r.run(main, ....)
