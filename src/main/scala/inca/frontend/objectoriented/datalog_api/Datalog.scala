package inca.frontend.objectoriented.datalog_api

import inca.compiler.{CompiledDatalogModule, CompiledModule}
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

/*trait Operation
case class Insertion(name: String, tuple: Tuple) extends Operation
case class Deletion(name: String, tuple: Tuple) extends Operation

trait Input {
  def edits: Option[EditScript]
  def inputs: Seq[Operation]
}
// ...
// feed.processEditScript(edits)
// inputs.foreach { o => feed.insert(o.name, o.tuple) }

case class PatternInput(pat: String, tuple: Tuple, edits: Option[EditScript]) extends Input {
  override def inputs: Seq[Operation] = Seq(Insertion(demandPatternExtensionalPrefix + pat, tuple))
}

case class EDBInput(insertions: Seq[Relation], deletions: Seq[Relation], edits: Option[EditScript] = None) extends Input {
  override def inputs: Seq[Operation] =
    insertions.flatMap { rel =>
      rel.entries.map(tuple => Insertion(rel.name, Tuples.flatTupleOf(rel.flattenEntry(tuple))))
    } ++
    deletions.flatMap { rel =>
      rel.entries.map(tuple => Insertion(rel.name, Tuples.flatTupleOf(rel.flattenEntry(tuple))))
    }
}*/

// Constraint:
// def execute[T <: Diffable](tree: T, pat: String, input: Tuple = null)
//  PatternInput(pat, input, Some(tree.loadEdits))

// Functional:
// def executeInput(main: String, args: Seq[meta.Term])
//  val (es, tuple) = input(args)
//  PatternInput(main, input, Some(es))

// TODO: Edit scripts
trait EDBChange {
  def insertions: Map[RelationName, Relation]
  def deletions: Map[RelationName, Relation]
}

trait Datalog {
  protected def engine: AdvancedViatraQueryEngine
  protected def feed: Database
  protected def compiled: CompiledModule

  def changeEDB(edb: EDBChange): Unit = engine.delayUpdatePropagation { () =>
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

  def query(relName: RelationName)(input: Relation = UnitRelation(relName)): Relation = {
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