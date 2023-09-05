package inca.runtime.db

import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple
import scala.collection.mutable
import truechange.EditScript

case class DatabaseInput(
    es: EditScript,
    insertions: Map[String, Set[Tuple]],
    deletions: Map[String, Set[Tuple]]) {

  def combine(other: DatabaseInput): DatabaseInput = {
    val combinedES = EditScript(es.edits ++ other.es.edits)
    val combinedInsertions = combineMap(insertions, other.insertions)
    val combinedDeletions = combineMap(deletions, other.deletions)
    DatabaseInput(combinedES, combinedInsertions, combinedDeletions)
  }

  private def combineMap(
      lhs: Map[String, Set[Tuple]],
      rhs: Map[String, Set[Tuple]]
    ): Map[String, Set[Tuple]] = {
    val combined = mutable.Map() ++ lhs
    rhs.foreach { case (rel, tuples) =>
      combined.get(rel) match {
        case Some(oldTuples) =>
          combined(rel) = oldTuples ++ tuples
        case None =>
          combined(rel) = tuples
      }
    }
    combined.toMap
  }
}
object DatabaseInput {
  def empty: DatabaseInput = DatabaseInput(EditScript(Seq()), Map(), Map())
}
