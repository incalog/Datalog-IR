package inca.frontend.runner

import inca.compiler.CompiledModule
import inca.runtime.Query
import inca.runtime.Query.Specification
import inca.runtime.db.Database
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import scala.jdk.CollectionConverters.CollectionHasAsScala


protected[frontend] trait Runner[I <: Input] {
  def relName: RelationName
  def compiled: CompiledModule
  def engine: AdvancedViatraQueryEngine
  def database: Database

  val specification: Specification = compiled.psystemModule.patterns(relName)()
  val matcher: Query.Matcher = specification.getMatcher(engine)
  val parameterNames: Seq[RelationName] = matcher.getParameterNames.asScala.toSeq

  def update(input: I): Unit = {
    val edbChange = input.change
    database.processEditScript(edbChange.es)

    edbChange.insertions.foreach { case (name, relation) =>
      relation.entries.foreach { tuple =>
        database.insert(name, Tuples.flatTupleOf(relation.flattenEntry(tuple):_*))
      }
    }
    
    edbChange.deletions.foreach { case (name, relation) =>
      relation.entries.foreach { tuple =>
        database.delete(name, Tuples.flatTupleOf(relation.flattenEntry(tuple):_*))
      }
    }
  }

  protected[frontend] def runWithInputRelation(input: Relation): Relation = {
    val output =
      if (input.entries.nonEmpty)
        input.entries.flatMap { t =>
          val inputMatch = toQueryMatch(input.parameterNames, parameterNames.size, input.flattenEntry(t), specification)
          matcher.getAllMatches(inputMatch).asScala
        }
      else
        matcher.getAllMatches().asScala
    Relation.fromQueryMatches(parameterNames, relName, output)
  }

  private def toQueryMatch(parameterNames: Seq[String], arity: Int, values: Seq[AnyRef], spec: Specification): Query.Match = {
    val params = parameterNames.zip(values).map { case (p, v) => spec.getPositionOfParameter(p) -> v }.toMap
    val arr = Seq.range(0, arity).map(params.getOrElse(_, null))
    Query.Match(spec, arr.toArray, isMutable = false)
  }
}
// OVER(X, Y, Z)
// OVER(X -> 1, Z -> 2)
// PATH(X, Y)
// path(Y -> 3)
// Seq(3)
// Seq(1, 2)
// Seq(1, null, 2)

protected[frontend] class IRRunner(override val relName: RelationName,
               override val compiled: CompiledModule,
               override val engine: AdvancedViatraQueryEngine,
               override val database: Database)
  extends Runner[IRInput]
{
  def run(input: IRInput = IRInput(UnitRelation(relName))): Relation = {
    update(input)
    runWithInputRelation(input.args)
  }
}