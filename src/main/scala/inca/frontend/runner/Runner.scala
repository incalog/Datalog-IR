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

  def update(input: I): Unit = {
    val edbChange = input.translate
    database.processEditScript(edbChange.es)

    edbChange.insertions.foreach { case (name, relation) =>
      relation.entries.foreach { tuple =>
        database.insert(name, Tuples.flatTupleOf(relation.flattenEntry(tuple)))
      }
    }
    
    edbChange.deletions.foreach { case (name, relation) =>
      relation.entries.foreach { tuple =>
        database.delete(name, Tuples.flatTupleOf(relation.flattenEntry(tuple)))
      }
    }
  }

  protected[frontend] def runWithInputRelation(input: Relation): Relation = {
    val specification = compiled.psystemModule.patterns(relName)()
    val matcher = specification.getMatcher(engine)
    val parameterNames = matcher.getParameterNames.asScala.toSeq
    val output =
      if (input.entries.nonEmpty)
        input.entries.flatMap { t =>
          val inputMatch = toQueryMatch(input.flattenEntry(t), specification)
          matcher.getAllMatches(inputMatch).asScala
        }
      else
        matcher.getAllMatches().asScala
    Relation.from(parameterNames, relName, output)
  }

  private def toQueryMatch(values: Seq[AnyRef], spec: Specification): Query.Match =
    Query.Match(spec, values.toArray, isMutable = false)
}

protected[frontend] class IRRunner(override val relName: RelationName,
               override val compiled: CompiledModule,
               override val engine: AdvancedViatraQueryEngine,
               override val database: Database)
  extends Runner[IRInput]
{
  def run(input: IRInput = IRInput(UnitRelation(relName))): Relation =
    runWithInputRelation(input.rel)
}