package inca.frontend.runner

import inca.compiler.CompiledModule
import inca.frontend.Constants.RelationName
import inca.frontend.runner
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


  def update(change: EDBChange): Unit = {
    database.processEditScript(change.es)

    change.insertions.foreach { relation =>
      relation.entries.foreach { tuple =>
        database.insert(relation.name, Tuples.flatTupleOf(relation.flattenEntry(tuple):_*))
      }
    }

    change.deletions.foreach { relation =>
      relation.entries.foreach { tuple =>
        database.delete(relation.name, Tuples.flatTupleOf(relation.flattenEntry(tuple):_*))
      }
    }
  }

  def read(input: Relation): Relation = {
    val specification: Specification = compiled.psystemModule.patterns(input.name)()
    val matcher: Query.Matcher = specification.getMatcher(engine)
    val parameterNames: Seq[RelationName] = matcher.getParameterNames.asScala.toSeq
    val output =
      if (input.size > 0)
        input.entries.flatMap { t =>
          val inputMatch = toQueryMatch(input.parameterNames, parameterNames.size, input.flattenEntry(t), specification)
          matcher.getAllMatches(inputMatch).asScala
        }
      else
        matcher.getAllMatches().asScala
    Relation.fromQueryMatches(relName, parameterNames, output)
  }

  private def toQueryMatch(parameterNames: Seq[String], arity: Int, values: Seq[AnyRef], spec: Specification): Query.Match = {
    val params = parameterNames.zip(values).map { case (p, v) => spec.getPositionOfParameter(p) -> v }.toMap
    val arr = Seq.range(0, arity).map(params.getOrElse(_, null))
    Query.Match(spec, arr.toArray, isMutable = false)
  }

  def run(f: (CompiledModule, RelationName) => I): Relation
}

protected[frontend] class IRRunner(override val relName: RelationName,
               override val compiled: CompiledModule,
               override val engine: AdvancedViatraQueryEngine,
               override val database: Database)
  extends Runner[IRInput]
{
  override def run(f: (CompiledModule, RelationName) => IRInput = IRInput.empty()): Relation = {
    val input = f(compiled, relName)
    update(input.change)
    read(input.args)
  }
}