package inca.frontend.datalog

import inca.compiler.CompiledModule
import inca.frontend.Constants.RelationName
import inca.runtime.{EnginePool, Query}
import inca.runtime.Query.Specification
import inca.runtime.context.QueryScope
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

import scala.jdk.CollectionConverters.CollectionHasAsScala

class Datalog(compiled: CompiledModule) {
  private val scope = new QueryScope(compiled.dataModel)
  protected val (engine, database) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

  def update(change: EDBChange): Unit = {
    database.processEditScript(change.es)

    change.insertions.foreach {
      case rel: UnitRelation =>
        database.insert(rel.name, Tuples.flatTupleOf())
      case rel: Relation =>
        rel.entries.foreach { tuple =>
          database.insert(rel.name, Tuples.flatTupleOf(rel.flattenEntry(tuple): _*))
        }
    }

    change.deletions.foreach {
      case rel: UnitRelation =>
        database.delete(rel.name, Tuples.flatTupleOf())
      case rel: Relation =>
        rel.entries.foreach { tuple =>
          database.delete(rel.name, Tuples.flatTupleOf(rel.flattenEntry(tuple): _*))
        }
    }
  }

  def readAll: Seq[Relation] = {
    val pattern = compiled.psystemModule.patterns.keys.toSeq.sorted
    pattern.map(n => read(UnitRelation(n)))
  }

  def read(input: Relation): Relation = {
    val pattern = compiled.psystemModule.patterns.getOrElse(input.name, return UnitRelation(input.name))
    val specification: Specification = pattern()
    val matcher: Query.Matcher = specification.getMatcher(engine)
    val parameterNames: Seq[RelationName] = matcher.getParameterNames.asScala.toSeq

    val output =
      if (input.size > 0)
        input.entries.flatMap { t =>
          val inputMatch = toQueryMatch(input.parameterNames, parameterNames.size, input.flattenEntry(t), specification)
          matcher.getAllMatches(inputMatch).asScala
        }
      else {
        val queryMatch = toQueryMatch(parameterNames, parameterNames.size, Seq(), specification)
        matcher.getAllMatches(queryMatch).asScala
      }
    Relation.fromQueryMatches(input.name, parameterNames, output)
  }

  private def toQueryMatch(parameterNames: Seq[String], arity: Int, values: Seq[AnyRef], spec: Specification): Query.Match = {
    val params = parameterNames.zip(values).map { case (p, v) => spec.getPositionOfParameter(p) -> v }.toMap
    val arr = Seq.range(0, arity).map(params.getOrElse(_, null))
    Query.Match(spec, arr.toArray, isMutable = false)
  }
}
