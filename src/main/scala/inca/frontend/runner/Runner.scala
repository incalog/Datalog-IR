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
  type InputClosure = (CompiledModule, RelationName) => I

  def relName: RelationName
  def compiled: CompiledModule
  def engine: AdvancedViatraQueryEngine
  def database: Database
  
  def update(change: EDBChange): Unit = {
    database.processEditScript(change.es)

    change.insertions.foreach {
      case rel: UnitRelation =>
        database.insert(rel.name, Tuples.flatTupleOf())
      case rel: Relation =>
        rel.entries.foreach { tuple =>
          database.insert(rel.name, Tuples.flatTupleOf(rel.flattenEntry(tuple):_*))
        }
    }

    change.deletions.foreach {
      case rel: UnitRelation =>
        database.delete(rel.name, Tuples.flatTupleOf())
      case rel: Relation =>
        rel.entries.foreach { tuple =>
        database.delete(rel.name, Tuples.flatTupleOf(rel.flattenEntry(tuple):_*))
      }
    }
  }

  def readAll: Seq[Relation] = {
    val pattern = compiled.psystemModule.patterns.keys
    pattern.map(n => read(UnitRelation(n))).toSeq
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

  def run(f: InputClosure): Relation
}

protected[frontend] class IRRunner(override val relName: RelationName,
               override val compiled: CompiledModule,
               override val engine: AdvancedViatraQueryEngine,
               override val database: Database)
  extends Runner[IRInput]
{
  override def run(f: InputClosure = IRInput.empty()): Relation = {
    val input = f(compiled, relName)
    update(input.change)
    read(input.args)
  }
}