package inca.frontend.objectoriented.runner

import inca.compiler.CompiledModule
import inca.frontend.runner.{Relation, RelationName, Runner}
import inca.runtime.Query
import inca.runtime.Query.Specification
import inca.runtime.db.Database
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples

import scala.jdk.CollectionConverters.CollectionHasAsScala


class ObjectOrientedRunner(override val relName: RelationName,
               override val compiled: CompiledModule,
               override val engine: AdvancedViatraQueryEngine,
               override val database: Database)
  extends Runner[ObjectOrientedInput]
{
  def run(input: ObjectOrientedInput): Relation = {
    //update(input)
    // TODO: How to update the input
    runWithInputRelation(input.args)
  }
}