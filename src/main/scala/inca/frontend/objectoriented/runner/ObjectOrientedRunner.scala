package inca.frontend.objectoriented.runner

import inca.backend.transform.magic.demand.DemandTransformation.demandPatternExtensionalPrefix
import inca.compiler.CompiledModule
import inca.frontend.Constants.RelationName
import inca.frontend.runner.{EDBChange, Relation, Runner}
import inca.runtime.Query
import inca.runtime.Query.Specification
import inca.runtime.db.Database
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples

import scala.jdk.CollectionConverters.CollectionHasAsScala


final class ObjectOrientedRunner(override val relName: RelationName,
               override val compiled: CompiledModule,
               override val engine: AdvancedViatraQueryEngine,
               override val database: Database)
  extends Runner[ObjectOrientedInput]
{
  def run(f: (CompiledModule, RelationName) => ObjectOrientedInput): Relation = {
    val input = f(compiled, relName)

    // TODO: Calculate delta etc. that means lastExtInput = input.change.insertions.head ... and so on

    update(input.change)

    val rel = read(input.args)

    // truncate the output to exclude the input parameter
    val numInputArgs = input.args.arity
    val numArgs = rel.parameterNames.size
    rel.slice(numInputArgs, numArgs)
  }
}