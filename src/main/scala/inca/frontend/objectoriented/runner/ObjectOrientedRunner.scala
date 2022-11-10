package inca.frontend.objectoriented.runner

import inca.backend.transform.magic.demand.DemandTransformation.demandPatternPrefix
import inca.compiler.CompiledModule
import inca.frontend.Constants.RelationName
import inca.frontend.objectoriented.lowering.GenerateDatalog.castPatName
import inca.frontend.runner.{Relation, Runner, UnitRelation}
import inca.runtime.data.ObjectID
import inca.runtime.db.Database
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine

import scala.jdk.CollectionConverters.CollectionHasAsScala

final case class TypeCastException(obj: ObjectID, typ: String) extends RuntimeException(s"Could not cast $obj to type $typ!")

final class ObjectOrientedRunner(override val relName: RelationName,
               override val compiled: CompiledModule,
               override val engine: AdvancedViatraQueryEngine,
               override val database: Database)
  extends Runner[ObjectOrientedInput]
{
  def run(terms: meta.Term*): Relation = {
    run(ObjectOrientedInput(terms:_*))
  }

  def run(f: InputClosure): Relation = {
    val input = f(compiled, relName)

    // TODO: Calculate delta etc. that means lastExtInput = input.change.insertions.head ... and so on

    update(input.change)
    val rel = read(input.args)
    throwTypeCastExceptionIfRequired()

    // truncate the output to exclude the input parameter
    val numInputArgs = input.args.arity
    val numArgs = rel.parameterNames.size
    rel.slice(numInputArgs, numArgs)
  }

  private def throwTypeCastExceptionIfRequired(): Unit = {
    val casts = read(UnitRelation(castPatName))
    val castInputs = read(UnitRelation(demandPatternPrefix + castPatName))

    val castObjects = casts.toSet.asInstanceOf[Set[(ObjectID, String)]]
    val castInputObjects = castInputs.toSet.asInstanceOf[Set[(ObjectID, String)]]

    // find a $cast match for each input$cast
    castInputObjects.diff(castObjects).map { case (failureObj, failureType) =>
      throw TypeCastException(failureObj, failureType)
    }
  }
}