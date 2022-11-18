package inca.frontend.objectoriented.runner

import inca.backend.transform.magic.demand.DemandTransformation.demandPatternPrefix
import inca.compiler.CompiledModule
import inca.frontend.Constants.RelationName
import inca.frontend.objectoriented.lowering.GenerateDatalog.castPatName
import inca.frontend.runner.{EDBChange, Relation, Runner, UnitRelation}
import inca.runtime.data.ObjectID
import inca.runtime.db.Database
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import truechange.EditScript
import truediff.Diffable

import scala.jdk.CollectionConverters.CollectionHasAsScala

final case class TypeCastException(obj: ObjectID, typ: String) extends RuntimeException(s"Could not cast $obj to type $typ!")

final class ObjectOrientedRunner(override val relName: RelationName,
               override val compiled: CompiledModule,
               override val engine: AdvancedViatraQueryEngine,
               override val database: Database)
  extends Runner[ObjectOrientedInput]
{
  type DiffableChange = (EDBChange, Seq[AnyRef])

  private var lastSeenChange: Option[DiffableChange] = None

  def run(terms: meta.Term*): Relation = {
    run(ObjectOrientedInput(terms:_*))
  }

  def run(f: InputClosure): Relation = {
    val input = f(compiled, relName)

    val (change, diffables) =
      if (lastSeenChange.isDefined) {
        determineChanges(lastSeenChange.get, (input.change, input.diffables))
      } else {
        // only load the inheritance edb if we have no previous input
        val newEDB = EDBChange(input.change.es, input.change.insertions ++ input.inheritanceEDB, input.change.deletions)
        (newEDB, input.diffables)
      }

    lastSeenChange = Some((change, diffables))
    update(change)

    val rel = read(input.args)
    throwTypeCastExceptionIfRequired()

    // truncate the output to exclude the input parameter
    val numInputArgs = input.args.arity
    val numArgs = rel.parameterNames.size
    rel.slice(numInputArgs, numArgs)
  }

  private def determineChanges(lastChange: DiffableChange, newChange: DiffableChange): DiffableChange = {
    val (lastEDBChange, lastDiffables) = lastChange
    val (newEDBChange, newDiffables) = newChange

    val (ess, cargs, updatedArgs) = newDiffables.zip(lastDiffables).map {
      case (newArg: Diffable, oldArg: Diffable) =>
        val (edits, updatedArg) = oldArg.compareTo(newArg)
        (edits, updatedArg.uri, updatedArg)
      case (litnew, _) => (EditScript(Seq()), litnew, litnew)
    }.unzip3

    // Remove the old demand relation and add a new one
    val lastDemandInputArg = lastEDBChange.insertions.head
    val newDemandInputArg = newEDBChange.insertions.head
    val demandInputArg = Relation.from(newDemandInputArg.name, newDemandInputArg.parameterNames, Seq(cargs))

    val deleteDiff = lastDemandInputArg +: newEDBChange.deletions
    val insertDiff = demandInputArg +: newEDBChange.insertions.tail

    (EDBChange(EditScript(ess.flatMap(_.edits)), insertDiff, deleteDiff), updatedArgs)
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