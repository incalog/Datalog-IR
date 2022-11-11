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

import scala.jdk.CollectionConverters.CollectionHasAsScala

final case class TypeCastException(obj: ObjectID, typ: String) extends RuntimeException(s"Could not cast $obj to type $typ!")

final class ObjectOrientedRunner(override val relName: RelationName,
               override val compiled: CompiledModule,
               override val engine: AdvancedViatraQueryEngine,
               override val database: Database)
  extends Runner[ObjectOrientedInput]
{
  private var lastSeenInput: Option[ObjectOrientedInput] = None

  def run(terms: meta.Term*): Relation = {
    run(ObjectOrientedInput(terms:_*))
  }

  def run(f: InputClosure): Relation = {
    val input = f(compiled, relName)

    // TODO: Calculate delta etc. that means lastExtInput = input.change.insertions.head ... and so on
    val change = determineChanges(lastSeenInput, input)
    lastSeenInput = Some(input)
    update(change)

    val rel = read(input.args)
    throwTypeCastExceptionIfRequired()

    // truncate the output to exclude the input parameter
    val numInputArgs = input.args.arity
    val numArgs = rel.parameterNames.size
    rel.slice(numInputArgs, numArgs)
  }

  def diffRelations(newRelations: Seq[Relation], lastRelations: Seq[Relation]): Seq[Relation] = {
    val lastRels = lastRelations.map(r => r.name -> r).toMap
    val newRels = newRelations.map(r => r.name -> r).toMap
    newRels.flatMap { case (name, rel) =>
      val lastRel = lastRels.get(name)
      if (lastRel.isDefined) rel.diff(lastRel.get) else Some(rel)
    }.toSeq
  }

  private def determineChanges(lastInputOption: Option[ObjectOrientedInput], newInput: ObjectOrientedInput): EDBChange = {
    val lastInput = lastInputOption.getOrElse(return newInput.change)

    val lastChange = lastInput.change
    val newChange = newInput.change

    var insertDiff = diffRelations(newChange.insertions, lastChange.insertions).toSet
    var deleteDiff = diffRelations(newChange.deletions, lastChange.deletions).toSet

    // Remove the old demand relation and add a new one if required or keep the old one
    val lastDemandInputArg = lastChange.insertions.head
    val newDemandInputArg = newChange.insertions.head
    if (lastDemandInputArg != newDemandInputArg)
      deleteDiff = deleteDiff + lastDemandInputArg
    else
      insertDiff = insertDiff - lastDemandInputArg

    // TODO: Since we not allow URI objects for now, it should be enough to return an empty edit script
    EDBChange(EditScript(Seq()), insertDiff.toSeq, deleteDiff.toSeq)
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