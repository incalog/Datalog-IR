package inca.frontend.objectoriented.datalog

import inca.backend.transform.magic.demand.DemandTransformation.demandPatternPrefix
import inca.frontend.objectoriented.compiler.CompiledObjectModule
import inca.frontend.ir.{Datalog, EDBChange, Relation, UnitRelation}
import inca.frontend.objectoriented.lowering.GenerateDatalog.castPatName
import inca.runtime.data.objectoriented.ObjectID
import truechange.EditScript
import truediff.Diffable

final case class TypeCastException(obj: ObjectID, typ: String) extends RuntimeException(s"Could not cast $obj to type $typ!")

final class ObjectOrientedDatalog(compiled: CompiledObjectModule) extends Datalog(compiled) {
  type DiffableChange = (EDBChange, Seq[AnyRef])

  private var lastSeenChange: Option[DiffableChange] = None

  def run(clazz: String, mainMethod: String, terms: meta.Term*): Relation = {
    run(ObjectOrientedInput(terms, compiled, clazz + "$" + mainMethod))
  }

  private def run(input: ObjectOrientedInput): Relation = {
    val (change, diffables) =
      if (lastSeenChange.isDefined) {
        val newChange = (input.change, input.diffables)
        determineChanges(lastSeenChange.get, newChange)
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

  def measure(clazz: String, mainMethod: String, edb: Seq[Relation], terms: meta.Term*): Long = {
    measure(ObjectOrientedInput(terms, compiled, clazz + "$" + mainMethod), edb)
  }

  private def measure(input: ObjectOrientedInput, edb: Seq[Relation]): Long = {
    val change = EDBChange(input.change.es, edb ++ input.change.insertions ++ input.inheritanceEDB, input.change.deletions)
    measure(input.args.name, change)
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
    val castInputs = read(UnitRelation(demandPatternPrefix + castPatName + "$bb"))

    val castObjects = casts.toSet.asInstanceOf[Set[(ObjectID, String)]]
    val castInputObjects = castInputs.toSet.asInstanceOf[Set[(ObjectID, String)]]

    // find a $cast match for each input$cast
    castInputObjects.diff(castObjects).map { case (failureObj, failureType) =>
      throw TypeCastException(failureObj, failureType)
    }
  }
}
