package inca.ir.optimize
import inca.ir.*
import inca.ir.Hint.{Key, preserveHints}
import inca.ir.extension.aggregate.Aggregate
import inca.ir.extension.impure.MainHint
import inca.ir.typing.Mode
import inca.ir.visitors.IRVisitor

object QueryRelation extends Hint, Hint.Key:
  override def key: Key = this


/*
* This Optimization should be run after the demand transformation!
*/

trait RemoveUnusedParameters extends IRVisitor:
  override def name: String = "RemoveUnusedParameters"

  private enum Phase:
    case IdentifyQueryRelations
    case MarkRequiredColumns
    case CountVariableBindings
    case RemoveUnusedColumns

  private var phase: Phase = _
  private var dirty = true
  private var requiredColumns: Map[String, Set[Int]] = Map() // Pattern name -> Set[Column index]
  private var requiredVars: Set[Name] = Set()
  private var bindingVars: Set[Name] = Set()
  private var boundVars: Set[Name] = Set()


  private def scoped[A](f: => A): A =
    val oldRequiredVars = requiredVars
    try {
      val a = f
      a
    } finally {
      requiredVars = oldRequiredVars
    }
  private def isColumnRequired(patternName: String, colIdx: Int): Boolean = {
    if (requiredColumns.contains(patternName)) {
      return requiredColumns(patternName).contains(colIdx)
    }
    false
  }
  private def addRequiredColumn(patternName: String, index: Int): Unit = {
    if (!isColumnRequired(patternName, index)) {
      dirty = true
      var newColumns = Set(index)
      if (requiredColumns.contains(patternName)) {
        newColumns = newColumns ++ requiredColumns(patternName)
      }
      requiredColumns = requiredColumns.updated(patternName, newColumns)
    }
  }

  private def getVariableNames(arg: Arg): Seq[Name] = {
    arg.vars.map(_.ref.name)
  }

  private def getVariableNames(term: Term): Seq[Name] = {
    term.vars.map(_.ref.name)
  }

  override def visitModule(module: Module): Module =
    phase = Phase.IdentifyQueryRelations
    var newModule = super.visitModule(module)
    phase = Phase.MarkRequiredColumns
    dirty = true
    while (dirty){
      dirty = false
      newModule = super.visitModule(newModule)
    }
    phase = Phase.RemoveUnusedColumns
    newModule = super.visitModule(newModule)
    newModule
  override def visitRelation(relation: Relation): Seq[Relation] =
    preserveHints(relation) {
      phase match
        case Phase.IdentifyQueryRelations =>
          if (relation.hasHint(MainHint) || relation.hasHint(QueryRelation)) {
            relation.params.indices.foreach {
              addRequiredColumn(relation.name, _)
            }
          }
          Seq(relation)
        case Phase.MarkRequiredColumns =>
          scoped {
            val requiredParams = relation.params.indices
              .filter(paramIdx => isColumnRequired(relation.name, paramIdx))
              .map(relation.params(_).name)
              .toSet
            requiredVars = requiredVars ++ requiredParams
            super.visitRelation(relation)
          }
        case Phase.RemoveUnusedColumns =>
          var newParams = Seq(): Seq[Param]
          if (requiredColumns.contains(relation.name)) {
            newParams = relation.params
              .zipWithIndex
              .filter((param, idx) => requiredColumns(relation.name).contains(idx))
              .map((param, idx) => param)
          }
          if (newParams.size != relation.params.size) {
            val removedParams = relation.params.zipWithIndex.collect {
              case (element, index) if (relation.params.indices.toSet -- newParams.indices.toSet).contains(index) => element
            }
          }
          super.visitRelation(Relation(relation.name, newParams, relation.bodies))
        case _ => super.visitRelation(relation)
    }

  override def visitBody(body: Body): Seq[Body] =
    preserveHints(body) {
      phase match
        case Phase.MarkRequiredColumns => scoped {
          super.visitBody(body)
          requiredVars = requiredVars ++ bindingVars.intersect(boundVars)
          bindingVars = Set()
          boundVars = Set()
          super.visitBody(body)
        }
        case _ => super.visitBody(body)
    }

  override def visitTerm(term: Term): Seq[Term] =
    phase match
      case Phase.MarkRequiredColumns if term.typ.isDefined=> term.mode match
        case Mode.Binding => bindingVars = bindingVars ++ getVariableNames(term)
        case Mode.Bound => boundVars = boundVars ++ getVariableNames(term)
        case _ => // nothing
      case _ => // nothing
    super.visitTerm(term)

  override def visitAtom(atom: Atom): Seq[Atom] =
    preserveHints(atom) {
      def isArgConst(arg: Arg): Boolean = {
        arg match
          case TermArg(t: Term) => t.isConstant
          case _ => false
      }

      def markRequiredColumns(name: Name, args: Seq[Arg]): Unit = {
        val columns = args
          .zipWithIndex
          .filter((arg, idx) => requiredVars.intersect(getVariableNames(arg).toSet).nonEmpty || isArgConst(arg))
          .map((arg, idx) => idx)
        columns.foreach(addRequiredColumn(name, _))
      }

      def getRequiredArgs(name: Name, args: Seq[Arg]): Seq[Arg] = {
        args
          .zipWithIndex
          .filter((arg, idx) => requiredColumns(name).contains(idx))
          .map((arg, idx) => arg)
      }

      phase match
        case Phase.MarkRequiredColumns =>
          atom match
            case Call(ref, args, neg) =>
              markRequiredColumns(ref.name, args)
            case Aggregate(rel, args, op) =>
              markRequiredColumns(rel.name, args)
            case _ =>
          super.visitAtom(atom)

        case Phase.RemoveUnusedColumns => atom match
          case Call(ref, args, neg) if requiredColumns.contains(ref.name) =>
            val newArgs = getRequiredArgs(ref.name, args)
            super.visitAtom(Call(ref, newArgs, neg))
          case Call(ref, args, neg) => Seq(Call(ref, Seq(), neg))
          case Aggregate(rel, args, op) =>
            val newArgs = getRequiredArgs(rel.name, args)
            super.visitAtom(Aggregate(rel, newArgs, op))
          case _ => super.visitAtom(atom)
        case _ => super.visitAtom(atom)
    }