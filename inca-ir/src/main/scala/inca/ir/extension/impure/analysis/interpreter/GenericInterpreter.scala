package inca.ir.extension.impure.analysis.interpreter

import inca.ir
import inca.ir.*
import inca.ir.extension.impure.{Impure, ImpurityKind}
import inca.ir.analysis.base.interpreter.{Adornment, BaseGenericInterpreter, SupColumn}
import sturdy.data.MayJoin
import sturdy.effect.Effect
import sturdy.values.{MaybeChanged, Widen, Join}

// TODO: Currently we don't iterate enough because of the way our scoping works
class ImpurityEffect[RV](using joinRV: Join[RV]) extends Effect:
  var counter: Map[ImpurityKind, RV] = Map()

  def get(kind: ImpurityKind): Option[RV] = counter.get(kind)
  def update(kind: ImpurityKind, updated: RV) = counter += kind -> updated

  def scoped[A](f: => A): A = {
    val old = counter
    try {
      val a = f
      a
    } finally {
      counter = old
    }
  }

  override type State = Map[ImpurityKind, RV]
  override def getState: State = counter
  override def setState(st: State): Unit = counter = st
  override def join: Join[State] = (m1: State, m2: State) =>
    val allKeys = m1.keys ++ m2.keys
    var changed = false
    val joinedState = allKeys.map { k =>
      (m1.get(k), m2.get(k)) match
        case (Some(v), None) => changed = true; k -> v
        case (None, Some(v)) => changed = true; k -> v
        case (Some(v1), Some(v2)) => joinRV(v1, v2) match
          case MaybeChanged.Changed(v) => changed = true; k -> v
          case MaybeChanged.Unchanged(v) => changed = false; k -> v
        case (None, None) => throw IllegalStateException()
    }.toMap
    if (changed)
      MaybeChanged.Changed(joinedState)
    else
      MaybeChanged.Unchanged(joinedState)
  override def widen: Widen[State] = (v1: State, v2: State) => join(v1, v2)

// In contrast to the lowering, the impurity counter in the interpreter are never materialised in the supplementary.
// That is possible, because our type system guarantees that we can never access the internal variables used to
// represent the impurity counter.
trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:

  private lazy val impurityCounter: ImpurityEffect[RV] = new ImpurityEffect[RV](using joinRV)

  def getImpurityCounter(kind: ImpurityKind): RV = impurityCounter.get(kind) match
    case Some(rv) => rv
    case _ => throw IllegalStateException(s"Can not read uninitialized impurity counter for kind $kind!")

  def updateImpurityCounter(kind: ImpurityKind, updated: RV): Unit =
    impurityCounter.update(kind, updated)

  override def additionalEffects: Seq[Effect] = super.additionalEffects :+ impurityCounter
  override def additionalInputEffects: Seq[Effect] = super.additionalInputEffects :+ impurityCounter

  override def evalBodyOpen(b: Body, paramNames: Seq[String])(using rec: Fixed): (RV, RV) = impurityCounter.scoped {
    // Important: eval body is only called for bodies of relations, not for bodies of disjunctions
    super.evalBodyOpen(b, paramNames)
  }

  override def evalAtomOpen(at: Atom)(using Fixed): Unit = at match
    case Impure(v, Seq(), update, kind) if !update.vars.map(_.name).contains(v.name) =>
      supplementaryTable.scoped {
        val counterCol = evalTerm(update)
        val newCounter = relationOps.project(supplementaryTable.getTable, Seq(counterCol))
        updateImpurityCounter(kind, newCounter)
      }
    case Impure(v, atoms, update, kind) =>
      val counter = getImpurityCounter(kind)
      updateSupplementaryUnchecked { sup =>
        val Seq(counterCol) = relationOps.columns(counter)
        val res = relationOps.naturalJoin(sup, counter)
        relationOps.rename(res, Map(counterCol -> v.name.name))
      }
      updateSupplementaryChecked { sup =>
        evalAtoms(atoms)
        supplementaryTable.getTable
      }
      supplementaryTable.scoped {
        val counterCol = evalTerm(update)
        val newCounter = relationOps.project(supplementaryTable.getTable, Seq(counterCol))
        updateImpurityCounter(kind, newCounter)
      }
    case _ => super.evalAtomOpen(at)

