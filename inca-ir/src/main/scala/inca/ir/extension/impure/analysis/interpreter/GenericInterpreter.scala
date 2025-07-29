package inca.ir.extension.impure.analysis.interpreter

import inca.ir
import inca.ir.*
import inca.ir.extension.impure.{Impure, ImpurityKind}
import inca.ir.analysis.base.interpreter.{Adornment, BaseGenericInterpreter, IndexedBindingInfo, SupColumn}
import inca.ir.extension.impure.util.CollectImpurityAffectedRelations
import inca.ir.hints.MainHint
import inca.util.Gensym
import sturdy.data.MayJoin

import scala.collection.immutable.ListMap
import scala.compiletime.uninitialized

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  type RelName = Name
  type InName = Name
  type OutName = Name

  if (!interRelational)
    println("[WARNING:] Intra-relational impure analysis is not supported!")

  private var impurityCounter: Map[ImpurityKind, SupColumn] = Map()
  def updateImpurityCounter(kind: ImpurityKind, col: SupColumn): Unit = impurityCounter += kind -> col
  def hasImpurityCounter(kind: ImpurityKind): Boolean = impurityCounter.contains(kind)
  def getImpurityCounter(kind: ImpurityKind): SupColumn = impurityCounter(kind)

  def impurityScoped[A](f: => A): A = {
    val old = impurityCounter
    try {
      supplementaryTable.scoped {
        val a = f
        a
      }
    } finally {
      impurityCounter = old
    }
  }

  // Use a ListMap to make sure that the impurity params are always in the same order
  private var impurityVars: Map[RelName, ListMap[ImpurityKind, (InName, OutName)]] = Map()
  def getImpurityVars(relName: RelName): Map[ImpurityKind, (InName, OutName)] =
    impurityVars.getOrElse(relName, Map())

  // Additional impurity params for a relation
  private def additionalParams(relName: RelName): Seq[Param] =
    getImpurityVars(relName).flatMap { case (kind, (inName, outName)) =>
      Seq(
        Param(inName, kind.ty),
        Param(outName, kind.ty)
      )
    }.toSeq

  override def evalModule(m: Module)(using Fixed): Map[SupColumn, RV] =
    val affectedRelationsCollector = new CollectImpurityAffectedRelations
    affectedRelationsCollector.visitModule(m)
    val affectedRelations = affectedRelationsCollector.affectedRelations

    // determine the new impurity params for each relation
    impurityVars = m.relations.flatMap { (_, r) =>
      if (!r.hasHint(MainHint))
        // use a local gensym for each relation here
        val gensym = Gensym(r.bodies.flatMap(_.vars.map(_.name.name)))
        val relevantImpurities = affectedRelations.filter { (k, relNames) =>
          relNames.contains(r.name)
        }.keys
        val impurityParamNames = relevantImpurities.map { impurity =>
          val inName = gensym.freshName(Name(impurity.name + "$in"))
          val outName = gensym.freshName(Name(impurity.name + "$out"))
          impurity -> (inName, outName)
        }
        Some(r.name -> ListMap.from(impurityParamNames))
      else
        None
    }

    super.evalModule(m)

  private var currentRel: Relation = uninitialized
  override def evalRelationOpen(r: Relation, adorn: Adornment)(using Fixed): RV = gensym.scoped {
    val oldRelation = currentRel
    currentRel = r

    // add the additional impurity params
    val additionalImpurityVars = getImpurityVars(currentRel.name)
      .values
      .flatMap((inName, outName) => Seq(inName.name, outName.name))
    gensym.register(additionalImpurityVars)

    val res = super.evalRelationOpen(r, adorn)
    currentRel = oldRelation
    res
  }

  override def evalBodyOpen(b: Body, paramNames: Seq[SupColumn])(using rec: Fixed): (RV, RV) = impurityScoped {
    // register input impurity
    val relevantImpurities = getImpurityVars(currentRel.name)
    relevantImpurities.foreach { case (kind, (inName, _)) =>
      updateImpurityCounter(kind, inName.name)
    }
    
    evalAtoms(b.atoms)
    val sup = supplementaryTable.getTable

    // copy last impurity vars to match the output impurity parameters
    val inOutMapping = relevantImpurities.map { case (imp, (_, outName)) => getImpurityCounter(imp) -> outName.name }
    val rawBody = inOutMapping.foldLeft(sup) { case (acc, inCol -> outCol) =>
      relationOps.copyColumn(sup, inCol, outCol)
    }
    
    val projectedBody = relationOps.project(rawBody, paramNames)
    (projectedBody, rawBody)
  }

  private def additionalArgs[R <: ModuleEntry](r: R): Seq[Arg] =
    val relevantImpurities = getImpurityVars(r.name)
    val counters = relevantImpurities.flatMap { (kind, _) =>
      val inCounter = Name(getImpurityCounter(kind))
      val outCounter = gensym.freshName(kind.name)
      updateImpurityCounter(kind, outCounter.name)
      Seq(inCounter, outCounter)
    }
    counters.map(n => Var(n).arg).toSeq

  // Extensional relations never need impurity
  private def relationNeedsImpurity[R <: ModuleEntry](r: R): Boolean = r match
    case _: Relation | _: RequireRelation => true
    case _ => false

  // Add impurity params to the relation definition
  override def relationParams[R <: ModuleEntry](r: R): Seq[ir.Param] =
    if (relationNeedsImpurity(r))
      super.relationParams(r) ++ additionalParams(r.name)
    else
      super.relationParams(r)

  // Add the new impurity vars to the evaluation context
  override def evaluationContextForCall[R <: ModuleEntry](r: R, params: Seq[Param], args: Seq[Arg])(using Fixed): (RV, IndexedBindingInfo) =
    super.evaluationContextForCall(r, params, args ++ additionalArgs(r))

  override def evalAtomOpen(at: Atom)(using Fixed): Unit = at match
    case Impure(v, Seq(), update, kind) if !update.vars.map(_.name).contains(v.name) =>
      // init
      val outCol = evalTerm(update)
      updateImpurityCounter(kind, outCol)
    case Impure(v, atoms, update, kind) =>
      if (hasImpurityCounter(kind)) {
        val inCol = getImpurityCounter(kind)
        updateSupplementaryUnchecked { sup =>
          relationOps.copyColumn(sup, inCol, v.name.name)
        }
      }
      updateSupplementaryChecked { sup =>
        evalAtoms(atoms)
        val outCol = evalTerm(update)
        updateImpurityCounter(kind, outCol)
        supplementaryTable.getTable
      }
    case _ => super.evalAtomOpen(at)
