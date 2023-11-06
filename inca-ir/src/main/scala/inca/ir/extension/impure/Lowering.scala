package inca.ir.extension.impure

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.extension.arithmetic
import inca.ir.extension.demand
import inca.ir.lowering.BaseLowering
import inca.ir.visitors.IRVisitor
import inca.ir.{Atom, BaseIR, Body, Call, Eq, Name, NegCall, Param, Relation, Var}

import scala.collection.mutable.ListBuffer

trait Lowering extends BaseLowering:
  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set(arithmetic.IR, demand.IR)

  var impurities: Seq[ImpurityKind] = Seq()
  var pureRelations: Set[Name] = Set()

  private var impurityCounters: Map[ImpurityKind, Name] = Map()
  private def getImpurityCounter(k: ImpurityKind): Var =
    Var(impurityCounters(k))
  private def freshImpurityCounter(kind: ImpurityKind): Var =
    val freshCounter = Name(gensym.fresh(kind.name))
    impurityCounters += kind -> freshCounter
    Var(freshCounter)

  override def visitProgram(modules: Seq[ir.Module]): Seq[ir.Module] =
    val collector = new CollectImpurityKinds()
    collector.visitProgram(modules)
    impurities = collector.impurities.toSeq.sortBy(_.name)
    pureRelations = modules.flatMap(_.relations).flatMap {
      case (_, r) if r.hasHint(Hints.PureKey) => Some(r.name)
      case _ => None
    }.toSet
    super.visitProgram(modules)

  override def visitRelation(relation: Relation): Seq[Relation] = gensym.scoped {
    val impInParams = impurities.map(k => Param(Name(gensym.fresh(k.name)), demand.TDemand(k.ty)))
    val impInVars = impurities.map(freshImpurityCounter)
    val impInEqs = impInParams.zip(impInVars).map((p,v) => Eq(Var(p.name), v))

    val rels = super.visitRelation(relation)
    if (pureRelations.contains(relation.name))
      rels
    else
      val impOutParams = impurities.map(k => Param(getImpurityCounter(k).name, k.ty))

      rels.map(r =>
        r.copy(
          params = r.params ++ impInParams ++ impOutParams,
          bodies = r.bodies.map(b => Body(impInEqs ++ b.atoms)))
      )
  }

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Impure(v, atoms, up, kind) =>
      val counter = getImpurityCounter(kind)
      val as = atoms.flatMap(visitAtom)
      val freshCounter = freshImpurityCounter(kind)
      Eq(v, counter) +: as :+ Eq(freshCounter, up)
    case Call(name, args) if !pureRelations.contains(name) =>
      val impVars = impurities.map(getImpurityCounter)
      val freshImpVars = impurities.map(freshImpurityCounter)
      Seq(Call(name, args.flatMap(visitTerm) ++ impVars ++ freshImpVars))
    case NegCall(name, args) if !pureRelations.contains(name) =>
      Seq(NegCall(name, args.flatMap(visitTerm) ++ (impurities ++ impurities).map(_ => Var(Name(gensym.fresh("_"))))))
    // TODO: What about aggregations ?
    case _ => super.visitAtom(atom)

class CollectImpurityKinds extends IRVisitor:
  var impurities: Set[ImpurityKind] = Set()
  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Impure(_, _, _, kind) =>
      impurities += kind
      super.visitAtom(atom)
    case _ => super.visitAtom(atom)