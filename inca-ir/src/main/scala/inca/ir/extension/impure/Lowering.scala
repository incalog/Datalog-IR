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
  private var pureRelations: Set[Name] = Set()

  private var impurityCounters: Map[ImpurityKind, Name] = Map()
  private def getImpurityCounter(k: ImpurityKind): Var =
    Var(impurityCounters.getOrElse(k, freshImpurityCounter(k).name))
  private def freshImpurityCounter(kind: ImpurityKind): Var =
    val freshCounter = Name(gensym.fresh(kind.name))
    impurityCounters += kind -> freshCounter
    Var(freshCounter)

  def impurityScoped[A](f: => A): A = {
    val oldImpurityCounters = impurityCounters
    try {
      val a = f
      a
    } finally {
      impurityCounters = oldImpurityCounters
    }
  }

  override def visitProgram(modules: Seq[ir.Module]): Seq[ir.Module] =
    val collector = new CollectImpurityKinds()
    collector.visitProgram(modules)
    impurities = collector.impurities.toSeq.sortBy(_.name)
    pureRelations = modules.flatMap(_.relations).flatMap {
      case (_, r) if r.hasHint(Hints.PureKey) => Some(r.name)
      case _ => None
    }.toSet
    super.visitProgram(modules)

  var impOutParams: Seq[Param] = Seq()
  var isPureRelation: Boolean = false

  override def visitRelation(relation: Relation): Seq[Relation] = impurityScoped {
    isPureRelation = pureRelations.contains(relation.name)

    if (isPureRelation)
      super.visitRelation(relation)
    else
      impOutParams = impurities.map(k => Param(freshImpurityCounter(k).name, k.ty))
      val impInParams = impurities.map(k => Param(freshImpurityCounter(k).name, demand.TDemand(k.ty)))
      val rels = super.visitRelation(relation)

      preserveHints(relation) {
        rels.map(r =>
          r.copy(
            params = r.params ++ impInParams ++ impOutParams,
            bodies = r.bodies.map(b => Body(b.atoms)))
        )
      }
  }

  // We need special handling in case that one body has more impurities than another body
  // of the same relation
  override def visitBody(body: Body): Seq[Body] = impurityScoped {
    val bodies = super.visitBody(body)

    if (isPureRelation)
      bodies
    else
      val impMaxVars = impurities.map(getImpurityCounter)
      val impOutEqs = impMaxVars.zip(impOutParams).map((v, p) => Eq(Var(p.name), v))
      bodies.map { b =>
        preserveHints(b)(Body(b.atoms ++ impOutEqs))
      }
  }

  override def visitAtom(atom: Atom): Seq[Atom] =
    atom match
      case Impure(v, atoms, up, kind) =>
        val counter = getImpurityCounter(kind)
        val as = atoms.flatMap(visitAtom)
        val freshCounter = freshImpurityCounter(kind)
        Eq(v, counter) +: as :+ Eq(freshCounter, up)
      // TODO: This is ugly, since we now use some key here from the demand relation
      //  How do we make this nice ?
      case Call(name, args) if !pureRelations.contains(name) && atom.hasHint(demand.Hints.IgnoreCallKey) =>
        preserveHints(atom) {
          Seq(Call(name, args.flatMap(visitTerm) ++ (impurities ++ impurities).map(_ => Var(Name(gensym.fresh("_"))))))
        }
      case Call(name, args) if !pureRelations.contains(name) =>
        val impVars = impurities.map(getImpurityCounter)
        val freshImpVars = impurities.map(freshImpurityCounter)
        preserveHints(atom) {
          Seq(Call(name, args.flatMap(visitTerm) ++ impVars ++ freshImpVars))
        }
      case NegCall(name, args) if !pureRelations.contains(name) =>
        preserveHints(atom) {
          Seq(NegCall(name, args.flatMap(visitTerm) ++ (impurities ++ impurities).map(_ => Var(Name(gensym.fresh("_"))))))
        }
      // TODO: What about aggregations ?
      case _ => super.visitAtom(atom)

class CollectImpurityKinds extends IRVisitor:
  var impurities: Set[ImpurityKind] = Set()
  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Impure(_, _, _, kind) =>
      impurities += kind
      super.visitAtom(atom)
    case _ => super.visitAtom(atom)