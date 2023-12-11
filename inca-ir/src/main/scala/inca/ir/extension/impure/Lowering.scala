package inca.ir.extension.impure

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.extension.arithmetic
import inca.ir.extension.demand
import inca.ir.lowering.BaseLowering
import inca.ir.visitors.{BaseIRVisitor, IRVisitor}
import inca.ir.{Atom, BaseIR, Body, Call, Eq, Name, Param, RefByName, Relation, Var, WildcardArg}
import inca.ir.extension.aggregate.Aggregate

import scala.collection.mutable.ListBuffer

/**
 * General usage note:
 *
 * You can generate code, such that one body of a relation introduces more impurities than the other.
 * While the lowering supports this case, you have to make sure, that only one consistent impurity counter is derived.
 *
 * A typical example where this case might occur, is e.g. an object creation inside one branch of an if in OODL.
 * That is, one branch increases the impurity counter, while the other doesn't. Since if branches are mutually exclusive
 * only one consistent counter is produces after evaluating the relation that contains the if.
 * Nevertheless, one body increases the counter and the other does not.
 */
trait Lowering extends BaseLowering:
  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set(arithmetic.IR, demand.IR)

  var impurities: Seq[ImpurityKind] = Seq()
  private var pureRelations: Set[Name] = Set()

  private var impurityCounters: Map[ImpurityKind, Name] = Map()
  private def getImpurityCounter(k: ImpurityKind): Var =
    Var(impurityCounters.get(k).map(RefByName.apply).getOrElse(freshImpurityCounter(k).ref))
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

  def impKindsCollector: CollectImpurityKinds = new CollectImpurityKinds
  
  override def visitProgram(modules: Seq[ir.Module]): Seq[ir.Module] =
    val collector = impKindsCollector
    collector.visitProgram(modules)
    impurities = collector.impurities.toSeq.sortBy(_.name)
    pureRelations = modules.flatMap(_.relations).flatMap {
      case (_, r) if r.hasHint(PureHint) => Some(r.name)
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
        Eq(Var(v), counter) +: as :+ Eq(freshCounter, up)
      // TODO: This is ugly, since we now use some key here from the demand relation
      //  How do we make this nice ?
      case Call(RefByName(name), args, false) if !pureRelations.contains(name) && atom.hasHint(demand.DemandIgnoreCallHint) =>
        preserveHints(atom) {
          Seq(Call(name, args.flatMap(visitArg) ++ (impurities ++ impurities).map(_ => Var(Name(gensym.fresh("_"))).arg)))
        }
      case Call(RefByName(name), args, false) if !pureRelations.contains(name) =>
        val impVars = impurities.map(getImpurityCounter).map(_.arg)
        val freshImpVars = impurities.map(freshImpurityCounter).map(_.arg)
        preserveHints(atom) {
          Seq(Call(name, args.flatMap(visitArg) ++ impVars ++ freshImpVars))
        }
      case Call(RefByName(name), args, true) if !pureRelations.contains(name) =>
        preserveHints(atom) {
          Seq(Call(name, args.flatMap(visitArg) ++ (impurities ++ impurities).map(_ => WildcardArg()), true))
        }
      case Aggregate(rel, args, op) if !pureRelations.contains(rel.name) =>
        preserveHints(atom) {
          Seq(Aggregate(rel, args ++ (impurities ++ impurities).map(_ => WildcardArg()), op))
        }
      case _ => super.visitAtom(atom)

class CollectImpurityKinds extends IRVisitor:
  var impurities: Set[ImpurityKind] = Set()
  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Impure(_, _, _, kind) =>
      impurities += kind
      super.visitAtom(atom)
    case _ => super.visitAtom(atom)