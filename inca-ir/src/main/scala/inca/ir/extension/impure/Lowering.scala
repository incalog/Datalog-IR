package inca.ir.extension.impure

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.extension.arithmetic
import inca.ir.extension.demand
import inca.ir.lowering.BaseLowering
import inca.ir.visitors.IRVisitor
import inca.ir.{Atom, BaseIR, Body, Call, Eq, Name, Param, RefByName, Relation, Var, WildcardArg}
import inca.ir.extension.aggregate.Aggregate

/**
 * General notes:
 *
 * 1. Run before disjunction lowering
 * The disjunction lowering can not handle impurities inside disjunctions. Therefore, always apply the impurity lowering
 * before the disjunction lowering.
 *
 * 2. Insert impurity only if required
 * This Lowering only inserts impurity if needed. That is, it transitively computes all relations that need impurity,
 * changes their parameters and rewrite all calls to these relations.
 *
 * 3. Only insert the equality constraint in the relation body
 * Although disjunctions contain bodies too, we must only insert the output / input constraint in the bodies of a
 * relation, not in the body of disjunctions. Otherwise we might end up with multiple such constraints in a single body.
 *
 * 4. Different counter variables in different bodies
 * You can generate code, such that one body of a relation introduces more impurities than the other.
 * While the lowering supports this case, you have to make sure, that only one consistent impurity counter is derived.
 * A typical example where this case might occur, is e.g. an object creation inside one branch of an if in OODL.
 * That is, one branch increases the impurity counter, while the other doesn't. Since if branches are mutually exclusive
 * only one consistent counter is produces after evaluating the relation that contains the if.
 * Nevertheless, one body increases the counter and the other does not.
 *
 */
trait Lowering extends BaseLowering:
  override val name: String = "Impure"
  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set(arithmetic.IR, demand.IR)

  private var impurityCounter: Map[ImpurityKind, Name] = Map()
  private var lastImpurityCounter: Map[ImpurityKind, Name] = Map()
  private var affectedRelations: Map[ImpurityKind, Set[Name]] = Map()

  private def getImpurityCounter(kind: ImpurityKind): Var =
    impurityCounter.get(kind) match
      case Some(name) => Var(RefByName(name))
      case _ => Var(freshImpurityCounter(kind).ref)

  // Impurity counter before the last reset
  private def getLastImpurityCounter(kind: ImpurityKind): Var =
    lastImpurityCounter.get(kind) match
      case Some(name) => Var(RefByName(name))
      case _ => Var(freshImpurityCounter(kind).ref)

  private def freshImpurityCounter(kind: ImpurityKind): Var =
    val freshCounter = Name(gensym.fresh(kind.name))
    impurityCounter += kind -> freshCounter
    Var(freshCounter)

  def impurityScoped[A](f: => A): A = gensym.scoped {
    val oldImpurityCounter = impurityCounter
    try {
      val a = f
      a
    } finally {
      lastImpurityCounter = impurityCounter
      impurityCounter = oldImpurityCounter
    }
  }

  override def visitModule(module: ir.Module): ir.Module =
    val collector = new CollectImpurityAffectedRelations
    collector.visitModule(module)
    affectedRelations = collector.affectedRelations
    super.visitModule(module)

  override def visitRelation(relation: Relation): Seq[Relation] = impurityScoped {
    val relevantImpurities = affectedRelations.filter((_, rels) => rels.contains(relation.name)).keys.toSeq
    val impurityOutParams = relevantImpurities.map(k => Param(freshImpurityCounter(k).name, k.ty))
    val impurityInParams = relevantImpurities.map(k => Param(freshImpurityCounter(k).name, demand.TDemand(k.ty)))

    registerAllVars(relation)

    // TODO: Replace this with main hint
    val isPureRelation = relation.hasHint(PureHintKey)
    val impurityParams = if (!isPureRelation) {
        impurityInParams.zip(impurityOutParams).flatMap((i, o) => Seq(i,o))
    } else {
        Seq()
    }
    val newRelation = preserveHints(relation) {
      Relation(
        relation.name,
        relation.params.flatMap(visitParam) ++ impurityParams,
        relation.bodies.flatMap(body =>
          val newBodies = visitBody(body)
          val lastImpurityVarsInBody = relevantImpurities.map(k => getLastImpurityCounter(k))
          val impurityOutputParamEqs = impurityOutParams.zip(lastImpurityVarsInBody).map { (out, last) =>
            Eq(Var(out.name), last)
          }
          preserveHints(body)(newBodies.map { b =>
            Body(b.atoms ++ impurityOutputParamEqs)
          })
        )
      )
    }
    Seq(newRelation)
  }

  override def visitBody(body: Body): Seq[Body] = impurityScoped(super.visitBody(body))

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Impure(v, Seq(), up, kind) if !up.vars.contains(v) =>
      val freshCounter = freshImpurityCounter(kind)
      Eq(freshCounter, up) :: Nil
    case Impure(v, atoms, up, kind)  =>
      val counter = getImpurityCounter(kind)
      val as = atoms.flatMap(visitAtom)
      val freshCounter = freshImpurityCounter(kind)
      Eq(Var(v), counter) +: as :+ Eq(freshCounter, up)

    case Call(RefByName(name), args, neg) =>
      val impurityArgs = affectedRelations.flatMap { case (kind, affectedRels) =>
        if (affectedRels.contains(name))
          val previousImpurityVar = getImpurityCounter(kind)
          val freshImpurityVar = freshImpurityCounter(kind)
          Seq(previousImpurityVar.arg, freshImpurityVar.arg)
        //else if (affectedRels.contains(name) && neg)
        //  Seq(WildcardArg(), WildcardArg())
        else
          Seq()
      }
      preserveHints(atom) {
        Seq(Call(name, args.flatMap(visitArg) ++ impurityArgs, neg))
      }
    case Aggregate(rel, args, op) =>
      val impurityArgs = affectedRelations.flatMap { case (kind, affectedRels) =>
        if (affectedRels.contains(rel.name))
          val previousImpurityVar = getImpurityCounter(kind)
          val freshImpurityVar = freshImpurityCounter(kind)
          Seq(previousImpurityVar.arg, freshImpurityVar.arg)
          //Seq(WildcardArg(), WildcardArg())
        else
          Seq()
      }
      preserveHints(atom) {
        Seq(Aggregate(rel, args.flatMap(visitArg) ++ impurityArgs, op))
      }

    case _ => super.visitAtom(atom)

/** Transitively collect all relations affected by impurities */
class CollectImpurityAffectedRelations extends IRVisitor:
  var affectedRelations: Map[ImpurityKind, Set[Name]] = Map()
  private var currentRelation: Name = _

  private def addAffectedRelation(rel: Name, kind: ImpurityKind): Unit =
    val previousAffectedRelations = affectedRelations.getOrElse(kind, Set())
    affectedRelations += kind -> (previousAffectedRelations + rel)

  override def visitModule(module: ir.Module): ir.Module =
    var previousAffectedRelations: Map[ImpurityKind, Set[Name]] = Map()
    val mod: ir.Module = super.visitModule(module)
    // fixpoint computation
    while (previousAffectedRelations != affectedRelations) {
      previousAffectedRelations = affectedRelations
      super.visitModule(module)
    }
    mod

  override def visitRelation(relation: Relation): Seq[Relation] =
    currentRelation = relation.name
    super.visitRelation(relation)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Impure(_, _, _, kind) =>
      addAffectedRelation(currentRelation, kind)
      super.visitAtom(atom)

    case Call(RefByName(name), args, neg) =>
      affectedRelations.foreach { (kind, rels) =>
        if (rels.contains(name))
          addAffectedRelation(currentRelation, kind)
      }
      super.visitAtom(atom)

    case Aggregate(RefByName(name), args, op) =>
      affectedRelations.foreach { (kind, rels) =>
        if (rels.contains(name))
          addAffectedRelation(currentRelation, kind)
      }
      super.visitAtom(atom)

    case _ => super.visitAtom(atom)