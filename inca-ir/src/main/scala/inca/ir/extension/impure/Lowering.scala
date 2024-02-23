package inca.ir.extension.impure

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.extension.arithmetic
import inca.ir.extension.demand
import inca.ir.lowering.BaseLowering
import inca.ir.visitors.IRVisitor
import inca.ir.{Atom, BaseIR, Body, Call, Eq, Name, Param, RefByName, Relation, Var, WildcardArg}
import inca.ir.extension.aggregate.Aggregate
import inca.ir.util.SourceLocation
import inca.util.Gensym

import scala.collection.mutable

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
 * 5. Insert temporary impurity variables in nested bodies
 * If we, for example, nest a disjunction inside a relation than we need to account for the case, that the bodies of a
 * disjunction might increase the impurity counter differently. E.g Compiling an if-statement, where the first body
 * creates and object and the second body does not create an object, leads to a disjunction where the first body
 * increases the impurity count, while the second body does not. To account for this, the bodies of the disjunction must
 * use a temporary impurity variable. The first atom in the disjunction body sets this temporary variable to the current
 * impurity count from the enclosing parent body. The last atom in the disjunction body must set the new impurity count
 * that should be used after the disjunction body exists. This variable should correspond to the temporary variable.
 *
 * E.g.
 *
 * Wrong:
 * R(....) =
 *   alloc$1 = 0
 *   alloc$2 = alloc$1 + 1
 *   {alloc$3 = alloc$2 + 1} or {  } // first body has allocation, second body has not
 *   alloc$3 // not defined, need some kind of phi node here
 *
 * Solution:
 * R(....) =
 *   alloc$1 = 0
 *   alloc$2 = alloc$1 + 1
 *   {tmp$alloc$1 = alloc$2; tmp$alloc$2 = tmp$alloc$1 +1; alloc$3 = tmp$alloc$2} or { tmp$alloc$1 = alloc$2; alloc$3 = tmp$alloc$1 } // first body has allocation, second body has not
 *   alloc$3 // is defined body the previous bodies
 *
 */

// TODO: The implementation to tackle 5. is really, really messy and ugly, but I couldn't think of a better way
case class ImpurityCounter(kind: ImpurityKind, suffix: String)(gensym: Gensym):
  private var counter: Option[Name] = None

  def get(): Var = counter match
    case Some(name) => Var(RefByName(name))
    case _ => Var(freshCounter().ref)

  def freshCounter(): Var =
    val counterName = kind.name + suffix
    val freshCounter = Name(gensym.fresh(counterName))
    counter = Some(freshCounter)
    Var(freshCounter)

  def copy(): ImpurityCounter =
    val copy = ImpurityCounter(kind, suffix)(gensym)
    copy.counter = counter
    copy


trait Lowering extends BaseLowering with BodyAwareVisitor:
  override val name: String = "Impure"
  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set(arithmetic.IR, demand.IR)

  private var impurityCounter: Map[(ImpurityKind, SourceLocation), ImpurityCounter] = Map()
  private var affectedRelations: Map[ImpurityKind, Set[Name]] = Map()

  // All impurities required for the current relation
  private var relevantImpurities: Seq[ImpurityKind] = Seq()

  private var suffixes:  Map[SourceLocation, String] = Map()

  private def getBodyEnclosureSpecificImpuritySuffix(enclosure: SourceLocation): String =
    enclosure match
      case _: Relation => ""
      case _ =>
        suffixes.get(enclosure) match
          case Some(suffix) => gensym.fresh(suffix)
          case _ =>
            // TODO: This naming scheme is ugly
            val clsName = enclosure.getClass.getSimpleName //enclosure.hashCode().abs.toString
            val suffix = gensym.fresh(gensym.freshGlobal(clsName).replace("$", ""))
            suffixes += enclosure -> suffix
            "_" + suffix

  private def getOrCreateImpurityCounterInstance(kind: ImpurityKind, enclosure: SourceLocation): ImpurityCounter =
    impurityCounter.get((kind, enclosure)) match
      case Some(counter) => counter
      case None =>
        val suffix = getBodyEnclosureSpecificImpuritySuffix(enclosure)
        val counter = ImpurityCounter(kind, suffix)(gensym)
        impurityCounter += (kind, enclosure) -> counter
        counter

  private def freshImpurityCounter(kind: ImpurityKind, enclosure: SourceLocation) =
    getOrCreateImpurityCounterInstance(kind, enclosure).freshCounter()
  private def getImpurityCounter(kind: ImpurityKind, enclosure: SourceLocation) =
    getOrCreateImpurityCounterInstance(kind, enclosure).get()

  def impurityScoped[A](f: => A): A = gensym.scoped {
    val oldImpurityCounter = impurityCounter.map((kind, counter) => kind -> counter.copy())
    val oldSuffixes = suffixes
    try {
      val a = f
      a
    } finally {
      impurityCounter = oldImpurityCounter
      suffixes = oldSuffixes
    }
  }

  override def visitModule(module: ir.Module): ir.Module =
    val affectedRelationsCollector = new CollectImpurityAffectedRelations
    affectedRelationsCollector.visitModule(module)
    affectedRelations = affectedRelationsCollector.affectedRelations
    super.visitModule(module)

  var impurityOutParams: Seq[Param] = Seq()

  override def visitRelation(relation: Relation): Seq[Relation] = impurityScoped {
    relevantImpurities = affectedRelations.filter((_, rels) => rels.contains(relation.name)).keys.toSeq

    registerAllVars(relation)

    val isMainRelation = relation.hasHint(MainHint)

    impurityOutParams = relevantImpurities.map(k => Param(freshImpurityCounter(k, relation).name, k.ty))
    val impurityInParams = relevantImpurities.map(k => Param(freshImpurityCounter(k, relation).name, demand.TDemand(k.ty)))

    val impurityParams = if (!isMainRelation) {
      impurityInParams.zip(impurityOutParams).flatMap((i, o) => Seq(i, o))
    } else {
      impurityOutParams = Seq()
      Seq()
    }

    for r <- super.visitRelation(relation) yield
      r.copy(r.name, r.params ++ impurityParams, r.bodies)
  }

  override def exitEnclosure(enclosure: SourceLocation, parentEnclosureOption: Option[SourceLocation]): Unit =
    parentEnclosureOption match
      case Some(parentEnclosure) =>
        // We need to increase the impurity counter of the parent after the last body of an enclosure is processed
        // This is the case, because visiting the body is impurity scoped
        relevantImpurities.foreach(kind => freshImpurityCounter(kind, parentEnclosure))
      case _ => // nothing
  
  override def visitBody(body: Body, enclosure: SourceLocation, parentEnclosureOption: Option[SourceLocation]): Seq[Body] = impurityScoped {
    parentEnclosureOption match
      case None =>
        // Inside a relation
        for b <- super.visitBody(body, enclosure, parentEnclosureOption) yield
          val impurityOutVars = relevantImpurities.map { kind => getImpurityCounter(kind, enclosure) }
          val outEqs = impurityOutParams.zip(impurityOutVars).map { (p, v) =>
            Eq(Var(p.name), v)
          }
          preserveHints(body)(Body(b.atoms ++ outEqs))
      case Some(parentEnclosure) =>
        // Inside a Disjunction or some other atom that contains a body
        
        // Introduce temporary variables as impurity counter
        val inputImpurityConstraints = relevantImpurities.map { kind =>
          Eq(getImpurityCounter(kind, enclosure), getImpurityCounter(kind, parentEnclosure))
        }

        val bodies = super.visitBody(body, enclosure, parentEnclosureOption)
        preserveHints(body) {
          for b <- bodies yield
            // Read the final impurity counter after processing all atoms in the body
            // And assign the output variable, that should be used after the body
            val outputImpurityConstraints = relevantImpurities.map { kind =>
              Eq(freshImpurityCounter(kind, parentEnclosure), getImpurityCounter(kind, enclosure))
            }
            Body(inputImpurityConstraints ++ b.atoms ++ outputImpurityConstraints)
        }
  }

  override def visitAtom(atom: Atom, enclosure: SourceLocation, parentEnclosureOption: Option[SourceLocation]): Seq[Atom] =
    atom match
      case Impure(v, Seq(), up, kind) if !up.vars.map(_.name).contains(v.name) =>
        val freshCounter = freshImpurityCounter(kind, enclosure)
        Eq(freshCounter, up) :: Nil
      case Impure(v, atoms, up, kind)  =>
        val counter = getImpurityCounter(kind, enclosure)
        val as = atoms.flatMap(visitAtom)
        val freshCounter = freshImpurityCounter(kind, enclosure)
        Eq(Var(v), counter) +: as :+ Eq(freshCounter, up)

      case Call(RefByName(name), args, neg) =>
        val impurityArgs = affectedRelations.flatMap { case (kind, affectedRels) =>
          if (affectedRels.contains(name))
            val previousImpurityVar = getImpurityCounter(kind, enclosure)
            val freshImpurityVar = freshImpurityCounter(kind, enclosure)
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
            val previousImpurityVar = getImpurityCounter(kind, enclosure)
            val freshImpurityVar = freshImpurityCounter(kind, enclosure)
            Seq(previousImpurityVar.arg, freshImpurityVar.arg)
            //Seq(WildcardArg(), WildcardArg())
          else
            Seq()
        }
        preserveHints(atom) {
          Seq(Aggregate(rel, args.flatMap(visitArg) ++ impurityArgs, op))
        }

      case _ => super.visitAtom(atom, enclosure, parentEnclosureOption)


/**
 * For each construct that contains a body a possible parent construct exists, that contains this child body.
 * E.g a body of a disjunction is contained in the body of a relation, as such we say that the parent of the disjunction
 * is the relation. This visitor keeps track of such relationships and provides methods for visiting atoms and bodies
 * that includes this information.
 **/
trait BodyAwareVisitor extends IRVisitor:
  private def currentEnclosure: SourceLocation = _currentEnclosure.get
  private def parentEnclosure: Option[SourceLocation] = _parentEnclosure

  private var _parentEnclosure: Option[SourceLocation] = None
  private var _currentEnclosure: Option[SourceLocation] = None
  private var currentAtom: Option[Atom] = None
  private var currentRelation: Option[Relation] = None

  private var visitedLocations: Set[SourceLocation] = Set()

  protected def locationScoped[A](f: => A): A =
    val oldCurrentAtom = currentAtom
    val oldCurrentRelation = currentRelation
    val oldParentEnclosure = _parentEnclosure
    _parentEnclosure = _currentEnclosure
    try {
      val a = f
      a
    } finally {
      currentAtom = oldCurrentAtom
      currentRelation = oldCurrentRelation
      _currentEnclosure = _parentEnclosure
      _parentEnclosure = oldParentEnclosure
    }

  override def visitRelation(relation: Relation): Seq[Relation] =
    currentRelation = Some(relation)
    super.visitRelation(relation)

  def visitBody(body: Body, enclosure: SourceLocation, parentEnclosureOption: Option[SourceLocation]): Seq[Body] =
    super.visitBody(body)

  override def visitBody(body: Body): Seq[Body] = locationScoped {
    _currentEnclosure = currentAtom match
      case a@Some(atom) => a
      case _ => currentRelation
    visitedLocations += currentEnclosure
    val bs = visitBody(body, currentEnclosure, parentEnclosure)
    currentAtom = None
    bs
  }

  def exitEnclosure(enclosure: SourceLocation, parentEnclosureOption: Option[SourceLocation]): Unit

  def visitAtom(atom: Atom, enclosure: SourceLocation, parentEnclosureOption: Option[SourceLocation]): Seq[Atom] =
    super.visitAtom(atom)

  override def visitAtom(atom: Atom): Seq[Atom] =
    currentAtom = Some(atom)
    val as = visitAtom(atom, currentEnclosure, parentEnclosure)
    if (visitedLocations.contains(atom))
      exitEnclosure(atom, _currentEnclosure)
    as


/** Transitively collect all relations affected by impurities */
class CollectImpurityAffectedRelations extends IRVisitor:
  var affectedRelations: Map[ImpurityKind, Set[Name]] = Map()
  private var currentRelation: Relation = _

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
    currentRelation = relation
    super.visitRelation(relation)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Impure(_, _, _, kind) if !currentRelation.hasHint(MainHint) =>
      addAffectedRelation(currentRelation.name, kind)
      super.visitAtom(atom)

    case Call(RefByName(name), args, neg) =>
      affectedRelations.foreach { (kind, rels) =>
        if (rels.contains(name))
          addAffectedRelation(currentRelation.name, kind)
      }
      super.visitAtom(atom)

    case Aggregate(RefByName(name), args, op) =>
      affectedRelations.foreach { (kind, rels) =>
        if (rels.contains(name))
          addAffectedRelation(currentRelation.name, kind)
      }
      super.visitAtom(atom)

    case _ => super.visitAtom(atom)