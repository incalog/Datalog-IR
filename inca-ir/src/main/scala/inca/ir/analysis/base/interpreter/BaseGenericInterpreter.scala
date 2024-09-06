package inca.ir.analysis.base.interpreter

import inca.ir
import inca.ir.ModuleEntry
import inca.ir.analysis.base.effect.Failure.{UnresolvedVariable, InvalidBindings, ProgramFailure, RefNotFound, UnknownArg, UnknownAtom, UnknownTerm}
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{JoinVBool, RelationValue, Top, VBool, VBoolOps, Value}
import inca.ir.analysis.{AnalysisKey, AnalysisResult, RelationOps, SupplementaryEnvironment}
import inca.ir.extension.impure.MainHint
import inca.ir.typing.Mode
import sturdy.data.{MayJoin, WithJoin}
import sturdy.effect.{Effect, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.effect.store.Store
import sturdy.fix.Fixpoint
import sturdy.values.*
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.EqOps
import sturdy.values.references.AllocationSiteAddr
import sturdy.data.MakeJoined

enum FixIn:
  case Term(term: ir.Term)
  case Atom(atom: ir.Atom)
  case Body(body: ir.Body)
  case Relation(rel: ir.Relation)
  case ExtensionalRelation(rel: ir.ExtensionalRelation)

  def isLoop: Boolean = this match
    case FixIn.Atom(c: ir.Call) => true
    case _ => false

  override def toString: String = this match
    case FixIn.Term(t) => t.toString
    case FixIn.Atom(a) => a.toString
    case FixIn.Body(b) => b.toString
    case FixIn.Relation(rel: ir.Relation) => rel.toString
    case FixIn.ExtensionalRelation(rel: ir.ExtensionalRelation) => rel.toString

enum FixOut[V, B, RV]:
  case Term(values: Seq[V], pure: B)
  case Atom(pure: (RV, B))
  case Body(pure: (RV, B))
  case Relation(pure: (RV, B))
  case ExtensionalRelation(pure: (RV, B))

given FiniteFixIn: Finite[FixIn] with {}

given FiniteFixOut[V, B, RV]: Finite[FixOut[V, B, RV]] with {}

import inca.ir.analysis.base.interpreter.{ FixIn, FixOut }

given CombineFixOut[V, B, RV, VW <: Widening, BW <: Widening, RW <: Widening](using combineV: Combine[V, VW], combineB: Combine[B, BW], combineRV: Combine[RV, RW]): Combine[FixOut[V, B, RV], Widening.No] with
  override def apply(out1: FixOut[V, B, RV], out2: FixOut[V, B, RV]): MaybeChanged[FixOut[V, B, RV]] = (out1, out2) match
    case (FixOut.Term(vs1, p1), FixOut.Term(vs2, p2)) =>
      // We use a cartesian product here because of Datalog set semantics
      val v = for (v1 <- vs1; v2 <- vs2) yield combineV(v1, v2)
      val pure = combineB(p1, p2)
      if v.exists(_.hasChanged) || pure.hasChanged then
        Changed(FixOut.Term(v.map(_.get), pure.get))
      else
        Unchanged(FixOut.Term(v.map(_.get), pure.get))
    case (FixOut.Atom((rv1, p1)), FixOut.Atom((rv2, p2))) =>
      val rv = combineRV(rv1, rv2)
      val pure = combineB(p1, p2)
      if rv.hasChanged || pure.hasChanged then
        Changed(FixOut.Atom(rv.get, pure.get))
      else
        Unchanged(FixOut.Atom(rv.get, pure.get))
    case (FixOut.Body((rv1, p1)), FixOut.Body((rv2, p2))) =>
      val rv = combineRV(rv1, rv2)
      val pure = combineB(p1, p2)
      if rv.hasChanged || pure.hasChanged then
        Changed(FixOut.Body(rv.get, pure.get))
      else
       Unchanged(FixOut.Body(rv.get, pure.get))
    case (FixOut.Relation((rv1, p1)), FixOut.Relation((rv2, p2))) =>
      val rv = combineRV(rv1, rv2)
      val pure = combineB(p1, p2)
      if rv.hasChanged || pure.hasChanged then
        Changed(FixOut.Relation(rv.get, pure.get))
      else
        Unchanged(FixOut.Relation(rv.get, pure.get))
    case (FixOut.ExtensionalRelation((rv1, p1)), FixOut.ExtensionalRelation((rv2, p2))) =>
      val rv = combineRV(rv1, rv2)
      val pure = combineB(p1, p2)
      if rv.hasChanged || pure.hasChanged then
        Changed(FixOut.ExtensionalRelation(rv.get, pure.get))
      else
        Unchanged(FixOut.ExtensionalRelation(rv.get, pure.get))
    case _ => throw new IllegalArgumentException(s"Cannot combine outputs of different kind, $out1 and $out2")


trait BaseGenericInterpreter[C, V, B, RV]:

  // Fixpoint
  def fixpoint: EffectStack ?=> Fixpoint[FixIn, FixOut[V, B, RV]]
  type Fixed = FixIn => FixOut[V, B, RV]

  // Ops & Helper
  def relationOps: RelationOps[C, V, B, RV]

  def boolOps: BooleanOps[B]
  def boolTrue: B = boolOps.boolLit(true)
  def boolFalse: B = boolOps.boolLit(false)
  def boolTop: B

  def eqOps: EqOps[V, B]

  def failure: Failure

  def joinV: Join[V]
  def top: V

  def joinRV: Join[RV]

  def effects: EffectStack

  private val withJoinRV: WithJoin[RV] = MakeJoined(using joinRV, effects)

  def IDB: Store[AllocationSiteAddr, RV, WithJoin]
  def supplementaryEnv: SupplementaryEnvironment[RV, WithJoin]

  // Evaluation
  private lazy val fixed: Fixed = fixpoint(using effects) {
    case FixIn.Term(term) =>
      val (v, p) = evalTermExtend(term)
      FixOut.Term(v, p)
    case FixIn.Atom(atom) =>
      val (rv, p) = evalAtomExtend(atom)
      FixOut.Atom(rv, p)
    case FixIn.Body(body) =>
      val (rv, p) = evalBodyExtend(body)
      FixOut.Body(rv, p)
    case FixIn.Relation(rel) =>
      val (rv, p) = evalRelationExtend(rel)
      FixOut.Relation(rv, p)
    case FixIn.ExtensionalRelation(rel) =>
      val (rv, p) = evalExtensionalRelationExtend(rel)
      FixOut.ExtensionalRelation(rv, p)
  }

  private inline def external[A](f: Fixed ?=> A): A = f(using fixed)

  def evalProgram(p: Seq[ir.Module]): Unit = external(supplementaryEnv.scoped(p.foreach(evalModule)))

  def evalModule(m: ir.Module)(using Fixed): Unit = supplementaryEnv.scoped {
    supplementaryEnv.setState(relationOps.unit)
    val relEntryPoints = m.relations.values.filter(_.hasHint(MainHint)) match
      case mainRels if mainRels.nonEmpty => mainRels
      case _ => m.relations.values
    relEntryPoints.foreach(evalRelation(_))
  }

  protected def unionAll(rvs: Seq[RV]): RV =
    rvs.foldLeft(relationOps.unit)((acc, rv) => relationOps.union(acc, rv))

  private def merge(lhs: RV, rhs: RV, neg: Boolean): RV =
    if neg then
      relationOps.antiJoin(lhs, rhs)
    else
      relationOps.naturalJoin(lhs, rhs)

  protected def mergeIntoEnv(rel: RV, neg: Boolean): Unit =
    val merged = merge(supplementaryEnv.getState, rel, neg)
    supplementaryEnv.setState(merged)

  protected def insertIDB(name: ir.Name, rv: RV): Unit =
    val addr = AllocationSiteAddr.Variable(name.name)(true)
    val oldRVOption = IDB.read(addr)
    val result = oldRVOption.option(rv)(oldRV => relationOps.union(rv, oldRV))(using withJoinRV)
    IDB.free(addr)
    IDB.write(addr, result)

  inline def evalRelation(r: ir.Relation)(using rec: Fixed): (RV, B) = rec(FixIn.Relation(r)) match
    case FixOut.Relation(p) => p
    case _ => throw new IllegalStateException()

  /*private def makeInitTable(rel: ir.Relation): RV =
    // table with no entries, but not a unit or empty table!
    //  table(X, {()}
    relationOps.make(
      rel.params.map(p => relationOps.makeColumnName(p.name.name)),
      relationOps.embedRows(Seq.empty)
    )*/

  def evalRelationExtend(r: ir.Relation)(using Fixed): (RV, B) = supplementaryEnv.scoped {
    val paramNames = r.params.map(p => relationOps.makeColumnName(p.name.name))
    val (bodyRes, bodyPurity) = r.bodies.map { b =>
      val (res, pure) = evalBody(b)
      (relationOps.project(res, paramNames), pure)
    }.unzip

    // If all bodies fail, the relation failed
    val nonEmptyResults = bodyRes.filter(relationOps.isEmpty(_) == boolFalse)
    val relRes =
      if nonEmptyResults.isEmpty then
        relationOps.empty(paramNames)
      else
        unionAll(bodyRes)
    insertIDB(r.name, relRes)
    val p = bodyPurity.foldLeft(boolTrue)((acc, pure) => boolOps.and(acc, pure))
    (relRes, p)
  }

  inline def evalExtensionalRelation(r: ir.ExtensionalRelation)(using rec: Fixed): (RV, B) = rec(FixIn.ExtensionalRelation(r)) match
    case FixOut.ExtensionalRelation(p) => p
    case _ => throw new IllegalStateException()

  def evalExtensionalRelationExtend(r: ir.ExtensionalRelation)(using Fixed): (RV, B) = supplementaryEnv.scoped {
    ???
  }

  inline def evalBody(b: ir.Body)(using rec: Fixed): (RV, B) = rec(FixIn.Body(b)) match
    case FixOut.Body(rv, p) => (rv, p)
    case _ => throw new IllegalStateException()

  def evalBodyExtend(b: ir.Body)(using Fixed): (RV, B) = supplementaryEnv.scoped {
    val (_, atPurity) = b.atoms.map(a => evalAtom(a)).unzip
    val pure = atPurity.foldLeft(boolTrue)((acc, res) => boolOps.and(acc, res))
    (supplementaryEnv.getState, pure)
  }

  inline def evalAtom(at: ir.Atom)(using rec: Fixed): (RV, B) = rec(FixIn.Atom(at)) match
    case FixOut.Atom(p) => p
    case _ => throw new IllegalStateException()

  // I don't think that anything else can be binding in an equality. But if so, subclasses may override this
  def extractVarName(term: ir.Term): ir.Name = term match
    case ir.Var(ref) => ref.name
    case ir.Cast(t, _) => extractVarName(t)

  private final def evalAssign(to: ir.Term, from: ir.Term)(using Fixed): (RV, B) =
    val (_ , p1) = evalTerm(to)
    val (fs, p2) = evalTerm(from)
    val assignedName = extractVarName(to).name
    val assignedValues = fs.map(v => Seq(v))
    val colName = relationOps.makeColumnName(assignedName)
    val res = relationOps.make(Seq(colName), assignedValues)
    mergeIntoEnv(res, false)
    (res, boolOps.and(p1, p2))

  private final def evalCompare(lhs: ir.Term, rhs: ir.Term, neg: Boolean)(using Fixed): (RV, B) =
    val (ls, p1) = evalTerm(lhs)
    val (rs, p2) = evalTerm(rhs)

    // 1
    // 1, 2
    // 1 == 1   True
    // 1 == 2   False

    val lsRv = relationOps.make(Seq(), ls.map(v => Seq(v)))
    val rsRv = relationOps.make(Seq(), rs.map(v => Seq(v)))
    val combinations = relationOps.cartesian(lsRv, rsRv)
    val comparisonResult = relationOps.select(combinations) { case Seq(v1, v2) =>
      if neg then
        eqOps.neq(v1, v2) == boolTrue
      else
        eqOps.equ(v1, v2) == boolTrue
    }
    // if all comparisons fail the atom failed
    val res =
      if relationOps.isEmpty(comparisonResult) == boolTrue then
        relationOps.empty(Seq())
      else
        relationOps.unit
    mergeIntoEnv(res, false)

    (res, boolOps.and(p1, p2))

  private final def evalEq(lhs: ir.Term, rhs: ir.Term, neg: Boolean)(using Fixed): (RV, B) = (lhs.mode, rhs.mode, neg) match
    case (Mode.Binding, Mode.Binding, _) => failure(InvalidBindings, s"Equality between two binding terms: $lhs and $rhs")
    case (Mode.Binding, _, false) => evalAssign(lhs, rhs)
    case (_, Mode.Binding, false) => evalAssign(rhs, lhs)
    case (Mode.Bound, Mode.Bound, _) => evalCompare(lhs, rhs, neg)
    case (m1, m2, _) => failure(InvalidBindings, s"Can not evaluate equality with modes: $m1 <> $m2 and negation: $neg")

  def evalArg(arg: ir.Arg)(using Fixed): (Seq[V], B) = arg match
    case ir.TermArg(t) => evalTerm(t)
    case ir.WildcardArg() => (Seq(), boolTrue)
    case _ => failure(UnknownArg, s"Unknown atom $arg")

  def extractVarName(arg: ir.Arg): Option[ir.Name] = arg match
    case ir.TermArg(t) => Some(extractVarName(t))
    case ir.WildcardArg() => None

  private final def evalCallInternal[R <: ModuleEntry](r: R, params: Seq[ir.Param], args: Seq[ir.Arg], neg: Boolean)(evalRel: => R => (RV, B))(using Fixed): (RV, B) =
    // eval arguments in current scope
    val paramNames = params.map(p => relationOps.makeColumnName(p.name.name))
    val (argRes, argPurity) = args.map(evalArg).unzip
    val (relRes, relPurity) = supplementaryEnv.freshScoped {
      val argRV = paramNames.zip(argRes).map((p, a) => relationOps.make(Seq(p), a.map(v => Seq(v))))
      val evalContext = argRV.foldLeft(relationOps.unit)((acc, rv) => relationOps.naturalJoin(acc, rv))
      supplementaryEnv.setState(evalContext)
      evalRel(r)
    }

    // add all variables bound by the call to the context
    val boundVars = args.map(extractVarName)
    val subst = boundVars.zip(paramNames).flatMap {
      case (Some(varName), p) => Some((p, relationOps.makeColumnName(varName.name)))
      case _ => None
    }.toMap
    val res = relationOps.projectAndRename(relRes, subst)
    mergeIntoEnv(res, neg)
    val pure = (argPurity :+ relPurity).foldLeft(boolTrue)((acc, res) => boolOps.and(acc, res))
    (res, pure)

  private final def evalCall[R <: ir.ModuleEntry](ref: ir.Ref[R], args: Seq[ir.Arg], neg: Boolean)(using Fixed): (RV, B) =
    ref.target match
      case Some(r: ir.Relation) => evalCallInternal(r, r.params, args, neg)(evalRelation(_))
      case Some(r: ir.ExtensionalRelation) => evalCallInternal(r, r.params, args, neg)(evalExtensionalRelation(_))
      case _ => failure(RefNotFound, s"Can not find call reference $ref")

  def evalAtomExtend(at: ir.Atom)(using Fixed): (RV, B) = at match
    case ir.Eq(lhs, rhs, neg) => evalEq(lhs, rhs, neg)
    case ir.Call(ref, args, neg) => evalCall(ref, args, neg)
    case ir.ExtensionalCall(ref, args, neg) => evalCall(ref, args, neg)
    case _ => failure(UnknownAtom, s"Unknown atom $at")

  final def evalTerm(term: ir.Term)(using rec: Fixed): (Seq[V], B) = rec(FixIn.Term(term)) match
    case FixOut.Term(v, p) => (v, p)
    case _ => throw new IllegalStateException()

  def evalTermExtend(term: ir.Term)(using Fixed): (Seq[V], B) = term match
    case ir.Var(ref) if term.mode.isBound =>
      val currentScope = supplementaryEnv.getState
      val columnName = relationOps.makeColumnName(ref.name.name)
      val varEntry = relationOps.project(currentScope, Seq(columnName))
      // we only have a single value per row, since we projected a single column
      val values = relationOps.entries(varEntry).iterator.map(_.head)
      (values.toSeq, boolTrue)
    case ir.Var(ref) => (Seq(), boolTrue)
    case ir.Cast(t, _) => evalTerm(t)
    case _ => failure(UnknownTerm, s"Unknown term $term")