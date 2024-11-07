package inca.ir.analysis.base.interpreter

import inca.ir
import inca.ir.ModuleEntry
import inca.ir.analysis.base.effect.{AtomFailed, BaseIRException, BodyFailed, InvalidBindings, NoParamRelation, ProgramFailure, RefNotFound, RelationFailed, UnknownArg, UnknownAtom, UnknownTerm, UnresolvedVariable}
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{JoinVBool, RelationValue, Top, VBool, VBoolOps, Value}
import inca.ir.analysis.{AnalysisKey, AnalysisResult, RelationOps, SupplementaryEnvironment}
import inca.ir.extension.impure.MainHint
import inca.ir.typing.Mode
import sturdy.data.{MayJoin, WithJoin}
import sturdy.effect.{Effect, EffectStack, SturdyException, TrySturdy}
import sturdy.effect.failure.Failure
import sturdy.effect.store.Store
import sturdy.fix.Fixpoint
import sturdy.values.*
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.EqOps
import sturdy.values.references.AllocationSiteAddr
import sturdy.data.MakeJoined
import sturdy.effect.except.Except

// TODO:
//  1. Sturdy except instead of empty table
//  2. Context-Sensitive + Insensitive configurable
//  3. Concrete Interpreter (data + arith + string + agg?)
//  4. Abstract Interpreter - Constant Analysis (data + arith + string + agg?)
//  5. Logger to annotate information
//  6. Optimize program

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

enum FixOut[V, RV]:
  case Term(values: Seq[V])
  case Atom()
  case Body(value: RV)
  case Relation(value: RV)
  case ExtensionalRelation(value: RV)

given FiniteFixIn: Finite[FixIn] with {}

given FiniteFixOut[V, RV]: Finite[FixOut[V, RV]] with {}

import inca.ir.analysis.base.interpreter.{ FixIn, FixOut }

given CombineFixOut[V, RV, VW <: Widening, RW <: Widening](using combineV: Combine[V, VW], combineRV: Combine[RV, RW]): Combine[FixOut[V, RV], Widening.No] with
  override def apply(out1: FixOut[V, RV], out2: FixOut[V, RV]): MaybeChanged[FixOut[V, RV]] = (out1, out2) match
    case (FixOut.Term(vs1), FixOut.Term(vs2)) =>
      // We use a cartesian product here because of Datalog set semantics
      val v = for (v1 <- vs1; v2 <- vs2) yield combineV(v1, v2)
      if (v.exists(_.hasChanged)) {
        Changed(FixOut.Term(v.map(_.get)))
      } else {
        Unchanged(FixOut.Term(v.map(_.get)))
      }
    case (FixOut.Atom(), FixOut.Atom()) =>
      Unchanged(FixOut.Atom())
    case (FixOut.Body(rv1), FixOut.Body(rv2)) =>
      val rv = combineRV(rv1, rv2)
      if (rv.hasChanged) {
        Changed(FixOut.Body(rv.get))
      } else {
       Unchanged(FixOut.Body(rv.get))
      }
    case (FixOut.Relation(rv1), FixOut.Relation(rv2)) =>
      val rv = combineRV(rv1, rv2)
      if (rv.hasChanged) {
        Changed(FixOut.Relation(rv.get))
      } else {
        Unchanged(FixOut.Relation(rv.get))
      }
    case (FixOut.ExtensionalRelation(rv1), FixOut.ExtensionalRelation(rv2)) =>
      val rv = combineRV(rv1, rv2)
      if (rv.hasChanged) {
        Changed(FixOut.ExtensionalRelation(rv.get))
      } else {
        Unchanged(FixOut.ExtensionalRelation(rv.get))
      }
    case _ => throw new IllegalArgumentException(s"Cannot combine outputs of different kind, $out1 and $out2")


trait BaseGenericInterpreter[V, B, RV, J[_] <: MayJoin[_]]:

  // Fixpoint
  def fixpoint: EffectStack ?=> Fixpoint[FixIn, FixOut[V, RV]]
  type Fixed = FixIn => FixOut[V, RV]

  // Ops & Helper
  def relationOps: RelationOps[V, B, RV]

  def boolOps: BooleanOps[B]
  def boolTrue: B = boolOps.boolLit(true)
  def boolFalse: B = boolOps.boolLit(false)
  def boolTop: B

  def eqOps: EqOps[V, B]

  def failure: Failure
  
  def except: Except[BaseIRException, Option[RV], J]

  def joinV: J[V]
  def top: V

  def joinRV: J[RV]

  def effects: EffectStack

  private val withJoinRV: WithJoin[RV] = MakeJoined(using joinRV, effects)

  def IDB: Store[AllocationSiteAddr, RV, WithJoin]
  def supplementaryEnv: SupplementaryEnvironment[RV, J]

  // Evaluation
  private lazy val fixed: Fixed = fixpoint(using effects) {
    case FixIn.Term(term) =>
      val v = evalTermOpen(term)
      FixOut.Term(v)
    case FixIn.Atom(atom) =>
      evalAtomOpen(atom)
      FixOut.Atom()
    case FixIn.Body(body) =>
      val rv = evalBodyOpen(body)
      FixOut.Body(rv)
    case FixIn.Relation(rel) =>
      val rv = evalRelationOpen(rel)
      FixOut.Relation(rv)
    case FixIn.ExtensionalRelation(rel) =>
      val rv = evalExtensionalRelationOpen(rel)
      FixOut.ExtensionalRelation(rv)
  }

  private inline def external[A](f: Fixed ?=> A): A = f(using fixed)

  def evalProgram(p: Seq[ir.Module]): Unit = external(supplementaryEnv.scoped(p.foreach(evalModule)))

  def evalModule(m: ir.Module)(using Fixed): Unit = supplementaryEnv.scoped {
    val relEntryPoints = m.relations.values.filter(_.hasHint(MainHint)) match
      case mainRels if mainRels.nonEmpty => mainRels
      case _ => m.relations.values
    relEntryPoints.foreach(evalRelation(_))
  }

  private def merge(lhs: RV, rhs: RV, neg: Boolean): RV =
    if (neg) {
      relationOps.antiJoin(lhs, rhs)
    } else {
      relationOps.naturalJoin(lhs, rhs)
    }

  protected def mergeIntoEnv(rel: RV, neg: Boolean): Unit =
    val merged = merge(supplementaryEnv.getState, rel, neg)
    supplementaryEnv.setState(merged)

  protected def insertIDB(name: ir.Name, rv: RV): Unit =
    val addr = AllocationSiteAddr.Variable(name.name)(true)
    val oldRVOption = IDB.read(addr)
    val result = oldRVOption.option(rv)(oldRV => relationOps.union(rv, oldRV))//(using withJoinRV)
    IDB.free(addr)
    IDB.write(addr, result)

  inline def evalRelation(r: ir.Relation)(using rec: Fixed): RV = rec(FixIn.Relation(r)) match
    case FixOut.Relation(p) => p
    case _ => throw new IllegalStateException()

  def evalRelationOpen(r: ir.Relation)(using Fixed): RV = supplementaryEnv.scoped {
    val paramNames = r.params.map(p => p.name.name)
    val bodyRes = r.bodies.map { b =>
      // TODO: If all bodies fail, the relation failed, aka we produce an empty RV  
      val res = evalBody(b)
      relationOps.project(res, paramNames)
    }

    /*val nonEmptyResults = bodyRes.filter(relationOps.isEmpty(_) == boolFalse)
    val relRes =
      if (nonEmptyResults.isEmpty) {
        relationOps.empty(paramNames)
      } else {
        unionAll(bodyRes)
      }*/
    //insertIDB(r.name, relRes)
    //relRes
    ???
  }

  inline def evalExtensionalRelation(r: ir.ExtensionalRelation)(using rec: Fixed): RV = rec(FixIn.ExtensionalRelation(r)) match
    case FixOut.ExtensionalRelation(p) => p
    case _ => throw new IllegalStateException()

  def evalExtensionalRelationOpen(r: ir.ExtensionalRelation)(using Fixed): RV = supplementaryEnv.scoped {
    ???
  }

  inline def evalBody(b: ir.Body)(using rec: Fixed): RV = rec(FixIn.Body(b)) match
    case FixOut.Body(rv) => rv
    case _ => throw new IllegalStateException()

  def evalBodyOpen(b: ir.Body)(using Fixed): RV = supplementaryEnv.scoped {
    except.tryCatch(b.atoms.foreach(a => evalAtom(a))) {
      case AtomFailed(msg) => except.throws(BodyFailed(s"Body failed: $b"))
    }
    supplementaryEnv.getState
  }

  inline def evalAtom(at: ir.Atom)(using rec: Fixed): Unit = rec(FixIn.Atom(at)) match
    case FixOut.Atom() => ()
    case _ => throw new IllegalStateException()

  // I don't think that anything else can be binding in an equality. But if so, subclasses may override this
  def extractVarName(term: ir.Term): ir.Name = term match
    case ir.Var(ref) => ref.name
    case ir.Cast(t, _) => extractVarName(t)

  private final def evalAssign(to: ir.Term, from: ir.Term)(using Fixed): Unit =
    //evalTerm(to)
    val fs = evalTerm(from)
    val assignedName = extractVarName(to).name
    val assignedValues = fs.map(v => Seq(v))
    val res = relationOps.make(Seq(assignedName), assignedValues)
    mergeIntoEnv(res, false)

  private final def evalCompare(lhs: ir.Term, rhs: ir.Term, neg: Boolean)(using Fixed): Unit =
    val ls = evalTerm(lhs)
    val rs = evalTerm(rhs)
    val lsRv = relationOps.make(Seq(), ls.map(v => Seq(v)))
    val rsRv = relationOps.make(Seq(), rs.map(v => Seq(v)))
    val combinations = relationOps.cartesian(lsRv, rsRv)
    val comparisonResults = relationOps.select(combinations) { case Seq(v1, v2) =>
      if (neg) {
        eqOps.neq(v1, v2) == boolTrue
      } else {
        eqOps.equ(v1, v2) == boolTrue
      }
    }
    // if all comparisons fail the atom failed
    if (relationOps.entries(comparisonResults).iterator.isEmpty) {
      val op = if neg then "!=" else "=="
      except.throws(AtomFailed(s"Comparison $lhs $op $rhs always fails"))
    }

  private final def evalEq(lhs: ir.Term, rhs: ir.Term, neg: Boolean)(using Fixed): Unit = (lhs.mode, rhs.mode, neg) match
    case (Mode.Binding, Mode.Binding, _) => failure(InvalidBindings, s"Equality between two binding terms: $lhs and $rhs")
    case (Mode.Binding, _, false) => evalAssign(lhs, rhs)
    case (_, Mode.Binding, false) => evalAssign(rhs, lhs)
    case (Mode.Bound, Mode.Bound, _) => evalCompare(lhs, rhs, neg)
    case (m1, m2, _) => failure(InvalidBindings, s"Can not evaluate equality with modes: $m1 <> $m2 and negation: $neg")

  def evalArg(arg: ir.Arg)(using Fixed): Seq[V] = arg match
    case ir.TermArg(t) if t.mode.isBound => evalTerm(t)
    case ir.TermArg(t) => Seq()
    case ir.WildcardArg() => Seq()
    case _ => failure(UnknownArg, s"Unknown atom $arg")

  def extractVarName(arg: ir.Arg): Option[ir.Name] = arg match
    case ir.TermArg(t) => Some(extractVarName(t))
    case ir.WildcardArg() => None

  private final def evalCallInternal[R <: ModuleEntry](r: R, params: Seq[ir.Param], args: Seq[ir.Arg], neg: Boolean)(evalRel: => R => RV)(using Fixed): Unit =
    if (params.isEmpty) {
      // Relation with no parameters... This should not happen, even though viatra supports it
      failure(NoParamRelation, s"Relation ${r.name} has no Parameters!")
    } else {
      // eval arguments in current scope
      val paramNames = params.map(p => p.name.name)
      val argRes = args.map(evalArg)

      // eval the actual call in a scoped environment
      val relRes = supplementaryEnv.freshScoped {
        val argRV = paramNames.zip(argRes).map((p, a) => relationOps.make(Seq(p), a.map(v => Seq(v))))
        // since we have at least one parameter argRV is defined
        val evalContext = argRV.foldLeft(argRV.head)((acc, rv) => relationOps.naturalJoin(acc, rv))
        supplementaryEnv.setState(evalContext)
        evalRel(r)
      }

      // add all variables bound by the call to the context
      val boundVars = args.map(extractVarName)
      val subst = boundVars.zip(paramNames).flatMap {
        case (Some(varName), p) => Some((p, varName.name))
        case _ => None
      }.toMap
      val res = relationOps.projectAndRename(relRes, subst)

      // update the environment after the call
      mergeIntoEnv(res, neg)
    }

  private final def evalCall[R <: ir.ModuleEntry](ref: ir.Ref[R], args: Seq[ir.Arg], neg: Boolean)(using Fixed): Unit =
    ref.target match
      case Some(r: ir.Relation) => evalCallInternal(r, r.params, args, neg)(evalRelation(_))
      case Some(r: ir.ExtensionalRelation) => evalCallInternal(r, r.params, args, neg)(evalExtensionalRelation(_))
      case _ => failure(RefNotFound, s"Can not find call reference $ref")

  def evalAtomOpen(at: ir.Atom)(using Fixed): Unit = at match
    case ir.Eq(lhs, rhs, neg) => evalEq(lhs, rhs, neg)
    case ir.Call(ref, args, neg) => evalCall(ref, args, neg)
    case ir.ExtensionalCall(ref, args, neg) => evalCall(ref, args, neg)
    case _ => failure(UnknownAtom, s"Unknown atom $at")

  final def evalTerm(term: ir.Term)(using rec: Fixed): Seq[V] = rec(FixIn.Term(term)) match
    case FixOut.Term(v) => v
    case _ => throw new IllegalStateException()

  def evalTermOpen(term: ir.Term)(using Fixed): Seq[V] = term match
    case ir.Var(ref) if term.mode.isBound =>
      val currentScope = supplementaryEnv.getState
      val columnName = ref.name.name
      val varEntry = relationOps.project(currentScope, Seq(columnName))
      // we only have a single value per row, since we projected a single column
      val values = relationOps.entries(varEntry).iterator.map(_.head)
      values.toSeq
    case ir.Var(ref) => failure(UnresolvedVariable, s"Unbound variable $ref")
    case ir.Cast(t, _) => evalTerm(t)
    case _ => failure(UnknownTerm, s"Unknown term $term")