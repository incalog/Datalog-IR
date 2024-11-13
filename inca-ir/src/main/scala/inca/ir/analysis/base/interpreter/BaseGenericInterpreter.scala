package inca.ir.analysis.base.interpreter

import inca.ir
import inca.ir.ModuleEntry
import inca.ir.analysis.base.effect.{AtomFailed, BaseIRException, BodyFailed, InvalidBindings, NoParamRelation, ProgramFailure, RefNotFound, RelationFailed, UnknownArg, UnknownAtom, UnknownTerm, UnresolvedVariable}
import inca.ir.analysis.{RelationOps, SupplementaryEnvironment}
import inca.ir.extension.impure.MainHint
import inca.ir.typing.Mode
import sturdy.data.MayJoin
import sturdy.effect.EffectStack
import sturdy.effect.failure.Failure
import sturdy.effect.store.Store
import sturdy.fix.Fixpoint
import sturdy.values._
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.EqOps
import sturdy.values.references.AllocationSiteAddr
import sturdy.effect.except.Except

// TODO:
//  1. Sturdy except instead of empty table
//  2. Make Context-Sensitive + Insensitive configurable
//  3. Concrete Interpreter (data + arith + string + agg?)
//  4. Abstract Interpreter - Constant Analysis (data + arith + string + agg?)
//  5. Logger to annotate information
//  6. Optimize program

enum FixIn:
  case Term(term: ir.Term)
  case Atom(atom: ir.Atom)
  case EnterCall[R <: ModuleEntry](r: R, params: Seq[ir.Param], args: Seq[ir.Arg], neg: Boolean)
  case Body(body: ir.Body)
  case Relation(rel: ir.Relation)
  case ExtensionalRelation(rel: ir.ExtensionalRelation)
  case Module(mod: ir.Module)

  override def toString: String = this match
    case FixIn.Term(t) => t.toString
    case FixIn.Atom(a) => a.toString
    case FixIn.EnterCall(r, _, args, neg) =>
      val s = r.name.name + args.mkString("(", ", ", ")")
      if (neg) s"~$s" else s
    case FixIn.Body(b) => b.toString
    case FixIn.Relation(rel: ir.Relation) => rel.toString
    case FixIn.ExtensionalRelation(rel: ir.ExtensionalRelation) => rel.toString
    case FixIn.Module(mod: ir.Module) => mod.toString

enum FixOut[V, RV]:
  case Term(values: Seq[V])
  case Atom()
  case ExitCall(value: RV)
  case Body(value: RV)
  case Relation(value: RV)
  case ExtensionalRelation(value: RV)
  case Module(mod: Map[String, RV])

given FiniteFixIn: Finite[FixIn] with {}

given FiniteFixOut[V, RV]: Finite[FixOut[V, RV]] with {}

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
    case (FixOut.Atom(), FixOut.Atom()) => Unchanged(FixOut.Atom())
    case (FixOut.ExitCall(rv1), FixOut.ExitCall(rv2)) => combineRV(rv1, rv2).map(FixOut.ExitCall.apply)
    case (FixOut.Body(rv1), FixOut.Body(rv2)) => combineRV(rv1, rv2).map(FixOut.Body.apply)
    case (FixOut.Relation(rv1), FixOut.Relation(rv2)) => combineRV(rv1, rv2).map(FixOut.Relation.apply)
    case (FixOut.ExtensionalRelation(rv1), FixOut.ExtensionalRelation(rv2)) => combineRV(rv1, rv2).map(FixOut.ExtensionalRelation.apply)
    case (FixOut.Module(idb1), FixOut.Module(idb2)) =>
      val allKeys = idb1.keys ++ idb2.keys
      val res = for (k <- allKeys) yield
        (idb1.get(k), idb2.get(k)) match
          case (Some(rv1), Some(rv2)) => k -> combineRV(rv1, rv2).get
          case (Some(rv1), _) => k -> rv1
          case (_, Some(rv2)) => k -> rv2
          case _ => throw IllegalStateException(s"IDB key not found: $k")
      val idb = res.toMap
      if (idb != idb1) {
        Changed(FixOut.Module(idb))
      } else {
        Unchanged(FixOut.Module(idb))
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

  //def except: Except[BaseIRException, BaseIRException, J]

  def joinV: J[V]

  def top: V

  def joinRV: J[RV]

  given J[RV] = joinRV

  def effects: EffectStack

  given EffectStack = effects

  def IDB: Store[AllocationSiteAddr, RV, J]

  def supplementaryEnv: SupplementaryEnvironment[RV, J]

  def joinUnit: J[Unit]

  given J[Unit] = joinUnit

  // Evaluation
  private lazy val fixed: Fixed = fixpoint(using effects) {
    case FixIn.Term(term) => FixOut.Term(evalTermOpen(term))
    case FixIn.EnterCall(r, params, args, neg) => FixOut.ExitCall(exitCall(r, params, args, neg))
    case FixIn.Atom(atom) => evalAtomOpen(atom); FixOut.Atom()
    case FixIn.Body(body) => FixOut.Body(evalBodyOpen(body))
    case FixIn.Relation(rel) => FixOut.Relation(evalRelationOpen(rel))
    case FixIn.ExtensionalRelation(rel) => FixOut.ExtensionalRelation(evalExtensionalRelationOpen(rel))
    // TODO: Let this run in a fixpoint. Is this necessary?
    case FixIn.Module(mod) => FixOut.Module(evalModuleOpen(mod))
  }

  private inline def external[A](f: Fixed ?=> A): A = f(using fixed)

  def evalProgram(p: Seq[ir.Module]): Unit = external(p.foreach(evalModule))

  def evalModule(m: ir.Module)(using rec: Fixed): Map[String, RV] = rec(FixIn.Module(m)) match
    case FixOut.Module(idb) => idb
    case _ => throw new IllegalStateException()

  def evalModuleOpen(m: ir.Module)(using Fixed): Map[String, RV] = supplementaryEnv.scoped {
    val relEntryPoints = m.relations.values.filter(_.hasHint(MainHint)) match
      case mainRels if mainRels.nonEmpty => mainRels
      case _ => m.relations.values
    relEntryPoints.map(r => r.name.name -> evalRelation(r)).toMap
  }

  private def merge(lhs: RV, rhs: RV, neg: Boolean): RV =
    val res = if (neg) {
      relationOps.antiJoin(lhs, rhs)
    } else {
      relationOps.naturalJoin(lhs, rhs)
    }
    res

  protected def mergeIntoEnv(rv: RV, neg: Boolean): Unit =
    val merged = merge(supplementaryEnv.getState, rv, neg)
    supplementaryEnv.setState(merged)

  protected def insertIDB(name: ir.Name, rv: RV): Unit =
    val addr = AllocationSiteAddr.Variable(name.name)(true)
    val oldRVOption = IDB.read(addr)
    val result = oldRVOption.option(rv)(oldRV => relationOps.union(rv, oldRV))
    IDB.free(addr)
    IDB.write(addr, result)

  inline def evalRelation(r: ir.Relation)(using rec: Fixed): RV = rec(FixIn.Relation(r)) match
    case FixOut.Relation(p) => p
    case _ => throw new IllegalStateException()

  def evalRelationOpen(r: ir.Relation)(using Fixed): RV = supplementaryEnv.scoped {
    val paramNames = r.params.map(p => p.name.name)
    supplementaryEnv.setState(relationOps.make(paramNames, Seq(Seq())))
    val bodyRes = r.bodies.map { b =>
      // TODO: If all bodies fail, the relation failed, aka we produce an empty RV
      /*val res = except.tryCatch(evalBody(b)) {
        case _: BodyFailed => except.throws(RelationFailed(s"Relation $r failed"))
      }*/
      val res = evalBody(b)
      relationOps.project(res, paramNames)
    }

    // we have at least one body res, otherwise we have thrown an exception
    val relRes = bodyRes.tail.foldLeft(bodyRes.head) {
      case (acc, rv) => relationOps.union(acc, rv)
    }

    insertIDB(r.name, relRes)
    relRes
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

  def evalBodyOpen(b: ir.Body)(using rec: Fixed): RV = supplementaryEnv.scoped {
    //except.tryCatch(
    b.atoms.foreach(a => evalAtom(a))
    /*) {
      case AtomFailed(msg) => except.throws(BodyFailed(s"Body failed: $b"))
      case _ => ???
    }*/
    supplementaryEnv.getState
  }

  inline def evalAtom(at: ir.Atom)(using rec: Fixed): Unit = rec(FixIn.Atom(at)) match
    case FixOut.Atom() => ()
    case _ => throw new IllegalStateException()

  inline def enterCall[R <: ModuleEntry](r: R, params: Seq[ir.Param], args: Seq[ir.Arg], neg: Boolean)(using rec: Fixed): RV = rec(FixIn.EnterCall(r, params, args, neg)) match
    case FixOut.ExitCall(rv) => rv
    case _ => throw new IllegalStateException()

  inline def exitCall[R <: ModuleEntry](r: R, params: Seq[ir.Param], args: Seq[ir.Arg], neg: Boolean)(using rec: Fixed): RV =
    val relRes = r match
      case rel: ir.Relation => evalRelation(rel)
      case extRel: ir.ExtensionalRelation => evalExtensionalRelation(extRel)
    // add all variables bound by the call to the context
    val boundVars = args.map(extractVarName)
    val subst = boundVars.zip(params).flatMap {
      case (Some(varName), p) => Some((p.name.name, varName.name))
      case _ => None
    }.toMap
    relationOps.projectAndRename(relRes, subst)

  // I don't think that anything else can be binding in an equality. But if so, subclasses may override this
  def extractVarName(term: ir.Term): ir.Name = term match
    case ir.Var(ref) => ref.name
    case ir.Cast(t, _) => extractVarName(t)

  inline private final def evalAssign(to: ir.Term, from: ir.Term)(using Fixed): Unit =
    //evalTerm(to)
    val fs = evalTerm(from)
    val assignedName = extractVarName(to).name
    val assignedValues = fs.map(v => Seq(v))
    val res = relationOps.make(Seq(assignedName), assignedValues)
    mergeIntoEnv(res, false)

  inline final def cartesian(v1: Seq[V], v2: Seq[V]): RV =
    val lsRv = relationOps.make(Seq(), v1.map(v => Seq(v)))
    val rsRv = relationOps.make(Seq(), v2.map(v => Seq(v)))
    relationOps.cartesian(lsRv, rsRv)

  inline private final def evalCompare(lhs: ir.Term, rhs: ir.Term, neg: Boolean)(using Fixed): Unit =
    val ls = evalTerm(lhs)
    val rs = evalTerm(rhs)
    val combinations = cartesian(ls, rs)
    val comparisonResults = relationOps.filter(combinations) { case Seq(v1, v2) =>
      if (neg) {
        eqOps.neq(v1, v2)
      } else {
        eqOps.equ(v1, v2)
      }
    }
    // if all comparisons fail the atom failed
    if (relationOps.entries(comparisonResults).iterator.isEmpty) {
      val op = if neg then "!=" else "=="
      //except.throws(AtomFailed(s"Comparison $lhs $op $rhs always fails"))
    }

  inline private final def evalEq(lhs: ir.Term, rhs: ir.Term, neg: Boolean)(using Fixed): Unit = (lhs.mode, rhs.mode, neg) match
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

  inline private final def call[R <: ModuleEntry](r: R, params: Seq[ir.Param], args: Seq[ir.Arg], neg: Boolean)(using Fixed): RV =
    if (params.isEmpty) {
      // Relation with no parameters... This should not happen, even though viatra supports it
      failure(NoParamRelation, s"Relation ${r.name} has no Parameters!")
    } else {
      // eval arguments in current scope
      val paramNames = params.map(p => p.name.name)
      val argRes = args.map(evalArg)

      // eval the actual call in a new scoped environment
      supplementaryEnv.freshScoped {
        // TODO: Use something like callOps to differentiate between context-sensitive and insensitive?
        val argRV = paramNames.zip(argRes).map((p, a) => relationOps.make(Seq(p), a.map(v => Seq(v))))
        // since we have at least one parameter argRV is defined
        val evalContext = argRV.foldLeft(argRV.head)((acc, rv) => relationOps.naturalJoin(acc, rv))
        supplementaryEnv.setState(evalContext)
        enterCall(r, params, args, neg)
      }
    }

  def evalAtomOpen(at: ir.Atom)(using Fixed): Unit = at match
    case ir.Eq(lhs, rhs, neg) => evalEq(lhs, rhs, neg)
    case ir.Call(ref, args, neg) => ref.target match
      case Some(r: ir.Relation) => mergeIntoEnv(call(r, r.params, args, neg), neg)
      case _ => failure(RefNotFound, s"Can not find call reference $ref")
    case ir.ExtensionalCall(ref, args, neg) => ref.target match
      case Some(r: ir.ExtensionalRelation) => mergeIntoEnv(call(r, r.params, args, neg), neg)
      case _ => failure(RefNotFound, s"Can not find extensional call reference $ref")
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