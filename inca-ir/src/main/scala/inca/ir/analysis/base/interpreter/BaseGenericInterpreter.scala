package inca.ir.analysis.base.interpreter

import inca.ir
import inca.ir.ModuleEntry
import inca.ir.analysis.base.effect.{AtomFailed, BaseIRException, BodyFailed, InvalidBindings, NoParamRelation, ProgramFailure, RefNotFound, RelationFailed, UnknownArg, UnknownAtom, UnknownTerm, UnresolvedVariable}
import inca.ir.analysis.{RelationOps, SupplementaryEnvironment}
import inca.ir.extension.impure.MainHint
import inca.ir.typing.Mode
import sturdy.data.{MayJoin, mapJoin}
import sturdy.effect.{EffectList, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.effect.store.{CStore, Store}
import sturdy.fix.Fixpoint
import sturdy.values.*
import sturdy.values.booleans.{BooleanBranching, BooleanOps}
import sturdy.values.ordering.EqOps
import sturdy.values.references.AllocationSiteAddr
import sturdy.effect.except.Except
import sturdy.data.MakeJoined
import sturdy.data.MayJoin.WithJoin

// TODO:
//  1. Sturdy except when an atom or a body fails
//  2. Make Context-Sensitive + Insensitive configurable
//  3. Concrete Interpreter (data + arith + string + agg?)
//  4. Abstract Interpreter - Constant Analysis (data + arith + string + agg?)
//  5. Logger to annotate information
//  6. Optimize program

enum FixIn:
  //case Term(term: ir.Term)
  //case Atom(atom: ir.Atom)
  // TODO: Rename enter Relation
  //case EnterCall[R <: ModuleEntry](r: R, params: Seq[ir.Param], args: Seq[ir.Arg], neg: Boolean)
  //case Body(body: ir.Body)
  // TODO: Remove this
  case Relation(rel: ir.Relation)
  //case ExtensionalRelation(rel: ir.ExtensionalRelation)
  // TODO: Remove it
  //case Module(mod: ir.Module)

  override def toString: String = this match
    //case FixIn.Term(t) => t.toString
    //case FixIn.Atom(a) => a.toString
    /*case FixIn.EnterCall(r, _, args, neg) =>
      val s = r.name.name + args.mkString("(", ", ", ")")
      if (neg) s"~$s" else s*/
    //case FixIn.Body(b) => s"Body: ${b.hashCode()}" //b.toString
    case FixIn.Relation(rel: ir.Relation) => rel.name.name //rel.toString
    //case FixIn.ExtensionalRelation(rel: ir.ExtensionalRelation) => rel.toString
    //case FixIn.Module(mod: ir.Module) => mod.name.name //mod.toString

enum FixOut[V, RV]:
  case Term(values: RV)
  case Atom()
  case ExitCall(value: RV)
  case Body(value: RV)
  case Relation(value: RV)
  case ExtensionalRelation(value: RV)
  case Module()

given FiniteFixIn: Finite[FixIn] with {}


trait BaseGenericInterpreter[V, B, RV, J[_] <: MayJoin[?]]: // ExcV
  val RESULT_COLUMN: String = "result"

  // Fixpoint
  def fixpoint: EffectStack ?=> Fixpoint[FixIn, FixOut[V, RV]]

  type Fixed = FixIn => FixOut[V, RV]

  // Ops & Helper
  def relationOps: RelationOps[V, B, RV]

  //def branchOps: BooleanBranching[B, Unit]

  def boolOps: BooleanOps[B]

  def boolTrue: B = boolOps.boolLit(true)

  def boolFalse: B = boolOps.boolLit(false)

  //def boolTop: B

  def eqOps: EqOps[V, B]

  def failure: Failure

  //def except: Except[BaseIRException, ExcV, J]

  def joinV: J[V]

  //def top: V

  implicit def joinRV: Join[RV]

  //given Join[RV] = joinRV

  def effects: EffectStack = new EffectStack(EffectList(supplementaryTable, failure, idb), {
    case _: FixIn.Relation => EffectList(supplementaryTable, failure, idb)
    case _ => EffectList(supplementaryTable, failure, idb)
  }, {
    case _: FixIn.Relation => EffectList(failure, idb)
    case _ => EffectList(failure, idb)
  })
  given EffectStack = effects

  def idb: Store[AllocationSiteAddr, RV, WithJoin]

  def supplementaryTable: SupplementaryEnvironment[RV, J]

  implicit def joinUnit: J[Unit]

  //given J[Unit] = joinUnit

  // Evaluation
  private lazy val fixed: Fixed = fixpoint(using effects) {
    //case FixIn.Term(term) => FixOut.Term(evalTerm(term))
    //case FixIn.EnterCall(r, params, args, neg) => FixOut.ExitCall(evalCall(r, params, args, neg))
    //case FixIn.Atom(atom) => evalAtomOpen(atom); FixOut.Atom()
    //case FixIn.Body(body) => FixOut.Body(evalBodyOpen(body))
    case FixIn.Relation(rel) => FixOut.Relation(enterRelationOpen(rel))
    //case FixIn.ExtensionalRelation(rel) => FixOut.ExtensionalRelation(evalExtensionalRelationOpen(rel))
    // TODO: Do we need to run this in a fixpoint as well?
    //case FixIn.Module(mod) => evalModuleOpen(mod); FixOut.Module()
  }

  private inline def external[A](f: Fixed ?=> A): A = f(using fixed)

  def evalProgram(p: Seq[ir.Module]): Unit = external(p.foreach(evalModule))

  /*def evalModule(m: ir.Module)(using rec: Fixed): Unit = rec(FixIn.Module(m)) match
    case FixOut.Module() =>
    case _ => throw new IllegalStateException()*/

  def evalModule(m: ir.Module)(using Fixed): Unit = supplementaryTable.scoped {
    val relEntryPoints = m.relations.values.filter(_.hasHint(MainHint)) match
      case mainRels if mainRels.nonEmpty => mainRels
      case _ => m.relations.values
    relEntryPoints.foreach(evalRelation(_))
  }

  private def merge(lhs: RV, rhs: RV, neg: Boolean): RV =
    val res = if (neg) {
      relationOps.antiJoin(lhs, rhs)
    } else {
      relationOps.naturalJoin(lhs, rhs)
    }
    res

  protected def mergeIntoEnv(rv: RV, neg: Boolean): Unit =
    val merged = merge(supplementaryTable.getState, rv, neg)
    supplementaryTable.setState(merged)

  protected def insertIDB(name: ir.Name, rv: RV): Unit =
    idb.write(AllocationSiteAddr.Variable(name.name)(true), rv)

  inline def evalRelation(r: ir.Relation)(using rec: Fixed): RV = rec(FixIn.Relation(r)) match
    case FixOut.Relation(p) => p
    case _ => throw new IllegalStateException()

  def evalRelationOpen(r: ir.Relation)(using Fixed): RV = supplementaryTable.scoped {
    val paramNames = r.params.map(p => p.name.name)

    //var bodyRes: CRV = ??? // empty table with param names as columns
    //var isEmpty: Boolean = ???
    mapJoin(r.bodies, { b =>
      val res = relationOps.project(evalBody(b), paramNames)
      // TODO: Do we need to check if it is not already in the idb?
      insertIDB(r.name, res)
      res

      /*try {
        val res = relationOps.project(evalBody(b), paramNames)
        // Important, put that in the try block
        insertIDB(r.name, res)
        bodyRes = relationOps.union(bodyRes, res)
      } catch {
        case _: BodyFailed => // nothing
      }*/
    })

    //if (isEmpty)
      // except.throws....
    //  throw RelationFailed
  }

  /*inline def evalExtensionalRelation(r: ir.ExtensionalRelation)(using rec: Fixed): RV = rec(FixIn.ExtensionalRelation(r)) match
    case FixOut.ExtensionalRelation(p) => p
    case _ => throw new IllegalStateException()*/

  def evalExtensionalRelation(r: ir.ExtensionalRelation)(using Fixed): RV = supplementaryTable.scoped {
    ???
  }

  /*inline def evalBody(b: ir.Body)(using rec: Fixed): RV = rec(FixIn.Body(b)) match
    case FixOut.Body(rv) => rv
    case _ => throw new IllegalStateException()*/

  def evalBody(b: ir.Body)(using rec: Fixed): RV = supplementaryTable.scoped {
    //except.tryCatch(
    b.atoms.foreach(a => evalAtom(a))
    /*) {
      case AtomFailed(msg) => except.throws(BodyFailed(s"Body failed: $b"))
      case _ => ???
    }*/
    supplementaryTable.getState
  }

  /*inline def evalAtom(at: ir.Atom)(using rec: Fixed): Unit = rec(FixIn.Atom(at)) match
    case FixOut.Atom() => ()
    case _ => throw new IllegalStateException()

  def evalCall[R <: ModuleEntry](r: R, params: Seq[ir.Param], args: Seq[ir.Arg], neg: Boolean)(using rec: Fixed): RV = rec(FixIn.EnterCall(r, params, args, neg)) match
    case FixOut.ExitCall(rv) => rv
    case _ => throw new IllegalStateException()*/

  def enterRelationOpen(rel: ir.Relation)(using rec: Fixed): RV =
    evalRelationOpen(rel)

  // I don't think that anything else can be binding in an equality. But if so, subclasses may override this
  def extractVarName(term: ir.Term): ir.Name = term match
    case ir.Var(ref) => ref.name
    case ir.Cast(t, _) => extractVarName(t)

  inline private final def evalAssign(to: ir.Term, from: ir.Term)(using Fixed): RV =
    relationOps.rename(evalTerm(from), Map(RESULT_COLUMN -> extractVarName(to).name))

  inline final def cartesian(rv1: RV, rv2: RV): RV =
    val ls = relationOps.rename(rv1, Map(RESULT_COLUMN -> "lhs"))
    val rs = relationOps.rename(rv2, Map(RESULT_COLUMN -> "rhs"))
    relationOps.cartesian(ls, rs)

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
    /*if (relationOps.entries(comparisonResults).iterator.isEmpty) {
      val op = if neg then "!=" else "=="
      //except.throws(AtomFailed(s"Comparison $lhs $op $rhs always fails"))
    }*/

    // TODO: Use branch ops
    /*branchOps.boolBranch(relationOps.isEmpty(comparisonResults)){
      // TODO: throw exception
    } {
      // nothing
    }*/

  inline private final def evalEq(lhs: ir.Term, rhs: ir.Term, neg: Boolean)(using Fixed): Unit = (lhs.mode, rhs.mode, neg) match
    case (Mode.Binding, Mode.Binding, _) => failure(InvalidBindings, s"Equality between two binding terms: $lhs and $rhs")
    case (Mode.Binding, _, false) => mergeIntoEnv(evalAssign(lhs, rhs), false)
    case (_, Mode.Binding, false) => mergeIntoEnv(evalAssign(rhs, lhs), false)
    case (Mode.Bound, Mode.Bound, _) => evalCompare(lhs, rhs, neg)
    case (m1, m2, _) => failure(InvalidBindings, s"Can not evaluate equality with modes: $m1 <> $m2 and negation: $neg")

  def evalArg(arg: ir.Arg)(using Fixed): RV = arg match
    case ir.TermArg(t) if t.mode.isBound => evalTerm(t)
    case ir.TermArg(t) => relationOps.unit
    case ir.WildcardArg() => relationOps.unit
    case _ => failure(UnknownArg, s"Unknown arg $arg")

  def extractVarName(arg: ir.Arg): Option[ir.Name] = arg match
    case ir.TermArg(t) => Some(extractVarName(t))
    case ir.WildcardArg() => None

  private final def evalCall[R <: ModuleEntry](r: R, params: Seq[ir.Param], args: Seq[ir.Arg], neg: Boolean)(using Fixed): Unit =
    if (params.isEmpty) {
      // Relation with no parameters... This should not happen, even though viatra supports it
      failure(NoParamRelation, s"Relation ${r.name} has no Parameters!")
    } else {
      // eval arguments in current scope
      val argRes = params.zip(args).map { case (p, a) =>
        relationOps.rename(evalArg(a), Map(RESULT_COLUMN -> p.name.name))
      }

      // eval the actual call in a new scoped environment
      val res = supplementaryTable.freshScoped {
        // since we have at least one parameter argRV is defined
        val evalContext = argRes.foldLeft(argRes.head)((acc, rv) => relationOps.naturalJoin(acc, rv))
        supplementaryTable.setState(evalContext)

        val relRes = r match
          case rel: ir.Relation => evalRelation(rel)
          case extRel: ir.ExtensionalRelation => evalExtensionalRelation(extRel)
        // add all variables bound by the call to the context
        val boundVars = args.map(extractVarName)
        val subst = boundVars.zip(params).flatMap {
          case (Some(varName), p) => Some((p.name.name, varName.name))
          case _ => None
        }.toMap
        // TODO: natJoin with value from IDB?
        relationOps.projectAndRename(relRes, subst)
      }

      // merge all variables that where bound
      mergeIntoEnv(res, neg)
    }

  def evalAtom(at: ir.Atom)(using Fixed): Unit = at match
    case ir.Eq(lhs, rhs, neg) => evalEq(lhs, rhs, neg)
    case ir.Call(ref, args, neg) => ref.target match
      case Some(r: ir.Relation) => evalCall(r, r.params, args, neg)
      case _ => failure(RefNotFound, s"Can not find call reference $ref")
    case ir.ExtensionalCall(ref, args, neg) => ref.target match
      case Some(r: ir.ExtensionalRelation) => evalCall(r, r.params, args, neg)
      case _ => failure(RefNotFound, s"Can not find extensional call reference $ref")
    case _ => failure(UnknownAtom, s"Unknown atom $at")

  /*final def evalTerm(term: ir.Term)(using rec: Fixed): Seq[V] = rec(FixIn.Term(term)) match
    case FixOut.Term(v) => v
    case _ => throw new IllegalStateException()*/

  def evalTerm(term: ir.Term)(using Fixed): RV = term match
    case ir.Var(ref) if term.mode.isBound =>
      relationOps.projectAndRename(supplementaryTable.getState, Map(ref.name.name -> RESULT_COLUMN))
    case ir.Var(ref) =>
      failure(UnresolvedVariable, s"Unbound variable $ref")
    case ir.Cast(t, _) =>
      evalTerm(t)
    case _ =>
      failure(UnknownTerm, s"Unknown term $term")