package inca.ir.analysis.base.interpreter

import inca.ir
import inca.ir.ModuleEntry
import inca.ir.analysis.base.effect.{AtomFailed, BaseIRException, InvalidBindings, MergeFailed, NoParamRelation, ProgramFailure, RefNotFound, RelationFailed, UnknownArg, UnknownAtom, UnknownTerm, UnresolvedVariable}
import inca.ir.analysis.{RelationOps, SupplementaryTable}
import inca.ir.extension.impure.MainHint
import inca.ir.typing.Mode
import sturdy.data.{MayJoin, mapJoin}
import sturdy.effect.{EffectList, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.effect.store.Store
import sturdy.fix.Fixpoint
import sturdy.values.*
import sturdy.values.booleans.{BooleanBranching, BooleanOps}
import sturdy.values.ordering.EqOps
import sturdy.values.references.AllocationSiteAddr
import sturdy.effect.except.Except
import sturdy.data.MakeJoined
import sturdy.data.MayJoin.WithJoin
import sturdy.values.references.AllocationSiteAddr.Variable

// TODO:
//  1. Sturdy except when an atom or a body fails
//  2. Make Context-Sensitive + Insensitive configurable
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

  override def toString: String = this match
    case FixIn.Term(t) => t.toString
    case FixIn.Atom(a) => a.toString
    case FixIn.Body(b) => s"Body: ${b.hashCode()}" //b.toString
    case FixIn.Relation(rel: ir.Relation) => rel.name.name //rel.toString
    case FixIn.ExtensionalRelation(rel: ir.ExtensionalRelation) => rel.toString

enum FixOut[V, RV]:
  case Term(values: RV)
  case Atom()
  case ExitCall(value: RV)
  case Body(value: RV)
  case Relation(value: RV)
  case ExtensionalRelation(value: RV)

given FiniteFixIn: Finite[FixIn] with {}


trait BaseGenericInterpreter[V, B, RV,  ExcV, J[_] <: MayJoin[?]]:
  val RESULT_COLUMN: String = "result"

  // Fixpoint
  def fixpoint: EffectStack ?=> Fixpoint[FixIn, FixOut[V, RV]]

  type Fixed = FixIn => FixOut[V, RV]

  // Ops & Helper
  val relationOps: RelationOps[V, B, RV]

  val boolOps: BooleanOps[B]

  lazy val boolTrue: B = boolOps.boolLit(true)

  lazy val boolFalse: B = boolOps.boolLit(false)

  val branchOps: BooleanBranching[B, Unit]

  val eqOps: EqOps[V, B]

  val failure: Failure

  val except: Except[BaseIRException, ExcV, WithJoin]

  val joinV: J[V]

  implicit val joinRV: Join[RV]

  def effects: EffectStack = new EffectStack(EffectList(supplementaryTable, failure, except, idb), {
    case _: FixIn.Relation => EffectList(except, failure, supplementaryTable, idb) //EffectList(supplementaryTable, failure, idb)
  }, {
    case _: FixIn.Relation => EffectList(except, failure, idb) //supplementaryTable
  })

  // Workaround 1.
  /*def effects: EffectStack = new EffectStack(EffectList(failure, idb), {
    case _: FixIn.Relation => EffectList(failure, idb)
    case _ => EffectList(failure, idb)
  }, {
    case _: FixIn.Relation => EffectList(failure, idb)
    case _ => EffectList(failure, idb)
  })*/
  given EffectStack = effects

  def idb: Store[AllocationSiteAddr, RV, WithJoin]

  def supplementaryTable: SupplementaryTable[RV, J]

  implicit def joinUnit: J[Unit]

  // Evaluation
  private lazy val fixed: Fixed = fixpoint(using effects) {
    case FixIn.Term(term) => FixOut.Term(evalTermOpen(term))
    case FixIn.Atom(atom) => evalAtomOpen(atom); FixOut.Atom()
    case FixIn.Body(body) => FixOut.Body(evalBodyOpen(body))
    case FixIn.Relation(rel) => FixOut.Relation(enterRelationOpen(rel))
    case FixIn.ExtensionalRelation(rel) => FixOut.ExtensionalRelation(evalExtensionalRelationOpen(rel))
  }

  private inline def external[A](f: Fixed ?=> A): A = f(using fixed)

  def evalProgram(p: Seq[ir.Module]): Unit = external(p.foreach(evalModule))

  def entryPoints(m: ir.Module): Iterable[ir.Relation] = //m.relations.values
    m.relations.values.filter(_.hasHint(MainHint)) match
      case mainRels if mainRels.nonEmpty => mainRels
      case _ => m.relations.values

  def evalModule(m: ir.Module)(using Fixed): Unit = supplementaryTable.scoped {
    entryPoints(m).foreach { rel =>
      val relRes = except.tryCatch {
        evalRelation(rel)
      } { case RelationFailed(msg) =>
        relationOps.make(rel.params.map(_.name.name), Seq())
      }
      // Workaround 1.
      //insertIDB(rel.name, relRes)
    }
  }

  private def merge(lhs: RV, rhs: RV, neg: Boolean): RV =
    val res = if (neg) {
      relationOps.antiJoin(lhs, rhs)
    } else {
      relationOps.naturalJoin(lhs, rhs)
    }

    // Anti join might produce empty table
    if (relationOps.isEmpty(res) == boolTrue)
      except.throws(MergeFailed("Merged empty table"))
    else
      res

  protected def mergeIntoEnv(rv: RV, neg: Boolean): Unit =
    val merged = merge(supplementaryTable.getTable, rv, neg)
    supplementaryTable.setTable(merged)

  protected def insertIDB(name: ir.Name, rv: RV): Unit =
    idb.write(AllocationSiteAddr.Variable(name.name)(true), rv)

  inline def evalRelation(r: ir.Relation)(using rec: Fixed): RV = rec(FixIn.Relation(r)) match
    case FixOut.Relation(p) => p
    case _ => throw new IllegalStateException()

  def evalRelationOpen(r: ir.Relation)(using Fixed): RV = supplementaryTable.scoped {
    val paramNames = r.params.map(p => p.name.name)

    var bodyRes = relationOps.unit
    var allBodiesFailed: Boolean = true
    val relRes = mapJoin(r.bodies, { b =>
      except.tryCatch {
        val res = relationOps.project(evalBody(b), paramNames)
        bodyRes = relationOps.union(bodyRes, res)
        allBodiesFailed = false
        bodyRes
      } { exc =>
        bodyRes
      }
    })

    if (allBodiesFailed)
      except.throws(RelationFailed(s"Relation ${r.name} failed"))
    else
      insertIDB(r.name, relRes)
      relRes
  }

  inline def evalExtensionalRelation(r: ir.ExtensionalRelation)(using rec: Fixed): RV = rec(FixIn.ExtensionalRelation(r)) match
    case FixOut.ExtensionalRelation(p) => p
    case _ => throw new IllegalStateException()

  def evalExtensionalRelationOpen(r: ir.ExtensionalRelation)(using Fixed): RV = supplementaryTable.scoped {
    ???
  }

  inline def evalBody(b: ir.Body)(using rec: Fixed): RV = rec(FixIn.Body(b)) match
    case FixOut.Body(rv) => rv
    case _ => throw new IllegalStateException()

  def evalBodyOpen(b: ir.Body)(using rec: Fixed): RV = supplementaryTable.scoped {
    b.atoms.foreach(a => evalAtom(a))
    supplementaryTable.getTable
  }

  inline def evalAtom(at: ir.Atom)(using rec: Fixed): Unit = rec(FixIn.Atom(at)) match
    case FixOut.Atom() => ()
    case _ => throw new IllegalStateException()

  def enterRelationOpen(rel: ir.Relation)(using rec: Fixed): RV =
    evalRelationOpen(rel)

  // I don't think that anything else can be binding in an equality. But if so, subclasses may override this
  def extractVarName(term: ir.Term): ir.Name = term match
    case ir.Var(ref) => ref.name
    case ir.Cast(t, _) => extractVarName(t)

  private final def evalAssign(to: ir.Term, from: ir.Term)(using Fixed): RV =
    relationOps.rename(evalTerm(from), Map(RESULT_COLUMN -> extractVarName(to).name))

  final def cartesian(rv1: RV, rv2: RV): RV =
    val ls = relationOps.rename(rv1, Map(RESULT_COLUMN -> "lhs"))
    val rs = relationOps.rename(rv2, Map(RESULT_COLUMN -> "rhs"))
    relationOps.cartesian(ls, rs)

  private final def evalCompare(lhs: ir.Term, rhs: ir.Term, neg: Boolean)(using Fixed): Unit =
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

    branchOps.boolBranch(relationOps.isEmpty(comparisonResults)){
      val op = if neg then "!=" else "=="
      except.throws(AtomFailed(s"Comparison $lhs $op $rhs always fails"))
    } { /* nothing */ }

  private final def evalEq(lhs: ir.Term, rhs: ir.Term, neg: Boolean)(using Fixed): Unit = (lhs.mode, rhs.mode, neg) match
    case (Mode.Binding, Mode.Binding, _) => failure(InvalidBindings, s"Equality between two binding terms: $lhs and $rhs")
    case (Mode.Binding, _, false) => mergeIntoEnv(evalAssign(lhs, rhs), false)
    case (_, Mode.Binding, false) => mergeIntoEnv(evalAssign(rhs, lhs), false)
    case (Mode.Bound, Mode.Bound, _) => evalCompare(lhs, rhs, neg)
    case (m1, m2, _) => failure(InvalidBindings, s"Can not evaluate equality with modes: $m1 <> $m2 and negation: $neg")

  def evalArg(arg: ir.Arg)(using Fixed): RV = arg match
    case ir.TermArg(t) =>
      // only proceed if all vars of an argument are found in the supplementary table otherwise the term might be
      // binding, and we return unit.
      // Note that the binding information will not align with the type information, since it changes over time.
      val sup = supplementaryTable.getTable
      if (t.vars.forall(v => relationOps.hasColumn(sup, v.name.name) == boolTrue))
        evalTerm(t)
      else
        relationOps.unit
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
        supplementaryTable.setTable(evalContext)

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
      }

      // merge all variables that where bound
      mergeIntoEnv(res, neg)
    }

  def evalAtomOpen(at: ir.Atom)(using Fixed): Unit = at match
    case ir.Eq(lhs, rhs, neg) => evalEq(lhs, rhs, neg)
    case ir.Call(ref, args, neg) => ref.target match
      case Some(r: ir.Relation) => evalCall(r, r.params, args, neg)
      case _ => failure(RefNotFound, s"Can not find call reference $ref")
    case ir.ExtensionalCall(ref, args, neg) => ref.target match
      case Some(r: ir.ExtensionalRelation) => evalCall(r, r.params, args, neg)
      case _ => failure(RefNotFound, s"Can not find extensional call reference $ref")
    case _ => failure(UnknownAtom, s"Unknown atom $at")

  inline final def evalTerm(term: ir.Term)(using rec: Fixed): RV = rec(FixIn.Term(term)) match
    case FixOut.Term(v) => v
    case _ => throw new IllegalStateException()

  def evalTermOpen(term: ir.Term)(using Fixed): RV = term match
    case ir.Var(ref) if relationOps.hasColumn(supplementaryTable.getTable, ref.name.name) == boolTrue =>
      relationOps.projectAndRename(supplementaryTable.getTable, Map(ref.name.name -> RESULT_COLUMN))
    case ir.Var(ref) =>
      failure(UnresolvedVariable, s"Unbound variable ${ref.name.name}")
    case ir.Cast(t, _) =>
      evalTerm(t)
    case _ =>
      failure(UnknownTerm, s"Unknown term $term")