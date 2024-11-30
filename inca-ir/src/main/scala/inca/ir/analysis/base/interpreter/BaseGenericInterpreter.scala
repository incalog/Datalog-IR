package inca.ir.analysis.base.interpreter

import inca.ir
import inca.ir.analysis.base.effect.*
import inca.ir.analysis.{RelationOps, SupplementaryTable}
import inca.ir.extension.impure.MainHint
import inca.ir.typing.Mode
import inca.ir.{ModuleEntry, TermType}
import inca.util.Gensym
import sturdy.data.MayJoin.WithJoin
import sturdy.data.{MakeJoined, MayJoin, mapJoin}
import sturdy.effect.except.Except
import sturdy.effect.failure.Failure
import sturdy.effect.store.Store
import sturdy.effect.{EffectList, EffectStack}
import sturdy.fix.Fixpoint
import sturdy.values.*
import sturdy.values.booleans.{BooleanBranching, BooleanOps}
import sturdy.values.ordering.EqOps
import sturdy.values.references.AllocationSiteAddr

// TODO:
//  1. Make Context-Sensitive + Insensitive configurable
//  2. Concrete Interpreter (data + arith + string + agg?)
//  3. Abstract Interpreter - Constant Analysis (data + arith + string + agg?)
//  4. Logger to annotate information
//  5. Optimize program

enum Adorn:
  case b
  case f

  override def toString: String = this match
    case Adorn.b => "b"
    case Adorn.f => "f"

case class Adornment(as: Seq[Adorn]):
  override def toString: String = as.mkString("")

enum FixIn:
  case Term(term: ir.Term)
  case Atom(atom: ir.Atom)
  case Body(body: ir.Body)
  case EnterRelation(rel: ir.Relation, adornment: Adornment)

  override def toString: String = this match
    case FixIn.Term(t) => t.toString
    case FixIn.Atom(a) => a.toString
    case FixIn.Body(b) => s"Body: ${b.hashCode()}" //b.toString
    case FixIn.EnterRelation(rel: ir.Relation, adornment: Adornment) => s"${rel.name.name}_$adornment" //rel.toString

type SupColumn = String

enum FixOut[V, RV]:
  case Term(col: SupColumn)
  case Atom()
  case ExitCall(value: RV)
  case Body(value: RV)
  case Relation(value: RV)

given FiniteFixIn: Finite[FixIn] with {}

given CCombineFixOut[V, RV, W <: Widening](using Combine[RV, W]): Combine[FixOut[V, RV], W] with
  override def apply(out1: FixOut[V, RV], out2: FixOut[V, RV]): MaybeChanged[FixOut[V, RV]] =
    (out1, out2) match
      case (FixOut.Term(rv1), FixOut.Term(rv2)) => assert(rv1 == rv2); MaybeChanged(FixOut.Term(rv1), out1)
      case (FixOut.Atom(), FixOut.Atom()) => Unchanged(FixOut.Atom())
      case (FixOut.ExitCall(rv1), FixOut.ExitCall(rv2)) => Combine(rv1, rv2).map(FixOut.ExitCall.apply)
      case (FixOut.Body(rv1), FixOut.Body(rv2)) => Combine(rv1, rv2).map(FixOut.Body.apply)
      case (FixOut.Relation(rv1), FixOut.Relation(rv2)) => Combine(rv1, rv2).map(FixOut.Relation.apply)
      case _ => throw new IllegalArgumentException(s"Cannot combine outputs of different kind, $out1 and $out2")


trait BaseGenericInterpreter[V, B, RV,  ExcV, J[_] <: MayJoin[?]]:
  // Fixpoint
  def fixpoint: EffectStack ?=> Fixpoint[FixIn, FixOut[V, RV]]

  type Fixed = FixIn => FixOut[V, RV]

  // Ops & Helper
  val relationOps: RelationOps[V, B, RV]

  val boolOps: BooleanOps[B]

  lazy val boolTrue: B = boolOps.boolLit(true)

  lazy val boolFalse: B = boolOps.boolLit(false)

  val branchOps: BooleanBranching[B, RV]

  lazy val eqOps: EqOps[V, B]

  lazy val failure: Failure

  lazy val except: Except[BaseIRException, ExcV, WithJoin]

  val joinV: J[V]

  private var edb: Map[String, RV] = Map()

  implicit val joinRV: Join[RV]

  val effects: EffectStack = new EffectStack(EffectList(supplementaryTable, failure, except, idb), {
    case _: FixIn.EnterRelation => EffectList(supplementaryTable, idb) //EffectList(supplementaryTable, failure, idb)
  }, {
    case _: FixIn.EnterRelation => EffectList(except, failure, idb) //supplementaryTable
  })

  given EffectStack = effects

  def idb: Store[AllocationSiteAddr, RV, WithJoin]

  def supplementaryTable: SupplementaryTable[RV]

  /** updates the supplementary table; ASSUMEs the new table is non-empty */
  inline def updateSupplementaryUnchecked(f: RV => RV): RV = supplementaryTable.update(f)

  /** updates the supplementary table; CHECKs the new table is non-empty */
  def updateSupplementaryChecked(f: RV => RV): RV =
    val rv = f(supplementaryTable.getTable)
    branchOps.boolBranch(relationOps.isEmpty(rv)) {
      except.throws(EmptySupplementary)
    } {
      supplementaryTable.setTable(rv)
      rv
    }

  implicit def joinUnit: J[Unit]

  // Evaluation
  private lazy val fixed: Fixed = fixpoint(using effects) {
    case FixIn.Term(term) => FixOut.Term(evalTermOpen(term))
    case FixIn.Atom(atom) => evalAtomOpen(atom); FixOut.Atom()
    case FixIn.Body(body) => FixOut.Body(evalBodyOpen(body))
    case FixIn.EnterRelation(rel, adornment) => FixOut.Relation(enterRelationOpen(rel))
  }

  private inline def external[A](f: Fixed ?=> A): A = f(using fixed)

  protected val gensym = Gensym()

  def resetIDB(): Unit

  def insertEDB(relName: String, rv: RV): Unit =
    edb += relName -> rv

  def removeEDB(relName: String, rv: RV): Unit = edb.get(relName) match
    case Some(edbRV) =>
      val paramNames = relationOps.columns(edbRV)
      val cols = relationOps.columns(rv)
      if (cols.size != paramNames.size)
        failure(InvalidBindings, s"Invalid bindings for EDB relation $relName")
      val removeRVs = relationOps.rename(rv, cols.zip(paramNames).toMap)
      edb += relName -> relationOps.antiJoin(edbRV, removeRVs)
    case _ => // nothing

  def evalProgram(p: Seq[ir.Module]): Map[String, Map[String, RV]] =
    external(p.map(m => m.name.name -> evalModule(m)).toMap)

  def entryPoints(m: ir.Module): Iterable[ir.Relation] = //m.relations.values
    m.relations.values.filter(_.hasHint(MainHint)) match
      case mainRels if mainRels.nonEmpty => mainRels
      case _ => m.relations.values

  def evalModule(m: ir.Module)(using Fixed): Map[String, RV] = {
    entryPoints(m).map { rel =>
      val allFreeAdorn = Adornment(rel.params.map(_ => Adorn.f))
      rel.name.name -> evalRelation(rel, allFreeAdorn)
    }.toMap
  }

  protected def insertIDB(name: ir.Name, rv: RV): Unit =
    idb.write(AllocationSiteAddr.Variable(name.name)(true), rv)

  inline def evalRelation(r: ir.Relation, adornment: Adornment)(using rec: Fixed): RV =
    rec(FixIn.EnterRelation(r, adornment)) match
      case FixOut.Relation(p) => p
      case _ => throw new IllegalStateException()

  def evalRelationOpen(r: ir.Relation)(using Fixed): RV = supplementaryTable.scoped { gensym.scoped {
    gensym.register(r.bodies.flatMap(_.vars.map(_.name.name)))

    val paramNames = r.params.map(p => p.name.name)
    val emptyRes = relationOps.make(paramNames, Seq())

    var allBodiesFailed: Boolean = true
    val relRes = mapJoin(r.bodies, { b =>
      except.tryCatch {
        val res = relationOps.project(evalBody(b), paramNames)
        allBodiesFailed = false
        res
      } /*catch*/ {
        exc => emptyRes
      }
    })
    
    insertIDB(r.name, relRes)
    relRes
  }}

  def evalExtensionalRelation(r: ir.ExtensionalRelation)(using Fixed): RV = supplementaryTable.scoped { gensym.scoped {
    gensym.register(r.params.map(_.name.name))

    val relName = r.name.name
    val paramNames = r.params.map(_.name.name)
    val rv = edb.get(relName) match
      case Some(value) => value
      case _ => relationOps.make(paramNames, Seq())

    // Make sure we have an edb entry for each column. We have no guarantee that the column names match.
    val cols = relationOps.columns(rv)
    if (cols.size != paramNames.size)
      failure(InvalidBindings, s"Invalid bindings for EDB relation $relName")

    // rename column according to parameters
    val edbRV = relationOps.rename(rv, cols.zip(paramNames).toMap)

    // filter edb rows based on current supplementary 
    relationOps.project(relationOps.naturalJoin(supplementaryTable.getTable, edbRV), paramNames)
  }}

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
  def extractVarName(term: ir.Term): Option[ir.Name] = term match
    case ir.Var(ref) => Some(ref.name)
    case ir.Cast(t, _) => extractVarName(t)
    case _ => None

  private final def evalAssign(to: String, from: ir.Term)(using Fixed): Unit =
    val fromCol = evalTerm(from)
    updateSupplementaryUnchecked { sup =>
      relationOps.copyColumn(sup, fromCol, to)
    }

  private final def evalCompare(lhs: ir.Term, rhs: ir.Term, neg: Boolean)(using Fixed): Unit =
    val ls = evalTerm(lhs)
    val rs = evalTerm(rhs)
    val eqOp = if (neg) eqOps.neq else eqOps.equ
    updateSupplementaryChecked { sup =>
      val lix = relationOps.columnIndex(sup, ls)
      val rix = relationOps.columnIndex(sup, rs)
      relationOps.filter(sup){ row => eqOp(row(lix), row(rix)) }
    }

  private def boundInSupplementary(s: String): Boolean =
    relationOps.hasColumn(supplementaryTable.getTable, s)

  private def boundInSupplementary(t: ir.Term): Boolean = t.typ match
    case Some(TermType(_, Mode.Bound)) => true /* term is always bound, independent of current query */
    case _ =>
      val sup = supplementaryTable.getTable
      t.vars.forall { v => relationOps.hasColumn(sup, v.name.name) }

  protected final def evalEq(lhs: ir.Term, rhs: ir.Term, neg: Boolean)(using Fixed): Unit =
    (boundInSupplementary(lhs), boundInSupplementary(rhs), neg) match
      case (false, false, _) => failure(InvalidBindings, s"Equality between two binding terms: $lhs and $rhs")
      case (true, true, _) => evalCompare(lhs, rhs, neg)
      case (false, _, false) => evalAssign(extractVarName(lhs).get.name, rhs)
      case (_, false, false) => evalAssign(extractVarName(rhs).get.name, lhs)
      case _ => failure(InvalidBindings, s"Equality with binding term in negation: $lhs and $rhs")

  def evalArg(arg: ir.Arg)(using Fixed): Option[SupColumn] = arg match
    case ir.TermArg(t) if boundInSupplementary(t) => Some(evalTerm(t))
    case ir.TermArg(t) => None
    case ir.WildcardArg() => None
    case _ => failure(UnknownArg, s"Unknown arg $arg")

  def extractVarName(arg: ir.Arg): Option[ir.Name] = arg match
    case ir.TermArg(t) => extractVarName(t)
    case ir.WildcardArg() => Some(ir.Name(gensym.fresh("_")))

  private final def evalCall[R <: ModuleEntry](r: R, params: Seq[ir.Param], args: Seq[ir.Arg], neg: Boolean)(using Fixed): Unit =
    if (params.isEmpty) {
      // Relation with no parameters... This should not happen, even though viatra supports it
      failure(NoParamRelation, s"Relation ${r.name} has no Parameters!")
    }

    // eval arguments in current scope
    val argMapping = params.zip(args).map { (p, a) => evalArg(a).map(_ -> p.name.name) }
    // rename the argument according to the parameters
    val evalContext = relationOps.projectAndRename(supplementaryTable.getTable, argMapping.flatten.toMap)
    // calculate the adornment
    val adornment = Adornment(argMapping.map {
      case Some(_) => Adorn.b
      case None => Adorn.f
    })

    // eval the actual call in a new scoped environment
    updateSupplementaryChecked { beforeCall =>
      supplementaryTable.setTable(evalContext)

      // evaluate the call
      val relRes = r match
        case rel: ir.Relation => evalRelation(rel, adornment)
        case extRel: ir.ExtensionalRelation => evalExtensionalRelation(extRel)

      // add all variables from the call to the context
      val paramNameToArgName = params.zip(args).flatMap { case (p, a) => extractVarName(a).map(p.name.name -> _.name) }.toMap
      val subst = argMapping.zip(params).map {
        case (Some(before, after), _) => after -> before
        case (_, p) => p.name.name -> paramNameToArgName(p.name.name)
      }.toMap

      val callRes = relationOps.projectAndRename(relRes, subst)

      if (neg)
        relationOps.antiJoin(beforeCall, callRes)
      else
        relationOps.naturalJoin(beforeCall, callRes)
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


  inline final def evalTerm(term: ir.Term)(using rec: Fixed): SupColumn = rec(FixIn.Term(term)) match
    case FixOut.Term(v) => v
    case _ => throw new IllegalStateException()

  protected def termResult(v: V): SupColumn =
    val resName = gensym.fresh("result")
    updateSupplementaryUnchecked { sup =>
      relationOps.map(sup, resName) { row => v }
    }
    resName
    
  protected def unaryOp(lhs: SupColumn)(f: V => V): SupColumn =
    val resName = gensym.fresh("result")
    updateSupplementaryUnchecked { sup =>
      val lhsIx = relationOps.columnIndex(sup, lhs)
      relationOps.map(sup, resName) { row => f(row(lhsIx)) }
    }
    resName

  protected def binaryOp(lhs: SupColumn, rhs: SupColumn)(f: (V, V) => V): SupColumn =
    val resName = gensym.fresh("result")
    updateSupplementaryUnchecked { sup =>
      val lhsIx = relationOps.columnIndex(sup, lhs)
      val rhsIx = relationOps.columnIndex(sup, rhs)
      relationOps.map(sup, resName) { row => f(row(lhsIx), row(rhsIx)) }
    }
    resName

  protected def naryOp(rs: Seq[SupColumn])(f: Seq[V] => V): SupColumn =
    val resName = gensym.fresh("result")
    updateSupplementaryUnchecked { sup =>
      val idx = rs.map(relationOps.columnIndex(sup, _))
      relationOps.map(sup, resName) { row => f(idx.map(row)) }
    }
    resName

  def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case ir.Var(ref) =>
      if (boundInSupplementary(ref.name.name))
        ref.name.name
      else
        failure(UnresolvedVariable, s"Unbound variable ${ref.name.name}")
    case ir.Cast(t, _) =>
      evalTerm(t)
    case _ =>
      failure(UnknownTerm, s"Unknown term $term")