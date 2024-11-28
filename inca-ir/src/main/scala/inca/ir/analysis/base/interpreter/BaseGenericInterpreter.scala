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

// TODO:
//  1. Sturdy except when an atom or a body fails
//  2. Make Context-Sensitive + Insensitive configurable
//  3. Concrete Interpreter (data + arith + string + agg?)
//  4. Abstract Interpreter - Constant Analysis (data + arith + string + agg?)
//  5. Logger to annotate information
//  6. Optimize program
//  7. EDB support + test cases

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
  case ExtensionalRelation(rel: ir.ExtensionalRelation)

  override def toString: String = this match
    case FixIn.Term(t) => t.toString
    case FixIn.Atom(a) => a.toString
    case FixIn.Body(b) => s"Body: ${b.hashCode()}" //b.toString
    case FixIn.EnterRelation(rel: ir.Relation, adornment: Adornment) => s"${rel.name.name}_$adornment" //rel.toString
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
  val LHS_COLUMN = "lhs"
  val RHS_COLUMN = "rhs"

  // Fixpoint
  def fixpoint: EffectStack ?=> Fixpoint[FixIn, FixOut[V, RV]]

  type Fixed = FixIn => FixOut[V, RV]

  // Ops & Helper
  val relationOps: RelationOps[V, B, RV]

  val boolOps: BooleanOps[B]

  lazy val boolTrue: B = boolOps.boolLit(true)

  lazy val boolFalse: B = boolOps.boolLit(false)

  val branchOps: BooleanBranching[B, Unit]

  lazy val eqOps: EqOps[V, B]

  lazy val failure: Failure

  lazy val except: Except[BaseIRException, ExcV, WithJoin]

  val joinV: J[V]

  private var edb: Map[String, RV] = Map()

  implicit val joinRV: Join[RV]

  def effects: EffectStack = new EffectStack(EffectList(supplementaryTable, failure, except, idb), {
    case _: FixIn.EnterRelation => EffectList(supplementaryTable, idb) //EffectList(supplementaryTable, failure, idb)
  }, {
    case _: FixIn.EnterRelation => EffectList(except, failure, idb) //supplementaryTable
  })

  given EffectStack = effects

  def idb: Store[AllocationSiteAddr, RV, WithJoin]

  def supplementaryTable: SupplementaryTable[RV]

  implicit def joinUnit: J[Unit]

  // Evaluation
  private lazy val fixed: Fixed = fixpoint(using effects) {
    case FixIn.Term(term) => FixOut.Term(evalTermOpen(term))
    case FixIn.Atom(atom) => evalAtomOpen(atom); FixOut.Atom()
    case FixIn.Body(body) => FixOut.Body(evalBodyOpen(body))
    case FixIn.EnterRelation(rel, adornment) => FixOut.Relation(enterRelationOpen(rel))
    case FixIn.ExtensionalRelation(rel) => FixOut.ExtensionalRelation(evalExtensionalRelationOpen(rel))
  }

  private inline def external[A](f: Fixed ?=> A): A = f(using fixed)

  def resetIDB(): Unit

  def insertEDB(relName: String, rv: RV): Unit =
    edb += relName -> rv

  def removeEDB(relName: String, rv: RV): Unit =
    // TODO: Filter edb and remove tuples accordingly. Look at evalExtensionRelation
    ???

  def evalProgram(p: Seq[ir.Module]): Unit = 
    external(p.foreach(evalModule))

  def entryPoints(m: ir.Module): Iterable[ir.Relation] = //m.relations.values
    m.relations.values.filter(_.hasHint(MainHint)) match
      case mainRels if mainRels.nonEmpty => mainRels
      case _ => m.relations.values

  def evalModule(m: ir.Module)(using Fixed): Unit = {
    entryPoints(m).foreach { rel =>
      val relRes = except.tryCatch {
        val allFreeAdorn = Adornment(rel.params.map(_ => Adorn.f))
        evalRelation(rel, allFreeAdorn)
      } { case RelationFailed(msg) =>
        relationOps.make(rel.params.map(_.name.name), Seq())
      }
    }
  }

  private def merge(lhs: RV, rhs: RV, neg: Boolean): RV =
    val res = if (neg) {
      //println(s"Anti join: $lhs :: $rhs")
      relationOps.antiJoin(lhs, rhs)
    } else {
      //println(s"Nat join: $lhs :: $rhs")
      relationOps.naturalJoin(lhs, rhs)
    }

    // Anti join might produce empty table
    branchOps.boolBranch(relationOps.isEmpty(res)) {
      //println("Now its empty...")
      except.throws(MergeFailed("Merged empty table"))
    } { /* nothing */ }

    res

  protected def mergeIntoEnv(rv: RV, neg: Boolean): Unit =
    val merged = merge(supplementaryTable.getTable, rv, neg)
    supplementaryTable.setTable(merged)

  protected def insertIDB(name: ir.Name, rv: RV): Unit =
    idb.write(AllocationSiteAddr.Variable(name.name)(true), rv)

  inline def evalRelation(r: ir.Relation, adornment: Adornment)(using rec: Fixed): RV =
    rec(FixIn.EnterRelation(r, adornment)) match
      case FixOut.Relation(p) => p
      case _ => throw new IllegalStateException()

  def evalRelationOpen(r: ir.Relation)(using Fixed): RV = supplementaryTable.scoped {
    val paramNames = r.params.map(p => p.name.name)
    val emptyRes = relationOps.make(paramNames, Seq())

    var allBodiesFailed: Boolean = true
    val relRes = mapJoin(r.bodies, { b =>
      except.tryCatch {
        val res = relationOps.project(evalBody(b), paramNames)
        //println(s"${r.name} :: $b :: $bodyRes")
        allBodiesFailed = false
        res
      } /*catch*/ {
        exc => emptyRes
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
    val relName = r.name.name
    val paramNames = r.params.map(_.name.name)
    val rv = edb.get(relName) match
      case Some(value) => value
      case _ => failure(RefNotFound, s"No EDB relation with name $relName found")

    // Make sure we have an edb entry for each column. We have no guarantee that the column names match.
    val cols = relationOps.columns(rv)
    if (cols.size != paramNames.size)
      failure(InvalidBindings, s"Invalid bindings for EDB relation $relName")

    // rename column according to parameters
    val renamedRv = relationOps.rename(rv, cols.zip(paramNames).toMap)

    // Filter the edb entries based on the current supplementary
    // TODO: Is there a nicer solution with anti-join
    val res = relationOps.filter(renamedRv) { row =>
      val bs = paramNames.zip(row).map { (p, r) =>
        if (boundInSupplementary(p))
          val combinations = relationOps.cartesian(
            relationOps.projectAndRename(supplementaryTable.getTable, Map(p -> LHS_COLUMN)),
            relationOps.make(Seq(RHS_COLUMN), Seq(Seq(r)))
          )
          val comparisonResults = relationOps.filter(combinations) { case Seq(v1, v2) => eqOps.equ(v1, v2) }
          boolOps.not(relationOps.isEmpty(comparisonResults))
        else
          boolTrue
      }
      bs.foldLeft(boolTrue)((acc, b) => boolOps.and(acc, b))
    }

    branchOps.boolBranch(relationOps.isEmpty(res)) {
      except.throws(RelationFailed(s"EDB relation $relName failed"))
    } { /* nothing */ }

    res
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
  def extractVarName(term: ir.Term): Option[ir.Name] = term match
    case ir.Var(ref) => Some(ref.name)
    case ir.Cast(t, _) => extractVarName(t)
    case _ => None

  private final def evalAssign(to: ir.Term, from: ir.Term)(using Fixed): Unit =
    val res = relationOps.rename(evalTerm(from), Map(RESULT_COLUMN -> extractVarName(to).get.name))
    mergeIntoEnv(res, false)

  private final def evalCompare(lhs: ir.Term, rhs: ir.Term, neg: Boolean)(using Fixed): Unit =
    val ls = evalTerm(lhs)
    val rs = evalTerm(rhs)

    val combinations = relationOps.cartesian(
      relationOps.rename(ls, Map(RESULT_COLUMN -> LHS_COLUMN)),
      relationOps.rename(rs, Map(RESULT_COLUMN -> RHS_COLUMN))
    )

    val comparisonResults = relationOps.filter(combinations) { case Seq(v1, v2) =>
      if (neg) {
        eqOps.neq(v1, v2)
      } else {
        eqOps.equ(v1, v2)
      }
    }

    // TODO: Filter supplementary
    branchOps.boolBranch(relationOps.isEmpty(comparisonResults)) {
      // All failed
      except.throws(AtomFailed("Comparison failed"))
    } /* catch */ { /*nothing*/ }

  private def boundInSupplementary(s: String): Boolean =
    relationOps.hasColumn(supplementaryTable.getTable, s) == boolTrue

  private def boundInSupplementary(t: ir.Term): Boolean =
    t.vars.forall { v => boundInSupplementary(v.name.name) }

  private final def evalEq(lhs: ir.Term, rhs: ir.Term, neg: Boolean)(using Fixed): Unit =
    val op = if (neg) "!=" else "=="
    //println(s"Eval eq: $lhs $op $rhs")
    (boundInSupplementary(lhs), boundInSupplementary(rhs), neg) match
      case (false, false, _) => failure(InvalidBindings, s"Equality between two binding terms: $lhs and $rhs")
      case (true, true, _) => evalCompare(lhs, rhs, neg)
      case (false, _, false) => evalAssign(lhs, rhs)
      case (_, false, false) => evalAssign(rhs, lhs)
      case _ => failure(InvalidBindings, s"Equality with binding term in negation: $lhs and $rhs")

  def evalArg(arg: ir.Arg)(using Fixed): RV = arg match
    case ir.TermArg(t) =>
      // Only proceed if all vars of an argument are found in the supplementary table otherwise the term is not bound,
      // and we return unit.
      // Note that the binding information will not align with the mode information of the types. E.g.
      //   path(x, y) :- edge(>x<, >y<)
      //   path(x, y) :- edge(>x<, >z<), path(<z>, >y<).
      // We might query path in a top-down evaluation at some step with an adornment path_fb. But, the type information
      // path(<z>, >y<) says y is free, even though it is bound for this particular query.
      if (boundInSupplementary(t))
        evalTerm(t)
      else
        // empty context. We need unit so that mergeIntoEnv is working.
        relationOps.unit
    case ir.WildcardArg() => relationOps.unit
    case _ => failure(UnknownArg, s"Unknown arg $arg")

  def extractVarName(arg: ir.Arg): Option[ir.Name] = arg match
    case ir.TermArg(t) => extractVarName(t)
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

      var positiveCallFailed = false

      // eval the actual call in a new scoped environment
      val res = except.tryCatch {
        supplementaryTable.freshScoped {
          // since we have at least one parameter argRV is defined
          val evalContext = argRes.foldLeft(argRes.head)((acc, rv) => relationOps.naturalJoin(acc, rv))
          supplementaryTable.setTable(evalContext)

          val boundParams = relationOps.columns(evalContext).toSet
          val adornment = Adornment(params.map {
            case p if boundParams.contains(p.name.name) => Adorn.b
            case _ => Adorn.f
          })

          val relRes = r match
            case rel: ir.Relation => evalRelation(rel, adornment)
            case extRel: ir.ExtensionalRelation => evalExtensionalRelation(extRel)

          // add all variables bound by the call to the context
          val boundVarsAfterCall = args.map(extractVarName)
          val subst = boundVarsAfterCall.zip(params).flatMap {
            case (Some(varName), p) => Some((p.name.name, varName.name))
            case _ => None
          }.toMap

          val res = relationOps.projectAndRename(relRes, subst)
          //println(s"Eval context: $evalContext")
          //println(s"Call ${r.name}${args.mkString("(", ", ", ")")} :: $res")
          res
        }
      } { exc =>
        positiveCallFailed = true
        relationOps.unit
      }

      (neg, positiveCallFailed) match
        case (true, true) => // nothing, negative call succeeded
        case (true, false) => except.throws(AtomFailed(s"Negative call failed: ~${r.name}(${args.mkString(",")})"))
        case (false, true) => except.throws(AtomFailed(s"Call failed: ${r.name}(${args.mkString(",")})"))
        case (false, false) => mergeIntoEnv(res, neg) // positive call succeeded
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

  protected def termResult(v: V): RV =
    relationOps.make(Seq(RESULT_COLUMN), Seq(Seq(v)))
    
  protected def unaryOp(lhs: RV)(f: V => V): RV =
    // TODO: Single scan for these 3 operations
    val renamed = relationOps.rename(lhs, Map(RESULT_COLUMN -> LHS_COLUMN))
    val mapped = relationOps.map(renamed, RESULT_COLUMN) { case Seq(l) => f(l) }
    relationOps.project(mapped, Seq(RESULT_COLUMN))
    
  protected def binaryOp(lhs: RV, rhs: RV)(f: (V, V) => V): RV =
    // TODO: Single scan for these 3 operations
    val combinations = relationOps.cartesian(
      relationOps.rename(lhs, Map(RESULT_COLUMN -> LHS_COLUMN)),
      relationOps.rename(rhs, Map(RESULT_COLUMN -> RHS_COLUMN))
    )
    val mapped = relationOps.map(combinations, RESULT_COLUMN) { case Seq(l, r) => f(l, r) }
    relationOps.project(mapped, Seq(RESULT_COLUMN))
    
  protected def naryOp(rs: Seq[RV])(f: Seq[V] => V): RV =
    val renamed = rs.zipWithIndex.map { case (r, idx) =>
      relationOps.rename(r, Map(RESULT_COLUMN -> s"Param$idx"))
    }
    val combinations = renamed.foldLeft(relationOps.unit) { case (acc, tv) => relationOps.cartesian(acc, tv) }
    val mapped = relationOps.map(combinations, RESULT_COLUMN)(f)
    relationOps.project(mapped, Seq(RESULT_COLUMN))

  def evalTermOpen(term: ir.Term)(using Fixed): RV = term match
    case ir.Var(ref) if boundInSupplementary(term) =>
      //val varEntry = relationOps.project(supplementaryTable.getTable, Seq(ref.name.name))
      //relationOps.naturalJoin(varEntry, relationOps.rename(varEntry, Map(ref.name.name -> RESULT_COLUMN)))
      relationOps.projectAndRename(supplementaryTable.getTable, Map(ref.name.name -> RESULT_COLUMN))
    case ir.Var(ref) =>
      failure(UnresolvedVariable, s"Unbound variable ${ref.name.name}")
    case ir.Cast(t, _) =>
      evalTerm(t)
    case _ =>
      failure(UnknownTerm, s"Unknown term $term")