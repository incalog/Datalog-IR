package inca.ir.analysis.base.interpreter

import inca.ir
import inca.ir.analysis.base.effect.*
import inca.ir.analysis.base.ordering.AtomOrderingOps
import inca.ir.analysis.{RelationOps, SupplementaryTable}
import inca.ir.analysis.base.effect
import inca.ir.{Atom, MainHint, ModuleEntry}
import inca.util.Gensym
import sturdy.data.MayJoin.WithJoin
import sturdy.data.{MakeJoined, MayJoin, mapJoin, noJoin}
import sturdy.effect.except.Except
import sturdy.effect.failure.{CollectedFailures, Failure}
import sturdy.effect.{Effect, EffectList, EffectStack}
import sturdy.fix.Fixpoint
import sturdy.values.*
import sturdy.values.booleans.{BooleanBranching, BooleanOps}
import sturdy.values.ordering.EqOps


// TODO:
//  1. Concrete Interpreter (agg)
//  2. Interval Analysis
//  3. Functional Dependency analysis?

enum Adorn:
  case b
  case f

  override def toString: String = this match
    case Adorn.b => "b"
    case Adorn.f => "f"

case class Adornment(as: Seq[Adorn]):
  override def toString: String = as.mkString("")
  lazy val unboundIndices: Seq[Int] = as.zipWithIndex.collect { case (Adorn.f, idx) => idx }

enum FixIn:
  case Term(term: ir.Term)
  case Atom(atom: ir.Atom)
  case Assign(to: ir.Term, from: ir.Term)
  case Body(rel: ir.Relation, ruleIx: Int, paramNames: Seq[String])
  case EnterRelation(rel: ir.Relation, adornment: Adornment)

  override def toString: String = this match
    case FixIn.Term(t) => t.toString
    case FixIn.Assign(to, from) => s"$to = $from"
    case FixIn.Atom(a) => a.toString
    case FixIn.Body(rel, ix, _) => s"${rel.name}: $ix" //b.toString
    case FixIn.EnterRelation(rel: ir.Relation, adornment: Adornment) => s"${rel.name.name}_$adornment"

type SupColumn = String

enum FixOut[V, RV]:
  case Term(col: SupColumn)
  case Assign(to: SupColumn, from: SupColumn)
  case Atom()
  case ExitCall(value: RV)
  case Body(value: RV, rawBody: RV)
  case Relation(value: RV)

given FiniteFixIn: Finite[FixIn] with {}

given CCombineFixOut[V, RV, W <: Widening](using Combine[RV, W]): Combine[FixOut[V, RV], W] with
  override def apply(out1: FixOut[V, RV], out2: FixOut[V, RV]): MaybeChanged[FixOut[V, RV]] =
    (out1, out2) match
      case (FixOut.Term(rv1), FixOut.Term(rv2)) => assert(rv1 == rv2); MaybeChanged(FixOut.Term(rv1), out1)
      case (FixOut.Assign(t1, f1), FixOut.Assign(t2, f2)) => assert(t1 == t2); MaybeChanged(FixOut.Assign(t1, f1), out1)
      case (FixOut.Atom(), FixOut.Atom()) => Unchanged(FixOut.Atom())
      case (FixOut.ExitCall(rv1), FixOut.ExitCall(rv2)) => Combine(rv1, rv2).map(FixOut.ExitCall.apply)
      case (FixOut.Body(rv1, rbv1), FixOut.Body(rv2, rbv2)) =>
        val c1 = Combine(rv1, rv2)
        val c2 = Combine(rbv1, rbv2)
        (c1.hasChanged, c2.hasChanged) match
          case (false, false) => Unchanged(FixOut.Body(c1.get, c2.get))
          case _ => Changed(FixOut.Body(c1.get, c2.get))
      case (FixOut.Relation(rv1), FixOut.Relation(rv2)) => Combine(rv1, rv2).map(FixOut.Relation.apply)
      case _ => throw new IllegalArgumentException(s"Cannot combine outputs of different kind, $out1 and $out2")


trait BaseGenericInterpreter[V, B, RV,  ExcV, J[_] <: MayJoin[?]]:
  val interRelational: Boolean = false

  // Fixpoint
  def fixpoint: EffectStack ?=> Fixpoint[FixIn, FixOut[V, RV]]

  type Fixed = FixIn => FixOut[V, RV]

  // Ops & Helper
  val relationOps: RelationOps[V, B, RV]

  lazy val boolOps: BooleanOps[B]

  val branchOps: BooleanBranching[B, RV]

  val atomOrderingOps: AtomOrderingOps

  lazy val eqOps: EqOps[V, B]

  lazy val failure: CollectedFailures[effect.BaseIRFailure]

  given Failure = failure

  // MayJoin on V used for excepts
  val mayJoinV: J[V]
  lazy val topV: V

  var edb: Map[String, RV] = Map()
  def getIDB: Map[String, RV]

  implicit val joinRV: Join[RV]

  // MayJoin on RV used for excepts
  lazy val mayJoinRV: J[RV]

  lazy val except: Except[BaseIRException, ExcV, J]

  val effects: EffectStack =
    new EffectStack(EffectList(supplementaryTable, failure, except), {
      case _: FixIn.EnterRelation => EffectList(supplementaryTable)
    }, {
      case _: FixIn.EnterRelation => EffectList(except, failure)
    })

  given EffectStack = effects

  def supplementaryTable: SupplementaryTable[RV]

  def snapshotSupplementary(): RV = supplementaryTable.getTable

  /** updates the supplementary table; ASSUMEs the new table is non-empty */
  inline def updateSupplementaryUnchecked(f: RV => RV): RV = supplementaryTable.update(f)

  /** updates the supplementary table; CHECKs the new table is non-empty */
  def updateSupplementaryChecked(f: RV => RV): RV =
    val rv = f(supplementaryTable.getTable)
    supplementaryTable.setTable(rv)
    branchOps.boolBranch(relationOps.isEmpty(rv)) {
      except.throws(EmptySupplementary)
    } {
      rv
    }

  implicit def mayJoinUnit: J[Unit]

  // Evaluation
  private lazy val fixed: Fixed = fixpoint(using effects) {
    case FixIn.Term(term) => FixOut.Term(evalTermOpen(term))
    case FixIn.Atom(atom) =>
      //(s"  ## Eval $atom :: ${supplementaryTable.getTable}")
      evalAtomOpen(atom);
      //println("  ## Success")
      FixOut.Atom()
    case FixIn.Assign(to, from) =>
      val (toSup, fromSup) = evalAssignOpen(to, from)
      FixOut.Assign(toSup, fromSup)
    case FixIn.Body(rel, ix, paramNames) =>
      //(s"## Eval ${rel.name} body $ix")
      val (rv, rawRV) = evalBodyOpen(rel.bodies(ix), paramNames)
      FixOut.Body(rv, rawRV)
    case FixIn.EnterRelation(rel, adornment) =>
      //println(s"## Eval ${rel.name}")
      FixOut.Relation(evalRelationOpen(rel, adornment))
  }

  private inline def external[A](f: Fixed ?=> A): A = f(using fixed)

  protected val gensym = Gensym()

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
    if (interRelational)
      m.relations.values.filter(_.hasHint(MainHint)) match
        case mainRels if mainRels.nonEmpty => mainRels
        case _ => m.relations.values
    else
      m.relations.values

  def evalModule(m: ir.Module)(using Fixed): Map[String, RV] = {
    entryPoints(m).map { rel =>

      val allFreeAdorn = Adornment(relationParams(rel).map(_ => Adorn.f))
      rel.name.name -> evalRelation(rel, allFreeAdorn)
    }.toMap
  }

  inline def evalRelation(r: ir.Relation, adornment: Adornment)(using rec: Fixed): RV =
    rec(FixIn.EnterRelation(r, adornment)) match
      case FixOut.Relation(p) => p
      case _ => throw new IllegalStateException()

  protected def relationParams[R <: ModuleEntry](r: R): Seq[ir.Param] = r match
    case rel: ir.Relation => rel.params
    case rel: ir.RequireRelation => rel.params
    case rel: ir.ExtensionalRelation => rel.params
    case rel: ir.RequireExtensionalRelation => rel.params
    case _ =>
      val relCls = r.getClass.getSimpleName
      throw IllegalArgumentException(s"Can not determine relation parameters for unknown relation type $relCls")

  def evalRelationOpen(r: ir.Relation, adorn: Adornment)(using Fixed): RV = supplementaryTable.scoped { gensym.scoped {
    gensym.register(r.bodies.flatMap(_.vars.map(_.name.name)))

    val paramNames = relationParams(r).map(p => p.name.name)

    val relRes = if (r.bodies.isEmpty)
      relationOps.make(paramNames, Seq())
    else
      except.tryCatch {
        mapJoin(r.bodies.indices, { ix =>
          evalBody(r, ix, paramNames)
        })
      } /*catch*/ { exc =>
        relationOps.make(paramNames, Seq())
      }(using mayJoinRV)
    relRes
  }}

  def evalExtensionalRelation(r: ir.ExtensionalRelation)(using Fixed): RV = supplementaryTable.scoped { gensym.scoped {
    gensym.register(relationParams(r).map(_.name.name))

    val relName = r.name.name
    val paramNames = relationParams(r).map(_.name.name)
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
    except.tryCatch {
      relationOps.project(relationOps.naturalJoin(supplementaryTable.getTable, edbRV), paramNames)
    } /*catch*/ { exc =>
      relationOps.make(paramNames, Seq())
    }(using mayJoinRV)
  }}

  inline def evalBody(rel: ir.Relation, ix: Int, paramNames: Seq[String])(using rec: Fixed): RV = rec(FixIn.Body(rel, ix, paramNames)) match
    case FixOut.Body(rv, _) => rv
    case _ => throw new IllegalStateException()

  protected def isAssignable(at: Atom, supCols: Seq[String]): Boolean = at match
    case ir.Eq(lhs, rhs, false) =>
      extractVarName(rhs).isDefined && lhs.unboundVars.isEmpty && lhs.boundVars.map(_.name.name).forall(supCols.contains) ||
        extractVarName(lhs).isDefined && rhs.unboundVars.isEmpty && rhs.boundVars.map(_.name.name).forall(supCols.contains)
    case _ => false

  protected def evalAtoms(ats: Seq[Atom])(using rec: Fixed): Unit =
    var rest = ats
    while (rest.nonEmpty) {
      val sup = supplementaryTable.getTable
      val supCols = relationOps.columns(sup)
      val (now, later) = rest.partition { at =>
        at.boundVars.map(_.name.name).forall(supCols.contains) || isAssignable(at, supCols)
      }
      val ordered = now.sortBy(at => atomOrderingOps.priority(at))
      if (rest.size == later.size)
        throw new IllegalStateException()
      ordered.foreach(evalAtom)
      rest = later
    }

  def evalBodyOpen(b: ir.Body, paramNames: Seq[String])(using rec: Fixed): (RV, RV) = supplementaryTable.scoped {
    evalAtoms(b.atoms)
    val rawBody = supplementaryTable.getTable
    val projectedBody = relationOps.project(rawBody, paramNames)
    // RawBody is only used for annotation purposes, it is not needed for the actual interpretation
    (projectedBody, rawBody)
  }

  inline def evalAtom(at: ir.Atom)(using rec: Fixed): Unit = rec(FixIn.Atom(at)) match
    case FixOut.Atom() => ()
    case _ => throw new IllegalStateException()


  // I don't think that anything else can be binding in an equality. But if so, subclasses may override this
  def extractVarName(term: ir.Term): Option[ir.Name] = term match
    case ir.Var(ref) => Some(ref.name)
    case ir.Cast(t, _) => extractVarName(t)
    case _ => None

  inline def evalAssign(to: ir.Term, from: ir.Term)(using rec: Fixed): Unit = rec(FixIn.Assign(to, from)) match
    case FixOut.Assign(_, _) => ()
    case _ => throw new IllegalStateException()

  private final def evalAssignOpen(to: ir.Term, from: ir.Term)(using Fixed): (SupColumn, SupColumn) =
    val fromCol = evalTerm(from)
    val toCol = extractVarName(to).get.name
    updateSupplementaryUnchecked { sup =>
      relationOps.copyColumn(sup, fromCol, toCol)
    }
    (toCol, fromCol)

  private final def evalCompare(lhs: ir.Term, rhs: ir.Term, neg: Boolean)(using Fixed): Unit =
    val ls = evalTerm(lhs)
    val rs = evalTerm(rhs)
    updateSupplementaryChecked { sup =>
      if (neg)
        relationOps.filterNeq(sup, ls, rs)
      else
        relationOps.filterEq(sup, ls, rs)
    }

  protected def boundInSupplementary(s: String): Boolean =
    relationOps.hasColumn(supplementaryTable.getTable, s)

  protected def canDetermineValue(t: ir.Term): Boolean =
    // This function assumes that all of our programs are well-typed.
    // Subclasses, e.g. for Blocks or Sets should override this method to correctly
    // handle arguments, such as SetComprehension to indicate that they can be computed.
    t.vars.forall { v => boundInSupplementary(v.name.name) }

  protected final def evalEq(lhs: ir.Term, rhs: ir.Term, neg: Boolean)(using Fixed): Unit =
    (canDetermineValue(lhs), canDetermineValue(rhs), neg) match
      case (false, false, _) => failure(InvalidBindings, s"Equality between two binding terms: $lhs and $rhs")
      case (true, true, _) => evalCompare(lhs, rhs, neg)
      case (false, _, false) => evalAssign(lhs, rhs)
      case (_, false, false) => evalAssign(rhs, lhs)
      case _ => failure(InvalidBindings, s"Equality with binding term in negation: $lhs and $rhs")

  protected def evalArg(arg: ir.Arg)(using Fixed): Option[SupColumn] = arg match
    case ir.TermArg(t) if canDetermineValue(t) => Some(evalTerm(t))
    case ir.TermArg(t) => None
    case ir.WildcardArg() => None
    case _ => failure(UnknownArg, s"Unknown arg $arg")

  def extractVarName(arg: ir.Arg): Option[ir.Name] = arg match
    case ir.TermArg(t) => extractVarName(t)
    case ir.WildcardArg() => Some(ir.Name(gensym.fresh("_")))

  // eval(arg) -> param name
  type BoundArgMapping = Seq[Option[(SupColumn, String)]]

  def evaluationContextForCall[R <: ModuleEntry](r: R, params: Seq[ir.Param], args: Seq[ir.Arg])(using Fixed): (RV, BoundArgMapping) =
    // Relation with no parameters... This should not happen, even though some engines support it
    if (params.isEmpty)
      failure(NoParamRelation, s"Relation ${r.name} has no parameters!")
    // eval arguments in current scope
    val boundArgsMapping = params.zip(args).map { (p, a) => evalArg(a).map(_ -> p.name.name) }
    // group all mappings by their name. if we pass the same variable twice to a function we get more than one mapping
    val multiMapping = boundArgsMapping.flatten.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    // filter / rename / duplicate the current arguments in the supplementary
    val evalContext = relationOps.projectAndRenameWithMultipleAliases(supplementaryTable.getTable, multiMapping)
    (evalContext, boundArgsMapping)

  protected final def calculateAdornment(argMapping: BoundArgMapping): Adornment =
    Adornment(argMapping.map {
      case Some(_) => Adorn.b
      case None => Adorn.f
    })

  protected final def evalRelation[R <: ModuleEntry](r: R, params: Seq[ir.Param], adornment: Adornment, evalContext: RV)(using Fixed): RV =
    supplementaryTable.setTable(evalContext)

    r match
      case rel: ir.Relation if interRelational =>
        evalRelation(rel, adornment)
      case extRel: ir.ExtensionalRelation =>
        evalExtensionalRelation(extRel)
      case _: ir.Relation | _: ir.RequireRelation | _: ir.RequireExtensionalRelation =>
        // TODO: We could evaluate across module boundaries here. For now we just assume top.
        // assume top for all unbound arguments
        adornment.unboundIndices.map(params).foldLeft[RV](evalContext) {
          case (acc, param) => relationOps.map(acc, param.name.name)(_ => topV)
        }
      case _ =>
        val relCls = r.getClass.getSimpleName
        throw IllegalArgumentException(s"Can not determine relation parameters for unknown relation type $relCls")

  def mappingFromParamToLocalVariable[R <: ModuleEntry](r: R, params: Seq[ir.Param], args: Seq[ir.Arg]): Map[String, String] =
    params.zip(args).flatMap { case (p, a) => extractVarName(a).map(p.name.name -> _.name) }.toMap

  protected final def evalCall[R <: ModuleEntry](r: R, params: Seq[ir.Param], args: Seq[ir.Arg], neg: Boolean)(using Fixed): Unit =
    val (evalContext, argMapping) = evaluationContextForCall(r, params, args)
    val adornment = calculateAdornment(argMapping)

    // eval the actual call in a new scoped environment
    updateSupplementaryChecked { beforeCall =>
      val relRes = evalRelation(r, params, adornment, evalContext)

      // add all variables bound by the call to the beforeContext
      val paramNameToArgName = mappingFromParamToLocalVariable(r, params, args)
      val subst = argMapping.zip(params).map {
        case (Some(before, after), _) => after -> before
        case (_, p) => p.name.name -> paramNameToArgName(p.name.name)
      }.toMap

      val validKeys = relationOps.columns(relRes)
      val callRes = relationOps.projectAndRename(relRes, subst.filter(kv => validKeys.contains(kv._1)))

      if (neg)
        relationOps.antiJoin(beforeCall, callRes)
      else
        relationOps.naturalJoin(beforeCall, callRes)
    }

  def evalAtomOpen(at: ir.Atom)(using Fixed): Unit = at match
    case ir.Eq(lhs, rhs, neg) => evalEq(lhs, rhs, neg)
    case ir.Call(ref, args, neg) => ref.target match
      case Some(r: ir.Relation) => evalCall(r, relationParams(r), args, neg)
      case Some(r: ir.RequireRelation) => evalCall(r, relationParams(r), args, neg)
      case _ => failure(RefNotFound, s"Can not find call reference $ref")
    case ir.ExtensionalCall(ref, args, neg) => ref.target match
      case Some(r: ir.ExtensionalRelation) => evalCall(r, relationParams(r), args, neg)
      case Some(r: ir.RequireExtensionalRelation) => evalCall(r, relationParams(r), args, neg)
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

  protected def ternaryOp(first: SupColumn, second: SupColumn, third: SupColumn)(f: (V, V, V) => V): SupColumn =
    val resName = gensym.fresh("result")
    updateSupplementaryUnchecked { sup =>
      val firstIx = relationOps.columnIndex(sup, first)
      val secondIx = relationOps.columnIndex(sup, second)
      val thirdIx = relationOps.columnIndex(sup, third)
      relationOps.map(sup, resName) { row => f(row(firstIx), row(secondIx), row(thirdIx)) }
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