package inca.ir.analysis.base.interpreter

import inca.ir
import inca.ir.analysis.base.effect.*
import inca.ir.analysis.{RelationOps, SupplementaryTable}
import inca.ir.analysis.base.effect
import inca.ir.hints.MainHint
import inca.ir.{Atom, ModuleEntry}
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

trait Index
type IndexPath = Seq[Index]

case class BindingInfo(col: SupColumn, indexPath: IndexPath, isBound: Boolean):
  def isToplevel: Boolean = indexPath.isEmpty
  def asBinding: BindingInfo = BindingInfo(col, indexPath, false)

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
  // logging only
  case Term(term: ir.Term)
  case Atom(atom: ir.Atom)
  case AtomGroup(atoms: Seq[ir.Atom])
  case Assign(to: ir.Term, from: ir.Term)
  case Body(rel: ir.Relation, ruleIx: Int, paramNames: Seq[String])
  // relevant for fixpoint computations
  case EnterRelation(rel: ir.Relation, adornment: Adornment)

  override def toString: String = this match
    case FixIn.Term(t) => t.toString
    case FixIn.Assign(to, from) => s"$to = $from"
    case FixIn.Atom(a) => a.toString
    case FixIn.AtomGroup(as) => as.mkString(",")
    case FixIn.Body(rel, ix, _) => s"${rel.name}: $ix" //b.toString
    case FixIn.EnterRelation(rel: ir.Relation, adornment: Adornment) => s"${rel.name.name}_$adornment"

type SupColumn = String

enum FixOut[V, RV]:
  case Term(col: SupColumn)
  case Assign()
  case Atom()
  case AtomGroup(value: RV)
  case ExitCall(value: RV)
  case Body(value: RV, rawBody: RV)
  case Relation(value: RV)

given FiniteFixIn: Finite[FixIn] with {}

given CCombineFixOut[V, RV, W <: Widening](using Combine[RV, W]): Combine[FixOut[V, RV], W] with
  override def apply(out1: FixOut[V, RV], out2: FixOut[V, RV]): MaybeChanged[FixOut[V, RV]] =
    (out1, out2) match
      case (FixOut.Term(rv1), FixOut.Term(rv2)) => assert(rv1 == rv2); MaybeChanged(FixOut.Term(rv1), out1)
      case (FixOut.Assign(), FixOut.Assign()) => Unchanged(FixOut.Assign())
      case (FixOut.Atom(), FixOut.Atom()) => Unchanged(FixOut.Atom())
      case (FixOut.AtomGroup(rv1), FixOut.AtomGroup(rv2)) => Combine(rv1, rv2).map(FixOut.AtomGroup.apply)
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

  def scopedSupplementary[A](f: RV => A): A = supplementaryTable.scoped {
    gensym.scoped {
      f(supplementaryTable.getTable)
    }
  }

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
    case FixIn.Term(term) =>
      //println(s"  ## Eval Term $term :: ${supplementaryTable.getTable}")
      val res = evalTermOpen(term)
      //println(s"  ## Success Term $term :: ${supplementaryTable.getTable}")
      FixOut.Term(res)
    case FixIn.Atom(atom) =>
      //println(s"  ## Eval Atom $atom :: ${supplementaryTable.getTable}")
      evalAtomOpen(atom);
      //println(s"  ## Success Atom :: ${supplementaryTable.getTable}")
      FixOut.Atom()
    case FixIn.AtomGroup(as) =>
      evalAtomGroupOpen(as)
      FixOut.AtomGroup(supplementaryTable.getTable)
    case FixIn.Assign(to, from) =>
      evalAssignOpen(to, from)
      FixOut.Assign()
    case FixIn.Body(rel, ix, paramNames) =>
      //println(s"## Eval ${rel.name} body $ix")
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

  def evalModule(m: ir.Module)(using Fixed): Map[String, RV] =
    entryPoints(m).map { rel =>
      val allFreeAdorn = Adornment(relationParams(rel).map(_ => Adorn.f))
      rel.name.name -> evalRelation(rel, allFreeAdorn)
    }.toMap

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
        // Join the result of all bodies together
        // In case of the Concrete interpreter this `join`
        // is a union operation, inferred from `joinRV: Join[RV]`.
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

  // evalAtomGroup is only used for annotation purposes. Whenever a construct such as a disjunction performs
  // a scoped operation on the supplementary, we need to make sure that the contained atoms are correctly annotated.
  inline def evalAtomGroup(atoms: Seq[Atom])(using rec: Fixed): Unit = rec(FixIn.AtomGroup(atoms)) match
    case FixOut.AtomGroup(_) => ()
    case _ => throw new IllegalStateException()

  protected def evalAtomGroupOpen(atoms: Seq[Atom])(using rec: Fixed): Unit =
    evalAtoms(atoms)

  protected def evalAtoms(ats: Seq[Atom])(using rec: Fixed): Unit =
    ats.foreach(evalAtom)

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

  protected def extractBindingInfo(arg: ir.Arg)(using rec: Fixed): Seq[BindingInfo] = arg match
    case ir.TermArg(t) => extractBindingInfo(t)
    case ir.WildcardArg() => Seq()
    case _ => throw IllegalStateException(s"Unknown binding arg $arg")

  protected def extractBindingInfo(term: ir.Term, indexPath: IndexPath = Seq())(using rec: Fixed): Seq[BindingInfo] =
    term match
      case ir.Var(ref) =>
        val isBound = canDetermineValue(term)
        Seq(BindingInfo(ref.name.name, indexPath, isBound))
      case ir.Cast(t, _) =>
        extractBindingInfo(t, indexPath)
      case _ =>
        if (canDetermineValue(term))
          val sup = evalTerm(term)
          Seq(BindingInfo(sup, indexPath, true))
        else
          throw IllegalStateException(s"Unknown binding term $term")

  protected def stepIndex(v: V, index: Index): V =
    throw IllegalStateException(s"Unknown index $index")

  protected final def process(rv: RV, infos: Seq[BindingInfo], from: SupColumn): RV =
    infos.foldLeft(rv) { (accSup, info) =>
      if (info.isBound)
        check(accSup, info, from)
      else
        bind(accSup, info, from)
    }

  protected final def bind(rv: RV, info: BindingInfo, from: SupColumn): RV =
    if (info.isBound)
      throw IllegalArgumentException(s"Can not bind already bound column ${info.col}")
    val fromColIdx = relationOps.columnIndex(rv, from)
    relationOps.map(rv, info.col) { row =>
      info.indexPath.foldLeft(row(fromColIdx))(stepIndex(_, _))
    }

  protected final def check(rv: RV, info: BindingInfo, from: SupColumn): RV =
    if (!info.isBound)
      throw IllegalArgumentException(s"Can not check unbound column ${info.col}")
    val lhsColIdx = relationOps.columnIndex(rv, from)
    val lhsCol = gensym.fresh("result")
    val newRv = relationOps.map(rv, lhsCol) { row =>
      info.indexPath.foldLeft(row(lhsColIdx))(stepIndex(_, _))
    }
    relationOps.filterEq(newRv, lhsCol, info.col)

  inline def evalAssign(to: ir.Term, from: ir.Term)(using rec: Fixed): Unit = rec(FixIn.Assign(to, from)) match
    case FixOut.Assign() => ()
    case _ => throw new IllegalStateException()

  protected def evalAssignOpen(to: ir.Term, from: ir.Term)(using Fixed): Unit =
    val fromCol = evalTerm(from)
    val bindingInfos = extractBindingInfo(to)
    // things in tuples might be bound as well and not just binding
    updateSupplementaryUnchecked { sup =>
      process(sup, bindingInfos, fromCol)
    }

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

  // This method assumes that all of our programs are well-typed.
  // Subclasses, e.g. for Blocks or Sets should override this method to correctly
  // handle arguments, such as SetComprehension to indicate that they can be computed.
  protected def canDetermineValue(t: ir.Term): Boolean = t match
    case ir.Var(ref) => boundInSupplementary(ref.name.name)
    case ir.Cast(t, _) => canDetermineValue(t)


  protected final def evalEq(lhs: ir.Term, rhs: ir.Term, neg: Boolean)(using Fixed): Unit =
    (canDetermineValue(lhs), canDetermineValue(rhs), neg) match
      case (false, false, _) => failure(InvalidBindings, s"Equality between two binding terms: $lhs and $rhs")
      case (true, true, _) => evalCompare(lhs, rhs, neg)
      case (false, _, false) => evalAssign(lhs, rhs)
      case (_, false, false) => evalAssign(rhs, lhs)
      case _ => failure(InvalidBindings, s"Equality with binding term in negation: $lhs and $rhs")

  // One `Seq` entry for each argument
  type ArgBindingInfo = Seq[Seq[BindingInfo]]

  def evaluationContextForCall[R <: ModuleEntry](r: R, params: Seq[ir.Param], args: Seq[ir.Arg])(using Fixed): (RV, ArgBindingInfo) =
    // Relation with no parameters... This should not happen, even though some engines support it
    if (params.isEmpty)
      failure(NoParamRelation, s"Relation ${r.name} has no parameters!")

    val info = args.map(extractBindingInfo)
    val argToParamMapping = params.zip(info).flatMap {
      case (p, Seq(binding)) if binding.isBound && binding.isToplevel =>
        // We can only directly pass top-level, fully evaluated args.
        // In particular, this excludes partially evaluated tuples,
        // such as (x, 5) where x is unbound.
        Some(binding.col -> p.name.name)
      case _ =>
        None
    }
    val multiMapping = argToParamMapping.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    val evalContext = relationOps.projectAndRenameWithMultipleAliases(supplementaryTable.getTable, multiMapping)
    (evalContext, info)

  protected final def calculateAdornment(argMapping: ArgBindingInfo): Adornment =
    Adornment(argMapping.map {
      case Seq(binding) if binding.isBound && binding.isToplevel => Adorn.b
      case _ => Adorn.f
    })

  protected final def evalRelationEntry[R <: ModuleEntry](r: R, params: Seq[ir.Param], adornment: Adornment, evalContext: RV)(using Fixed): RV =
    scopedSupplementary { _ =>
      supplementaryTable.setTable(evalContext)
      r match
        case rel: ir.Relation if interRelational =>
          evalRelation(rel, adornment)
        case extRel: ir.ExtensionalRelation =>
          evalExtensionalRelation(extRel)
        case _: ir.Relation | _: ir.RequireRelation | _: ir.RequireExtensionalRelation =>
          // assume top for all unbound arguments
          adornment.unboundIndices.map(params).foldLeft[RV](evalContext) {
            case (acc, param) => relationOps.map(acc, param.name.name)(_ => topV)
          }
        case _ =>
          val relCls = r.getClass.getSimpleName
          throw IllegalArgumentException(s"Can not determine relation parameters for unknown relation type $relCls")
    }

  def renameRelationResult(relRes: RV, params: Seq[ir.Param], argBindingInfo: ArgBindingInfo)(using Fixed): RV =
    val paramNames = params.map(_.name.name)
    val argColumns = argBindingInfo.flatMap(_.map(_.col))

    val localGensym = Gensym()
    localGensym.register(paramNames)
    localGensym.register(argColumns)

    // rename all columns to prevent name collisions
    val resultCols = paramNames.map(_ -> localGensym.fresh("result")).toMap
    val renamedRelRes = relationOps.rename(relRes, resultCols)

    // This is effectively a renaming combined with an unpacking.
    // In particular, that means partially bound tuples R((x, 3), 5)
    // are bound afterward.
    val extendedRelRes = paramNames
      .zip(argBindingInfo)
      .foldLeft(renamedRelRes) { case (rv, (p, infos)) =>
        val from = resultCols(p)
        infos.foldLeft(rv) { (accSup, info) =>
          bind(accSup, info.asBinding, from)
        }
    }
    relationOps.project(extendedRelRes, argColumns)

    // We need to filter all partial tuples.
    // E.g.
    //  R(x) :- x = (1,2) v x = (2,3)
    //  Q(x) :- R((_,2))
    // Should only yield one tuple for x.
    // However, since the call result is naturally joined in to the supplementary
    // and the supplementary contains partial results, we do not need to do this
    // natural join here. If we could write down a program with negation or aggregation
    // that uses a partial tuple, then we would need this.
    /*val sup = supplementaryTable.getTable
    val supColumns = relationOps.columns(sup)
    relationOps.naturalJoin(
      relationOps.project(extendedRelRes, argColumns),
      relationOps.project(sup, argColumns.intersect(supColumns)),
    )*/

  protected final def evalCall[R <: ModuleEntry](r: R, params: Seq[ir.Param], args: Seq[ir.Arg], neg: Boolean)(using Fixed): Unit =
    val (evalContext, argBindingInfo) = evaluationContextForCall(r, params, args)
    val adornment = calculateAdornment(argBindingInfo)

    updateSupplementaryChecked { beforeCall =>
      val relRes = evalRelationEntry(r, params, adornment, evalContext)
      val callRes = renameRelationResult(relRes, params, argBindingInfo)
      if (neg)
        // Project everything away that was freshly bound.
        // This is safe, since a negative call does not bind variables
        val colsBefore = relationOps.columns(beforeCall)
        val colsAfter = relationOps.columns(callRes)
        val projected = relationOps.project(callRes, colsBefore.intersect(colsAfter))
        relationOps.antiJoin(beforeCall, projected)
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