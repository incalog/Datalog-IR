package inca.ir.analysis.base.interpreter

import inca.ir.*
import inca.ir.analysis.base.effect.Failure.{MaybeEmptyCall, ProgramFailure, RefNotFound, EmptyVariable}
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{RelationValue, RelationValueOps, Top, VBool, VBoolOps, Value}
import inca.ir.analysis.{AnalysisKey, AnalysisResult}
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
  case EnterCall[R <: ModuleEntry](ref: Ref[R], args: Seq[Arg])

  override def toString: String = this match
    case EnterCall(ref, args) => s"${ref.name}(${args.mkString("(", ",", ")")})"

enum FixOut[RV]:
  case ExitCall(rv: RV)

given finiteFixIn: Finite[FixIn] with {}

import inca.ir.analysis.base.interpreter.{ FixIn, FixOut }


given CombineFixOut[RV, W <: Widening](using w: Combine[RV, W]): Combine[FixOut[RV], W] with
  override def apply(out1: FixOut[RV], out2: FixOut[RV]): MaybeChanged[FixOut[RV]] = (out1, out2) match
    case (FixOut.ExitCall(rv1), FixOut.ExitCall(rv2)) =>
      val rv = w(rv1, rv2)
      if rv.hasChanged then
        Changed(FixOut.ExitCall(rv.get))
      else
        Unchanged(out1)


trait BaseAbstractInterpreter:

  // Fixpoint
  def fixpoint: EffectStack ?=> Fixpoint[FixIn, FixOut[RelationValue]]
  type Fixed = FixIn => FixOut[RelationValue]

  inline def enterEvalCall[R <: ModuleEntry](ref: Ref[R], args: Seq[Arg])(using rec: Fixed): RelationValue = ???


  // TermResult
  case class TermResult(value: RelationValue, computationResult: Seq[Value], pure: VBool) extends AnalysisResult:
    val result: TermResult = this
    override val akey: TermKey.type = TermKey

    def mapResult(f: Value => Value): TermResult =
      TermResult(value, computationResult.map(f), pure)

    def asTable(resultName: String): RelationValue =
      val cols = relationOps.getCols(value)
      if cols.contains(resultName) then
        val otherCols = cols.filter(s => s != resultName)
        val compRV = relationOps.makeRelation(
          cols,
          relationOps.scan(relationOps.projection(value, otherCols))(identity)
            .zip(computationResult).map((vec, cr) => vec.appended(cr))
        )
        val res = relationOps.natJoin(compRV, value)
        res
      else
        relationOps.makeRelation(
          cols.appended(resultName),
          relationOps.scan(value)(identity).zip(computationResult).map((vec, cr) => vec.appended(cr))
        )

  case object TermKey extends AnalysisKey:
    override val key: String = "Term"
    override type Result = TermResult

  // Ops & Helper

  def relationOps: RelationValueOps
  def boolOps: VBoolOps
  def eqOps: BaseEqOps

  def failure: Failure
  def effects: EffectStack
  def IDB: Store[AllocationSiteAddr, RelationValue, WithJoin]
  def supplementaryTable: SupplementaryTable

  def joinV: Join[Value]
  def joinRV: Join[RelationValue]

  given Join[Value] = joinV
  given Join[RelationValue] = joinRV
  given EffectStack = effects

  // Evaluation
  private lazy val fixed: Fixed = fixpoint {
    case FixIn.EnterCall(ref, args) => FixOut.ExitCall(evalCall(ref, args))
  }

  private inline def external[A](f: Fixed ?=> A): A = f(using fixed)

  def evalProgram(p: Seq[Module]): Unit = external(p.foreach(evalModule))

  def evalModule(m: Module)(using Fixed): Unit =
    val mainRelation = m.relations.collectFirst { case (_, r) if r.hasHint(MainHint) => r }
    val relsToAnalyse = mainRelation match
      case Some(rel) => Seq(rel)
      case _ => m.relations.values
    relsToAnalyse.foreach { r =>
      val res = evalRelation(r)
      val hasCols = relationOps.getCols(res).nonEmpty
      val hasNoVals = relationOps.scan(res)(identity).map(_.isEmpty).reduce(_ && _)
      if hasCols && hasNoVals then
        failure(ProgramFailure, s"Program failed, because nothing was returned by $r")
    }

  def merge(rel: RelationValue, neg: Boolean): Unit =
    if neg then
      supplementaryTable.setTable(relationOps.antiJoin(supplementaryTable.getTable, rel))
    else
      supplementaryTable.setTable(relationOps.natJoin(supplementaryTable.getTable, rel))

  protected def insertIDB(rName: String, rv: RelationValue): Unit =
    val oldRVOption = IDB.read(AllocationSiteAddr.Variable(rName)(true))
    IDB.free(AllocationSiteAddr.Variable(rName)(true))
    val result = oldRVOption.option(rv)(oldRV => relationOps.union(rv, oldRV))
    IDB.write(AllocationSiteAddr.Variable(rName)(true), result)

  def evalRelation(r: Relation, initSuppTable: RelationValue = relationOps.empty)(using Fixed): RelationValue =
    relationOps.unionFold(r.bodies) { b =>
      supplementaryTable.freshScoped {
        merge(initSuppTable, false)
        evalBody(b)
        val res = relationOps.projection(supplementaryTable.getTable, r.params.map(p => p.name.name).toVector)
        val oldIDB = IDB.readOrElse(AllocationSiteAddr.Variable(r.name)(true),
          relationOps.makeRelation(r.params.map(p => p.name.name).toVector, Seq(Vector())))
        val subsetRes = relationOps.subset(oldIDB, res)
        if subsetRes != VBool.True then
          insertIDB(r.name, res)
        res
      }
    }

  def evalBody(b: Body)(using Fixed): RelationValue =
    var rest = b.atoms
    var success = true
    while (rest.nonEmpty && success) {
      evalAtom(rest.head)
      val hasCols = relationOps.getCols(supplementaryTable.getTable).nonEmpty
      val hasValues = relationOps.scan(supplementaryTable.getTable)(identity).map(_.nonEmpty).reduce(_ && _)
      if hasCols && !hasValues then
        success = false
      else
        rest = rest.tail
    }
    if success then
      supplementaryTable.getTable
    else
      relationOps.empty

  def evalAtom(at: Atom)(using Fixed): Unit = at match
    case Eq(lhs, rhs, false) => evalEquals(lhs, rhs)
    case Eq(lhs, rhs, true) => evalNotEquals(lhs, rhs)
    case Call(ref, args, neg) => merge(enterEvalCall(ref, args), neg)
    case ExtensionalCall(ref, args, neg) => merge(enterEvalCall(ref, args), neg)

  final private def evalIntensionalCall(rel: Relation, args: Seq[Arg])(using Fixed): RelationValue =
    val argRel = evalArgs(rel.params, args)
    val res = evalRelation(rel, argRel)
    val varRead = IDB.read(AllocationSiteAddr.Variable(rel.name)(true))
    relationOps.natJoin(varRead.getOrElse(relationOps.empty), argRel)

  final private def evalExtensionalCall(rel: ExtensionalRelation, args: Seq[Arg])(using Fixed): RelationValue =
    val vals = rel.params.zip(args).map {
      case (p, TermArg(a)) => a.mode match
        case Mode.Binding | Mode.Collapse =>
          TermResult(
            supplementaryTable.getTable,
            relationOps.scan(supplementaryTable.getTable)(_ => Top),
            VBool.True
          ).asTable(p.name.name)
        case Mode.Bound =>
          evalTerm(a).asTable(p.name.name)
      case (p, WildcardArg()) =>
        TermResult(
          supplementaryTable.getTable,
          relationOps.scan(supplementaryTable.getTable)(_ => Top),
          VBool.Top
        ).asTable(p.name.name)
    }
    effects.joinWithFailure(vals.reduce(relationOps.natJoin))(failure(MaybeEmptyCall, s"$rel may return nothing"))

  def evalCall[R <: ModuleEntry](ref: Ref[R], args: Seq[Arg])(using Fixed): RelationValue =
    val rel = ref.target.getOrElse {
      failure(RefNotFound, s"Could not resolve reference for call ref")
    }
    rel match
      case r: Relation => evalIntensionalCall(r, args)
      case e: ExtensionalRelation => evalExtensionalCall(e, args)


  def evalArgs(params: Seq[Param], args: Seq[Arg])(using Fixed): RelationValue =
    params.zip(args).filter {
      case (_, TermArg(t)) =>
        val cols = relationOps.getCols(supplementaryTable.getTable)
        cols.containsSlice(t.vars.map(v => v.name.name))
      case (_, WildcardArg()) =>
        false
    }.map {
      case (Param(name, _), TermArg(t)) => evalTerm(t).asTable(name.name)
      case _ => relationOps.empty
    }.fold(relationOps.empty)(relationOps.natJoin)

  final private def evalCompare(lhs: Term, rhs: Term, neg: Boolean)(using Fixed): Unit =
    val tr1 = evalTerm(lhs)
    val tr2 = evalTerm(rhs)
    val natJoined = relationOps.natJoin(tr1.asTable("_$temporaryResult"), tr2.asTable("_$temporaryResult"))
    merge(relationOps.projection(natJoined, relationOps.getCols(natJoined).filter(s => s != "_$temporaryResult")), true)

  final def evalNotEquals(lhs: Term, rhs: Term)(using Fixed): Unit = evalCompare(lhs, rhs, true)

  def assign(assignee: Term, tr: TermResult): Unit = assignee match
    case Var(x) => merge(tr.asTable(x.name.name), false)
    case Cast(t, ty) => assign(t, tr)
  
  final def evalEquals(lhs: Term, rhs: Term)(using Fixed): Unit = (rhs.mode, lhs.mode) match
    case (_, Mode.Binding) => assign(rhs, evalTerm(lhs))
    case (Mode.Binding, _) => assign(lhs, evalTerm(rhs))
    case _ => evalCompare(lhs, rhs, false)

  final def evalTerm(t: Term)(using Fixed): TermResult =
    val r = evalTermExtend(t)
    t.storeAnalysisResult(r)
    r

  def evalTermExtend(t: Term)(using Fixed): TermResult =
    val currSupTab = supplementaryTable.getTable
    t match
      case Var(RefByName(x)) if relationOps.getCols(currSupTab).contains(x.name) =>
        TermResult(currSupTab, relationOps.scan(relationOps.projection(currSupTab, Vector(x.name)))(vec => vec.head), VBool.True)
      case Var(RefByName(x)) =>
        failure(EmptyVariable, s"$x was referenced, but is not bound in $currSupTab")
      case Cast(t, ty) => evalTerm(t)
      case _ => TermResult(relationOps.empty, Seq(), VBool.Top)