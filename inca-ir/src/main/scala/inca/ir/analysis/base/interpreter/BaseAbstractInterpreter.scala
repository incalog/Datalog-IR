package inca.ir.analysis.base.interpreter

import inca.ir
import inca.ir.analysis.base.effect.Failure.{UnknownTerm, UnknownAtom, UnknownArg, InvalidBindings, ProgramFailure, RefNotFound}
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
  case Term(values: Set[V], pure: B)
  case Atom(pure: B)
  case Body(pure: B)
  case Relation(pure: B)
  case ExtensionalRelation(pure: B)

given FiniteFixIn: Finite[FixIn] with {}

given FiniteFixOut[V, B, RV]: Finite[FixOut[V, B, RV]] with {}

import inca.ir.analysis.base.interpreter.{ FixIn, FixOut }

given CombineFixOut[V, B, RV, VW <: Widening, BW <: Widening](using combineV: Combine[V, VW], combineB: Combine[B, BW]): Combine[FixOut[V, B, RV], Widening.No] with
  override def apply(out1: FixOut[V, B, RV], out2: FixOut[V, B, RV]): MaybeChanged[FixOut[V, B, RV]] = (out1, out2) match
    case (FixOut.Term(vs1, p1), FixOut.Term(vs2, p2)) =>
      // We use a cartesian product here because of Datalog set semantics
      val v = for (v1 <- vs1; v2 <- vs2) yield combineV(v1, v2)
      val pure = combineB(p1, p2)
      if v.exists(_.hasChanged) || pure.hasChanged then
        Changed(FixOut.Term(v.map(_.get), pure.get))
      else
        Unchanged(FixOut.Term(v.map(_.get), pure.get))
    case (FixOut.Atom(p1), FixOut.Atom(p2)) =>
      combineB(p1, p2).map(FixOut.Atom.apply)
    case (FixOut.Body(p1), FixOut.Body(p2)) =>
      combineB(p1, p2).map(FixOut.Body.apply)
    case (FixOut.Relation(p1), FixOut.Relation(p2)) =>
      combineB(p1, p2).map(FixOut.Relation.apply)
    case (FixOut.ExtensionalRelation(p1), FixOut.ExtensionalRelation(p2)) =>
      combineB(p1, p2).map(FixOut.ExtensionalRelation.apply)
    case _ => throw new IllegalArgumentException(s"Cannot combine outputs of different kind, $out1 and $out2")


trait BaseAbstractInterpreter[C, V, B, RV]:

  // Fixpoint
  def fixpoint: EffectStack ?=> Fixpoint[FixIn, FixOut[V, B, RV]]
  type Fixed = FixIn => FixOut[V, B, RV]

  // Ops & Helper
  def relationOps: RelationOps[C, V, B, RV]

  def boolOps: BooleanOps[B]
  def boolTrue: B = boolOps.boolLit(true)
  def boolFalse: B = boolOps.boolLit(true)
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
      FixOut.Atom(evalAtomExtend(atom))
    case FixIn.Body(body) =>
      FixOut.Atom(evalBodyExtend(body))
    case FixIn.Relation(rel) =>
      FixOut.Relation(evalRelationExtend(rel))
    case FixIn.ExtensionalRelation(rel) =>
      FixOut.ExtensionalRelation(evalExtensionalRelationExtend(rel))
  }

  private inline def external[A](f: Fixed ?=> A): A = f(using fixed)

  def evalProgram(p: Seq[ir.Module]): Unit = external(supplementaryEnv.scoped(p.foreach(evalModule)))

  def evalModule(m: ir.Module)(using Fixed): Unit = 
    supplementaryEnv.setTable(relationOps.unit)
    val relEntryPoints = m.relations.values.filter(_.hasHint(MainHint)) match
      case mainRels if mainRels.nonEmpty => mainRels
      case _ => m.relations.values
    relEntryPoints.foreach { rel =>
      supplementaryEnv.scoped {
        evalRelation(rel)
      }
    }

  protected def merge(rel: RV): Unit =
    ???

  protected def insertIDB(name: ir.Name, rv: RV): Unit =
    val addr = AllocationSiteAddr.Variable(name.name)(true)
    //val oldRVOption = IDB.read(addr)
    //val result = oldRVOption.option(rv)(oldRV => relationOps.union(rv, oldRV))(using withJoinRV)
    IDB.free(addr)
    IDB.write(addr, rv)

  inline def evalRelation(r: ir.Relation)(using rec: Fixed): B = rec(FixIn.Relation(r)) match
    case FixOut.Relation(p) => p
    case _ => throw new IllegalStateException()

  private def makeInitTable(rel: ir.Relation): RV =
    // table with no entries, but not a unit or empty table!
    //  table(X, {()}
    relationOps.make(
      rel.params.map(p => relationOps.makeColumnName(p.name.name)),
      relationOps.embedRows(Seq.empty)
    )

  def evalRelationExtend(r: ir.Relation)(using Fixed): B =
    val paramNames = r.params.map(p => relationOps.makeColumnName(p.name.name))
    val bodyPurity = r.bodies.map { b =>
      val (bodyRes, p) = supplementaryEnv.scoped {
        val pure = evalBody(b)
        val res = relationOps.project(supplementaryEnv.getTable, paramNames)
        (res, pure)
      }
      merge(bodyRes)
      p
    }
    
    insertIDB(r.name, supplementaryEnv.getTable)
    bodyPurity.foldLeft(boolTrue)((acc, pure) => boolOps.and(acc, pure))

  inline def evalExtensionalRelation(r: ir.ExtensionalRelation)(using rec: Fixed): B = rec(FixIn.ExtensionalRelation(r)) match
    case FixOut.ExtensionalRelation(p) => p
    case _ => throw new IllegalStateException()

  def evalExtensionalRelationExtend(r: ir.ExtensionalRelation)(using Fixed): B =
    ???

  inline def evalBody(b: ir.Body)(using rec: Fixed): B = rec(FixIn.Body(b)) match
    case FixOut.Body(p) => p
    case _ => throw new IllegalStateException()

  def evalBodyExtend(b: ir.Body)(using Fixed): B =
    val atPurity = b.atoms.map(a => evalAtom(a))
    atPurity.foldLeft(boolTrue)((acc, res) => boolOps.and(acc, res))

  inline def evalAtom(at: ir.Atom)(using rec: Fixed): B = rec(FixIn.Atom(at)) match
    case FixOut.Atom(p) => p
    case _ => throw new IllegalStateException()

  // I don't think that anything else can be binding in an equality. But if so, subclasses may override this
  def extractVarName(term: ir.Term): ir.Name = term match
    case ir.Var(ref) => ref.name
    case ir.Cast(t, _) => extractVarName(t)

  private final def evalAssign(to: ir.Term, from: ir.Term)(using Fixed): B =
    val (_ , p1) = evalTerm(to)
    val (fs, p2) = evalTerm(from)
    val assignedName = extractVarName(to).name
    val assignedValues = fs.map(v => Seq(v)).toSeq
    val colName = relationOps.makeColumnName(assignedName)
    val res = relationOps.make(Seq(colName), relationOps.embedRows(assignedValues:_*))
    merge(res)
    boolOps.and(p1, p2)

  private final def evalCompare(lhs: ir.Term, rhs: ir.Term, neg: Boolean)(using Fixed): B =
    val (ls, p1) = evalTerm(lhs)
    val (rs, p2) = evalTerm(rhs)
    // TODO: Use relation ops for this?
    val comparisonResults =
      for (r <- ls; l <- rs) yield
        if neg then
          eqOps.neq(r, l) == boolTrue
        else
          eqOps.equ(r, l) == boolTrue

    // if at least one doesn't fail we are good
    val res = if comparisonResults.contains(true) then
      relationOps.unit
    else
      relationOps.empty(Seq())
    merge(res)

    boolOps.and(p1, p2)

  private final def evalEq(lhs: ir.Term, rhs: ir.Term, neg: Boolean)(using Fixed): B = (lhs.mode, rhs.mode, neg) match
    case (Mode.Binding, Mode.Binding, _) => failure(InvalidBindings, s"Equality between two binding terms: $lhs and $rhs")
    case (Mode.Binding, _, false) => evalAssign(lhs, rhs)
    case (_, Mode.Binding, false) => evalAssign(rhs, lhs)
    case (Mode.Bound, Mode.Bound, _) => evalCompare(lhs, rhs, neg)
    case (m1, m2, _) => failure(InvalidBindings, s"Can not evaluate equality with modes: $m1 <> $m2 and negation: $neg")

  def evalArg(arg: ir.Arg)(using Fixed): (Set[V], B) = arg match
    case ir.TermArg(t) => evalTerm(t)
    case ir.WildcardArg() => (Set(), boolTrue)
    case _ => failure(UnknownArg, s"Unknown atom $arg")

  def extractVarName(arg: ir.Arg): Option[ir.Name] = arg match
    case ir.TermArg(t) => Some(extractVarName(t))
    case ir.WildcardArg() => None

  private final def evalCall[R <: ir.ModuleEntry](ref: ir.Ref[R], args: Seq[ir.Arg], neg: Boolean)(using Fixed): B =
    ref.target match
      case Some(r: ir.Relation) =>
        // eval arguments in current scope
        val paramNames = r.params.map(p => relationOps.makeColumnName(p.name.name))
        val (argRes, argPurity) = args.map(evalArg).unzip
        val (relRes, relPurity) = supplementaryEnv.freshScoped {
          val evalContext = relationOps.make(paramNames, relationOps.embedRows(argRes.map(_.toSeq): _*))
          supplementaryEnv.setTable(evalContext)
          val purity = evalRelation(r)
          (supplementaryEnv.getTable, purity)
        }

        // add all variables bound by the call to the context
        val boundVars = args.map(extractVarName)
        val subst = boundVars.zip(paramNames).flatMap {
          case (Some(varName), p) => Some((p, relationOps.makeColumnName(varName.name)))
          case _ => None
        }.toMap
        var res = relationOps.projectAndRename(relRes, subst)
        if neg then
          res = relationOps.markNegative(res)
        merge(res)
        (argPurity :+ relPurity).foldLeft(boolTrue)((acc, res) => boolOps.and(acc, res))
      case Some(r: ir.ExtensionalRelation) =>
        // eval arguments in current scope
        val paramNames = r.params.map(p => relationOps.makeColumnName(p.name.name))
        val (argRes, argPurity) = args.map(evalArg).unzip
        val (relRes, relPurity) = supplementaryEnv.freshScoped {
          val evalContext = relationOps.make(paramNames, relationOps.embedRows(argRes.map(_.toSeq): _*))
          supplementaryEnv.setTable(evalContext)
          val purity = evalExtensionalRelation(r)
          (supplementaryEnv.getTable, purity)
        }

        // add all variables bound by the call to the context
        val boundVars = args.map(extractVarName)
        val subst = boundVars.zip(paramNames).flatMap {
          case (Some(varName), p) => Some((p, relationOps.makeColumnName(varName.name)))
          case _ => None
        }.toMap
        var res = relationOps.projectAndRename(relRes, subst)
        if neg then
          res = relationOps.markNegative(res)
        merge(res)
        (argPurity :+ relPurity).foldLeft(boolTrue)((acc, res) => boolOps.and(acc, res))
      case _ => failure(RefNotFound, s"Can not find call reference $ref")

  def evalAtomExtend(at: ir.Atom)(using Fixed): B = at match
    case ir.Eq(lhs, rhs, neg) => evalEq(lhs, rhs, neg)
    case ir.Call(ref, args, neg) => evalCall(ref, args, neg)
    case ir.ExtensionalCall(ref, args, neg) => evalCall(ref, args, neg)
    case _ => failure(UnknownAtom, s"Unknown atom $at")

  final def evalTerm(term: ir.Term)(using rec: Fixed): (Set[V], B) = rec(FixIn.Term(term)) match
    case FixOut.Term(v, p) => (v, p)
    case _ => throw new IllegalStateException()

  def evalTermExtend(term: ir.Term)(using Fixed): (Set[V], B) = term match
    case ir.Var(ref) =>
      val currentScope = supplementaryEnv.getTable
      val columnName = relationOps.makeColumnName(ref.name.name)
      val varEntry = relationOps.project(currentScope, Seq(columnName))
      // we only have a single value per row, since we projected a single column
      val values = relationOps.entries(varEntry).map(_.head)
      (values.toSet, boolTrue)
    case ir.Cast(t, _) => evalTerm(t)
    case _ => failure(UnknownTerm, s"Unknown term $term")