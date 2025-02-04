package inca.ir.analysis.base.logger

import inca.ir.{Arg, Atom, Body, Call, Eq, ExtensionalCall, Relation, Term, TermArg, Var, WildcardArg}
import inca.ir.analysis.{AnalysisKey, AnalysisResult}
import inca.ir.analysis.base.interpreter.{FixIn, FixOut, SupColumn}
import inca.ir.analysis.base.values.Meet
import sturdy.effect.TrySturdy
import sturdy.fix.Logger
import sturdy.values.Join

import scala.compiletime.uninitialized
import scala.collection.immutable.{AbstractSet, SortedSet}

/*
 An analysis logger is used to annotate Datalog AST notes with the computed analysis results.
 Extensions may choose to override this class to guarantee that all AST nodes are annotated.
 */
trait BaseAnalysisAnnotator[V, RV, TV](using joinTV: Join[TV], joinRV: Join[RV], meetTV: Meet[TV])
  extends Logger[FixIn, FixOut[V, RV]]:

  def extractTermValue(col: SupColumn): Option[TV]

  case object TermKey extends AnalysisKey:
    override val key: String = "Term"
    override type Result = TermResult

  case class TermResult(value: TV) extends AnalysisResult:
    val result: TermResult = this
    override val akey: TermKey.type = TermKey
    override def toString: String = value.toString

  case object BodyKey extends AnalysisKey:
    override val key: String = "Body"
    override type Result = BodyResult

  case class BodyResult(res: RV) extends AnalysisResult:
    val result: BodyResult = this
    override val akey: BodyKey.type = BodyKey
    override def toString: String = res.toString

  case object RelationKey extends AnalysisKey:
    override val key: String = "Relation"
    override type Result = RelationResult

  case class RelationResult(res: RV) extends AnalysisResult:
    val result: RelationResult = this
    override val akey: RelationKey.type = RelationKey
    override def toString: String = res.toString


  override def enter(dom: FixIn): Unit = ()

  override def exit(dom: FixIn, codom: TrySturdy[FixOut[V, RV]]): Unit = (dom, codom.get) match
    case (FixIn.Term(t), Some(FixOut.Term(supName))) =>
      extractTermValue(supName).foreach(updateTermResult(t, _))
    case (FixIn.Atom(at, _), Some(FixOut.Atom())) =>
      updateAtomResult(at)
    case (FixIn.Body(rel, ix, _), Some(FixOut.Body(rv))) =>
      updateBodyResult(rel.bodies(ix), rv)
    case (FixIn.EnterRelation(r, _), Some(FixOut.Relation(rv))) =>
      updateRelationResult(r, rv)
    case _ => // nothing

  def extractTermAndVarName(arg: Arg): Option[(Term, String)] = arg match
    case TermArg(t@Var(ref)) => Some((t, ref.name.name))
    case WildcardArg() => None
    case _ => None

  def updateArgResult(args: Seq[Arg]): Unit =
    args.flatMap(extractTermAndVarName).foreach { (term, varName) =>
      extractTermValue(varName).foreach(updateTermResult(term, _))
    }

  def updateTermResult(term: Term, value: TV): Unit =
    val newRes = term.getAnalysisResult(TermKey).headOption match
      case Some(tr) => TermResult(joinTV(tr.value, value).get)
      case _ => TermResult(value)
    term.storeAnalysisResult(newRes)

  def updateVariables(binding: Term, bound: Term): Unit =
    bound.getAnalysisResult(TermKey).headOption.foreach(binding.storeAnalysisResult)

  def varIsBound(v: Var): Boolean =
    extractTermValue(v.name.name).isDefined

  // Not all AST-term nodes are visited. Handle the missing cases explicitly in this method.
  def updateAtomResult(at: Atom): Unit = at match
    case Call(_, args, _) => updateArgResult(args)
    case ExtensionalCall(_, args, _) => updateArgResult(args)
    case Eq(_, _, true) => // Ignore negative comparison
    case Eq(lhs, rhs, _) if lhs.mode.isBinding => updateVariables(lhs, rhs)
    case Eq(lhs, rhs, _) if rhs.mode.isBinding => updateVariables(rhs, lhs)
    case _ => // nothing

  def updateRelationResult(rel: Relation, value: RV): Unit =
    val newResult = rel.getAnalysisResult(RelationKey).headOption match
      case Some(RelationResult(v)) => RelationResult(joinRV(v, value).get)
      case _ => RelationResult(value)
    rel.storeAnalysisResult(newResult)

  def updateBodyResult(body: Body, value: RV): Unit =
    val newResult = body.getAnalysisResult(BodyKey).headOption match
      case Some(BodyResult(v)) => BodyResult(joinRV(v, value).get)
      case _ => BodyResult(value)
    body.storeAnalysisResult(newResult)
