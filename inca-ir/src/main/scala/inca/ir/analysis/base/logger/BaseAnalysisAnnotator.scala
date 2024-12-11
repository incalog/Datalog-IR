package inca.ir.analysis.base.logger

import inca.ir.{Arg, Atom, Body, Call, Eq, ExtensionalCall, Relation, Term, TermArg, Var, WildcardArg}
import inca.ir.analysis.{AnalysisKey, AnalysisResult}
import inca.ir.analysis.base.interpreter.{FixIn, FixOut, SupColumn}
import inca.util.Color
import sturdy.effect.TrySturdy
import sturdy.fix.Logger

/*
 An analysis logger is used to annotate Datalog AST notes with the computed analysis results.
 Extensions may choose to override this class to guarantee that all AST nodes are annotated.
 */
trait BaseAnalysisAnnotator[V, RV, TV] extends Logger[FixIn, FixOut[V, RV]]:
  def extractTermValue(col: SupColumn): TV

  case object TermKey extends AnalysisKey:
    override val key: String = "Term"
    override val color: Color = Color.Blue
    override type Result = TermResult

  case class TermResult(value: TV) extends AnalysisResult:
    val result: TermResult = this
    override val akey: TermKey.type = TermKey
    override def toString: String = value.toString

  case object BodyKey extends AnalysisKey:
    override val key: String = "Body"
    override val color: Color = Color.Yellow
    override type Result = RelationResult

  case class BodyResult(res: RV) extends AnalysisResult:
    val result: BodyResult = this
    override val akey: BodyKey.type = BodyKey
    override def toString: String = res.toString

  case object RelationKey extends AnalysisKey:
    override val key: String = "Relation"
    override val color: Color = Color.Green
    override type Result = RelationResult

  case class RelationResult(res: RV) extends AnalysisResult:
    val result: RelationResult = this
    override val akey: RelationKey.type = RelationKey
    override def toString: String = res.toString

  override def enter(dom: FixIn): Unit = () // nothing

  def extractTermAndVarName(arg: Arg): Option[(Term, String)] = arg match
    case TermArg(t@Var(ref)) => Some((t, ref.name.name))
    case WildcardArg() => None
    case _ => None

  def storeTermResult(term: Term, value: TV): Unit =
    term.storeAnalysisResult(TermResult(value))

  // Not all AST-term nodes are visited. Handle the missing cases explicitly in this method.
  def storeAtomResult(at: Atom): Unit = at match
    case Call(ref, args, neg) =>
      args.flatMap(extractTermAndVarName).foreach { (term, varName) =>
        storeTermResult(term, extractTermValue(varName))
      }
    case ExtensionalCall(ref, args, neg) =>
      args.flatMap(extractTermAndVarName).foreach { (term, varName) =>
        storeTermResult(term, extractTermValue(varName))
      }
    case Eq(lhs@Var(ref), rhs, false) if lhs.typ.get.mode.isBinding =>
      storeTermResult(lhs, extractTermValue(ref.name.name))
    case Eq(lhs, rhs@Var(ref), false) if rhs.typ.get.mode.isBinding =>
      storeTermResult(rhs, extractTermValue(ref.name.name))
    case _ => // nothing

  def storeRelationResult(rel: Relation, value: RV): Unit =
    rel.storeAnalysisResult(RelationResult(value))

  def storeBodyResult(body: Body, value: RV): Unit =
    body.storeAnalysisResult(BodyResult(value))

  override def exit(dom: FixIn, codom: TrySturdy[FixOut[V, RV]]): Unit = (dom, codom.get) match
    case (FixIn.Term(t), Some(FixOut.Term(supName))) => storeTermResult(t, extractTermValue(supName))
    case (FixIn.Atom(at), Some(FixOut.Atom())) => storeAtomResult(at)
    case (FixIn.Body(b, _), Some(FixOut.Body(rv))) => storeBodyResult(b, rv)
    case (FixIn.EnterRelation(r, _), Some(FixOut.Relation(rv))) => storeRelationResult(r, rv)
    case  _ => // nothing
