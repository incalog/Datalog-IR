package inca.ir.analysis.base.logger

import inca.ir.{Arg, Atom, Call, Eq, ExtensionalCall, Relation, Term, TermArg, Var, WildcardArg}
import inca.ir.analysis.{AnalysisKey, AnalysisResult, SupplementaryTable}
import inca.ir.analysis.base.interpreter.{FixIn, FixOut, SupColumn}
import sturdy.effect.TrySturdy
import sturdy.fix.{Contextual, Logger}

trait BaseAnalysisLogger[V, RV, TV] extends Logger[FixIn, FixOut[V, RV]]:
  def extractTermValue(col: SupColumn): TV

  case object TermKey extends AnalysisKey:
    override val key: String = "Term"
    override type Result = TermResult

  case class TermResult(value: TV) extends AnalysisResult:
    val result: TermResult = this
    override val akey: TermKey.type = TermKey

  case object RelationKey extends AnalysisKey:
    override val key: String = "Relation"
    override type Result = RelationResult

  case class RelationResult(res: RV) extends AnalysisResult:
    val result: RelationResult = this
    override val akey: RelationKey.type = RelationKey

  override def enter(dom: FixIn): Unit = () // nothing

  def extractTermAndVarName(arg: Arg): Option[(Term, String)] = arg match
    case TermArg(t@Var(ref)) => Some((t, ref.name.name)) 
    case WildcardArg() => None
    case _ => None

  def storeTermResult(term: Term, value: TV): Unit =
    term.storeAnalysisResult(TermResult(value))

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

  override def exit(dom: FixIn, codom: TrySturdy[FixOut[V, RV]]): Unit = (dom, codom.get) match
    case (FixIn.Term(t), Some(FixOut.Term(supName))) => storeTermResult(t, extractTermValue(supName))
    case (FixIn.Atom(at), Some(FixOut.Atom())) => storeAtomResult(at)
    case (FixIn.EnterRelation(r, _), Some(FixOut.Relation(rv))) => storeRelationResult(r, rv)
    case  _ => // nothing
