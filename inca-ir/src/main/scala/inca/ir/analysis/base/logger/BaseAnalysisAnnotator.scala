package inca.ir.analysis.base.logger

import inca.ir.{Module, Arg, Atom, Body, Call, Eq, ExtensionalCall, Relation, Term, TermArg, Var, WildcardArg}
import inca.ir.analysis.{AnalysisKey, AnalysisResult}
import inca.ir.analysis.base.interpreter.{FixIn, FixOut, SupColumn}
import inca.ir.analysis.base.values.Meet
import inca.ir.visitors.IRVisitor
import sturdy.effect.TrySturdy
import sturdy.fix.Logger
import sturdy.values.Join

import scala.compiletime.uninitialized
import scala.collection.immutable.{AbstractSet, SortedSet}
import scala.collection.mutable

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

  /**
   * During logging, we might over approximate some terms. This class computes the meet for each term
   * and refines the results.
   */
  class TermRefinement extends IRVisitor:
    val terms: mutable.ListBuffer[Term] = new mutable.ListBuffer
    val values: mutable.Map[Term, TV] = mutable.Map.empty

    override def visitRelation(relation: Relation): Seq[Relation] =
      val bs = relation.bodies.flatMap { b =>
        terms.clear()
        values.clear()
        val visitedBody = visitBody(b)
        terms.foreach(t => values.get(t).foreach(v => t.storeAnalysisResult(TermResult(v))))
        visitedBody
      }
      Seq(Relation(relation.name, relation.params.flatMap(visitParam), bs))

    override def visitTerm(term: Term): Seq[Term] =
      val oldVOption = term.getAnalysisResult(TermKey).headOption.map(_.value)
      val newVOption = values.get(term)
      (oldVOption, newVOption) match
        case (Some(oldV), Some(newV)) =>
          terms += term
          meetTV(oldV, newV).ifChanged(values.put(term, _))
        case (Some(oldV), _) =>
          terms += term
          values.put(term, oldV)
        //case (_, Some(newV)) => values.put(term, newV)
        case _ => // nothing
      super.visitTerm(term)


  def refineTerms(mod: Module): Unit =
    val refinement = new TermRefinement
    refinement.visitProgram(Seq(mod))

  override def enter(dom: FixIn): Unit = ()

  override def exit(dom: FixIn, codom: TrySturdy[FixOut[V, RV]]): Unit = (dom, codom.get) match
    case (FixIn.Term(t), Some(FixOut.Term(supName))) =>
      extractTermValue(supName).foreach(updateTermResult(t, _))
    case (FixIn.Atom(at, _), Some(FixOut.Atom())) =>
      updateAtomResult(at)
    case (FixIn.Assign(toTerm, fromTerm), Some(FixOut.Assign(toSup, fromSup))) =>
      //extractTermValue(toSup).foreach(updateTermResult(toTerm, _))
    case (FixIn.Body(rel, ix, _), Some(FixOut.Body(rv))) =>
      updateBodyResult(rel.bodies(ix), rv)
    case (FixIn.EnterRelation(r, _), Some(FixOut.Relation(rv))) =>
      updateRelationResult(r, rv)
    case _ => // nothing

  def extractTermAndVarName(arg: Arg): Option[(Term, String)] = arg match
    case TermArg(t@Var(ref)) => Some((t, ref.name.name))
    case WildcardArg() => None
    case _ => None

  def updateTermResult(term: Term, value: TV): Unit =
    val newRes = term.getAnalysisResult(TermKey).headOption match
      case Some(tr) => TermResult(joinTV(tr.value, value).get)
      case _ => TermResult(value)
    term.storeAnalysisResult(newRes)

  // This is an over approximation
  def updateArgResult(args: Seq[Arg]): Unit =
    args.flatMap(extractTermAndVarName).foreach { (term, varName) =>
      extractTermValue(varName).foreach(updateTermResult(term, _))
    }

  // This is an over approximation
  def updateVariableResult(lhs: Term, rhs: Term): Unit =
    val lhsRes = lhs.getAnalysisResult(TermKey).headOption
    val rhsRes = rhs.getAnalysisResult(TermKey).headOption
    (lhsRes, rhsRes) match
      case (Some(ltr), Some(rtr)) =>
        val join = joinTV(ltr.value, rtr.value).get
        lhs.storeAnalysisResult(TermResult(join))
      case (Some(ltr), None) =>
        rhs.storeAnalysisResult(ltr)
      case (None, Some(rtr)) =>
        lhs.storeAnalysisResult(rtr)
      case _ => // nothing

  // Not all AST-term nodes are visited. Handle the missing cases explicitly in this method.
  def updateAtomResult(at: Atom): Unit = at match
    case Call(_, args, _) => updateArgResult(args)
    case ExtensionalCall(_, args, _) => updateArgResult(args)
    case Eq(_, _, true) => // Ignore negative comparison
    case Eq(lhs, rhs, false) => updateVariableResult(lhs, rhs)
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
