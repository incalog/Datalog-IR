package inca.ir.analysis.base.logger

import inca.ir.{Body, Name, Relation, Term, Var}
import inca.ir.analysis.{AnalysisKey, AnalysisResult}
import inca.ir.analysis.base.interpreter.{FixIn, FixOut, SupColumn}
import inca.ir.analysis.base.values.Meet
import inca.ir.visitors.IRVisitor
import sturdy.effect.TrySturdy
import sturdy.fix.Logger
import sturdy.values.Join

import scala.collection.mutable

/*
 An analysis logger is used to annotate Datalog AST notes with the computed analysis results.
 Extensions may choose to override this class to guarantee that all AST nodes are annotated.
 */
trait BaseAnalysisAnnotator[V, RV, TV](using joinTV: Join[TV], joinRV: Join[RV], meetTV: Meet[TV])
  extends Logger[FixIn, FixOut[V, RV]]:

  def extractColumns(rv: RV): Seq[String]

  def extractTermValue(col: SupColumn, rv: RV): Option[TV]

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

  class TermAnnotator(termToValue: Map[Term, TV]) extends IRVisitor:
    override def visitTerm(term: Term): Seq[Term] =
      val oldVOption = term.getAnalysisResult(TermKey).headOption.map(_.value)
      val newVOption = termToValue.get(term)
      (oldVOption, newVOption) match
        case (Some(oldV), Some(newV)) =>
          joinTV(oldV, newV).ifChanged(tv => term.storeAnalysisResult(TermResult(tv)))
        case (_, Some(newV)) =>
          term.storeAnalysisResult(TermResult(newV))
        case _ => // nothing
      super.visitTerm(term)

  private val supColumnStack: mutable.Stack[mutable.Map[SupColumn, Term]] = mutable.Stack()

  override def enter(dom: FixIn): Unit = dom match
    case FixIn.Body(_, _, _) => supColumnStack.push(mutable.Map())
    case _ => // nothing

  override def exit(dom: FixIn, codom: TrySturdy[FixOut[V, RV]]): Unit = (dom, codom.get) match
    case (FixIn.Term(t), Some(FixOut.Term(supName))) =>
      supColumnStack.head.put(supName, t)
    case (FixIn.Body(rel, ix, _), None) => // body failed
      supColumnStack.pop()
    case (FixIn.Body(rel, ix, _), Some(FixOut.Body(rv, rawBody))) =>
      // map all terms to values
      val supColumnToTerm = supColumnStack.pop()
      var termToValue = supColumnToTerm.flatMap { (supCol, term) =>
        extractTermValue(supCol, rawBody).map(term -> _)
      }
      // we might miss some variables terms we have not visited in the fixpoint
      val collectedSupColumns = supColumnToTerm.keys.toSet
      val allSubColumns = extractColumns(rawBody).toSet
      termToValue = termToValue ++= allSubColumns.diff(collectedSupColumns).flatMap { missingCol =>
        extractTermValue(missingCol, rawBody).map(Var(Name(missingCol)) -> _)
      }
      // annotate the terms
      TermAnnotator(termToValue.toMap).visitBody(rel.bodies(ix))
      // annotate the body
      updateBodyResult(rel.bodies(ix), rv)
    case (FixIn.EnterRelation(r, _), Some(FixOut.Relation(rv))) =>
      // annotate the relation
      updateRelationResult(r, rv)
    case _ => // nothing

  def updateTermResult(term: Term, value: TV): Unit =
    val newRes = term.getAnalysisResult(TermKey).headOption match
      case Some(tr) => TermResult(joinTV(tr.value, value).get)
      case _ => TermResult(value)
    term.storeAnalysisResult(newRes)

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
