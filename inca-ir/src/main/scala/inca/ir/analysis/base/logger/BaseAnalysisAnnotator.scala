package inca.ir.analysis.base.logger

import inca.ir.{Arg, Atom, Body, Call, Eq, ExtensionalCall, Relation, Term, TermArg, Var, WildcardArg}
import inca.ir.analysis.{AnalysisKey, AnalysisResult}
import inca.ir.analysis.base.interpreter.{FixIn, FixOut, SupColumn}
import inca.ir.analysis.base.values.{BaseMeetV, Meet}
import inca.ir.printer.IRDebugPrinter
import inca.util.Color
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
  def isDefinitelyEmpty(rv: RV): Boolean = false

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
   * Transaction class to trace changes during the evaluation of a single body.
   */
  class Transaction:
    var changes: Map[Term, TermResult] = Map()
    var terms: Set[(Long, Term)] = Set()

    def applyChanges(): Unit =
      terms.foreach { (id, term) =>
        // Join all annotations in this body with the result from the previous body
        val result = changes(term)
        val newResult = term.getAnalysisResult(TermKey).headOption match
          case Some(tr@TermResult(v)) => TermResult(joinTV(v, result.value).get)
          case _ => TermResult(result.value)
        term.storeAnalysisResult(newResult)
      }

  private var transactions: Seq[Transaction] = Seq()
  private def currentTransaction: Option[Transaction] = transactions.headOption
  private def logChange(term: Term, result: TermResult): Unit =
    currentTransaction.foreach { trans =>
      trans.terms += term.id -> term
      trans.changes.get(term) match
        case Some(TermResult(v)) =>
          // meet the old value with the binding site
          meetTV(v, result.value).ifChanged(trans.changes += term -> TermResult(_))
        case _ =>
          // the binding position of a variable is the first time it's visited
          trans.changes += term -> result
    }

  protected def startContextTransaction(): Unit =
    val transaction = new Transaction
    transactions = transaction +: transactions

  protected def commitContextTransaction(): Unit =
    if (transactions.isEmpty)
      throw IllegalStateException("No transaction to commit.")
    currentTransaction.foreach(_.applyChanges())
    transactions = transactions.tail

  protected def abortContextTransaction(): Unit =
    if (transactions.isEmpty)
      throw IllegalStateException("No transaction to abort.")
    transactions = transactions.tail


  override def enter(dom: FixIn): Unit = dom match
    case FixIn.Body(rel, ix, _) => startContextTransaction()
    case _ => // nothing

  override def exit(dom: FixIn, codom: TrySturdy[FixOut[V, RV]]): Unit = (dom, codom.get) match
    case (FixIn.Term(t), Some(FixOut.Term(supName))) =>
      extractTermValue(supName).foreach(updateTermResult(t, _))
    case (FixIn.Atom(at, _), Some(FixOut.Atom())) =>
      updateAtomResult(at)
    case (FixIn.Body(rel, ix, _), Some(FixOut.Body(rv))) =>
      if (isDefinitelyEmpty(rv))
        // Don't annotate terms based on failing bodies
        abortContextTransaction()
      else
        commitContextTransaction()
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
    logChange(term, TermResult(value))

  def updateVariables(term: Term): Unit =
    term.vars.foreach(t => extractTermValue(t.name.name).map(updateTermResult(t, _)))

  def varIsBound(v: Var): Boolean =
    extractTermValue(v.name.name).isDefined

  // Not all AST-term nodes are visited. Handle the missing cases explicitly in this method.
  def updateAtomResult(at: Atom): Unit = at match
    case Call(_, args, _) => updateArgResult(args)
    case ExtensionalCall(_, args, _) => updateArgResult(args)
    case Eq(_, _, true) => // Ignore comparison
    case Eq(v1: Var, v2: Var, _) if varIsBound(v1) && varIsBound(v2) => // Ignore comparison
    case Eq(lhs, rhs, _) =>
      updateVariables(lhs)
      updateVariables(rhs)
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
