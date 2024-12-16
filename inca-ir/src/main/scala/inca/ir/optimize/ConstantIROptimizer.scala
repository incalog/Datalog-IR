package inca.ir.optimize

import inca.ir
import inca.ir.analysis.IRConstantAbstractInterpreter
import inca.ir.analysis.base.values.{ConstantRelation, Value, Top as TopV}
import inca.ir.{Atom, Body, Call, Eq, ExtensionalRelation, Name, Relation, Term, Var}
import inca.ir.extension.arithmetic.analysis.interpreter.{ConstantDoubleV, ConstantIntV}
import inca.ir.extension.string.analysis.interpreter.ConstantStringV
import inca.ir.extension.data.analysis.interpreter.ConstantDataV
import inca.ir.extension.arithmetic as irarith
import inca.ir.extension.string as irstr
import inca.ir.extension.data as irdata
import sturdy.values.Topped

//import java.awt.Toolkit
//import java.awt.datatransfer.StringSelection

// TODO: This is still wrong. Fix this tomorrow
// Currently this optimizer only works with arith + string + data
class ConstantIROptimizer(val assumeEdbIsNotEmpty: Boolean = false) extends BaseIROptimizer[Value, ConstantRelation, Value]:
  override def name: String = "Constant Optimizer"

  override val abstractInterpreter: IRConstantAbstractInterpreter = new IRConstantAbstractInterpreter()

  import abstractInterpreter.analysisAnnotator.{RelationKey, TermKey, BodyKey}

  override def getTermResult(term: Term): Set[Value] =
    term.getAnalysisResult(TermKey).map(_.value)

  override def getBodyResult(body: Body): Set[ConstantRelation] =
    body.getAnalysisResult(BodyKey).map(_.res)

  override def getRelationResult(relation: Relation): Set[ConstantRelation] =
    relation.getAnalysisResult(RelationKey).map(_.res)

  private def relationAlwaysFails(relation: Relation): Boolean =
    getRelationResult(relation)
      .map(_.empty).forall {
        case Topped.Actual(v) => v
        case Topped.Top => false
      }

  /*private def relationAlwaysSucceeds(relation: Relation): Boolean =
    getRelationResult(relation)
      .map(_.empty).forall {
        case Topped.Actual(v) => !v
        case Topped.Top => false
      }*/


  override def visitProgram(modules: Seq[ir.Module], dependencies: Seq[ir.Module]): Seq[ir.Module] =
    // We could make this more precise, by setting the `empty` flag correctly on edb relations
    modules.foreach { m =>
      m.entries.foreach {
        case (_, ExtensionalRelation(n, params)) =>
          val (paramNames, args) = params.map(p => (p.name.name, TopV)).unzip
          val empty = if (assumeEdbIsNotEmpty) Topped.Actual(false) else Topped.Top
          abstractInterpreter.insertEDB(n.name, ConstantRelation(paramNames, args, empty))
        case _ => // nothing
      }
    }

    val r = super.visitProgram(modules, dependencies)

    /*val stringSelection = new StringSelection(s"digraph G {${abstractInterpreter.graphBuilder.get.toGraphViz}\n}")
    val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
    clipboard.setContents(stringSelection, null)*/

    r

  private var valueCache: Map[Value, Term] = Map()

  private def valueToTerm(value: Value): Option[Term] = valueCache.get(value) match
    case Some(res) => Some(res)
    case _ =>
      val result = value match
        case ConstantIntV(v1) => Some(irarith.IntNum(v1))
        case ConstantDoubleV(v1) => Some(irarith.DoubleNum(v1))
        case ConstantStringV(v1) => Some(irstr.StringLit(v1))
        case ConstantDataV(dataDef, caseDef, args) =>
          val argsV = args.flatMap(valueToTerm)
          if (argsV.size != args.size)
            None
          else
            Some(irdata.Construct(caseDef.name, argsV))
        case _ => None
      if (result.isDefined)
        valueCache += value -> result.get
      result

  override def visitRelation(relation: Relation): Seq[Relation] =
    // Remove empty relations. We know that there can not be any call site for these relations, because a failing
    // call will lead to a failing body at the call site. Except if the call is a negative call, in which case it
    // always succeeds.
    if (relationAlwaysFails(relation))
      Seq()
    else
      super.visitRelation(relation)

  override def visitBody(body: Body): Seq[Body] =
    getBodyResult(body).headOption match
      case Some(res: ConstantRelation) if res.empty == Topped.Actual(true) =>
        // Remove failing bodies
        Seq()
      case Some(res: ConstantRelation) =>
        // We might have removed equality constraints for parameters, add them back
        // This also constraints the body if we have information about the parameters
        // TODO: res.cols are not the parameter names!
        val paramConstraints = res.cols.zip(res.rows).flatMap((c, r) => valueToTerm(r).map(t => Eq(Var(Name(c)), t)))
        super.visitBody(body).map(b => Body(paramConstraints ++ b.atoms))
      case _ => super.visitBody(body)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    // Remove equality constraints that always hold
    case Eq(lhs, rhs, neg) =>
      (getTermResult(lhs).headOption, getTermResult(rhs).headOption) match
        case (Some(v1), Some(v2)) => (valueToTerm(v1), valueToTerm(v2)) match
          case (Some(t1), Some(t2)) if !neg && (t1 == t2) => Seq()
          case (Some(t1), Some(t2)) if neg && (t1 != t2) => Seq()
          case _ => super.visitAtom(atom)
        case _ => super.visitAtom(atom)
    // A negative call to a failing relation always succeeds
    case Call(ref, args, true) =>
      ref.target match
        case Some(r: Relation) if relationAlwaysFails(r) => Seq()
        case _ => super.visitAtom(atom)
    case _ => super.visitAtom(atom)

  // Replace all terms with their constants if possible
  override def visitTerm(term: Term): Seq[Term] = getTermResult(term)
    .headOption
    .flatMap(valueToTerm)
    .map(Seq(_))
    .getOrElse(super.visitTerm(term))



