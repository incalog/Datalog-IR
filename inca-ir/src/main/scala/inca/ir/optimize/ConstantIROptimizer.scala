package inca.ir.optimize

import inca.ir
import inca.ir.analysis.IRConstantAbstractInterpreter
import inca.ir.analysis.base.values.{ConstantRelation, Value, Top as TopV}
import inca.ir.{Atom, Body, ExtensionalRelation, Relation, Term, Eq, Var, Name}
import inca.ir.extension.arithmetic.analysis.interpreter.{ConstantIntV, ConstantDoubleV}
import inca.ir.extension.string.analysis.interpreter.ConstantStringV
import inca.ir.extension.data.analysis.interpreter.ConstantDataV
import inca.ir.extension.arithmetic as irarith
import inca.ir.extension.string as irstr
import inca.ir.extension.data as irdata
import sturdy.values.Topped


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

  override def visitProgram(modules: Seq[ir.Module], dependencies: Seq[ir.Module]): Seq[ir.Module] =
    // We could make this more precise, by setting the `empty` flag correctly
    modules.foreach { m =>
      m.entries.foreach {
        case (_, ExtensionalRelation(n, params)) =>
          val (paramNames, args) = params.map(p => (p.name.name, TopV)).unzip
          val empty = if (assumeEdbIsNotEmpty) Topped.Actual(false) else Topped.Top
          abstractInterpreter.insertEDB(n.name, ConstantRelation(paramNames, args, empty))
        case _ => // nothing
      }
    }

    super.visitProgram(modules, dependencies)

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
    // TODO: Remove empty relations and everything that is transitively effected
    /*val isEmpty = getRelationResult(relation).map(_.empty).forall {
      case Topped.Actual(v) => v
      case Topped.Top => false
    }*/
    super.visitRelation(relation)

  override def visitBody(body: Body): Seq[Body] =
    getBodyResult(body).headOption match
      case Some(res: ConstantRelation) if res.empty == Topped.Actual(true) =>
        // remove empty bodies
        Seq()
      case Some(res: ConstantRelation) =>
        // We might have removed equality constraints for parameters, add them back
        val paramConstraints = res.cols.zip(res.rows).flatMap((c, r) => valueToTerm(r).map(t => Eq(Var(Name(c)), t)))
        println(s"Param constraints: $paramConstraints")
        super.visitBody(body).map(b => Body(paramConstraints ++ b.atoms))
      case _ => super.visitBody(body)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    // Remove unnecessary equality constraints
    case Eq(lhs, rhs, neg) =>
      (getTermResult(lhs).headOption, getTermResult(rhs).headOption) match
        case (Some(v1), Some(v2)) => (valueToTerm(v1), valueToTerm(v2)) match
          case (Some(t1), Some(t2)) if t1 == t2 => Seq()
          case _ => super.visitAtom(atom)
        case _ => super.visitAtom(atom)
    case _ => super.visitAtom(atom)

  override def visitTerm(term: Term): Seq[Term] = term match
    // Replace variables with their constants
    case Var(ref) =>
      getTermResult(term)
        .headOption
        .flatMap(valueToTerm)
        .map(Seq(_))
        .getOrElse(super.visitTerm(term))
    case _ => super.visitTerm(term)



