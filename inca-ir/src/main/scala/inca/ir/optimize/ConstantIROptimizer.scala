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

/*
  TODO: This class is currently pretty hacky. I'm just using it for debugging an error in my interpreter.
 */
class ConstantIROptimizer extends BaseIROptimizer[Value, ConstantRelation, Value]:
  override def name: String = "Constant Optimizer"

  override val abstractInterpreter: IRConstantAbstractInterpreter = new IRConstantAbstractInterpreter()

  private lazy val eqOps = abstractInterpreter.eqOps

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
          abstractInterpreter.insertEDB(n.name, ConstantRelation(paramNames, args, Topped.Top))
        case _ => // nothing
      }
    }

    super.visitProgram(modules, dependencies)

  private def valueToTerm(value: Value): Option[Term] = value match
    case ConstantIntV(v1) => Some(irarith.IntNum(v1))
    case ConstantDoubleV(v1) => Some(irarith.DoubleNum(v1))
    case ConstantStringV(v1) => Some(irstr.StringLit(v1))
    // Not sure if we want to replace ADTs
    case _ => None


  override def visitRelation(relation: Relation): Seq[Relation] =
    // TODO: Remove empty relations
    /*val isEmpty = getRelationResult(relation).map(_.empty).forall {
      case Topped.Actual(v) => v
      case Topped.Top => false
    }*/
    super.visitRelation(relation)

  override def visitBody(body: Body): Seq[Body] =
    // remove empty bodies
    getBodyResult(body).headOption match
      case Some(res: ConstantRelation) if res.empty == Topped.Actual(true) => Seq()
      case Some(res: ConstantRelation) =>
        val paramConstraints = res.cols.zip(res.rows).flatMap { (c, r) =>
          if (r.isActual)
            valueToTerm(r).map(t => Eq(Var(Name(c)), t))
          else
            None
        }
        super.visitBody(body).map { b =>
          Body(paramConstraints ++ b.atoms)
        }
      case _ => super.visitBody(body)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    // Remove unnecessary equality constraints
    case Eq(lhs, rhs, neg) =>
      (getTermResult(lhs).headOption, getTermResult(rhs).headOption) match
        // TODO: Probably problematic for ADTs
        case (Some(v1), Some(v2)) if eqOps.equ(v1, v2) == Topped.Actual(true) => Seq()
        case _ => super.visitAtom(atom)
    case _ => super.visitAtom(atom)

  override def visitTerm(term: Term): Seq[Term] = term match
    case Var(ref) => getTermResult(term).headOption match
      case Some(v) => valueToTerm(v) match
        case Some(value) => Seq(value)
        case _ => super.visitTerm(term)
      case _ => super.visitTerm(term)
    case _ => super.visitTerm(term)



