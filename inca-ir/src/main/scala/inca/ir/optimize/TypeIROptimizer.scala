package inca.ir.optimize

import inca.ir
import inca.ir.{Body, ExtensionalRelation, Relation, Term}
import inca.ir.analysis.IRTypeAbstractInterpreter
import inca.ir.analysis.base.values.{TypeRelation, TypeValue}
import sturdy.values.Topped

/*
  We can not really optimize anything with just the type information. This class is just here
  to be a proof of concept to show how the abstract interpreter results can be used in the end.
 */
class TypeIROptimizer extends BaseIROptimizer[TypeValue, TypeRelation, TypeValue]:
  override def name: String = "Type Optimizer"

  override val abstractInterpreter: IRTypeAbstractInterpreter = new IRTypeAbstractInterpreter()

  import abstractInterpreter.analysisAnnotator.{ TermKey, RelationKey, BodyKey }

  override def getTermResult(term: Term): Set[TypeValue] =
    term.getAnalysisResult(TermKey).map(_.value)

  override def getBodyResult(body: Body): Set[TypeRelation] =
    body.getAnalysisResult(BodyKey).map(_.res)

  override def getRelationResult(relation: Relation): Set[TypeRelation] =
    relation.getAnalysisResult(RelationKey).map(_.res)

  override def visitProgram(modules: Seq[ir.Module], dependencies: Seq[ir.Module]): Seq[ir.Module] =
    // We could make this more precise, by setting the `empty` flag correctly
    modules.foreach { m =>
      m.entries.foreach {
        case (_, ExtensionalRelation(n, params)) =>
          val (paramNames, tys) = params.map(p => (p.name.name, TypeValue.AType(p.ty))).unzip
          abstractInterpreter.insertEDB(n.name, TypeRelation(paramNames, tys, Topped.Top))
        case _ => // nothing
      }
    }

    super.visitProgram(modules, dependencies)

  override def visitRelation(relation: Relation): Seq[Relation] =
    // we could remove empty relations here
    /*val isEmpty = getRelationResult(relation).map(_.empty).forall {
      case Topped.Actual(v) => v
      case Topped.Top => false
    }*/
    super.visitRelation(relation)



