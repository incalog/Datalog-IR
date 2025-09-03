package inca.ir.optimize

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.{Body, ExtensionalRelation, Relation, Term}
import inca.ir.analysis.{IRTypeAbstractInterpreter, TypeEdbConfig, TypeValue}
import inca.ir.analysis.base.values.{AbstractRelation, Value}
import inca.ir.extension.tuple.analysis.{AbstractEdbConfig, EdbConfig}
import sturdy.values.Topped

/*
  We can not really optimize anything with just the type information. This class is just here
  to be a proof of concept to show how the abstract concrete results can be used in the end.
 */
class TypeIROptimizer(
                       override val computeControlEvents: Boolean, 
                       override val edbConfig: EdbConfig[AbstractRelation] = TypeEdbConfig.default) 
  extends BaseIROptimizer[Value, AbstractRelation, Value]:
  
  override def name: String = "Type Optimizer"

  override val abstractInterpreter: IRTypeAbstractInterpreter = new IRTypeAbstractInterpreter()

  import abstractInterpreter.analysisAnnotator.{ TermKey, RelationKey, BodyKey }

  override def getTermResult(term: Term): Set[Value] =
    term.getAnalysisResult(TermKey).map(_.value)

  override def getBodyResult(body: Body): Set[AbstractRelation] =
    body.getAnalysisResult(BodyKey).map(_.res)

  override def getRelationResult(relation: Relation): Set[AbstractRelation] =
    relation.getAnalysisResult(RelationKey).map(_.res)

  override def analyzeProgram(modules: Seq[ir.Module]): Unit =
    modules.foreach { m =>
      m.entries.foreach {
        case (_, ExtensionalRelation(n, params)) =>
          val aRel = edbConfig.abstractExtensionalRelation(n, params)
          abstractInterpreter.insertEDB(n.name, aRel)
        case _ => // nothing
      }
    }
    super.analyzeProgram(modules)

  private def relationAlwaysFails(relation: Relation): Boolean =
    getRelationResult(relation).map(_.empty).forall {
      case Topped.Actual(v) => v
      case Topped.Top => false
    }

  override def visitRelation(relation: Relation): Seq[Relation] = preserveHints(relation) {
    if (relationAlwaysFails(relation))
      Seq()
    else
      super.visitRelation(relation)
  }

  override def visitBody(body: Body): Seq[Body] = preserveHints(body) {
    // Remove failing bodies
    getBodyResult(body).headOption match
      case Some(res: AbstractRelation) if res.empty.isTrue => Seq()
      case _ => super.visitBody(body)
  }



