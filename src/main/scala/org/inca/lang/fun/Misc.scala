package org.inca.lang.fun

import org.inca.lang.core.Content._
import org.inca.lang.core.{IIncaModuleImport, IPatternModule}
import org.inca.lang.gp.Content.{EmptyGraphPatternContent, PatternParameter}
import org.inca.meta.MetaElements.MetaElement

trait IPatternFunctionModuleContent extends IPatternModuleContent

trait IReturnContainer

trait IBinary {
  val left: IExpression
  val right: IExpression
}

abstract class AbstractBinary extends IBinary

case class CostConsistentAttribute()

case class FunHorizontalLineContent() extends HorizontalLineContent with IPatternFunctionModuleContent

case class PatternFunction(override val name: String,
                           override val parameters: Seq[IParameter],
                           override val bodies: Seq[IPatternBody],
                           override val visibility: Option[IPatternVisibility],
                           outParameters: Seq[PatternFunctionAnonymousParameter])
  extends IPatternFunctionModuleContent with IPattern

case class PatternFunctionBody(contents: Seq[IPatternBodyContent])
  extends IPatternBody with IReturnContainer

case class PatternFunctionComment(text: String)
  extends Comment(text) with IStatement with IPatternFunctionModuleContent

case class PatternFunctionEmptyContent()
  extends EmptyContent with IStatement with IPatternFunctionModuleContent

case class PatternFunctionModule(name: String,
                                 imports: List[IIncaModuleImport],
                                 contents: List[IPatternModuleContent])
  extends IPatternModule

case class RelationAttribute()

case class PatternFunctionParameter(name: String,
                                    typ: Option[MetaElement])
  extends PatternParameter(name, typ)

case class PatternFunctionAnonymousParameter(name: String,
                                             typ: Option[MetaElement])
  extends PatternParameter(name, typ)