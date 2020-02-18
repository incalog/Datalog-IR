package org.inca.lang.fun

import org.inca.lang.core.Content.{HorizontalLineContent, IParameter, IPattern, IPatternBody, IPatternBodyContent, IPatternModuleContent, IPatternVisibility}
import org.inca.lang.core.{IIncaModuleImport, IPatternModule}
import org.inca.lang.gp.Content.{EmptyGraphPatternContent, GraphPatternBody, GraphPatternComment, GraphPatternParameter}
import org.inca.lang.gp.GraphPatternModule
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

case class PatternFunctionAnonymousParameter(override val name: String,
                                             override val typ: Option[MetaElement])
  extends PatternFunctionParameter(name, typ)

case class PatternFunctionBody(override val contents: Seq[IPatternBodyContent])
  extends GraphPatternBody(contents) with IPatternBody with IReturnContainer

case class PatternFunctionComment(override val text: String)
  extends GraphPatternComment(text) with IStatement with IPatternFunctionModuleContent

case class PatternFunctionEmptyContent()
  extends EmptyGraphPatternContent with IStatement with IPatternFunctionModuleContent

case class PatternFunctionModule(override val name: String,
                                 override val imports: List[IIncaModuleImport],
                                 override val contents: List[IPatternModuleContent])
  extends GraphPatternModule(name, imports, contents) with IPatternModule

case class RelationAttribute()

case class PatternFunctionParameter(override val name: String,
                                    override val typ: Option[MetaElement])
  extends GraphPatternParameter(name, typ)