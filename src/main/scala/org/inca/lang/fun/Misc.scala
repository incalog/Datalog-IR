package org.inca.lang.fun

import org.inca.lang.Core._
import org.inca.lang.Gp._
import org.inca.meta.MetaElements.MetaElement

trait IPatternFunctionModuleContent extends PatternModuleContent

trait IReturnContainer

trait IBinary {
  val left: IExpression
  val right: IExpression
}

abstract class AbstractBinary extends IBinary

case class CostConsistentAttribute()

case class FunHorizontalLineContent() extends IPatternFunctionModuleContent

case class PatternFunction(override val name: String,
                           override val parameters: Seq[Parameter],
                           override val bodies: Seq[PatternBody],
                           override val visibility: Option[PatternVisibility],
                           outParameters: Seq[PatternFunctionAnonymousParameter])
  extends IPatternFunctionModuleContent with Pattern

case class PatternFunctionBody(contents: Seq[PatternBodyContent])
  extends PatternBody with IReturnContainer

case class RelationAttribute()

case class PatternFunctionParameter(name: String,
                                    typ: Option[MetaElement])
  extends PatternParameter(name, typ)

case class PatternFunctionAnonymousParameter(name: String,
                                             typ: Option[MetaElement])
  extends PatternParameter(name, typ)