package org.inca.lang.core

import org.inca.lang.core.Values.IVariableValue
import org.inca.meta.MetaElements.MetaElement

object Content {

  trait IPatternBodyContent

  trait IPatternModuleContent

  trait IPatternVisibility

  trait IVariableBinder

  trait IVariableWithDeclaredType extends IVariable

  trait IParameter extends IVariable with IVariableWithDeclaredType

  trait IGenNameProvider

  trait IJoinTypeDef extends INamedConcept {
    val types: Seq[MetaElement]
  }

  trait IPatternBody {
    val contents: Seq[IPatternBodyContent]
  }

  trait INamedConcept {
    val name: String
  }

  trait IVariable extends INamedConcept with IGenNameProvider {
    val typ: Option[MetaElement]
  }

  trait IPattern extends INamedConcept with IPatternModuleContent with IGenNameProvider with IVariableBinder {
    val parameters: Seq[IParameter]
    val bodies: Seq[IPatternBody]
    val visibility: Option[IPatternVisibility]
  }

  abstract class EmptyContent extends IPatternModuleContent with IPatternBodyContent

  abstract class HorizontalLineContent

  abstract class Comment(text: String) extends IPatternBodyContent with IPatternModuleContent

  abstract class TemporaryVariable(name: String, typ: Option[MetaElement])

  case class JoinTypeDef(name: String, types: Seq[MetaElement]) extends IJoinTypeDef

  case class CoreTemporaryVariable(name: String, typ: Option[MetaElement])
    extends TemporaryVariable(name, typ) with IVariable with IVariableValue

}
