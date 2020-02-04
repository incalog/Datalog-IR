package org.inca.lang.core

import org.inca.lang.core.Values.IVariableValue
import org.inca.lang.mps.INamedConcept
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
  trait IVariable extends INamedConcept with IGenNameProvider {
    val typ: Option[MetaElement]
  }
  trait IPattern extends INamedConcept with IPatternModuleContent with IGenNameProvider with IVariableBinder {
    val parameters: Seq[IParameter]
    val bodies: Seq[IPatternBody]
    val visibility: Option[IPatternVisibility]
  }

  abstract class EmptyContent extends IPatternModuleContent with IPatternBodyContent

  case class JoinTypeDef(name: String, types: Seq[MetaElement]) extends IJoinTypeDef
  case class TemporaryVariable(name: String, typ: Option[MetaElement]) extends IVariable with IVariableValue
  case class Comment(text: String) extends IPatternBodyContent with IPatternModuleContent


}
