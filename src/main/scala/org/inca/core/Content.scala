package org.inca.core

import org.inca.core.Typ.ICompileTimeIncAType
import org.inca.core.Values.IVariableValue
import org.inca.mps.INamedConcept

object Content {
  trait IPatternBodyContent
  trait IPatternModuleContent
  trait IPatternVisibility
  trait IVariableBinder
  trait IVariableWithDeclaredType extends IVariable
  trait IParameter extends IVariable with IVariableWithDeclaredType
  trait IGenNameProvider
  trait IJoinTypeDef extends INamedConcept {
    val types: Seq[ICompileTimeIncAType]
  }
  trait IPatternBody {
    val contents: Seq[IPatternBodyContent]
  }
  trait IVariable extends INamedConcept with IGenNameProvider {
    val typ: Option[ICompileTimeIncAType]
  }
  trait IPattern extends INamedConcept with IPatternModuleContent with IGenNameProvider with IVariableBinder {
    val parameters: Seq[IParameter]
    val bodies: Seq[IPatternBody]
    val visibility: Option[IPatternVisibility]
  }

  abstract class EmptyContent extends IPatternModuleContent with IPatternBodyContent

  case class JoinTypeDef(name: String, types: Seq[ICompileTimeIncAType]) extends IJoinTypeDef
  case class TemporaryVariable(name: String, typ: Option[ICompileTimeIncAType]) extends IVariable with IVariableValue
  case class Comment(text: String) extends IPatternBodyContent with IPatternModuleContent


}
