package org.inca.lang.core

import org.inca.lang.core.Values.VariableValue
import org.inca.meta.MetaElements.MetaElement

object Content {

  trait PatternBodyContent

  trait PatternModuleContent

  trait PatternVisibility

  trait VariableWithDeclaredType extends Variable

  trait Parameter extends Variable with VariableWithDeclaredType

  trait GenNameProvider

  trait JoinTypeDef extends NamedConcept {
    val types: Seq[MetaElement]
  }

  trait PatternBody {
    val contents: Seq[PatternBodyContent]
  }

  trait NamedConcept {
    val name: String
  }

  trait Variable extends NamedConcept with GenNameProvider {
    val typ: Option[MetaElement]
  }

  trait Pattern extends NamedConcept with PatternModuleContent with GenNameProvider {
    val parameters: Seq[Parameter]
    val bodies: Seq[PatternBody]
    val visibility: Option[PatternVisibility]
  }

  abstract class EmptyContent extends PatternModuleContent with PatternBodyContent

  abstract class HorizontalLineContent

  abstract class Comment(text: String) extends PatternBodyContent with PatternModuleContent

  abstract class TemporaryVariable(name: String, typ: Option[MetaElement])

  case class CoreTemporaryVariable(name: String, typ: Option[MetaElement])
    extends TemporaryVariable(name, typ) with Variable with VariableValue

}
