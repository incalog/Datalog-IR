package org.inca.core

import org.inca.core.Content.{IJoinTypeDef, IPatternVisibility}
import org.inca.mps.{AbstractConceptDeclaration, BaseConcept, IType}

object Typ {
  // `type` folder
  trait ICompileTimeIncAType extends IType

  case class JoinType(joinTypeDef: IJoinTypeDef) extends ICompileTimeIncAType
  case class ConceptReferenceType(concept: AbstractConceptDeclaration) extends ICompileTimeIncAType

  // `type hint` folder
  trait ITypeHintKeyProvider {
    def getTypeHintKey: String
  }
  case class UserObjectEntry(key: String, value: BaseConcept) extends BaseConcept
  case class UserObjectMap(entries: List[UserObjectEntry]) extends BaseConcept

}
