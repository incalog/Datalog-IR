package org.inca.lang.core

import org.inca.lang.core.Content.IJoinTypeDef
import org.inca.lang.mps.{AbstractConceptDeclaration, INamedConcept}
import org.inca.meta.MetaElements.MetaElement

object Typp {
  case class JoinType(joinTypeDef: IJoinTypeDef) extends MetaElement
  case class ConceptReferenceType(concept: AbstractConceptDeclaration) extends MetaElement
  // todo `dataTypeDeclaration` is mps and is not `abstract`!
  abstract class DataTypeDeclaration(name: String) extends INamedConcept
  case class DataReferenceType(dataTypeDeclaration: DataTypeDeclaration) extends MetaElement

  // `type hint` folder
  trait ITypeHintKeyProvider {
    def getTypeHintKey: String
  }
//  case class UserObjectEntry(key: String, value: BaseConcept) extends BaseConcept
//  case class UserObjectMap(entries: List[UserObjectEntry]) extends BaseConcept

}
