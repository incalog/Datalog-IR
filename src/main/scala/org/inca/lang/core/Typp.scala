package org.inca.lang.core

import org.inca.lang.core.Content.IJoinTypeDef
import org.inca.lang.mps.{AbstractConceptDeclaration, INamedConcept}

object Typp {
  // `type` folder
  // was ICompileTimeIncAType
  // todo remove IType (only usecase is the extension below
  trait Typ

  case class JoinType(joinTypeDef: IJoinTypeDef) extends Typ
  case class ConceptReferenceType(concept: AbstractConceptDeclaration) extends Typ
  // todo `dataTypeDeclaration` is mps and is not `abstract`!
  abstract class DataTypeDeclaration(name: String) extends INamedConcept
  case class DataReferenceType(dataTypeDeclaration: DataTypeDeclaration) extends Typ

  // `type hint` folder
  trait ITypeHintKeyProvider {
    def getTypeHintKey: String
  }
//  case class UserObjectEntry(key: String, value: BaseConcept) extends BaseConcept
//  case class UserObjectMap(entries: List[UserObjectEntry]) extends BaseConcept

}
