package inca.analyzedLangs

import inca.runtime.context.DataModel
import truechange.SortType

package object tinyJava {
  val classDeclTag: String = classOf[ClassDeclaration].getCanonicalName
  val classDeclType: SortType = SortType(classDeclTag)
  val classMemberTag: String = classOf[ClassMember].getCanonicalName
  val classMemberType: SortType = SortType(classMemberTag)
  val fieldDeclTag: String = classOf[FieldDeclaration].getCanonicalName
  val fieldDeclType: SortType = SortType(fieldDeclTag)
  val visTag: String = classOf[Visibility].getCanonicalName
  val visType: SortType = SortType(visTag)
  val privateVisTag: String = classOf[PrivateVisibility].getCanonicalName
  val privateVisType: SortType = SortType(privateVisTag)
  val protectedVisTag: String = classOf[ProtectedVisibility].getCanonicalName
  val protectedVisType: SortType = SortType(protectedVisTag)
  val publicVisTag: String = classOf[PublicVisibility].getCanonicalName
  val publicVisType: SortType = SortType(publicVisTag)

  val model: DataModel = DataModel.from(
    ClassDeclaration,
    ClassMember,
    FieldDeclaration,
    Visibility,
    PublicVisibility,
    ProtectedVisibility,
    PrivateVisibility)
}
