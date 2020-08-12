package inca.analyzedLangs

import inca.runtime.context.LanguageMetaInfo
import truechange.{JavaLitType, ListType, SortType}

import scala.collection.immutable.MultiDict

package object tinyJava {
  val classDeclTag = classOf[ClassDeclaration].getCanonicalName
  val classDeclType = SortType(classDeclTag)
  val classMemberTag = classOf[ClassMember].getCanonicalName
  val classMemberType = SortType(classMemberTag)
  val fieldDeclTag = classOf[FieldDeclaration].getCanonicalName
  val fieldDeclType = SortType(fieldDeclTag)
  val visTag = classOf[Visibility].getCanonicalName
  val visType = SortType(visTag)
  val privateVisTag = classOf[PrivateVisibility].getCanonicalName
  val privateVisType = SortType(privateVisTag)
  val protectedVisTag = classOf[ProtectedVisibility].getCanonicalName
  val protectedVisType = SortType(protectedVisTag)
  val publicVisTag = classOf[PublicVisibility].getCanonicalName
  val publicVisType = SortType(publicVisTag)

  val langMetaInfo = new LanguageMetaInfo(
    MultiDict(
      fieldDeclType -> classMemberType,
      privateVisType -> visType,
      protectedVisType -> visType,
      publicVisType -> visType
    ),
    Map(
      (classDeclTag->"members") -> ListType(classMemberType),
      (fieldDeclTag->"visibility") -> visType
    ),
    Map(
      (classDeclTag->"name") -> JavaLitType(classOf[java.lang.String]),
      (classDeclTag->"isFinal") -> JavaLitType(classOf[java.lang.Boolean]),
      (fieldDeclTag->"name") -> JavaLitType(classOf[java.lang.String])
    )
  )
}
