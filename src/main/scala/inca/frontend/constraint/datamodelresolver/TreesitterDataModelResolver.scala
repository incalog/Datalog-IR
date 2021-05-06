package inca.frontend.constraint.datamodelresolver

import inca.frontend.constraint.core.{DataModel, TreesitterDataModel}
import inca.runtime.context

trait TreesitterDataModelResolver extends DataModelResolver {
  override def resolve(dataModel: DataModel): context.DataModel = dataModel match {
    case TreesitterDataModel(path) => super.resolve(dataModel)
    case _ => super.resolve(dataModel)
  }
}


import inca.runtime.context.DataModel
import truechange.{JavaLitType, ListType, LitType, OptionType, SortType, Type}
import scala.collection.immutable.MultiDict
import scala.collection.immutable.Map
import io.circe._
import io.circe.parser._

case class PreType(multiple: Boolean, required: Boolean, name: String)
case class PreLitType(multiple: Boolean, required: Boolean)

/*
* Assumptions: - Supertypes are declared explicity in treesitters grammar.js
*              - Children of AST-Nodes defined in node-types.json are defined as types and not explicitly
*
 */


class MetaModel(nodeTypes: Json, literalIdentifiers: Vector[String]) {

  def this(metaModelPath: String, literalIdentifiersPath: String) = this({
    val source = scala.io.Source.fromFile(metaModelPath)
    val lines = try source.getLines mkString "\n" finally source.close()
    parse(lines).getOrElse(Json.Null)}, {
    val source = scala.io.Source.fromFile(literalIdentifiersPath)
    try source.getLines.toVector finally source.close()})


  def getLanguageMetaInfo: LanguageMetaInfo = getLMIFromMultiMap(getMultiLinks._1, getMultiLinks._2, getSupertypeMap)


  private def getSupertypeMap: MultiDict[SortType, SortType] = {
    var directSupertypes = MultiDict[SortType, SortType]()

    val typeCursor : HCursor = nodeTypes.hcursor

    val typesList: Vector[Json] = typeCursor.focus.flatMap(_.asArray).getOrElse(Vector.empty)

    for(typeDef: Json <- typesList) {

      val supertypeSort: SortType = SortType(typeDef.hcursor.downField("type").as[String].getOrElse("Error"))

      val subtypes: Vector[Json] = typeDef.hcursor.downField("subtypes").focus.flatMap(_.asArray).getOrElse(Vector.empty)
      for(subtype: Json <- subtypes) {
        val subtypeSort: SortType = SortType(subtype.hcursor.downField("type").as[String].getOrElse("Error"))

        directSupertypes += (subtypeSort -> supertypeSort)
      }
    }

    directSupertypes
  }

  private def getLMIFromMultiMap(multiLinks: MultiDict[Link, PreType], litLinks: Map[Link, LitType], supertypeMap: MultiDict[SortType, SortType]): LanguageMetaInfo = {

    var links = Map[Link, Type]()
    var newSupertypeMap = supertypeMap

    for(link <- multiLinks.keySet) {
      val names = for (tpe <- multiLinks.get(link)) yield tpe.name

      val newSupertype = SortType(names.mkString("AND"))

      //todo: use gensym to generate fresh names
      if (names.size > 1)
        for (name <- names) newSupertypeMap += (SortType(name) -> newSupertype)

      //Get optional and list information about types from pretypes
      val argtpe = multiLinks.get(link).head

      val newArgType = getLinkType(argtpe.multiple, argtpe.required, newSupertype)
      links += (link -> newArgType)
    }

    new LanguageMetaInfo(newSupertypeMap, links, litLinks)
  }


  private def getMultiLinks: (MultiDict[Link, PreType], Map[Link, LitType]) = {
    var linksMap = MultiDict[Link, PreType]()
    var litLinksMap = Map[Link, LitType]()


    val typeCursor : HCursor = nodeTypes.hcursor
    val nodeTypesList: Vector[Json] = typeCursor.focus.flatMap(_.asArray).getOrElse(Vector.empty)

    for(typedef: Json <- nodeTypesList) {
      val nodeTypeName: String = typedef.hcursor.downField("type").as[String].getOrElse("Error")


      val fieldNames: Iterable[String] = typedef.hcursor.downField("fields").keys.getOrElse(Vector.empty)

      for (fieldName: String <- fieldNames) {
        val fieldTypes: Vector[Json] = typedef.hcursor.downField("fields").downField(fieldName).downField("types").focus.flatMap(_.asArray).getOrElse(Vector.empty)
        val fieldMultiple = typedef.hcursor.downField("fields").downField(fieldName).downField("multiple").as[Boolean].getOrElse(false)
        val fieldRequired = typedef.hcursor.downField("fields").downField(fieldName).downField("required").as[Boolean].getOrElse(true)
        val types: Vector[Either[String, SortType]] = extractTypes(fieldTypes)
        for (tpe: Either[String, SortType] <- types) {
          getNewLink(nodeTypeName, tpe, fieldName, fieldMultiple, fieldRequired) match {
            case Left(litLink) => litLinksMap += litLink
            case Right(sortTypeLink) => linksMap += sortTypeLink
          }
        }
      }


      if (typedef.hcursor.downField("children").keys.getOrElse(Vector.empty).nonEmpty) {
        val childMultiple = typedef.hcursor.downField("children").downField("multiple").as[Boolean].getOrElse(false)
        val childRequired = typedef.hcursor.downField("children").downField("required").as[Boolean].getOrElse(true)
        val childTypes: Vector[Json] = typedef.hcursor.downField("children").downField("types").focus.flatMap(_.asArray).getOrElse(Vector.empty)

        val types: Vector[Either[String, SortType]] = extractTypes(childTypes)
        val childNames: Vector[String] = types.indices.map(_.toString).toVector

        for ((tpe: Either[String, SortType], fieldName: String) <- types.zip(childNames)) {
          getNewLink(nodeTypeName, tpe, fieldName, childMultiple, childRequired) match {
            case Left(litLink) => litLinksMap += litLink
            case Right(sortTypeLink) => linksMap += sortTypeLink
          }
        }
      }
    }
    (linksMap, litLinksMap)
  }

  private def getNewLink(nodeTypeName: String, fieldType: Either[String, SortType], fieldName: String, multiple: Boolean, required: Boolean): Either[(Link, LitType), (Link, PreType)] = {
    fieldType match {
      case Right(sortType) =>
        Right((nodeTypeName, fieldName) -> PreType(multiple, required, sortType.name))
      case Left(_) =>
        Left((nodeTypeName, fieldName) -> getLitLinkType(multiple, required))
    }
  }

  private def extractTypes(types: Vector[Json]): Vector[Either[String, SortType]] =
    for { tpe: Json <- types
          if tpe.hcursor.downField("named").as[Boolean].getOrElse(false) } yield {
      val typeName: String = tpe.hcursor.downField("type").as[String].getOrElse("Error")
      if(literalIdentifiers.contains(typeName))
        Left(typeName)
      else
        Right(SortType(typeName))
    }

  private def getLinkType(multiple: Boolean, required: Boolean, tpe: Type): Type = (multiple, required) match {
    case (true, true) => ListType(tpe)
    case (true, false) => OptionType(ListType(tpe))
    case (false, true) => tpe
    case (false, false) => OptionType(tpe)
  }

  private def getLitLinkType(multiple: Boolean, required: Boolean): JavaLitType = (multiple, required) match {
    case (true, true) => JavaLitType(classOf[List[String]])
    case (true, false) => JavaLitType(classOf[Option[List[String]]])
    case (false, true) => JavaLitType(classOf[String])
    case (false, false) => JavaLitType(classOf[Option[String]])
  }
}

