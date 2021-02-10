package inca.metamodel

import fastparse.Parsed
import fastparse.Parsed.Success
import inca.runtime.context.LanguageMetaInfo
import inca.runtime.index.MetaElements.Link
import io.circe.Json
import truechange.{JavaLitType, ListType, LitType, OptionType, SortType, Type}

import scala.collection.immutable.MultiDict
import io.circe._
import io.circe.parser._
import io.circe.optics.JsonPath._


class MetaModel(typedefs: Json) {

  def this(path: String) = this({
    val source = scala.io.Source.fromFile(path)
    val lines = try source.getLines mkString "\n" finally source.close()
    parse(lines).getOrElse(Json.Null)})


  def getLanguageMetaInfo: LanguageMetaInfo = {

    val supertypes: MultiDict[SortType, SortType] = getSupertypeMap(typedefs)

    val metaInfo = new LanguageMetaInfo(supertypes, null, null)

    val links: (Map[Link, Type], Map[Link, LitType]) = getLinks(typedefs, metaInfo.directNodeSubtypes)


    new LanguageMetaInfo(supertypes, links._1, links._2)
  }


  private def getSupertypeMap(types: Json): MultiDict[SortType, SortType] = {
    var directSupertypes = MultiDict[SortType, SortType]()

    val typeCursor : HCursor = types.hcursor

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

  private def getLinks(nodeTypes: Json, supertypeMap: MultiDict[SortType, SortType]): (Map[Link, Type], Map[Link, LitType]) = {
    var linksMap = scala.collection.mutable.Map[Link, Type]()
    var litLinksMap = scala.collection.mutable.Map[Link, LitType]()

    val typeCursor : HCursor = nodeTypes.hcursor
    val nodeTypesList: Vector[Json] = typeCursor.focus.flatMap(_.asArray).getOrElse(Vector.empty)

    for(typedef: Json <- nodeTypesList) {
      val nodeTypeName: String = typedef.hcursor.downField("type").as[String].getOrElse("Error")


      val fieldNames: Iterable[String] = typedef.hcursor.downField("fields").keys.getOrElse(Vector.empty)

      for (fieldName: String <- fieldNames) {
        val fieldTypes: Vector[Json] = typedef.hcursor.downField("fields").downField(fieldName).downField("types").focus.flatMap(_.asArray).getOrElse(Vector.empty)
        val fieldMultiple = typedef.hcursor.downField("fields").downField(fieldName).downField("multiple").as[Boolean].getOrElse(false)
        val fieldRequired = typedef.hcursor.downField("fields").downField(fieldName).downField("required").as[Boolean].getOrElse(true)
        val types: Vector[Either[String, SortType]] = extractTypes(fieldTypes, supertypeMap)
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

        val types: Vector[Either[String, SortType]] = extractTypes(childTypes, supertypeMap)
        val childNames: Vector[String] = types.indices.map(_.toString).toVector

        for ((tpe: Either[String, SortType], fieldName: String) <- types.zip(childNames)) {
           getNewLink(nodeTypeName, tpe, fieldName, childMultiple, childRequired) match {
             case Left(litLink) => litLinksMap += litLink
             case Right(sortTypeLink) => linksMap += sortTypeLink
           }
        }
      }
    }
    (linksMap.toMap, litLinksMap.toMap)
  }

  private def getNewLink(nodeTypeName: String, fieldType: Either[String, SortType], fieldName: String, multiple: Boolean, required: Boolean): Either[(Link, JavaLitType), (Link, Type)] = {
    fieldType match {
      case Right(sortType) => {
        val linkType: Type = getLinkType(multiple, required, sortType)
        Right((nodeTypeName, fieldName) -> linkType)
      }
      case Left(_) => {
        //todo: need to annotate with 'optional' flags here?
        //val linkType: LitType = getLinkType(fieldMultiple, fieldRequired, SortType(lit))
        Left((nodeTypeName, fieldName) -> JavaLitType(classOf[java.lang.String]))
      }
    }
  }

  private def extractTypes(types: Vector[Json], supertypes: MultiDict[SortType, SortType]): Vector[Either[String, SortType]] =
    for { tpe: Json <- types
          if tpe.hcursor.downField("named").as[Boolean].getOrElse(false) } yield {
      val typeName: String = tpe.hcursor.downField("type").as[String].getOrElse("Error")
      if(supertypes.sets.get(SortType(typeName)).isEmpty)
      //todo: use JavaLitType?
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
}

