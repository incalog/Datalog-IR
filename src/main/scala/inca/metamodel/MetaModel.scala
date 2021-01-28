package inca.metamodel

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
      val typeName: String = typedef.hcursor.downField("type").as[String].getOrElse("Error")


      val fieldNames: Iterable[String] = typedef.hcursor.downField("fields").keys.getOrElse(Vector.empty)

      for(fieldName: String <- fieldNames) {
        val fieldTypes: Vector[Json] = typedef.hcursor.downField("fields").downField(fieldName).downField("types").focus.flatMap(_.asArray).getOrElse(Vector.empty)
        val fieldMultiple = typedef.hcursor.downField("fields").downField(fieldName).downField("multiple").as[Boolean].getOrElse(false)
        val fieldRequired = typedef.hcursor.downField("fields").downField(fieldName).downField("required").as[Boolean].getOrElse(true)
        val types: Vector[Either[String, SortType]] = extractTypes(fieldTypes, supertypeMap)
        for (tpe: Either[String, SortType] <- types) {
          tpe match {
            case Right(sort) => {
              val linkType: Type = getLinkType(fieldMultiple, fieldRequired, sort)
              linksMap += (typeName, fieldName) -> linkType
            }
            case Left(lit) => {
              //todo: use right representation for literals
              //val linkType: LitType = getLinkType(fieldMultiple, fieldRequired, SortType(lit))
              litLinksMap += (typeName, fieldName) -> JavaLitType(classOf[java.lang.String])
            }
          }

        }
      }
    }
    (linksMap.toMap, litLinksMap.toMap)
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

