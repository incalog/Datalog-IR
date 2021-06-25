package inca.frontend.constraint.datamodelresolver

import inca.frontend.constraint.core.{DataModel, TreesitterDataModel}
import inca.runtime.context
import inca.runtime.context.DataModel.Link
import truechange.{JavaLitType, ListType, LitType, OptionType, SortType, Type}

import scala.collection.immutable.MultiDict
import scala.collection.immutable.Map
import io.circe._
import io.circe.parser._
import java.io.File

import inca.util.Gensym

//intermediate representations of types for easier access of properties later on
case class PreType(multiple: Boolean, required: Boolean, name: String)
case class PreLitType(multiple: Boolean, required: Boolean)

/***
 * Extension of TreesitterDataModelResolver
 */
trait TreesitterDataModelResolver extends DataModelResolver {
  override def resolve(dataModel: DataModel): context.DataModel = dataModel match {
    case TreesitterDataModel(path) =>
      new DataModelParser(path + File.separator + "node-types.json",
        path + File.separator + "token-nodes").getContextDataModel
    case _ => super.resolve(dataModel)
  }

  /*
  * Assumptions: - Supertypes are declared explicity in treesitters grammar.js
  *              - Children of AST-Nodes defined in node-types.json are defined as types and not explicitly
  *              - its important to have a close look on literals
  */
  class DataModelParser(nodeTypes: Json, literalIdentifiers: Vector[String]) {

    def this(metaModelPath: String, literalIdentifiersPath: String) = this({
      val source = scala.io.Source.fromFile(metaModelPath)
      val lines = try source.getLines mkString "\n" finally source.close()
      parse(lines).getOrElse(Json.Null)}, {
      val source = scala.io.Source.fromFile(literalIdentifiersPath)
      try source.getLines.toVector finally source.close()})


    def getContextDataModel: context.DataModel = getDataModelFromMultiMap(getMultiLinks._1, getMultiLinks._2, getSupertypeMap)


    /***
     *
     * @return returns a mapping from subtypes to supertyps while multiple supertypes can exist
     */
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

    /***
     * @param multiLinks literal links where one link can has multiple types
     * @param litLinks
     * @param supertypeMap multidict supertype map
     * @return Language Datamodel with multiple supertypes merged together by creating new names using gensym.
     */
    private def getDataModelFromMultiMap(multiLinks: MultiDict[Link, PreType], litLinks: Map[Link, LitType], supertypeMap: MultiDict[SortType, SortType]): context.DataModel = {
      //get all occurring types to intit gensym
      val gensym = new Gensym(for (tpe: SortType <- supertypeMap.keySet.toSet) yield tpe.name)
      gensym.register(for (tpe: SortType <- supertypeMap.values.toSet) yield tpe.name)

      var createdSupertypes: Map[Set[PreType], String] = Map()


      var links = Map[Link, Type]()
      var newSupertypeMap = supertypeMap

      for(link <- multiLinks.keySet) {
        val names = (for (tpe <- multiLinks.get(link)) yield tpe).toSet


        //create new supertype if needed
        val newSupertype = createdSupertypes.get(names) match {
          case Some(name) => SortType(name)
          case _ => {
            if (names.size > 1) {
              val newname = gensym.fresh((for (tpe <- names) yield tpe.name).mkString("AND"))
              createdSupertypes += (names -> newname)
              SortType(newname)}
            else {
              SortType(names.head.name)
            }
          }
        }


        //update supertype map
        if (names.size > 1)
          for (name <- names) newSupertypeMap += (SortType(name.name) -> newSupertype)

        //Get optional and list information about types from pretypes
        val argtpe = multiLinks.get(link).head

        //create new supertype and add new link
        val newArgType = getLinkType(argtpe.multiple, argtpe.required, newSupertype)
        links += (link -> newArgType)
      }

      new context.DataModel(newSupertypeMap.keySet.toSet ++ newSupertypeMap.values.toSet , newSupertypeMap, links, litLinks)
    }


    /***
     * parses note-types.json to get links yielding eventually to multiple types
     * @return Multi map from links to pretypes collecting informations about types which are generated later.
     *         Also generates a map for literal Links
     */
    private def getMultiLinks: (MultiDict[Link, PreType], Map[Link, LitType]) = {
      var linksMap = MultiDict[Link, PreType]()
      var litLinksMap = Map[Link, LitType]()


      val typeCursor : HCursor = nodeTypes.hcursor
      val nodeTypesList: Vector[Json] = typeCursor.focus.flatMap(_.asArray).getOrElse(Vector.empty)

      //parse named child nodes
      for(typedef: Json <- nodeTypesList) {
        val nodeTypeName: String = typedef.hcursor.downField("type").as[String].getOrElse("Error")


        val fieldNames: Iterable[String] = typedef.hcursor.downField("fields").keys.getOrElse(Vector.empty)

        for (fieldName: String <- fieldNames) {
          val fieldTypes: Vector[Json] = typedef.hcursor.downField("fields").downField(fieldName).downField("types").focus.flatMap(_.asArray).getOrElse(Vector.empty)
          //get characteristic information about childs (multiple, required)
          val fieldMultiple = typedef.hcursor.downField("fields").downField(fieldName).downField("multiple").as[Boolean].getOrElse(false)
          val fieldRequired = typedef.hcursor.downField("fields").downField(fieldName).downField("required").as[Boolean].getOrElse(true)
          //get all possible types the child node can have
          val types: Vector[Either[String, SortType]] = extractTypes(fieldTypes)
          //create links
          for (tpe: Either[String, SortType] <- types) {
            getNewLink(nodeTypeName, tpe, fieldName, fieldMultiple, fieldRequired) match {
              case Left(litLink) => litLinksMap += litLink
              case Right(sortTypeLink) => linksMap += sortTypeLink
            }
          }
        }

        //parse unnamed child nodes
        //name them _0, _1, ...
        if (typedef.hcursor.downField("children").keys.getOrElse(Vector.empty).nonEmpty) {
          //get characteristic information about childs (multiple, required)
          val childMultiple = typedef.hcursor.downField("children").downField("multiple").as[Boolean].getOrElse(false)
          val childRequired = typedef.hcursor.downField("children").downField("required").as[Boolean].getOrElse(true)
          //get all possible types the child node can have
          val childTypes: Vector[Json] = typedef.hcursor.downField("children").downField("types").focus.flatMap(_.asArray).getOrElse(Vector.empty)

          val types: Vector[Either[String, SortType]] = extractTypes(childTypes)
          val childNames: Vector[String] = types.indices.map("_" + _.toString).toVector
          //create links
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

    /***
     *
     * @param nodeTypeName type of the parent node
     * @param fieldType    type of the child node
     * @param fieldName    name of the child nodes field
     * @param multiple     multitplicity
     * @param required     optionality
     * @return creates a new Link from child node information above using pretypes
     */
    private def getNewLink(nodeTypeName: String, fieldType: Either[String, SortType], fieldName: String, multiple: Boolean, required: Boolean): Either[(Link, LitType), (Link, PreType)] = {
      fieldType match {
        case Right(sortType) =>
          Right((nodeTypeName, fieldName) -> PreType(multiple, required, sortType.name))
        case Left(_) =>
          Left((nodeTypeName, fieldName) -> getLitLinkType(multiple, required))
      }
    }

    /***
     *
     * @param types in json file
     * @return Creates SortTypes and checks for literals
     */
    private def extractTypes(types: Vector[Json]): Vector[Either[String, SortType]] =
      for { tpe: Json <- types
            if tpe.hcursor.downField("named").as[Boolean].getOrElse(false) } yield {
        val typeName: String = tpe.hcursor.downField("type").as[String].getOrElse("Error")
        if(literalIdentifiers.contains(typeName))
          Left(typeName)
        else
          Right(SortType(typeName))
      }

    /***
     *
     * @param multiple
     * @param required
     * @param tpe
     * @return create Link type depending on information above
     */
    private def getLinkType(multiple: Boolean, required: Boolean, tpe: Type): Type = (multiple, required) match {
      case (true, true) => ListType(tpe)
      case (true, false) => OptionType(ListType(tpe))
      case (false, true) => tpe
      case (false, false) => OptionType(tpe)
    }

    /***
     *
     * @param multiple
     * @param required
     * @return create literal link depending on information above
     */
    private def getLitLinkType(multiple: Boolean, required: Boolean): JavaLitType = (multiple, required) match {
      case (true, true) => JavaLitType(classOf[List[String]])
      case (true, false) => JavaLitType(classOf[Option[List[String]]])
      case (false, true) => JavaLitType(classOf[String])
      case (false, false) => JavaLitType(classOf[Option[String]])
    }
  }
}


