package inca.runtime

import inca.runtime.context.LanguageMetaInfo
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import truechange.SortType



import scala.collection.immutable.MultiDict

class LanguageMetaInfoTest extends AnyFunSuite {
  val exp = SortType("Exp")
  val add = SortType("Add")
  val mult = SortType("Mult")
  val node = SortType("Node")
  val iNumExp = SortType("INumExp")
  val and = SortType("And")

  test("0 step trans closure") {
    val metaInfo = new LanguageMetaInfo(
      MultiDict(
        add -> exp,
        mult -> exp)
      , null, null)
    metaInfo.nodeSupertypes.toSet should contain theSameElementsAs Set(add -> exp, mult -> exp)
    metaInfo.directNodeSubtypes.toSet should contain theSameElementsAs Set(exp -> add, exp -> mult)
    metaInfo.nodeSubtypes.toSet should contain theSameElementsAs Set(exp -> add, exp -> mult)
  }

  test("1 step trans closure") {
    val metaInfo = new LanguageMetaInfo(
      MultiDict(
        add -> iNumExp,
        mult -> iNumExp,
        iNumExp -> exp)
      , null, null)
    metaInfo.nodeSupertypes.toSet should contain theSameElementsAs Set(add -> iNumExp, add -> exp, mult -> iNumExp, mult -> exp, iNumExp -> exp)
    metaInfo.directNodeSubtypes.toSet should contain theSameElementsAs Set(exp -> iNumExp, iNumExp -> add, iNumExp -> mult)
    metaInfo.nodeSubtypes.toSet should contain theSameElementsAs Set(exp -> add, exp -> mult, exp -> iNumExp, iNumExp -> add, iNumExp -> mult)
  }

  test("2 step trans closure") {
    val metaInfo = new LanguageMetaInfo(
      MultiDict(
        add -> iNumExp,
        mult -> iNumExp,
        iNumExp -> exp,
        exp -> node)
      , null, null)
    metaInfo.nodeSupertypes.toSet should contain theSameElementsAs Set(add -> iNumExp, add -> exp, add -> node, mult -> iNumExp, mult -> exp, mult -> node, iNumExp -> exp, iNumExp -> node, exp -> node)
    metaInfo.directNodeSubtypes.toSet should contain theSameElementsAs Set(exp -> iNumExp, iNumExp -> add, iNumExp -> mult, node -> exp)
    metaInfo.nodeSubtypes.toSet should contain theSameElementsAs Set(exp -> add, exp -> mult, exp -> iNumExp, iNumExp -> add, iNumExp -> mult, node -> exp, node -> iNumExp, node -> add, node -> mult)
  }

  test("inital multi inheritance trans closure") {
    val metaInfo = new LanguageMetaInfo(
      MultiDict(
        and -> exp,
        and -> node,
        add -> exp,
        add -> iNumExp,
        add -> exp,
        iNumExp -> exp,
        exp -> node)
      , null, null)
    metaInfo.nodeSupertypes.toSet should contain theSameElementsAs Set(and -> exp, and -> node, add -> iNumExp, add -> exp, add -> node, iNumExp -> exp, iNumExp -> node, exp -> node)
    metaInfo.directNodeSubtypes.toSet should contain theSameElementsAs Set(exp -> iNumExp, exp -> add, exp -> and, iNumExp -> add, node -> exp, node -> and)
    metaInfo.nodeSubtypes.toSet should contain theSameElementsAs Set(exp -> add, exp -> and, exp -> iNumExp, iNumExp -> add, node -> exp, node -> iNumExp, node -> add, node -> and)
  }

  import io.circe._, io.circe.parser._
  import io.circe.optics.JsonPath._

  val rawJson: String = """
  {
  "foo": "bar",
  "baz": 123,
  "list of stuff": [ 4, 5, 6 ]
  }"""

  def getSupertypeMap(types: Json): MultiDict[SortType, SortType] = {
    var directSupertypes = MultiDict[SortType, SortType]()

    val typeCursor : HCursor = types.hcursor
    val typesList: Vector[Json] = typeCursor.downField("types").focus.flatMap(_.asArray).getOrElse(Vector.empty)

    for(typeDef: Json <- typesList) {

      val supertypeSort: SortType = typeDef.hcursor.downField("type").as[String] match {
        case Right(a) => SortType(a)
          //Todo(mschmi): Handle this case. It should never occur as long as jsons are in the right shape.
        case _ => SortType("Error") }

      val subtypes: Vector[Json] = typeDef.hcursor.downField("subtypes").focus.flatMap(_.asArray).getOrElse(Vector.empty)
        for(subtype: Json <- subtypes) {
          val subtypeSort: SortType = subtype.hcursor.downField("type").as[String] match {
            case Right(a) => SortType(a)
              // Todo(mschmi): Handle this case. It should never occur as long as jsons are in the right shape.
            case _ => SortType("Error")
          }
          directSupertypes += (subtypeSort -> supertypeSort)
        }
    }


    directSupertypes
  }

  test("test") {

    val source = scala.io.Source.fromFile("/home/moritz/Repos/inca-scala/src/test/scala/inca/analyzedLangs/GoLang.json")
    val lines = try source.getLines mkString "\n" finally source.close()


    val types: Json = parse(lines).getOrElse(Json.Null)
    val cursor: HCursor = types.hcursor


    val directSupertypes = getSupertypeMap(types)
    print(directSupertypes.sets)

    val metaInfo = new LanguageMetaInfo(
      MultiDict(
        and -> exp,
        and -> node,
        add -> exp,
        add -> iNumExp,
        add -> exp,
        iNumExp -> exp,
        exp -> node)
      , null, null)
    metaInfo.nodeSupertypes.toSet should contain theSameElementsAs Set(and -> exp, and -> node, add -> iNumExp, add -> exp, add -> node, iNumExp -> exp, iNumExp -> node, exp -> node)
    metaInfo.directNodeSubtypes.toSet should contain theSameElementsAs Set(exp -> iNumExp, exp -> add, exp -> and, iNumExp -> add, node -> exp, node -> and)
    metaInfo.nodeSubtypes.toSet should contain theSameElementsAs Set(exp -> add, exp -> and, exp -> iNumExp, iNumExp -> add, node -> exp, node -> iNumExp, node -> add, node -> and)
  }
}
