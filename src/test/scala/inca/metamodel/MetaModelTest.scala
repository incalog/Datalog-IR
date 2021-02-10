package inca.metamodel

import inca.metamodel.MetaModel
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import io.circe._
import org.scalatest.matchers.should.Matchers._
import io.circe.parser._
import io.circe.optics.JsonPath._
import truechange.{JavaLitType, ListType, OptionType, SortType}

class MetaModelTest extends AnyFunSuite {

  test("parsing subtypes 1") {
    val lanMetaModel = new MetaModel("./src/test/scala/inca/metamodel/PartialGoLang.json")

    val metaInfo = lanMetaModel.getLanguageMetaInfo

    metaInfo.nodeSupertypes.toSet should contain theSameElementsAs Set(SortType("binary_expression") -> SortType("_expression"),
      SortType("call_expression") -> SortType("_expression"))
  }

  test("parsing subtypes 2") {
    val lanMetaModel = new MetaModel("./src/test/scala/inca/metamodel/PartialGoLang2.json")

    val metaInfo = lanMetaModel.getLanguageMetaInfo

    metaInfo.directNodeSupertypes.toSet should contain theSameElementsAs Set(SortType("binary_expression") -> SortType("_expression"),
      SortType("call_expression") -> SortType("_expression"), SortType("_expression") -> SortType("_simple_statement"),
      SortType("assignment_statement") -> SortType("_simple_statement"))
  }

  test("parsing links") {
    val lanMetaModel = new MetaModel("./src/test/scala/inca/metamodel/PartialGoLang.json")

    val metaInfo = lanMetaModel.getLanguageMetaInfo

    metaInfo.litLinks.toSet should contain theSameElementsAs Set(("function_declaration","body") -> JavaLitType(classOf[java.lang.String]))
    metaInfo.links.toSet should contain theSameElementsAs Set(
      ("function_declaration", "optionalarg") -> OptionType(SortType("_expression")),
      ("function_declaration", "optlistarg") -> OptionType(ListType(SortType("_expression"))),
      ("function_declaration", "listarg") -> ListType(SortType("_expression")))

  }
}
