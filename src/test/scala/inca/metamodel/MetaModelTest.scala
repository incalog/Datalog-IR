package inca.metamodel

import inca.metamodel.MetaModel
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import io.circe._
import org.scalatest.matchers.should.Matchers._
import io.circe.parser._
import io.circe.optics.JsonPath._
import truechange.SortType

class MetaModelTest extends AnyFunSuite {

  test("parsing subtypes") {
    val lanMetaModel = new MetaModel("./src/test/scala/inca/metamodel/PartialGoLang.json")

    val metaInfo = lanMetaModel.getLanguageMetaInfo

    metaInfo.nodeSupertypes.toSet should contain theSameElementsAs Set(SortType("binary_expression") -> SortType("_expression"),
      SortType("call_expression") -> SortType("_expression"))


  }
}
