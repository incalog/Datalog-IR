package inca.metamodel
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import truechange.{JavaLitType, ListType, OptionType, SortType}

class MetaModelTest extends AnyFunSuite {

  test("parsing subtypes 1") {
    val lanMetaModel = new MetaModel("./src/test/scala/inca/metamodel/PartialGoLang.json", "./src/test/scala/inca/metamodel/tokenNodesGoLang")

    val metaInfo = lanMetaModel.getLanguageMetaInfo

    metaInfo.nodeSupertypes.toSet should contain theSameElementsAs Set(SortType("binary_expression") -> SortType("_expression"),
      SortType("call_expression") -> SortType("_expression"))
  }

  test("parsing subtypes 2") {
    val lanMetaModel = new MetaModel("./src/test/scala/inca/metamodel/PartialGoLang2.json", "./src/test/scala/inca/metamodel/tokenNodesGoLang")

    val metaInfo = lanMetaModel.getLanguageMetaInfo

    metaInfo.directNodeSupertypes.toSet should contain theSameElementsAs Set(SortType("binary_expression") -> SortType("_expression"),
      SortType("call_expression") -> SortType("_expression"), SortType("_expression") -> SortType("_simple_statement"),
      SortType("assignment_statement") -> SortType("_simple_statement"))
  }


  test("parsing links and literal links"){
    val lanMetaModel1 = new MetaModel("./src/test/scala/inca/metamodel/PartialGoLangLiterals.json", "./src/test/scala/inca/metamodel/tokenNodesGoLang")

    val metaInfo1 = lanMetaModel1.getLanguageMetaInfo

    metaInfo1.litLinks.toSet should contain theSameElementsAs Set(("function_declaration", "name") -> JavaLitType(classOf[String]), ("function_declaration","parameters") -> JavaLitType(classOf[String]))

    metaInfo1.links.toSet should contain theSameElementsAs Set(("function_declaration","body") -> OptionType(ListType(SortType("block"))),
      ("function_declaration","result") -> OptionType(SortType("parameter_list")))
  }


  test("parsing links and literal links, multiple nodes") {
    // first test example:
    val lanMetaModel = new MetaModel("./src/test/scala/inca/metamodel/LiteralsMultipleDefinitions.json", "./src/test/scala/inca/metamodel/tokenNodesGoLang")

    val metaInfo = lanMetaModel.getLanguageMetaInfo

    metaInfo.litLinks.toSet should contain theSameElementsAs Set(
      ("function_declaration","name")       -> JavaLitType(classOf[String]),
      ("function_declaration","parameters") -> JavaLitType(classOf[String]),
      ("otherNode","litarg")                -> JavaLitType(classOf[Option[String]]))

    metaInfo.links.toSet should contain theSameElementsAs Set(
      ("otherNode","body") -> OptionType(SortType("block")), ("function_declaration","body") -> OptionType(ListType(SortType("block"))), ("function_declaration","result") -> OptionType(SortType("parameter_list")))

  }

  test("parsing children to links"){
    val lanMetaModel = new MetaModel("./src/test/scala/inca/metamodel/Children.json", "./src/test/scala/inca/metamodel/tokenNodesGoLang")

    val metaInfo = lanMetaModel.getLanguageMetaInfo

    metaInfo.litLinks.toSet should contain theSameElementsAs Set()

    metaInfo.links.toSet should contain theSameElementsAs Set(("import_declaration","1") -> SortType("import_spec_list"),
      ("import_declaration","0") -> SortType("import_spec"),
      ("import_spec_list","0") -> OptionType(ListType(SortType("import_spec"))))
  }

  test("parsing literal children to literal links"){
    val lanMetaModel = new MetaModel("./src/test/scala/inca/metamodel/LiteralChildren.json", "./src/test/scala/inca/metamodel/tokenNodesGoLang")

    val metaInfo = lanMetaModel.getLanguageMetaInfo

    print(metaInfo.litLinks)


    metaInfo.litLinks.toSet should contain theSameElementsAs Set(("import_spec_list","0") -> JavaLitType(classOf[Option[List[String]]]),
      ("import_declaration","2") -> JavaLitType(classOf[String]))


    metaInfo.links.toSet should contain theSameElementsAs Set(("import_declaration","1") -> SortType("import_spec_list"),
      ("import_declaration","0") -> SortType("import_spec"),
      ("import_spec_list","1") -> OptionType(ListType(SortType("import_spec"))))
  }

  test("ignoring syntax (named = false)") {
    val lanMetaModel = new MetaModel("./src/test/scala/inca/metamodel/GoLangNamedFalse.json", "./src/test/scala/inca/metamodel/tokenNodesGoLang")

    val metaInfo = lanMetaModel.getLanguageMetaInfo

    metaInfo.litLinks.toSet should contain theSameElementsAs Set()

    metaInfo.links.toSet should contain theSameElementsAs Set()
  }
}
