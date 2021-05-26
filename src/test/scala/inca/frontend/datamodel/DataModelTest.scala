package inca.metamodel
import inca.frontend.constraint.core.TreesitterDataModel
import inca.frontend.constraint.datamodelresolver.TreesitterDataModelResolver
import inca.runtime.context.DataModel
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import truechange.{JavaLitType, ListType, OptionType, SortType}

class DataModelTest extends AnyFunSuite {

  test("parsing subtypes 1") {
    val dataModel: DataModel = new TreesitterDataModelResolver {}.resolve(
      TreesitterDataModel("./src/test/scala/inca/frontend/datamodel/metainfos/Subtypes1"))


    dataModel.nodeSupertypes.toSet should contain theSameElementsAs Set(SortType("binary_expression") -> SortType("_expression"),
      SortType("call_expression") -> SortType("_expression"))
  }

  test("parsing subtypes 2") {
    val dataModel: DataModel = new TreesitterDataModelResolver {}.resolve(
      TreesitterDataModel("./src/test/scala/inca/frontend/datamodel/metainfos/Subtypes2"))


    dataModel.directNodeSupertypes.toSet should contain theSameElementsAs Set(SortType("binary_expression") -> SortType("_expression"),
      SortType("call_expression")      -> SortType("_expression"), SortType("_expression") -> SortType("_simple_statement"),
      SortType("assignment_statement") -> SortType("_simple_statement"))
  }

  test("parsing links and literal links, merging of multiple argument types"){
    val dataModel: DataModel = new TreesitterDataModelResolver {}.resolve(
      TreesitterDataModel("./src/test/scala/inca/frontend/datamodel/metainfos/LinksAndMerging"))


    dataModel.litLinks.toSet should contain theSameElementsAs Set(
      ("function_declaration", "name")      -> JavaLitType(classOf[String]),
      ("function_declaration","parameters") -> JavaLitType(classOf[String]))

    dataModel.links.toSet should contain theSameElementsAs Set(
      ("function_declaration","body")   -> OptionType(ListType(SortType("block"))),
      ("function_declaration","result") -> OptionType(SortType("_simple_typeANDparameter_list$0")))
    }


    test("parsing links and literal links, multiple nodes, merging of multiple argument types") {
      val dataModel: DataModel = new TreesitterDataModelResolver {}.resolve(
        TreesitterDataModel("./src/test/scala/inca/frontend/datamodel/metainfos/MultipleDefinitions"))

      dataModel.litLinks.toSet should contain theSameElementsAs Set(
        ("function_declaration","name")       -> JavaLitType(classOf[String]),
        ("function_declaration","parameters") -> JavaLitType(classOf[String]),
        ("otherNode","litarg")                -> JavaLitType(classOf[Option[String]]))

      dataModel.links.toSet should contain theSameElementsAs Set(
        ("otherNode","body")              -> OptionType(SortType("block")),
        ("function_declaration","body")   -> OptionType(ListType(SortType("block"))),
        ("function_declaration","result") -> OptionType(SortType("_simple_typeANDparameter_list$0")))
    }


    test("parsing children to links"){
      val dataModel: DataModel = new TreesitterDataModelResolver {}.resolve(
        TreesitterDataModel("./src/test/scala/inca/frontend/datamodel/metainfos/Children"))


      dataModel.links.toSet should contain theSameElementsAs Set(("import_declaration","1") -> SortType("import_spec_list"),
        ("import_declaration","0") -> SortType("import_spec"),
        ("import_spec_list","0")   -> OptionType(ListType(SortType("import_spec"))))
    }

    test("parsing literal children to literal links"){
      val dataModel: DataModel = new TreesitterDataModelResolver {}.resolve(
        TreesitterDataModel("./src/test/scala/inca/frontend/datamodel/metainfos/LiteralChildren"))

      dataModel.litLinks.toSet should contain theSameElementsAs Set(("import_spec_list","0") -> JavaLitType(classOf[Option[List[String]]]),
        ("import_declaration","2") -> JavaLitType(classOf[String]))


      dataModel.links.toSet should contain theSameElementsAs Set(("import_declaration","1") -> SortType("import_spec_list"),
        ("import_declaration","0") -> SortType("import_spec"),
        ("import_spec_list","1")   -> OptionType(ListType(SortType("import_spec"))))
    }


    test("merging of multiple argument types") {
      val dataModel: DataModel = new TreesitterDataModelResolver {}.resolve(
        TreesitterDataModel("./src/test/scala/inca/frontend/datamodel/metainfos/MultipleArgTypes"))


      dataModel.litLinks.toSet should contain theSameElementsAs Set(
        ("function_declaration","name")       -> JavaLitType(classOf[String]),
        ("function_declaration","parameters") -> JavaLitType(classOf[String]),
        ("otherNode","litarg")                -> JavaLitType(classOf[Option[String]]))

      dataModel.links.toSet should contain theSameElementsAs Set(
        ("otherNode","body")              -> OptionType(SortType("blockANDfunction_declaration$0")),
        ("function_declaration","body")   -> OptionType(ListType(SortType("block"))),
        ("function_declaration","result") -> OptionType(SortType("_simple_typeANDparameter_listANDotherNode$0")))

    }

    test("merging of multiple argument types and creating new supertypes") {
      // first test example:
      val dataModel: DataModel = new TreesitterDataModelResolver {}.resolve(
        TreesitterDataModel("./src/test/scala/inca/frontend/datamodel/metainfos/MultipleArgTypes"))


      dataModel.litLinks.toSet should contain theSameElementsAs Set(
        ("function_declaration","name")       -> JavaLitType(classOf[String]),
        ("function_declaration","parameters") -> JavaLitType(classOf[String]),
        ("otherNode","litarg")                -> JavaLitType(classOf[Option[String]]))

      dataModel.links.toSet should contain theSameElementsAs Set(
        ("otherNode","body")                -> OptionType(SortType("blockANDfunction_declaration$0")),
        ("function_declaration","body")     -> OptionType(ListType(SortType("block"))),
        ("function_declaration","result")   -> OptionType(SortType("_simple_typeANDparameter_listANDotherNode$0")))

    }

  test("merging of multiple argument types with overlapping names and creating new supertypes") {
    // first test example:
    val dataModel = new TreesitterDataModelResolver{}.resolve(
      TreesitterDataModel("./src/test/scala/inca/frontend/datamodel/metainfos/OverlappingNames"))


    dataModel.litLinks.toSet should contain theSameElementsAs Set(
      ("function_declaration","name")          -> JavaLitType(classOf[String]),
      ("function_declaration","parameters")    -> JavaLitType(classOf[String]),
      ("function_declaration_01","name")       -> JavaLitType(classOf[String]),
      ("function_declaration_01","parameters") -> JavaLitType(classOf[String]),
      ("otherNode","litarg")                   -> JavaLitType(classOf[Option[String]]))

    dataModel.links.toSet should contain theSameElementsAs Set(
      ("otherNode","body")                   -> OptionType(SortType("blockANDfunction_declaration$0")),
      ("function_declaration","body")        -> OptionType(ListType(SortType("block"))),
      ("function_declaration","result")      -> OptionType(SortType("_simple_typeANDparameter_listANDotherNode$0")),
      ("function_declaration_01","body")     -> OptionType(ListType(SortType("block"))),
      ("function_declaration_01","result")   -> OptionType(SortType("_simple_typeANDparameter_listANDotherNode$1")))

  }

    test("ignoring syntax (named = false)") {
      val dataModel: DataModel = new TreesitterDataModelResolver {}.resolve(
        TreesitterDataModel("./src/test/scala/inca/frontend/datamodel/metainfos/NamedFalse"))

      dataModel.litLinks.toSet should contain theSameElementsAs Set()

      dataModel.links.toSet should contain theSameElementsAs Set()
    }
}
