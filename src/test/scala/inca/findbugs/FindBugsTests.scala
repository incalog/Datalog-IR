package inca.findbugs

import inca.analyzedLangs._
import inca.lang.fun.CompileToGP
import inca.lang.fun.Fun._
import inca.runtime.indices.LanguageMetaInfo
//import inca.trans.generated.FindBugs_confusedInheritanceQuerySpecification
import inca.util.AnalysisWriter
import org.scalatest.funsuite.AnyFunSuite

class FindBugsTests extends AnyFunSuite {


  val classDeclType = TNode(classOf[ClassDeclaration].getCanonicalName)
  val classMemberType = TNode("inca.analyzedLangs.ClassMember")
  val fieldDeclType = TNode(classOf[FieldDeclaration].getCanonicalName)
  val visType = TNode("inca.analyzedLangs.Visibility")
  val privateVisType = TNode(classOf[PrivateVisibility].getCanonicalName)
  val publicVisType = TNode(classOf[PublicVisibility].getCanonicalName)

  val langMetaInfo = new LanguageMetaInfo(
    Map(
      classDeclType.name -> Set(),
      classMemberType.name -> Set(),
      fieldDeclType.name -> Set(classMemberType.name),
      visType.name -> Set(),
      privateVisType.name -> Set(visType.name),
      publicVisType.name -> Set(visType.name)
    ),
    Map(
      classDeclType.name -> Map("name" -> "java.lang.String", "isFinal" -> "java.lang.Boolean", "members" -> TList(classMemberType).toString),
      fieldDeclType.name -> Map("name" -> "java.lang.String", "visibility" -> visType.name)
    ))

  test("Confused Inheritance") {
    val classDeclType = TNode(classOf[ClassDeclaration].getCanonicalName)
    val fieldDeclType = TNode(classOf[FieldDeclaration].getCanonicalName)
    val protectedVisType = TNode(classOf[ProtectedVisibility].getCanonicalName)
    val confusedInheritance = PatternFunction(
      None,
      "confusedInheritance",
      Seq(Param("class", Some(classDeclType))),
      Seq(),
      Seq(
        Body(
          Seq(
            Assert(Eq(PathAccess(Var("class"), classDeclType("isFinal")), Constant(BooleanLiteral(true)))),
            Assignment(Seq("members"), PathAccess(Var("class"), classDeclType("members"))),
            Assignment(Seq("member"), PathAccess(Var("members"), ChildrenLink)),
            Assert(InstanceOf(Var("member"), fieldDeclType)),
            Assert(InstanceOf(PathAccess(Var("member"), Seq(fieldDeclType("visibility"))), protectedVisType))
          ))))
    val module = Module("FindBugs", Seq(), Seq(confusedInheritance))
    val compiledModule = CompileToGP.transformModule(module)
    AnalysisWriter.writeModule(compiledModule)
    val clazz = ClassDeclaration("Foo", true, List(FieldDeclaration("baz", PublicVisibility()), FieldDeclaration("bar", ProtectedVisibility())))

//    var virtualIndices = Seq[VirtualIndex]()
//    val nextIndex = new ListNextIndex
//    virtualIndices +:= nextIndex
//    virtualIndices +:= new ParentIndex(nextIndex)
//    val scope = new QueryScope(langMetaInfo, virtualIndices)
//    val changeset = Diffable.load(clazz)
//    val matcher = EnginePool.getMatcher(FindBugs_confusedInheritanceQuerySpecification.instance(), scope, DifferentialReteBackendFactory.INSTANCE)
//    val indices = scope.getEngineContext.getBaseIndex
//    indices.processChangeset(changeset)
//    assert(matcher.getAllValues("class").contains(clazz.uri))
//
//    val clazz2 = ClassDeclaration("Foo", true, List(FieldDeclaration("baz", PublicVisibility())))
//    val (diffset, updatedclazz) = clazz.compareTo(clazz2)
//    indices.processChangeset(diffset)
//    assert(matcher.getAllMatches.isEmpty)
//
//    val clazz3 = ClassDeclaration("Foo", false, List(FieldDeclaration("baz", PublicVisibility())))
//    val (diffset2, updatedclazz2) = updatedclazz.compareTo(clazz3)
//    indices.processChangeset(diffset2)
//    assert(matcher.getAllMatches.isEmpty)
//
//    val clazz4 = ClassDeclaration("Foo", true, List(
//      FieldDeclaration("bar", PrivateVisibility()),
//      FieldDeclaration("baz", PublicVisibility()),
//      FieldDeclaration("baaz", ProtectedVisibility())))
//    val (diffset3, updatedclazz3) = updatedclazz2.compareTo(clazz4)
//    indices.processChangeset(diffset3)
//    assert(matcher.getAllValues("class").contains(updatedclazz3.uri))
//
//    EnginePool.disposeAllEngines()
  }

}