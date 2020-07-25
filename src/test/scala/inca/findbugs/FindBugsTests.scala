package inca.findbugs

import inca.analyzedLangs._
import inca.lang.fun.CompileToGP
import inca.lang.fun.Fun._
import inca.runtime.context.LanguageMetaInfo
import inca.runtime.index.MetaElements
//import inca.trans.generated.FindBugs_confusedInheritanceQuerySpecification
import inca.util.AnalysisWriter
import org.scalatest.funsuite.AnyFunSuite

class FindBugsTests extends AnyFunSuite {


  val classDeclType = MetaElements.NodeType(classOf[ClassDeclaration].getCanonicalName)
  val classMemberType = MetaElements.NodeType("inca.analyzedLangs.ClassMember")
  val fieldDeclType = MetaElements.NodeType(classOf[FieldDeclaration].getCanonicalName)
  val visType = MetaElements.NodeType("inca.analyzedLangs.Visibility")
  val privateVisType = MetaElements.NodeType(classOf[PrivateVisibility].getCanonicalName)
  val publicVisType = MetaElements.NodeType(classOf[PublicVisibility].getCanonicalName)

  val langMetaInfo = new LanguageMetaInfo(
    Map(
      classDeclType -> Set(),
      classMemberType -> Set(),
      fieldDeclType -> Set(classMemberType),
      visType -> Set(),
      privateVisType -> Set(visType),
      publicVisType -> Set(visType)
    ),
    Map(
      MetaElements.NamedLink(classDeclType, "name") -> MetaElements.PrimitiveType("java.lang.String"),
      MetaElements.NamedLink(classDeclType, "isFinal") -> MetaElements.PrimitiveType("java.lang.Boolean"),
      MetaElements.NamedLink(classDeclType, "members") -> MetaElements.ListType(classMemberType),
      MetaElements.NamedLink(fieldDeclType, "name") -> MetaElements.PrimitiveType("java.lang.String"),
      MetaElements.NamedLink(fieldDeclType, "visibility") -> visType
    )
  )

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