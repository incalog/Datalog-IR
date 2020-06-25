package inca.findbugs

import inca.MetaElements.{ListElementsLink, ListType, NodeType, ParentLink}
import inca.lang.FunLang._
import inca.analyzedLangs.{ClassDeclaration, FieldDeclaration, PrivateVisibility, ProtectedVisibility, PublicVisibility}
import inca.backend.indices.{EnginePool, LanguageMetaInfo, QueryScope}
import inca.backend.virtual.{ListElementsIndex, ParentIndex, VirtualIndex}
import inca.lang.FunLang
import inca.print.GraphPatternLangPrinter
import inca.trans.fun.FunToGPTranslator
import inca.trans.generated.FindBugs_confusedInheritanceQuerySpecification
import inca.util.AnalysisWriter
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite
import truediff.Diffable

import scala.collection.mutable

class FindBugsTests extends AnyFunSuite {


  val classDeclType = NodeType(classOf[ClassDeclaration].getCanonicalName)
  val classMemberType = NodeType("inca.analyzedLangs.ClassMember")
  val fieldDeclType = NodeType(classOf[FieldDeclaration].getCanonicalName)
  val visType = NodeType("inca.analyzedLangs.Visibility")
  val privateVisType = NodeType(classOf[PrivateVisibility].getCanonicalName)
  val publicVisType = NodeType(classOf[PublicVisibility].getCanonicalName)

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
      classDeclType.name -> Map("name" -> "java.lang.String", "isFinal" -> "java.lang.Boolean", "members" -> ListType(classMemberType).name),
      fieldDeclType.name -> Map("name" -> "java.lang.String", "visibility" -> visType.name)
    ))

  test("Confused Inheritance") {
    val classDeclType = NodeType(classOf[ClassDeclaration].getCanonicalName)
    val fieldDeclType = NodeType(classOf[FieldDeclaration].getCanonicalName)
    val protectedVisType = NodeType(classOf[ProtectedVisibility].getCanonicalName)
    val confusedInheritance = PatternFunction(
      None,
      "confusedInheritance",
      Seq(Param("class", Some(classDeclType))),
      Seq(),
      Seq(
        Alternative(
          Seq(
            Assert(Eq(PathAccess(Var("class"), Seq(classDeclType("isFinal"))), Constant(BooleanLiteral(true)))),
            Assignment(Seq("members"), PathAccess(Var("class"), Seq(classDeclType("members")))),
            Assignment(Seq("member"), PathAccess(Var("members"), Seq(ListElementsLink()))),
            Assert(InstanceOf(Var("member"), fieldDeclType)),
            Assert(InstanceOf(PathAccess(Var("member"), Seq(fieldDeclType("visibility"))), protectedVisType))
          ))))
    val module = Module("FindBugs", Seq(), Seq(confusedInheritance))
    val compiledModule = FunToGPTranslator.transformModule(module)
    AnalysisWriter.writeModule(compiledModule)
    val clazz = ClassDeclaration("Foo", true, List(FieldDeclaration("baz", PublicVisibility()), FieldDeclaration("bar", ProtectedVisibility())))

    val scope = new QueryScope(langMetaInfo, Map[String, VirtualIndex]("parent" -> new ParentIndex, "elements" -> new ListElementsIndex()))
    val changeset = Diffable.load(clazz)
    val matcher = EnginePool.getMatcher(FindBugs_confusedInheritanceQuerySpecification.instance(), scope, DifferentialReteBackendFactory.INSTANCE)
    val indices = scope.getEngineContext.getBaseIndex
    indices.processChangeset(changeset)
    assert(matcher.getAllValues("class").contains(clazz.uri))

    val clazz2 = ClassDeclaration("Foo", true, List(FieldDeclaration("baz", PublicVisibility())))
    val (diffset, updatedclazz) = clazz.compareTo(clazz2)
    indices.processChangeset(diffset)
    assert(matcher.getAllMatches.isEmpty)

    val clazz3 = ClassDeclaration("Foo", false, List(FieldDeclaration("baz", PublicVisibility())))
    val (diffset2, updatedclazz2) = updatedclazz.compareTo(clazz3)
    indices.processChangeset(diffset2)
    assert(matcher.getAllMatches.isEmpty)

    val clazz4 = ClassDeclaration("Foo", true, List(
      FieldDeclaration("bar", PrivateVisibility()),
      FieldDeclaration("baz", PublicVisibility()),
      FieldDeclaration("baaz", ProtectedVisibility())))
    val (diffset3, updatedclazz3) = updatedclazz2.compareTo(clazz4)
    indices.processChangeset(diffset3)
    assert(matcher.getAllValues("class").contains(updatedclazz3.uri))

    EnginePool.disposeAllEngines()
  }

}