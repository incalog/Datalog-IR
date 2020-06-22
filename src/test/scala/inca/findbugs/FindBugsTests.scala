package inca.findbugs

import inca.MetaElements.{ListType, NodeType}
import inca.lang.FunLang._
import inca.analyzedLangs.{ClassDeclaration, FieldDeclaration, PrivateVisibility, ProtectedVisibility, PublicVisibility}
import inca.backend.indices.{EnginePool, LanguageMetaInfo, QueryScope}
import inca.backend.virtual.{ParentIndex, VirtualIndex}
import inca.lang.FunLang
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite
import truediff.Diffable

import scala.collection.mutable

class FindBugsTests extends AnyFunSuite {

  val clazz = ClassDeclaration("Foo", true, List(FieldDeclaration("bar", ProtectedVisibility())))

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
    )
  )

  test("Confused Inheritance") {
    val classDeclType = NodeType(classOf[ClassDeclaration].getCanonicalName)
    val fieldDeclType = NodeType(classOf[FieldDeclaration].getCanonicalName)
    val privateVisType = NodeType(classOf[PrivateVisibility].getCanonicalName)
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
            Assignment(Seq("member"), Var("members")),
            Assert(InstanceOf(Var("member"), fieldDeclType)),
            Assert(InstanceOf(PathAccess(Var("member"), Seq(fieldDeclType("visibility"))), privateVisType))
          ))))
    val module = Module("FindBugs", Seq(), Seq(confusedInheritance))

    val scope = new QueryScope(langMetaInfo, Map[String, VirtualIndex]("parent" -> new ParentIndex))
    val changeset = Diffable.load(clazz)
    val matcher = EnginePool.getMatcher(ConfusedInheritance.instance(), scope, DifferentialReteBackendFactory.INSTANCE)
    val indices = scope.getEngineContext.getBaseIndex
    indices.processChangeset(changeset)
    println(matcher.getAllMatches)
    val clazz2 = ClassDeclaration("Foo", false, List(FieldDeclaration("bar", ProtectedVisibility())))
    val (diffset, _) = clazz.compareTo(clazz2)
    indices.update(() => {
      indices.processChangeset(diffset)
    })
    println(matcher.getAllMatches)

    EnginePool.disposeAllEngines()
  }

}