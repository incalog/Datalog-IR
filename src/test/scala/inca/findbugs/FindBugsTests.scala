package inca.findbugs

import inca.analyzedLangs._
import inca.lang.fun.Fun._
import inca.runtime.EnginePool
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import inca.runtime.index.dynamic.ParentIndex
import inca.trans.generated.FindBugs_confusedInheritanceQuerySpecification
import inca.{AnalysisWriter, analyzedLangs}
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import truechange.{JavaLitType, ListType, SortType}
import truediff.Diffable
//import inca.trans.generated.FindBugs_confusedInheritanceQuerySpecification
import org.scalatest.funsuite.AnyFunSuite

class FindBugsTests extends AnyFunSuite {


  val classDeclTag = classOf[ClassDeclaration].getCanonicalName
  val classDeclType = SortType(classDeclTag)
  val classMemberTag = classOf[ClassMember].getCanonicalName
  val classMemberType = SortType(classMemberTag)
  val fieldDeclTag = classOf[FieldDeclaration].getCanonicalName
  val fieldDeclType = SortType(fieldDeclTag)
  val visType = SortType(classOf[analyzedLangs.Visibility].getCanonicalName)
  val privateVisTag = classOf[PrivateVisibility].getCanonicalName
  val privateVisType = SortType(privateVisTag)
  val publicVisTag = classOf[PublicVisibility].getCanonicalName
  val publicVisType = SortType(publicVisTag)

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
      (classDeclTag->"members") -> ListType(classMemberType),
      (fieldDeclTag->"visibility") -> visType
    ),
    Map(
      (classDeclTag->"name") -> JavaLitType(classOf[java.lang.String]),
      (classDeclTag->"isFinal") -> JavaLitType(classOf[java.lang.Boolean]),
      (fieldDeclTag->"name") -> JavaLitType(classOf[java.lang.String])
    )
  )

  test("Confused Inheritance") {
    val classDeclType = TNode(classDeclTag)
    val classMemberType = TNode(classMemberTag)
    val fieldDeclType = TNode(fieldDeclTag)
    val visType = TNode(classOf[analyzedLangs.Visibility].getCanonicalName)
    val protectedVisType = TNode(classOf[ProtectedVisibility].getCanonicalName)
    val confusedInheritance = PatternFunction(
      None,
      "confusedInheritance",
      Seq(Param("class", Some(classDeclType))),
      Seq(),
      Seq(
        Body(
          Seq(
            Assert(Eq(PathAccess(Var("class"), classDeclType("isFinal")).typed(TBool), Constant(BooleanLiteral(true)))),
            Assign(Seq("members"), PathAccess(Var("class"), classDeclType("members")).typed(TList(classMemberType))),
            Assign(Seq("member"), PathAccess(Var("members"), ChildrenLink).typed(classMemberType)),
            Assert(InstanceOf(Var("member"), fieldDeclType)),
            Assert(InstanceOf(PathAccess(Var("member"), fieldDeclType("visibility")).typed(visType), protectedVisType))
          ))))
    val module = Module("FindBugs", Seq(), Seq(confusedInheritance))
    AnalysisWriter.writeModule(module)

    val additionalIndices = Seq(new ParentIndex)
    val scope = QueryScope(langMetaInfo, additionalIndices)
    val (feed,matcher) = EnginePool.loadQuery(FindBugs_confusedInheritanceQuerySpecification.instance, scope, DifferentialReteBackendFactory.INSTANCE)

    val clazz = ClassDeclaration("Foo", true, List(FieldDeclaration("baz", PublicVisibility()), FieldDeclaration("bar", ProtectedVisibility())))
    val editScript = Diffable.load(clazz)
    feed.processEditScript(editScript)
    assert(matcher.getAllValues("class").contains(clazz.uri))

    val clazz2 = ClassDeclaration("Foo", true, List(FieldDeclaration("baz", PublicVisibility())))
    val (diffset, updatedclazz) = clazz.compareTo(clazz2)
    feed.processEditScript(diffset)
    assert(matcher.getAllMatches.isEmpty)

    val clazz3 = ClassDeclaration("Foo", false, List(FieldDeclaration("baz", PublicVisibility())))
    val (diffset2, updatedclazz2) = updatedclazz.compareTo(clazz3)
    feed.processEditScript(diffset2)
    assert(matcher.getAllMatches.isEmpty)

    val clazz4 = ClassDeclaration("Foo", true, List(
      FieldDeclaration("bar", PrivateVisibility()),
      FieldDeclaration("baz", PublicVisibility()),
      FieldDeclaration("baaz", ProtectedVisibility())))
    val (diffset3, updatedclazz3) = updatedclazz2.compareTo(clazz4)
    feed.processEditScript(diffset3)
    assert(matcher.getAllValues("class").contains(updatedclazz3.uri))

  }

}