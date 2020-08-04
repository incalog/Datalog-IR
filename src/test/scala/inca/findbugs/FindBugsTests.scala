package inca.findbugs

import inca.analyzedLangs.tinyJava
import inca.lang.fun.Fun._
import inca.runtime.EnginePool
import inca.runtime.context.QueryScope
import inca.util.Meta
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import truediff.Diffable
//import inca.trans.generated.FindBugs_confusedInheritanceQuerySpecification
import org.scalatest.funsuite.AnyFunSuite

class FindBugsTests extends AnyFunSuite {


  test("Confused Inheritance") {
    val classDeclType = TNode(tinyJava.classDeclTag)
    val classMemberType = TNode(tinyJava.classMemberTag)
    val fieldDeclType = TNode(tinyJava.fieldDeclTag)
    val visType = TNode(tinyJava.visTag)
    val protectedVisType = TNode(tinyJava.protectedVisTag)
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



    val scope = new QueryScope(tinyJava.langMetaInfo)
    val spec = Meta.loadModule(module).patterns("confusedInheritance")
    val (feed,matcher) = EnginePool.loadQuery(spec(), scope, DifferentialReteBackendFactory.INSTANCE)

    import tinyJava._
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