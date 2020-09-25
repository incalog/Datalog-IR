package inca.caseStudies.findbugs

import inca.analyzedLangs.tinyJava
import inca.frontend.core.Core._
import inca.runtime.EnginePool
import inca.runtime.context.QueryScope
import inca.{Compiler, CompilerOptions}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truediff.Diffable
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
      Seq(Param("class", classDeclType)),
      Seq(),
      Seq(
        Body(
          Seq(
            Assert(Eq(PathAccess(Var("class"), classDeclType("isFinal")).typed(TBool), Constant(BooleanLiteral(true)))),
            Assign(Seq("members"), PathAccess(Var("class"), classDeclType("members")).typed(TList(classMemberType))),
            Assign(Seq("member"), PathAccess(Var("members").typed(TList(classMemberType)), ChildrenLink).typed(classMemberType)),
            Assert(InstanceOf(Var("member"), fieldDeclType)),
            Assert(InstanceOf(PathAccess(Var("member"), fieldDeclType("visibility")).typed(visType), protectedVisType))
          ))))
    val module = Module("FindBugs", Seq(), Seq(confusedInheritance))



    val scope = new QueryScope(tinyJava.langMetaInfo)
    val options = CompilerOptions(tinyJava.langMetaInfo)
    val spec = Compiler.compileAndLoadFunModule(module, compilerOptions = options).patterns("confusedInheritance")
    val (feed,matcher) = EnginePool.loadQuery(spec(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

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