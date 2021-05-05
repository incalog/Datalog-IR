package inca.caseStudies.findbugs

import inca.analyzedLangs.tinyJava
import inca.compiler.{Compiler, ConstraintOptions, Options}
import inca.executor.ConstraintExecutor
import inca.frontend.constraint.core.tree._
import inca.runtime.EnginePool
import inca.runtime.context.QueryScope
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite
import truediff.Diffable

import scala.language.implicitConversions

class FindBugsTests extends AnyFunSuite {

  implicit def name(s: String): Name = Name(s)

  test("Confused Inheritance") {
    val classDeclType = TNode(tinyJava.classDeclTag)
    val classMemberType = TNode(tinyJava.classMemberTag)
    val fieldDeclType = TNode(tinyJava.fieldDeclTag)
    val visType = TNode(tinyJava.visTag)
    val protectedVisType = TNode(tinyJava.protectedVisTag)
    val code =
      s"""
         |module FindBugs
         |datamodel inca.analyzedLangs.tinyJava.model
         |
         |node inca.analyzedLangs.tinyJava._
         |
         |
         |def confusedInheritance(class: ClassDeclaration): Unit = {
         |  assert class.isFinal == true
         |  val members = class.members
         |  val member = members.children
         |  val fieldDecl = member:FieldDeclaration
         |  assert fieldDecl.visibility.isInstanceOf[ProtectedVisibility]
         |  yield unit
         |}
         |""".stripMargin

    val scope = new QueryScope(tinyJava.model)
    val options = ConstraintOptions()
    val spec = Compiler.compileConstraint(code, options).psystemModule.patterns("confusedInheritance")

    val feed = EnginePool.loadDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    val matcher = EnginePool.loadQuery(spec(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

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