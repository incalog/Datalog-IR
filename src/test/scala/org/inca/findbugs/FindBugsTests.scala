package org.inca.findbugs

import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.inca.incer.indices.{EnginePool, TFQueryScope}
import org.scalatest.funsuite.AnyFunSuite

class FindBugsTests extends AnyFunSuite {

  val clazz = ClassDeclaration("Foo", true, List(FieldDeclaration("bar", ProtectedVisibility())))

  test("Confused Inheritance") {
    val scope = new TFQueryScope(clazz)
    val matcher = EnginePool.getMatcher(ConfusedInheritance.instance(), scope, DifferentialReteBackendFactory.INSTANCE)
    val matches = matcher.getAllMatches
    println(matches)
    EnginePool.disposeAllEngines()
  }

}