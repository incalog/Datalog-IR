package inca.findbugs

import inca.MetaElements.NodeType
import inca.analyzedLangs.{ClassDeclaration, FieldDeclaration, ProtectedVisibility}
import inca.backend.indices.{EnginePool, TFQueryScope}
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite

class FindBugsTests extends AnyFunSuite {

  val clazz = ClassDeclaration("Foo", true, List(FieldDeclaration("bar", ProtectedVisibility())))

  test("Confused Inheritance") {
    val scope = new TFQueryScope(clazz)
    val matcher = EnginePool.getMatcher(ConfusedInheritance.instance(), scope, DifferentialReteBackendFactory.INSTANCE)
    println(matcher.getAllMatches)

    val indices = scope.getEngineContext.getBaseIndex
    indices.update(() => {
      indices.deleteNodeLinkInstance(clazz, NodeType(classOf[ClassDeclaration])("isFinal"), true)
      indices.deleteDataTypeInstance(true)
      indices.insertNodeLinkInstance(clazz, NodeType(classOf[ClassDeclaration])("isFinal"), false)
      indices.insertDataTypeInstance(false)
    })
    println(matcher.getAllMatches)

    indices.update(() => {
      indices.deleteNodeLinkInstance(clazz, NodeType(classOf[ClassDeclaration])("isFinal"), false)
      indices.deleteDataTypeInstance(false)
      indices.insertNodeLinkInstance(clazz, NodeType(classOf[ClassDeclaration])("isFinal"), true)
      indices.insertDataTypeInstance(true)
    })
    println(matcher.getAllMatches)

    EnginePool.disposeAllEngines()
  }

}