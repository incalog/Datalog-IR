package inca.findbugs

import inca.MetaElements.NodeType
import inca.analyzedLangs.{BooleanConstant, ClassDeclaration, FieldDeclaration, ProtectedVisibility}
import inca.backend.indices.{EnginePool, QueryScope}
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite
import truediff.Diffable

class FindBugsTests extends AnyFunSuite {

  val clazz = ClassDeclaration("Foo", BooleanConstant(true), List(FieldDeclaration("bar", ProtectedVisibility())))

  test("Confused Inheritance") {
    // TODO support lists in the backend and adapt the analysis
    val scope = new QueryScope(null, null, null)
    val changeset = Diffable.load(clazz)
    val matcher = EnginePool.getMatcher(ConfusedInheritance.instance(), scope, DifferentialReteBackendFactory.INSTANCE)
    val indices = scope.getEngineContext.getBaseIndex
    indices.processChangeset(changeset)
    println(matcher.getAllMatches)
    val clazz2 = ClassDeclaration("Foo", BooleanConstant(false), List(FieldDeclaration("bar", ProtectedVisibility())))
    val (diffset, _) = clazz.compareTo(clazz2)
    indices.update(() => {
      indices.processChangeset(diffset)
    })
    println(matcher.getAllMatches)

    EnginePool.disposeAllEngines()
  }

}