package inca.frontend.constraint.datamodelresolver

import inca.frontend.constraint.core.tree.NativeDataModel
import org.scalatest.funsuite.AnyFunSuite
import inca.analyzedLangs.tinyJava
import inca.analyzedLangs.Exp


class NativeDataModelResolverTest extends AnyFunSuite {
  test("resolve top-level declarations") {
    val resolver = new DataModelResolver with NativeDataModelResolver {}
    val lmi = resolver.resolve(NativeDataModel("inca.analyzedLangs.tinyJava.model"))
    assert(lmi.types.forall { typ =>
      tinyJava.model.types.contains(typ)
    })
  }
  test("resolve declarations within object") {
    val resolver = new DataModelResolver with NativeDataModelResolver {}
    val lmi = resolver.resolve(NativeDataModel("inca.analyzedLangs.Exp.model"))
    assert(lmi.types.forall { typ =>
      Exp.model.types.contains(typ)
    })
  }

}
