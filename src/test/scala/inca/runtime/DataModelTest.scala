package inca.runtime

import inca.runtime.context.DataModel
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import truechange.SortType

import scala.collection.immutable.MultiDict

class DataModelTest extends AnyFunSuite {
  val exp = SortType("Exp")
  val add = SortType("Add")
  val mult = SortType("Mult")
  val node = SortType("Node")
  val iNumExp = SortType("INumExp")
  val and = SortType("And")

  test("0 step trans closure") {
    val metaInfo = new DataModel(
      Set(),
      MultiDict(
        add -> exp,
        mult -> exp)
      , null, null)
    metaInfo.nodeSupertypes.toSet should contain theSameElementsAs Set(add -> exp, mult -> exp)
    metaInfo.directNodeSubtypes.toSet should contain theSameElementsAs Set(exp -> add, exp -> mult)
    metaInfo.nodeSubtypes.toSet should contain theSameElementsAs Set(exp -> add, exp -> mult)
  }

  test("1 step trans closure") {
    val metaInfo = new DataModel(
      Set(),
      MultiDict(
        add -> iNumExp,
        mult -> iNumExp,
        iNumExp -> exp)
      , null, null)
    metaInfo.nodeSupertypes.toSet should contain theSameElementsAs Set(add -> iNumExp, add -> exp, mult -> iNumExp, mult -> exp, iNumExp -> exp)
    metaInfo.directNodeSubtypes.toSet should contain theSameElementsAs Set(exp -> iNumExp, iNumExp -> add, iNumExp -> mult)
    metaInfo.nodeSubtypes.toSet should contain theSameElementsAs Set(exp -> add, exp -> mult, exp -> iNumExp, iNumExp -> add, iNumExp -> mult)
  }

  test("2 step trans closure") {
    val metaInfo = new DataModel(
      Set(),
      MultiDict(
        add -> iNumExp,
        mult -> iNumExp,
        iNumExp -> exp,
        exp -> node)
      , null, null)
    metaInfo.nodeSupertypes.toSet should contain theSameElementsAs Set(add -> iNumExp, add -> exp, add -> node, mult -> iNumExp, mult -> exp, mult -> node, iNumExp -> exp, iNumExp -> node, exp -> node)
    metaInfo.directNodeSubtypes.toSet should contain theSameElementsAs Set(exp -> iNumExp, iNumExp -> add, iNumExp -> mult, node -> exp)
    metaInfo.nodeSubtypes.toSet should contain theSameElementsAs Set(exp -> add, exp -> mult, exp -> iNumExp, iNumExp -> add, iNumExp -> mult, node -> exp, node -> iNumExp, node -> add, node -> mult)
  }

  test("inital multi inheritance trans closure") {
    val metaInfo = new DataModel(
      Set(),
      MultiDict(
        and -> exp,
        and -> node,
        add -> exp,
        add -> iNumExp,
        add -> exp,
        iNumExp -> exp,
        exp -> node)
      , null, null)
    metaInfo.nodeSupertypes.toSet should contain theSameElementsAs Set(and -> exp, and -> node, add -> iNumExp, add -> exp, add -> node, iNumExp -> exp, iNumExp -> node, exp -> node)
    metaInfo.directNodeSubtypes.toSet should contain theSameElementsAs Set(exp -> iNumExp, exp -> add, exp -> and, iNumExp -> add, node -> exp, node -> and)
    metaInfo.nodeSubtypes.toSet should contain theSameElementsAs Set(exp -> add, exp -> and, exp -> iNumExp, iNumExp -> add, node -> exp, node -> iNumExp, node -> add, node -> and)
  }
}
