package inca.runtime

import inca.runtime.context.LanguageMetaInfo
import inca.runtime.index.MetaElements.NodeType
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class LanguageMetaInfoTest extends AnyFunSuite {
  val exp = NodeType("Exp")
  val add = NodeType("Add")
  val mult = NodeType("Mult")
  val node = NodeType("Node")
  val iNumExp = NodeType("INumExp")
  val and = NodeType("And")

  test("0 step trans closure") {
    val metaInfo = new LanguageMetaInfo(
      Map(
        add -> Set(exp),
        mult -> Set(exp),
        exp -> Set())
      , null)
    metaInfo.nodeSupertypes should contain allOf (add -> Set(exp), mult -> Set(exp), exp -> Set())
    metaInfo.directNodeSubtypes should contain allOf (add -> Set(), mult -> Set(), exp -> Set(add, mult))
    metaInfo.nodeSubtypes should contain allOf (add -> Set(), mult -> Set(), exp -> Set(add, mult))
  }

  test("1 step trans closure") {
    val metaInfo = new LanguageMetaInfo(
      Map(
        add -> Set(iNumExp),
        mult -> Set(iNumExp),
        iNumExp -> Set(exp),
        exp -> Set())
      , null)
    metaInfo.nodeSupertypes should contain allOf (add -> Set(iNumExp, exp), mult -> Set(iNumExp, exp), iNumExp -> Set(exp), exp -> Set())
    metaInfo.directNodeSubtypes should contain allOf (add -> Set(), mult -> Set(), exp -> Set(iNumExp), iNumExp -> Set(add, mult))
    metaInfo.nodeSubtypes should contain allOf (add -> Set(), mult -> Set(), exp -> Set(add, mult, iNumExp), iNumExp -> Set(add, mult))
  }

  test("2 step trans closure") {
    val metaInfo = new LanguageMetaInfo(
      Map(
        add -> Set(iNumExp),
        mult -> Set(iNumExp),
        iNumExp -> Set(exp),
        exp -> Set(node),
        node -> Set())
      , null)
    metaInfo.nodeSupertypes should contain allOf (add -> Set(iNumExp, exp, node), mult -> Set(iNumExp, exp, node), iNumExp -> Set(exp, node), exp -> Set(node), node -> Set())
    metaInfo.directNodeSubtypes should contain allOf (add -> Set(), mult -> Set(), exp -> Set(iNumExp), iNumExp -> Set(add, mult), node -> Set(exp))
    metaInfo.nodeSubtypes should contain allOf (add -> Set(), mult -> Set(), exp -> Set(add, mult, iNumExp), iNumExp -> Set(add, mult), node -> Set(exp, iNumExp, add, mult))
  }

  test("inital multi inheritance trans closure") {
    val metaInfo = new LanguageMetaInfo(
      Map(
        and -> Set(exp, node),
        add -> Set(exp, iNumExp, exp),
        iNumExp -> Set(exp),
        exp -> Set(node),
        node -> Set())
      , null)
    metaInfo.nodeSupertypes should contain allOf (and -> Set(exp, node), add -> Set(iNumExp, exp, node), iNumExp -> Set(exp, node), exp -> Set(node), node -> Set())
    metaInfo.directNodeSubtypes should contain allOf (add -> Set(), exp -> Set(iNumExp, add, and), iNumExp -> Set(add), node -> Set(exp, and))
    metaInfo.nodeSubtypes should contain allOf (add -> Set(), and -> Set(), exp -> Set(add, and, iNumExp), iNumExp -> Set(add), node -> Set(exp, iNumExp, add, and))
  }
}
