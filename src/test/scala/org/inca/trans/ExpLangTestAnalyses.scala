package org.inca.trans

import org.inca.analyzedLangs.expLang._
import org.inca.lang.FunLang.{Exp => _, _}
import org.inca.meta.MetaElements._

object ExpLangTestAnalyses {
  private val addType: NodeType = NodeType(classOf[Add])
  private val expType: NodeType = NodeType(classOf[Exp])
  val idFun: PatternFunction = PatternFunction(
    None,
    "id",
    List(Param("add", Some(addType))),
    List(AnnoParam("out", expType)),
    List(
      Alternative(
        List(
          Return(Var("add"))))))

  val lhsLink: Link = addType("lhs")
  val rhsLink: Link = addType("rhs")
  val childrenFun: PatternFunction = PatternFunction(
    None,
    "children",
    List(Param("add", Some(addType))),
    List(AnnoParam("out", expType)),
    List(
      Alternative(
        List(
          Return(PathAccess(Var("add"), Seq(lhsLink))))),
      Alternative(
        List(
          Return(PathAccess(Var("add"), Seq(rhsLink)))))))

  val lhChildFun: PatternFunction = PatternFunction(
    None,
    "lhChild",
    List(Param("add", Some(addType))),
    List(AnnoParam("out", expType)),
    List(
      Alternative(
        List(
          Return(PathAccess(Var("add"), Seq(lhsLink)))))))

  val callLhChildFun = PatternFunction(
    None,
    "callLhChild",
    List(Param("add", Some(addType))),
    List(AnnoParam("out", expType)),
    List(
      Alternative(
        List(
          Assignment(Seq("lhschild"), Call(PatternCall("lhChild", Seq(Var("add")), transitive = false), count = false)),
          Return(Var("lhschild"))))))

  val instanceAddFun = PatternFunction(
    None,
    "instanceAdd",
    List(Param("add", Some(addType))),
    List(AnnoParam("out", expType)),
    List(
      Alternative(
        List(
          Assignment(Seq("lhschild"), PathAccess(Var("add"), Seq(lhsLink))),
          Assert(InstanceOf(Var("lhschild"), addType)),
          Return(Var("lhschild"))))))
}
