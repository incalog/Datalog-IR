package inca.trans

import inca.analyzedLangs.expLang._
import inca.lang.FunLang.{Exp => _, _}
import inca.MetaElements._

object ExpLangTestAnalyses {
  private val addType: NodeType = NodeType(classOf[Add].getCanonicalName)
  private val expType: NodeType = NodeType(classOf[Exp].getCanonicalName)
  val idFun: PatternFunction = PatternFunction(
    None,
    "id",
    Seq(Param("add", Some(addType))),
    Seq(AnnoParam(Some("out"), expType)),
    Seq(
      Alternative(
        Seq(
          Return(Var("add"))))))

  val lhsLink: Link = addType("lhs")
  val rhsLink: Link = addType("rhs")
  val childrenFun: PatternFunction = PatternFunction(
    None,
    "children",
    Seq(Param("add", Some(addType))),
    Seq(AnnoParam(None, expType)),
    Seq(
      Alternative(
        Seq(
          Return(PathAccess(Var("add"), Seq(lhsLink))))),
      Alternative(
        Seq(
          Return(PathAccess(Var("add"), Seq(rhsLink)))))))

  val lhChildFun: PatternFunction = PatternFunction(
    None,
    "lhChild",
    Seq(Param("add", Some(addType))),
    Seq(AnnoParam(None, expType)),
    Seq(
      Alternative(
        Seq(
          Return(PathAccess(Var("add"), Seq(lhsLink)))))))

  val callLhChildFun = PatternFunction(
    None,
    "callLhChild",
    Seq(Param("add", Some(addType))),
    Seq(AnnoParam(None, expType)),
    Seq(
      Alternative(
        Seq(
          Assignment(Seq("lhschild"), Call(PatternCall("lhChild", Seq(Var("add")), transitive = false), count = false)),
          Return(Var("lhschild"))))))

  val instanceAddFun = PatternFunction(
    None,
    "instanceAdd",
    Seq(Param("add", Some(addType))),
    Seq(AnnoParam(None, expType)),
    Seq(
      Alternative(
        Seq(
          Assignment(Seq("lhschild"), PathAccess(Var("add"), Seq(lhsLink))),
          Assert(InstanceOf(Var("lhschild"), addType)),
          Return(Var("lhschild"))))))

  val noParamTypeFun = PatternFunction(
    None,
    "noParamType",
    Seq(Param("add", None)),
    Seq(),
    Seq(
      Alternative(
        Seq(
          Assert(InstanceOf(Var("add"), addType))))))

  private val boolType: NodeType = NodeType(classOf[BooleanLit].getCanonicalName)
  val isBooleanFun = PatternFunction(
    None,
    "isBoolean",
    Seq(Param("in", Some(boolType))),
    Seq(AnnoParam(None, TBool)),
    Seq(
      Alternative(
        Seq(
          Return(Constant(BooleanLiteral(true)))))))

  val primitiveParamFun = PatternFunction(
    None,
    "idBool",
    Seq(Param("in", Some(TBool))),
    Seq(AnnoParam(None, TBool)),
    Seq(
      Alternative(
        Seq(Return(Var("in"))))))
}
