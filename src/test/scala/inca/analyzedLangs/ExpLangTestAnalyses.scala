package inca.analyzedLangs

import inca.frontend.core.Core._

object ExpLangTestAnalyses {
  private val addType: TNode = TNode(Exp.addTag)
  private val expType: TNode = TNode(Exp.expTag)
  private val boolType: TNode = TNode(Exp.boolTag)

  val idFun: PatternFunction = PatternFunction(
    None,
    "id",
    Seq(Param("add", addType)),
    Seq(AnnoParam(Some("out"), expType)),
    Seq(
      Body(
        Seq(
          Yield(Var("add"))))))

  val lhsLink: Link = addType("lhs")
  val rhsLink: Link = addType("rhs")
  val childrenFun: PatternFunction = PatternFunction(
    None,
    "children",
    Seq(Param("add", addType)),
    Seq(AnnoParam(None, expType)),
    Seq(
      Body(
        Seq(
          Yield(PathAccess(Var("add"), lhsLink).typed(expType)))),
      Body(
        Seq(
          Yield(PathAccess(Var("add"), rhsLink).typed(expType))))))

  val lhChildFun: PatternFunction = PatternFunction(
    None,
    "lhChild",
    Seq(Param("add", addType)),
    Seq(AnnoParam(None, expType)),
    Seq(
      Body(
        Seq(
          Yield(PathAccess(Var("add"), lhsLink).typed(expType))))))

  val callLhChildFun = PatternFunction(
    None,
    "callLhChild",
    Seq(Param("add", addType)),
    Seq(AnnoParam(None, expType)),
    Seq(
      Body(
        Seq(
          Assign(Seq("lhschild"), Call("lhChild", Seq(Var("add"))).typed(expType)),
          Yield(Var("lhschild"))))))

  val instanceAddFun = PatternFunction(
    None,
    "instanceAdd",
    Seq(Param("add", addType)),
    Seq(AnnoParam(None, expType)),
    Seq(
      Body(
        Seq(
          Assign(Seq("lhschild"), PathAccess(Var("add"), lhsLink).typed(expType)),
          Assert(InstanceOf(Var("lhschild"), addType)),
          Yield(Var("lhschild"))))))

  val noParamTypeFun = PatternFunction(
    None,
    "noParamType",
    Seq(Param("add", TAny)),
    Seq(),
    Seq(
      Body(
        Seq(
          Assert(InstanceOf(Var("add"), addType))))))

  val isBooleanFun = PatternFunction(
    None,
    "isBoolean",
    Seq(Param("in", boolType)),
    Seq(AnnoParam(None, TBool)),
    Seq(
      Body(
        Seq(
          Yield(Constant(BooleanLiteral(false)))))))

  val primitiveParamFun = PatternFunction(
    None,
    "idBool",
    Seq(Param("in", TBool)),
    Seq(AnnoParam(None, TBool)),
    Seq(
      Body(
        Seq(Yield(Var("in"))))))
}
