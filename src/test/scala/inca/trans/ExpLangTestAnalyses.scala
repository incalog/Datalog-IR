package inca.trans

import inca.analyzedLangs.expLang._
import inca.lang.fun.Fun.{Exp => _, _}

object ExpLangTestAnalyses {
  private val addType: TNode = TNode(classOf[Add].getCanonicalName)
  private val expType: TNode = TNode(classOf[Exp].getCanonicalName)
  val idFun: PatternFunction = PatternFunction(
    None,
    "id",
    Seq(Param("add", Some(addType))),
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
    Seq(Param("add", Some(addType))),
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
    Seq(Param("add", Some(addType))),
    Seq(AnnoParam(None, expType)),
    Seq(
      Body(
        Seq(
          Yield(PathAccess(Var("add"), lhsLink).typed(expType))))))

  val callLhChildFun = PatternFunction(
    None,
    "callLhChild",
    Seq(Param("add", Some(addType))),
    Seq(AnnoParam(None, expType)),
    Seq(
      Body(
        Seq(
          Assign(Seq("lhschild"), Call("lhChild", Seq(Var("add")), transitive = false, count = false)),
          Yield(Var("lhschild"))))))

  val instanceAddFun = PatternFunction(
    None,
    "instanceAdd",
    Seq(Param("add", Some(addType))),
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
    Seq(Param("add", None)),
    Seq(),
    Seq(
      Body(
        Seq(
          Assert(InstanceOf(Var("add"), addType))))))

  private val boolType: TNode = TNode(classOf[BooleanLit].getCanonicalName)
  val isBooleanFun = PatternFunction(
    None,
    "isBoolean",
    Seq(Param("in", Some(boolType))),
    Seq(AnnoParam(None, TBool)),
    Seq(
      Body(
        Seq(
          Yield(Constant(BooleanLiteral(true)))))))

  val primitiveParamFun = PatternFunction(
    None,
    "idBool",
    Seq(Param("in", Some(TBool))),
    Seq(AnnoParam(None, TBool)),
    Seq(
      Body(
        Seq(Yield(Var("in"))))))
}
