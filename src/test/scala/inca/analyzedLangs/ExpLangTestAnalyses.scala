package inca.analyzedLangs

import inca.frontend.constraint.core._

import scala.language.implicitConversions

object ExpLangTestAnalyses {
  private val addType: TNode = TNode(Exp.addTag)
  private val expType: TNode = TNode(Exp.expTag)
  private val boolType: TNode = TNode(Exp.boolTag)

  implicit def name(s: String): Name = Name(s)

  val idFun: PatternFunction = PatternFunction(
    Seq(),
    None,
    "id",
    Seq(Param("add", addType)),
    expType,
    Seq(
      Body(
        Seq(
          Yield(Var("add"))))))

  val lhsLink: Link = addType("lhs")
  val rhsLink: Link = addType("rhs")
  val childrenFun: PatternFunction = PatternFunction(
    Seq(),
    None,
    "children",
    Seq(Param("add", addType)),
    expType,
    Seq(
      Body(
        Seq(
          Yield(PathAccess(Var("add"), lhsLink)))),
      Body(
        Seq(
          Yield(PathAccess(Var("add"), rhsLink))))))

  val lhChildFun: PatternFunction = PatternFunction(
    Seq(),
    None,
    "lhChild",
    Seq(Param("add", addType)),
    expType,
    Seq(
      Body(
        Seq(
          Yield(PathAccess(Var("add"), lhsLink))))))

  val callLhChildFun = PatternFunction(
    Seq(MainFunctionAnno),
    None,
    "callLhChild",
    Seq(Param("add", addType)),
    expType,
    Seq(
      Body(
        Seq(
          Assign(Seq("lhschild"), Call("lhChild", Seq(Var("add")))),
          Yield(Var("lhschild"))))))

  val instanceAddFun = PatternFunction(
    Seq(),
    None,
    "instanceAdd",
    Seq(Param("add", addType)),
    expType,
    Seq(
      Body(
        Seq(
          Assign(Seq("lhschild"), PathAccess(Var("add"), lhsLink)),
          Assert(InstanceOf(Var("lhschild"), addType)),
          Yield(Var("lhschild"))))))

  val noParamTypeFun = PatternFunction(
    Seq(),
    None,
    "noParamType",
    Seq(Param("add", TAny)),
    TUnit,
    Seq(
      Body(
        Seq(
          Assert(InstanceOf(Var("add"), addType))))))

  val isBooleanFun = PatternFunction(
    Seq(),
    None,
    "isBoolean",
    Seq(Param("in", boolType)),
    TScalaBoolean,
    Seq(
      Body(
        Seq(
          Yield(Constant(BooleanLiteral(false)))))))

  val primitiveParamFun = PatternFunction(
    Seq(),
    None,
    "idBool",
    Seq(Param("in", TLiteral.Bool)),
    TLiteral.Bool,
    Seq(
      Body(
        Seq(Yield(Var("in"))))))
}
