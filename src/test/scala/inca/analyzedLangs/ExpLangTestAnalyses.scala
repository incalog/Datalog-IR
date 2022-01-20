package inca.analyzedLangs

import inca.backend.ir.Datalog
import inca.frontend.constraint.core._

import scala.language.implicitConversions

object ExpLangTestAnalyses {
  private val addType: TNode = TNode(Exp.addTag)
  private val expType: TNode = TNode(Exp.expTag)
  private val boolType: TNode = TNode(Exp.boolTag)

  implicit def name(s: String): Name = Name(s)

  val idFun: PatternFunction = PatternFunction(
    Seq(MainFunctionAnno),
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
    Seq(MainFunctionAnno),
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
    Seq(MainFunctionAnno),
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
    Seq(MainFunctionAnno),
    None,
    "noParamType",
    Seq(Param("add", TAny)),
    TUnit,
    Seq(
      Body(
        Seq(
          Assert(InstanceOf(Var("add"), addType))))))

  val isBooleanFun = PatternFunction(
    Seq(MainFunctionAnno),
    None,
    "isBoolean",
    Seq(Param("in", boolType)),
    TScalaBoolean,
    Seq(
      Body(
        Seq(
          Yield(Constant(BooleanLiteral(false)))))))

  val primitiveParamFun = PatternFunction(
    Seq(MainFunctionAnno),
    None,
    "idBool",
    Seq(Param("in", TLiteral.Bool)),
    TLiteral.Bool,
    Seq(
      Body(
        Seq(Yield(Var("in"))))))


  val mulPattern =
    Datalog.Pattern(None, "mul", Seq(Datalog.Param("mul", Datalog.TNode(Exp.expTag))),
      Seq(
        Datalog.Body(Seq(
          Datalog.HasType(Datalog.Var("mul"), Datalog.TNode(Exp.multTag))))
      ))

  val mulIntLitPattern =
    Datalog.Pattern(None, "mulIntLit", Seq(Datalog.Param("mul", Datalog.TNode(Exp.expTag)), Datalog.Param("intLit", Datalog.TNode(Exp.intTag))),
      Seq(
        Datalog.Body(Seq(
          Datalog.HasType(Datalog.Var("mul"), Datalog.TNode(Exp.multTag)),
          Datalog.HasType(Datalog.Var("intLit"), Datalog.TNode(Exp.intTag))))
      ))

  val lhsPattern =
    Datalog.Pattern(None, "lhs", Seq(Datalog.Param("exp", Datalog.TNode(Exp.expTag)), Datalog.Param("res", Datalog.TNode(Exp.expTag))),
      Seq(
        Datalog.Body(Seq(
          Datalog.HasType(Datalog.Var("exp"), Datalog.TNode(Exp.addTag)),
          Datalog.Path(Datalog.Var("exp"), Datalog.TNode(Exp.addTag), Datalog.NamedLink(Datalog.TNode(Exp.addTag), "lhs"), Datalog.Var("res"), Datalog.TNode(Exp.expTag)),
        )),
        Datalog.Body(Seq(
          Datalog.HasType(Datalog.Var("exp"), Datalog.TNode(Exp.multTag)),
          Datalog.Path(Datalog.Var("exp"), Datalog.TNode(Exp.multTag), Datalog.NamedLink(Datalog.TNode(Exp.multTag), "lhs"), Datalog.Var("res"), Datalog.TNode(Exp.expTag)),
        )),
      ))

  val lhsPattern2 =
    Datalog.Pattern(None, "lhs", Seq(Datalog.Param("exp", Datalog.TNode(Exp.expTag)), Datalog.Param("res", Datalog.TNode(Exp.expTag))),
      Seq(
        Datalog.Body(Seq(
          Datalog.Path(Datalog.Var("exp"), Datalog.TNode(Exp.addTag), Datalog.NamedLink(Datalog.TNode(Exp.addTag), "lhs"), Datalog.Var("res"), Datalog.TNode(Exp.expTag)),
        )),
        Datalog.Body(Seq(
          Datalog.Path(Datalog.Var("exp"), Datalog.TNode(Exp.multTag), Datalog.NamedLink(Datalog.TNode(Exp.multTag), "lhs"), Datalog.Var("res"), Datalog.TNode(Exp.expTag)),
        )),
      ))

  val intVal =
    Datalog.Pattern(None, "intVal", Seq(Datalog.Param("exp", Datalog.TNode(Exp.intTag)), Datalog.Param("v", Datalog.TLiteral.Int)),
      Seq(
        Datalog.Body(Seq(
          Datalog.Path(Datalog.Var("exp"), Datalog.TNode(Exp.intTag), Datalog.NamedLink(Datalog.TNode(Exp.intTag), "value"), Datalog.Var("v"), Datalog.TLiteral.Int)
        )),
      ))
}
