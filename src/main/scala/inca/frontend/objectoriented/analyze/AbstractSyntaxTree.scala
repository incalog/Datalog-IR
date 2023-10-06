package inca.frontend.objectoriented.analyze

import inca.backend.analyze.Graph
import inca.frontend.objectoriented.analyze.AbstractSyntaxTree._
import inca.frontend.objectoriented.core._

import java.util.UUID
import scala.util.hashing.MurmurHash3

object AbstractSyntaxTree {
  sealed trait DependencyEdge {
    val color: String = "black"
    val label: String = ""
  }
  case object ClassEdge extends DependencyEdge
  case object ConstructorEdge extends DependencyEdge
  case object MethodEdge extends DependencyEdge
  case object FieldEdge extends DependencyEdge
  case class ParamEdge(text: String) extends DependencyEdge {
    override val label: String = text
  }
  case class StatementEdge(text: Option[String]) extends DependencyEdge {
    override val label: String = text.getOrElse("")
  }
  case class ExpressionEdge(text: Option[String]) extends DependencyEdge {
    override val label: String = text.getOrElse("")
  }


  sealed trait NodeType {
    val shape: String = "plaintext"
    val fillColor: String = "white"
    val fontColor: String = "black"
  }
  case object ModuleNode extends NodeType
  case object ClassNode extends NodeType {
    override val shape: String = "box"
  }
  case object FieldNode extends NodeType {
    override val shape: String = "diamond"
  }
  case object ParamNode extends NodeType {
    override val shape: String = "polygon"
  }
  case object ConstructorNode extends NodeType {
    override val shape: String = "doubleoctagon"
  }
  case object MethodNode extends NodeType {
    override val shape: String = "octagon"
  }
  case object StatementNode extends NodeType {
    override val shape: String = "Mcircle"
    override val fillColor: String = "gray"
  }
  case object ExpressionNode extends NodeType {
    override val shape: String = "circle"
    override val fontColor: String = "white"
    override val fillColor: String = "black"
  }
}

case class AstNode(name: String, uuid: UUID, typ: NodeType) {
  // make sure each node is unique no matter the name or source location
  // the source location might be (-1, -1) for generated statements
  override def hashCode(): Int = MurmurHash3.stringHash(uuid.toString)
}

class AbstractSyntaxTree(module: Module) extends Graph[AstNode, DependencyEdge] {

  def addAstNode(name: String, typ: NodeType): AstNode = {
    val node = AstNode(name, UUID.randomUUID(), typ)
    this.addNode(node)
    node
  }

  val mNode: AstNode = this.addAstNode(module.name.raw, ModuleNode)
  module.classes.foreach(analyzeClass(mNode, _))

  def analyzeClass(parent: AstNode, classDef: ClassDef): Unit = {
    val clsNode = this.addAstNode(classDef.name.raw, ClassNode)
    this.addEdge(parent, clsNode, ClassEdge)

    classDef.content.foreach {
      case c: ConstructorDef => analyzeConstructor(clsNode, c)
      case f: FieldDef => analyzeField(clsNode, f)
      case m: MethodDef => analyzeMethod(clsNode, m)
    }
  }

  def analyzeParam(parent: AstNode, param: Param, edgeLabel: String): Unit = {
    val paramNode = this.addAstNode(param.name.raw, ParamNode)
    this.addEdge(parent, paramNode, ParamEdge(edgeLabel))
  }

  def analyzeParams(parent: AstNode, params: Seq[Param]): Unit = {
    params.zipWithIndex.foreach(p => analyzeParam(parent, p._1, s"param[${p._2}]"))
  }

  def analyzeConstructor(parent: AstNode, constructorDef: ConstructorDef): Unit = {
    val constrNode = this.addAstNode("this", ConstructorNode)
    this.addEdge(parent, constrNode, ConstructorEdge)
    analyzeStatements(constrNode, constructorDef.body)
    analyzeParams(constrNode, constructorDef.params)
  }

  def analyzeMethod(parent: AstNode, methodDef: MethodDef): Unit = {
    val methodNode = this.addAstNode(methodDef.name.raw, MethodNode)
    this.addEdge(parent, methodNode, MethodEdge)
    analyzeStatements(methodNode, methodDef.body)
    analyzeParams(methodNode, methodDef.params)
  }

  def analyzeField(parent: AstNode, fieldDef: FieldDef): Unit = {
    val fieldNode = this.addAstNode(fieldDef.name.raw, FieldNode)
    this.addEdge(parent, fieldNode, FieldEdge)
    if (fieldDef.body.isDefined)
      analyzeExpression(fieldNode, fieldDef.body.get)
  }

  def analyzeStatements(parent: AstNode, stmts: Seq[Statement], edgeLabel: Option[String] = None): Unit = {
    val label = edgeLabel.getOrElse("body")
    stmts.zipWithIndex.foreach(s => analyzeStatement(parent, s._1, Some(s"$label[${s._2}]")))
  }

  def analyzeStatement(parent: AstNode, stmt: Statement, edgeLabel: Option[String] = None): Unit = {
    val stmtNode = this.addAstNode(stmt.getClass.getSimpleName, StatementNode)
    this.addEdge(parent, stmtNode, StatementEdge(edgeLabel))
    stmt match {
      case ExprStmt(expression) =>
        analyzeExpression(stmtNode, expression)
      case ReturnStmt(expression) =>
        analyzeExpression(stmtNode, expression)
      case FieldAssignStmt(recv, name, expression) =>
        analyzeExpression(stmtNode, recv)
        analyzeExpression(stmtNode, expression, Some(name.raw))
      case VarDeclareStmt(name, _, maybeExpression, _) =>
        if (maybeExpression.isDefined)
          analyzeExpression(stmtNode, maybeExpression.get, Some(name.raw))
      case VarAssignStmt(targetName, expression) =>
        analyzeExpression(stmtNode, expression, Some(targetName.raw))
      case VarPhiAssignStmt(_, _, _, _, _) =>
        // nothing
      case IfStmt(cnd, thn, els) =>
        analyzeExpression(stmtNode, cnd, Some("cnd"))
        analyzeStatements(stmtNode, thn, Some("thn"))
        analyzeStatements(stmtNode, els, Some("els"))
    }
  }

  def analyzeExpressions(parent: AstNode, exprs: Seq[Expression]): Unit = {
    exprs.zipWithIndex.foreach(e => analyzeExpression(parent, e._1, Some(s"arg[${e._2}]")))
  }

  def analyzeExpression(parent: AstNode, expr: Expression, edgeLabel: Option[String] = None): Unit = {
    val exprNode = this.addAstNode(expr.getClass.getSimpleName, ExpressionNode)
    this.addEdge(parent, exprNode, ExpressionEdge(edgeLabel))
    expr match {
      case FieldReadExpr(recv, targetName) =>
        analyzeExpression(exprNode, recv, Some("recv"))
      case VarReadExpr(targetName) =>
        // nothing
      case ConstructorExpr(classRef, tyArgs, args) =>
        analyzeExpressions(exprNode, args)
      case SuperExpr(args) =>
        analyzeExpressions(exprNode, args)
      case MethodCallExpr(recv, fun, tyArgs, args, fix) =>
        analyzeExpression(exprNode, recv, Some("recv"))
        analyzeExpressions(exprNode, args)
      case TypeCastExpr(recv, toTyp) =>
        analyzeExpression(exprNode, recv, Some("recv"))
      case InstanceOfExpr(recv, ofTyp) =>
        analyzeExpression(exprNode, recv, Some("recv"))
      case NullExpr() => // nothing
      case TupleReadExpr(recv, index) =>
        analyzeExpression(exprNode, recv, Some("recv"))
      case TupleExpr(exps) =>
        analyzeExpressions(exprNode, exps)
      case SetExpr(exps, tty) =>
        analyzeExpressions(exprNode, exps)
      case SetMemberExpr(_, recv, predicate) =>
        analyzeExpression(exprNode, recv, Some("recv"))
        if (predicate.isDefined)
          analyzeExpression(exprNode, predicate.get, Some("pred"))
      case SetComprehension(member, body) =>
        analyzeExpressions(exprNode, member)
        analyzeExpression(exprNode, body, Some("yield"))
      case SetFold(recv, projection, _, _, neutral) =>
        analyzeExpressions(exprNode, projection)
        analyzeExpression(exprNode, recv, Some("recv"))
        analyzeExpression(exprNode, neutral, Some("neutral"))
      case BaseLitExpr(_) =>
        // nothing
      case BaseApplyExpr(_, args) =>
        analyzeExpressions(exprNode, args)
      case BaseApplyInfixExpr(left, _, right) =>
        analyzeExpression(exprNode, left, Some("left"))
        analyzeExpression(exprNode, right, Some("right"))
      case BaseApplyMethodExpr(recv, _, args) =>
        analyzeExpression(exprNode, recv)
        if (args.isDefined)
          analyzeExpressions(exprNode, args.get)
      case BaseApplyUnaryExpr(_, exp) =>
        analyzeExpression(exprNode, exp)
    }
  }

  override protected def nodeToGraphViz(n: AstNode): String = n.hashCode().toString

  override protected def nodeGraphVizAttributes(n: AstNode): String = Map(
      "shape" -> n.typ.shape,
      "label" -> n.name,
      "fontcolor" -> n.typ.fontColor,
      "fillcolor" -> n.typ.fillColor,
      "style" -> "filled"
    ).map { case (k, v) =>
      s"$k=$v"
    }.mkString(", ")

  override protected def edgeGraphVizAttributes(from: AstNode, to: AstNode, kind: DependencyEdge): String = Map(
      "color" -> kind.color,
      "label" -> s""""${kind.label}""""
    ).map { case (k, v) =>
      s"$k=$v"
    }.mkString(", ")
}
