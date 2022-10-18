package inca.frontend.objectoriented.lowering

import inca.compiler.SourceLocation
import inca.frontend.objectoriented.core._

// TODO: Trans visibility and annos as well

// Traverse the AST and recreate each node to clear all resolved targets and types.
trait ModuleLowering {

  private[lowering] def preserveLoc[U <: SourceLocation](loc: U)(f: U => U): U = {
    val transLoc = f(loc)
    transLoc.startIndex = loc.startIndex
    transLoc.endIndex = loc.endIndex
    transLoc
  }

  def module: Module

  // These methods wrap the internal methods to preserve the source location start and end index information.

  protected[frontend] def transModule(): Module = preserveLoc(module)(transModuleInternal)
  protected[frontend] def transClass(classDef: ClassDef): ClassDef = preserveLoc(classDef)(transClassInternal)
  protected[frontend] def transContent(content: ClassContent, classDef: ClassDef): ClassContent = content match {
    case constructor: ConstructorDef => preserveLoc(constructor)(c => transConstructorInternal(c, classDef))
    case method: MethodDef => preserveLoc(method)(m => transMethodInternal(m, classDef))
    case field: FieldDef => preserveLoc(field)(f => transFieldInternal(f, classDef))
  }
  protected[frontend] def transStatements(stmts: Seq[Statement]): Seq[Statement] = stmts.map(transStatement)
  protected[frontend] def transStatement(stmt: Statement): Statement = preserveLoc(stmt)(transStatementInternal)
  protected[frontend] def transExpressions(exprs: Seq[Expression]): Seq[Expression] = exprs.map(transExpression)
  protected[frontend] def transExpression(expression: Expression): Expression = preserveLoc(expression)(transExpressionInternal)
  protected[frontend] def transParams(params: Seq[Param]): Seq[Param] = params.map(transParam)
  protected[frontend] def transParam(param: Param): Param = preserveLoc(param)(transParamInternal)
  protected[frontend] def transType(typ: Type): Type = preserveLoc(typ)(transTypeInternal)


  // Override these methods in a subclass to customize the behaviour.

  private[lowering] def transModuleInternal(module: Module): Module = {
    val Module(name, imports, classes) = module
    val transClasses = classes.map(transClass)
    Module(name, imports, transClasses)
  }

  private[lowering] def transClassInternal(classDef: ClassDef): ClassDef = {
    val ClassDef(annos, vis, name, parents, content) = classDef
    val newContent = content.map(c => transContent(c, classDef))
    ClassDef(annos, vis, name, parents.map(c => ClassRef(c.name)), newContent)
  }

  private[lowering] def transParamInternal(param: Param): Param =
    Param(param.name, param.typ)

  private[lowering] def transFieldInternal(fieldDef: FieldDef, classDef: ClassDef): FieldDef = {
    val FieldDef(annos, vis, name, typ, body, immutable) = fieldDef
    val newBody = if (body.isDefined) Some(transExpression(body.get)) else None
    FieldDef(annos, vis, name, typ, newBody, immutable)
  }

  private[lowering] def transMethodInternal(methodDef: MethodDef, classDef: ClassDef): MethodDef = {
    val MethodDef(annos, vis, name, params, outType, body) = methodDef
    val newBody = transStatements(body)
    MethodDef(annos, vis, name, transParams(params), transType(outType), newBody)
  }

  private[lowering] def transConstructorInternal(constructorDef: ConstructorDef, classDef: ClassDef): ConstructorDef = {
    val ConstructorDef(annos, vis, params, body) = constructorDef
    val newBody = transStatements(body)
    ConstructorDef(annos, vis, transParams(params), newBody)
  }

  private[lowering] def transStatementInternal(stmt: Statement): Statement = stmt match {
    case VarDeclareStmt(name, typ, maybeExpression, immutable) =>
      val exprOption = if (maybeExpression.isDefined) Some(transExpression(maybeExpression.get)) else None
      VarDeclareStmt(name, transType(typ), exprOption, immutable)
    case VarAssignStmt(targetName, expression) =>
      VarAssignStmt(targetName, expression)
    case ReturnStmt(expr) =>
      ReturnStmt(transExpression(expr))
    case ExprStmt(expr) =>
      ExprStmt(transExpression(expr))
    case FieldAssignStmt(recv, name, expression) =>
      FieldAssignStmt(transExpression(recv), name, transExpression(expression))
    case IfStmt(cnd, thn, els) =>
      IfStmt(transExpression(cnd), transStatements(thn), transStatements(els))
    case s =>
      throw new RuntimeException(s"Can not transform statement: $s")
  }

  private[lowering] def transExpressionInternal(expression: Expression): Expression = expression match {
    case FieldReadExpr(recv, targetName) =>
      FieldReadExpr(transExpression(recv), targetName)
    case VarReadExpr(targetName) =>
      // Rewrite all VarReadExpr to use the latest generated name for the variable
      VarReadExpr(Name(targetName.raw))
    case ConstructorExpr(ClassRef(name), args) =>
      ConstructorExpr(ClassRef(name), transExpressions(args))
    case SuperExpr(args) =>
      SuperExpr(transExpressions(args))
    case MethodCallExpr(recv, fun, args) =>
      MethodCallExpr(transExpression(recv), fun, transExpressions(args))
    case TypeCastExpr(recv, toTyp) =>
      TypeCastExpr(transExpression(recv), transType(toTyp))
    case InstanceOfExpr(recv, ofTyp) =>
      InstanceOfExpr(transExpression(recv), transType(ofTyp))
    case TupleExpr(exps) =>
      TupleExpr(transExpressions(exps))
    case TupleReadExpr(recv, index) =>
      TupleReadExpr(transExpression(recv), index)
    case SetExpr(exps) =>
      SetExpr(transExpressions(exps))
    case SetMemberExpr(name, recv, predicate) =>
      val pred = if (predicate.isDefined) Some(transExpression(predicate.get)) else None
      SetMemberExpr(name, transExpression(recv), pred)
    //case SetReduce(recv, op) =>
    //  SetReduce(transExpression(recv), op)
    case SetComprehension(exps, body) =>
      SetComprehension(transExpressions(exps), transExpression(body))
    case BaseApplyExpr(fun, args) =>
      BaseApplyExpr(fun, transExpressions(args))
    case BaseApplyInfixExpr(left, op, right) =>
      BaseApplyInfixExpr(transExpression(left), op, transExpression(right))
    case BaseApplyMethodExpr(recv, method, args) =>
      BaseApplyMethodExpr(transExpression(recv), method, Some(transExpressions(args.getOrElse(Seq()))))
    case BaseApplyUnaryExpr(op, exp) =>
      BaseApplyUnaryExpr(op, transExpression(exp))
    case NullExpr() =>
      NullExpr()
    case BaseLitExpr(code) =>
      BaseLitExpr(code)
    case expr =>
      throw new RuntimeException(s"Can not transform expression: $expr")
  }

  private[lowering] def transTypeInternal(typ: Type): Type = {
    typ match {
      case TAny => TAny
      case TNull => TNull
      case TTuple(ts) => TTuple(ts.map(transType))
      case TSet(ty) => TSet(transType(ty))
      case TScala(ty) => TScala(ty)
      // create a new ClassRef to invalidate the current target
      case TClass(ClassRef(name)) => TClass(ClassRef(name))
    }
  }
}
