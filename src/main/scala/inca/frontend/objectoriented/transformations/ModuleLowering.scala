package inca.frontend.objectoriented.transformations

import inca.compiler.SourceLocation
import inca.frontend.objectoriented.core._

// TODO: Trans visibility and annos as well

/**
 * Traverse the AST and recreate each node to clear all resolved targets and types.
 * Subclasses can implement this trait and hook into arbitrary internal functions to modify part of the transformations.
 */
trait ModuleLowering {

  private[transformations] def preserveLoc[U <: SourceLocation](loc: U)(f: U => U): U = {
    val transLoc = f(loc)
    transLoc.startIndex = loc.startIndex
    transLoc.endIndex = loc.endIndex
    transLoc
  }

  private[transformations] def preserveLocs[U <: SourceLocation](loc: U)(f: U => Seq[U]): Seq[U] = {
    f(loc).map { transLoc =>
      transLoc.startIndex = loc.startIndex
      transLoc.endIndex = loc.endIndex
      transLoc
    }
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
  protected[frontend] def transStatements(stmts: Seq[Statement]): Seq[Statement] = stmts.flatMap(transStatement)
  protected[frontend] def transStatement(stmt: Statement): Seq[Statement] = preserveLocs(stmt)(transStatementInternal)
  protected[frontend] def transExpressions(exprs: Seq[Expression]): Seq[Expression] = exprs.flatMap(transExpression)
  protected[frontend] def transExpression(expression: Expression): Seq[Expression] = preserveLocs(expression)(transExpressionInternal)
  protected[frontend] def transParams(params: Seq[Param]): Seq[Param] = params.map(transParam)
  protected[frontend] def transParam(param: Param): Param = preserveLoc(param)(transParamInternal)
  protected[frontend] def transType(typ: Type): Type = preserveLoc(typ)(transTypeInternal)


  // Override these methods in a subclass to customize the behaviour.

  private[transformations] def transModuleInternal(module: Module): Module = {
    val Module(name, imports, classes) = module
    val transClasses = classes.map(transClass)
    Module(name, imports, transClasses)
  }

  private[transformations] def transClassInternal(classDef: ClassDef): ClassDef = {
    val ClassDef(annos, vis, name, parents, content) = classDef
    val newContent = content.map(c => transContent(c, classDef))
    ClassDef(annos, vis, name, parents.map(c => ClassRef(c.name)), newContent)
  }

  private[transformations] def transParamInternal(param: Param): Param =
    Param(param.name, transType(param.typ))

  private[transformations] def transFieldInternal(fieldDef: FieldDef, classDef: ClassDef): FieldDef = {
    val FieldDef(annos, vis, name, typ, body, immutable) = fieldDef
    val newBody = if (body.isDefined) Some(transExpression(body.get).head) else None
    FieldDef(annos, vis, name, transType(typ), newBody, immutable)
  }

  private[transformations] def transMethodInternal(methodDef: MethodDef, classDef: ClassDef): MethodDef = {
    val MethodDef(annos, vis, name, params, outType, body) = methodDef
    val newParams = transParams(params)
    val newBody = transStatements(body)
    MethodDef(annos, vis, name, newParams, transType(outType), newBody)
  }

  private[transformations] def transConstructorInternal(constructorDef: ConstructorDef, classDef: ClassDef): ConstructorDef = {
    val ConstructorDef(annos, vis, params, body) = constructorDef
    val newParams = transParams(params)
    val newBody = transStatements(body)
    ConstructorDef(annos, vis, newParams, newBody)
  }

  private[transformations] def transStatementInternal(stmt: Statement): Seq[Statement] = Seq(stmt match {
    case VarDeclareStmt(name, typ, maybeExpression, immutable) =>
      val exprOption = if (maybeExpression.isDefined) Some(transExpression(maybeExpression.get).head) else None
      VarDeclareStmt(name, transType(typ), exprOption, immutable)
    case VarAssignStmt(targetName, expression) =>
      VarAssignStmt(targetName, transExpression(expression).head)
    case ReturnStmt(expr) =>
      ReturnStmt(transExpression(expr).head)
    case ExprStmt(expr) =>
      ExprStmt(transExpression(expr).head)
    case FieldAssignStmt(recv, name, expression) =>
      FieldAssignStmt(transExpression(recv).head, name, transExpression(expression).head)
    case IfStmt(cnd, thn, els) =>
      IfStmt(transExpression(cnd).head, transStatements(thn), transStatements(els))
    case VarPhiAssignStmt(name, typ, ifStmt, thnName, elsName) =>
      // TODO: Actually we need to keep track of the current if stmt and pass a reference to it here
      VarPhiAssignStmt(name, transType(typ), transStatement(ifStmt).head.asInstanceOf[IfStmt], thnName, elsName)
    case s =>
      throw new RuntimeException(s"Can not transform statement: $s")
  })

  private[transformations] def transExpressionInternal(expression: Expression): Seq[Expression] = Seq(expression match {
    case FieldReadExpr(recv, targetName) =>
      FieldReadExpr(transExpression(recv).head, targetName)
    case VarReadExpr(targetName) =>
      VarReadExpr(targetName)
    case constr@ConstructorExpr(ClassRef(name), args) =>
      val newConstr = ConstructorExpr(ClassRef(name), transExpressions(args))
      newConstr.tyParams = constr.tyParams.map(transType)
      newConstr
    case SuperExpr(args) =>
      SuperExpr(transExpressions(args))
    case MethodCallExpr(recv, fun, args, isFix) =>
      MethodCallExpr(transExpression(recv).head, fun, transExpressions(args), isFix)
    case TypeCastExpr(recv, toTyp) =>
      TypeCastExpr(transExpression(recv).head, transType(toTyp))
    case InstanceOfExpr(recv, ofTyp) =>
      InstanceOfExpr(transExpression(recv).head, transType(ofTyp))
    case TupleExpr(exps) =>
      TupleExpr(transExpressions(exps))
    case TupleReadExpr(recv, index) =>
      TupleReadExpr(transExpression(recv).head, index)
    case SetExpr(exps, tty) =>
      SetExpr(transExpressions(exps), if (tty.isDefined) Some(transType(tty.get)) else None)
    case SetMemberExpr(name, recv, predicate) =>
      val pred = if (predicate.isDefined) Some(transExpression(predicate.get).head) else None
      SetMemberExpr(name, transExpression(recv).head, pred)
    case SetFold(recv, projection, ClassRef(name), method, neutral) =>
      SetFold(transExpression(recv).head, transExpressions(projection), ClassRef(name), method, transExpression(neutral).head)
    case SetComprehension(exps, body) =>
      SetComprehension(transExpressions(exps), transExpression(body).head)
    case BaseApplyExpr(fun, args) =>
      BaseApplyExpr(fun, transExpressions(args))
    case BaseApplyInfixExpr(left, op, right) =>
      BaseApplyInfixExpr(transExpression(left).head, op, transExpression(right).head)
    case BaseApplyMethodExpr(recv, method, args) =>
      val argsOptions =
        if (args.isEmpty)
          None
        else
          Some(transExpressions(args.get))
      BaseApplyMethodExpr(transExpression(recv).head, method, argsOptions)
    case BaseApplyUnaryExpr(op, exp) =>
      BaseApplyUnaryExpr(op, transExpression(exp).head)
    case NullExpr() =>
      NullExpr()
    case BaseLitExpr(code) =>
      BaseLitExpr(code)
    case SetFromEdb(edbName, tty) =>
      SetFromEdb(edbName, transType(tty))
    case expr =>
      throw new RuntimeException(s"Can not transform expression: $expr")
  })

  private[transformations] def transTypeInternal(typ: Type): Type = {
    typ match {
      case TAny => TAny
      case TNull => TNull
      case TTuple(ts) => TTuple(ts.map(transType))
      case TSet(ty) => TSet(transType(ty))
      case TScala(ty) => TScala(ty)
      // create a new ClassRef to invalidate the current target
      case tcls@TClass(ClassRef(name)) =>
        val ty = TClass(ClassRef(name))
        ty.tyParams = tcls.tyParams.map(transType)
        ty
    }
  }
}
