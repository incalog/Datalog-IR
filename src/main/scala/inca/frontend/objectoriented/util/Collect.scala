package inca.frontend.objectoriented.util

import inca.frontend.objectoriented.core._

trait Collect[R] {
  def apply(module: Module): Seq[R] = module.classes.flatMap(collectClassDef)

  def collectClassDef(clazz: ClassDef): Seq[R] =
    clazz.annos.flatMap(collectAnnotation) ++
      clazz.fields.flatMap(collectField) ++
      clazz.methods.flatMap(collectMethod) ++
      clazz.constructors.flatMap(collectConstructor)

  def collectField(field: FieldDef): Seq[R] =
    field.annos.flatMap(collectAnnotation) ++
      collectType(field.typ) ++
      (if (field.body.isDefined) collectExpression(field.body.get) else Seq())

  def collectMethod(method: MethodDef): Seq[R] =
    method.annos.flatMap(collectAnnotation) ++
      method.params.flatMap(collectParam) ++
      method.body.flatMap(collectStatement) ++
      collectType(method.outType)

  def collectConstructor(constructor: ConstructorDef): Seq[R] =
    constructor.annos.flatMap(collectAnnotation) ++
      constructor.params.flatMap(collectParam) ++
      constructor.body.flatMap(collectStatement)

  def collectParam(param: Param): Seq[R] =
    collectType(param.typ)

  def collectType(t: Type): Seq[R] = t match {
    case TAny => Seq()
    case TNull => Seq()
    case TTuple(ts) => ts.flatMap(collectType)
    case TScala(ty) => Seq()
    case TClass(ref) => Seq()
    case TSet(ty) => collectType(ty)
  }

  def collectAnnotation(anno: Annotation): Seq[R] = Seq()

  def collectStatement(stmt: Statement): Seq[R] = stmt match {
    case ExprStmt(expression) => collectExpression(expression)
    case ReturnStmt(expression) => collectExpression(expression)
    case FieldAssignStmt(recv, _, expression) => collectExpression(recv) ++ collectExpression(expression)
    case VarDeclareStmt(_, typ, Some(expr), _) => collectType(typ) ++ collectExpression(expr)
    case VarDeclareStmt(_, typ, None, _) => collectType(typ)
    case VarAssignStmt(_, expression) => collectExpression(expression)
    case VarPhiAssignStmt(_, typ, _, _, _) => collectType(typ)
    case IfStmt(cnd, thn, els) =>
      collectExpression(cnd) ++ thn.flatMap(collectStatement) ++ els.flatMap(collectStatement)
  }

  def collectExpression(expr: Expression): Seq[R] = expr match {
    case VarReadExpr(_) => Seq()
    case FieldReadExpr(recv, _) => collectExpression(recv)
    case ConstructorExpr(_, args) => args.flatMap(collectExpression)
    case SuperExpr(args) => args.flatMap(collectExpression)
    case MethodCallExpr(recv, _, args, _) => collectExpression(recv) ++ args.flatMap(collectExpression)
    case TypeCastExpr(recv, toTyp) => collectExpression(recv) ++ collectType(toTyp)
    case InstanceOfExpr(recv, ofTyp) => collectExpression(recv) ++ collectType(ofTyp)
    case NullExpr() => Seq()
    case TupleReadExpr(recv, _) => collectExpression(recv)
    case TupleExpr(exps) => exps.flatMap(collectExpression)
    case SetExpr(exps, Some(tty)) => exps.flatMap(collectExpression) ++ collectType(tty)
    case SetExpr(exps, None) => exps.flatMap(collectExpression)
    case SetMemberExpr(_, recv, Some(predicate)) => collectExpression(recv) ++ collectExpression(predicate)
    case SetMemberExpr(_, recv, None) => collectExpression(recv)
    case SetComprehension(member, body) => member.flatMap(collectExpression) ++ collectExpression(body)
    case SetFold(recv, projection, _, _, neutral) =>
      collectExpression(recv) ++ projection.flatMap(collectExpression) ++ collectExpression(neutral)
    case BaseLitExpr(_) => Seq()
    case BaseApplyExpr(_, args) => args.flatMap(collectExpression)
    case BaseApplyInfixExpr(left, _, right) => collectExpression(left) ++ collectExpression(right)
    case BaseApplyMethodExpr(recv, _, Some(args)) => collectExpression(recv) ++ args.flatMap(collectExpression)
    case BaseApplyMethodExpr(recv, _, None) => collectExpression(recv)
    case BaseApplyUnaryExpr(_, exp) => collectExpression(exp)
    case SetFromEdb(_, tty) => collectType(tty)
  }
}
