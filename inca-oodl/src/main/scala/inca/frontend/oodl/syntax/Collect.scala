package inca.frontend.oodl.syntax

trait Collect[R]:

  def apply(module: Module): Seq[R] = module.content.flatMap {
    case fun: FunctionDef => collectFunctionDef(fun)
    case classDef: ClassDef => collectClassDef(classDef)
  }

  def collectClassDef(classDef: ClassDef): Seq[R] =
    classDef.annos.flatMap(collectAnnotation) ++ classDef.content.flatMap(collectClassContent)

  def collectFunctionDef(fun: FunctionDef): Seq[R] =
    fun.annos.flatMap(collectAnnotation) ++ fun.params.flatMap(collectParam) ++ fun.body.flatMap(collectStatement)

  def collectClassContent(content: ClassContent): Seq[R] = content match
    case f: FieldDef => collectFieldDef(f)
    case m: MethodDef => collectMethodDef(m)
    case c: ConstructorDef => collectConstructorDef(c)

  def collectFieldDef(field: FieldDef): Seq[R] =
    field.annos.flatMap(collectAnnotation) ++ field.body.flatMap(collectExpression) ++ collectType(field.typ)

  def collectMethodDef(method: MethodDef): Seq[R] =
    method.annos.flatMap(collectAnnotation)
    ++ method.body.flatMap(collectStatement)
    ++ method.params.flatMap(collectParam) ++ collectType(method.outType)

  def collectConstructorDef(constructor: ConstructorDef): Seq[R] =
    constructor.annos.flatMap(collectAnnotation)
    ++ constructor.body.flatMap(collectStatement)
    ++ constructor.params.flatMap(collectParam)

  def collectAnnotation(anno: Annotation): Seq[R] = Seq()

  def collectParam(par: Param): Seq[R] = collectType(par.typ)

  def collectStatement(stmt: Statement): Seq[R] = stmt match
    case Expr(expression) => collectExpression(expression)
    case Return(expression) => collectExpression(expression)
    case Assign(lhs, op, rhs) => collectExpression(lhs) ++ collectExpression(rhs)
    case VarDeclare(name, typ, maybeExpression, immutable) => maybeExpression.flatMap(collectExpression).toSeq
    case Super(args) => args.flatMap(collectExpression)
    case If(cnd, thn, els) => collectExpression(cnd) ++ thn.flatMap(collectStatement) ++ els.flatMap(collectStatement)

  def collectExpression(expr: Expression): Seq[R] = expr match
    case Var(name) => Seq()
    case Select(recv, targetName) => collectExpression(recv)
    case ConstructorCall(name, tyArgs, args) => args.flatMap(collectExpression)
    case MethodCall(recv, fun, tyArgs, args, isFix) => collectExpression(recv) ++ args.flatMap(collectExpression)
    case TypeCast(recv, toTyp) => collectExpression(recv) ++ collectType(toTyp)
    case InstanceOf(recv, ofTyp) => collectExpression(recv) ++ collectType(ofTyp)
    case TupleExp(exps) => exps.flatMap(collectExpression)
    case SetExp(exps, tty) => exps.flatMap(collectExpression)
    case SetMember(name, recv, predicate) => collectExpression(recv) ++ predicate.flatMap(collectExpression)
    case SetComprehension(member, body) => member.flatMap(collectExpression) ++ collectExpression(body)
    case NullLit() => Seq()
    case BoolLit(b) => Seq()
    case IntLit(i) => Seq()
    case DoubleLit(d) => Seq()
    case StringLit(s) => Seq()
    case BinOp(e1, op, e2) => collectExpression(e1) ++ collectExpression(e2)
    case UnOp(op, e) => collectExpression(e)

  def collectType(ty: Type): Seq[R] = ty match
    case TAny => Seq()
    case TNull => Seq()
    case TTuple(ts) => ts.flatMap(collectType)
    case TName(name, tyArgs) => Seq()
    case TSet(ty) => collectType(ty)

