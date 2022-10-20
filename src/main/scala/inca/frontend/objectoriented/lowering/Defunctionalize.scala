package inca.frontend.objectoriented.lowering

import inca.frontend.objectoriented.core._
import inca.util.Gensym

object Defunctionalize {
  def transformModule(module: Module): Module =
    new StaticSingleAssignment(module).transModule()

  def transformModules(modules: Seq[Module]): Seq[Module] =
    modules.map(transformModule)
}

// TODO: val a: Set[Any] = [1, 2, 3] will fail to translate correctly

class Defunctionalize(val module: Module) extends ModuleLowering {
  private val gensym: Gensym = new Gensym(Iterable.empty)

  var callClassDefs: Set[ClassDef] = Set()
  var defnClassDefs: Map[Type, ClassDef] = Map()

  private def genDefunClassDef(ty: Type): ClassDef = {
    val typ = clearType(ty)
    if (defnClassDefs.contains(typ))
      return defnClassDefs(typ)
    val apply = MethodDef(Seq(), Some(Private), Name("apply"), Seq(), TSet(typ), Seq())
    val constr = ConstructorDef(Seq(), None, Seq(), Seq())
    val clazz = ClassDef(Seq(), Some(Private), Name(gensym.fresh("Defun")), Seq(), Seq(constr, apply), Some(typ))
    defnClassDefs += typ -> clazz
    clazz
  }

  private def transVarReadToFieldRead(expression: Expression, vars: Set[Name]): Expression = expression match {
    case VarReadExpr(targetName) if !vars.contains(targetName) =>
      throw new IllegalArgumentException(s"Found unresolved var $targetName!")
    case VarReadExpr(targetName) if vars.contains(targetName) =>
      FieldReadExpr(VarReadExpr(Name("this")), targetName)
    case FieldReadExpr(recv, targetName) =>
      FieldReadExpr(transVarReadToFieldRead(recv, vars), targetName)
    case ConstructorExpr(ClassRef(name), args) =>
      ConstructorExpr(ClassRef(name), args.map(transVarReadToFieldRead(_, vars)))
    case SuperExpr(args) =>
      SuperExpr(args.map(transVarReadToFieldRead(_, vars)))
    case MethodCallExpr(recv, fun, args) =>
      MethodCallExpr(transVarReadToFieldRead(recv, vars), fun, args.map(transVarReadToFieldRead(_, vars)))
    case TypeCastExpr(recv, toTyp) =>
      TypeCastExpr(transVarReadToFieldRead(recv, vars), clearType(toTyp))
    case InstanceOfExpr(recv, ofTyp) =>
      InstanceOfExpr(transVarReadToFieldRead(recv, vars), clearType(ofTyp))
    case TupleReadExpr(recv, index) =>
      TupleReadExpr(transVarReadToFieldRead(recv, vars), index)
    case TupleExpr(exps) =>
      TupleExpr(exps.map(transVarReadToFieldRead(_, vars)))
    case SetExpr(exps) =>
      SetExpr(exps.map(transVarReadToFieldRead(_, vars)))
    case SetMemberExpr(name, recv, predicate) =>
      val pred = if (predicate.isDefined) Some(transVarReadToFieldRead(predicate.get, vars)) else None
      SetMemberExpr(name, transVarReadToFieldRead(recv, vars), pred)
    case SetComprehension(member, body) =>
      SetComprehension(member.map(transVarReadToFieldRead(_, vars)), transVarReadToFieldRead(body, vars))
    case BaseApplyExpr(fun, args) =>
      BaseApplyExpr(fun, args.map(transVarReadToFieldRead(_, vars)))
    case BaseApplyInfixExpr(left, op, right) =>
      BaseApplyInfixExpr(transVarReadToFieldRead(left, vars), op, transVarReadToFieldRead(right, vars))
    case BaseApplyMethodExpr(recv, method, args) =>
      val newArgs = if (args.isDefined) Some(args.get.map(transVarReadToFieldRead(_, vars))) else None
      BaseApplyMethodExpr(transVarReadToFieldRead(recv, vars), method, newArgs)
    case BaseApplyUnaryExpr(op, exp) =>
      BaseApplyUnaryExpr(op, transVarReadToFieldRead(exp, vars))
    case _ => clearExpression(expression)
  }

  private def genAuxDef(fieldParams: Map[Name, Type], innerSetType: Type, parent: ClassRef, expr: Expression): ClassDef = {
    // transform local variables to fields
    val fields = fieldParams.map {
      case (name, typ) => FieldDef(Seq(), None, name, clearType(typ), None, immutable = false)
    }.toSeq

    // all reads to local variables should now access the correct field instead
    val ret = ReturnStmt(transVarReadToFieldRead(expr, fieldParams.keySet))
    val apply = MethodDef(Seq(), Some(Private), Name("apply"), Seq(), TSet(clearType(innerSetType)), Seq(ret))
    // create a default constructor
    val constrParams = fields.map(f => Param(f.name, f.typ))
    val constrBody = fields.map { f =>
      FieldAssignStmt(
        VarReadExpr(Name("this")), f.name, VarReadExpr(f.name)
      )
    }
    val constr = ConstructorDef(Seq(), None, constrParams, constrBody)
    val clsName = Name(gensym.fresh("Aux"))
    val clazz = ClassDef(Seq(), Some(Private), clsName, Seq(parent), fields :+ constr :+ apply)
    callClassDefs += clazz
    clazz
  }

  var usedVars: Map[Name, Type] = Map()

  override private[lowering] def transModuleInternal(module: Module): Module = {
    val Module(name, imports, classes) = module
    // make sure the defun class name is unique
    gensym.register(module.usedModuleNames.map(_.raw))
    gensym.register(classes.map(_.name.raw))
    val transClasses = classes.map(transClass)
    Module(name, imports, transClasses ++ callClassDefs ++ defnClassDefs.values)
  }

  override private[lowering] def transMethodInternal(methodDef: MethodDef, classDef: ClassDef): MethodDef = {
    usedVars = Map()
    super.transMethodInternal(methodDef, classDef)
  }

  override private[lowering] def transConstructorInternal(constructorDef: ConstructorDef, classDef: ClassDef): ConstructorDef = {
    usedVars = Map()
    super.transConstructorInternal(constructorDef, classDef)
  }

  override private[lowering] def transFieldInternal(fieldDef: FieldDef, classDef: ClassDef): FieldDef = fieldDef match {
    // Transform: set fields to object set fields
    case FieldDef(annos, vis, name, TSet(ty), body, immutable) =>
      val newBody = if (body.isDefined) Some(sanitize(body.get)) else body
      super.transFieldInternal(FieldDef(annos, vis, name, genDefunClassDef(ty).typ, newBody, immutable), classDef)
    case f =>
      super.transFieldInternal(f, classDef)
  }

  override private[lowering] def transParamInternal(param: Param): Param = param match {
    // Transform: set params to object set params
    case Param(name, TSet(ty)) =>
      usedVars += (name -> ty)
      Param(name, genDefunClassDef(ty).typ)
    case p => p
  }

  override private[lowering] def transStatementInternal(stmt: Statement): Statement = stmt match {
    case ExprStmt(expression) =>
      ExprStmt(sanitize(expression))

    case FieldAssignStmt(recv, name, expression) =>
      FieldAssignStmt(sanitize(recv), name, sanitize(expression))

    // Transform: set variables to object set variables
    case VarDeclareStmt(name, TSet(ty), maybeExpression, immutable) =>
      usedVars += (name -> ty)
      val expr = if (maybeExpression.isDefined) Some(sanitize(maybeExpression.get)) else None
      VarDeclareStmt(name, genDefunClassDef(ty).typ, expr, immutable)

    case VarDeclareStmt(name, typ, maybeExpression, immutable) =>
      usedVars += (name -> typ)
      super.transStatementInternal(stmt)

    case VarAssignStmt(_, _) =>
      throw new IllegalArgumentException("Static single assignment failed! Encountered unexpected var assignment.")

    case VarPhiAssignStmt(_, _, _, _, _) =>
      super.transStatementInternal(stmt)

    case IfStmt(cnd, thn, els) =>
      super.transStatementInternal(IfStmt(sanitize(cnd), thn, els))

    // Return: Keep the set without applying any transformation to the expression
    case ReturnStmt(expr) =>
      ReturnStmt(sanitize(expr, requiresTrueSet = expr.typ.exists(_.isInstanceOf[TSet])))
  }

  /**
   * Make a real set out of an objectified expression.
   * @param expression The expression to transform to a real set.
   * @param typ The original type of the expression to transform (e.g. Set[Int]), not the objectified type.
   * @return The expression transformed to a real set.
   */
  def apply(expression: Expression, typ: Option[Type]): Expression = {
    val isSet = typ.exists(_.isInstanceOf[TSet])
    if (isSet)
      MethodCallExpr(clearExpression(expression), Name("apply"), Seq())
    else
      clearExpression(expression)
  }

  /**
   * Make an object out of a true set if required. This function does not traverse the ast.
   * @param expression The expression to objectify.
   * @param allowsTrueSet True to skip the objectifying and return a real set instead.
   * @param typ The original type of the expression to objectify.
   * @return Expression that is replaced by an object if required (that means it is a set and allowsTrueSet is false).
   */
  def unapply(expression: Expression, allowsTrueSet: Boolean, typ: Option[Type]): Expression = {
    val isSet = typ.exists(_.isInstanceOf[TSet])
    if (allowsTrueSet | !isSet)
      clearExpression(expression)
    else {
      //val typ = expression.typ.getOrElse(throw new IllegalArgumentException(s"Untyped expression $expression"))
      val TSet(ty) = typ.getOrElse(throw new IllegalArgumentException(s"Untyped expression $expression"))
      val exprVarNames = expression.vars.keySet
      val vars = usedVars.filter { case (k, _) => exprVarNames.contains(k) }
      val auxClass = genAuxDef(vars, clearType(ty), genDefunClassDef(ty).typ.ref, expression)
      val args = vars.map { case (k, _) => VarReadExpr(k) }.toSeq
      ConstructorExpr(auxClass.typ.ref, args)
    }
  }

  /**
   * Clear the target and type information of an expression.
   * @param expression The expression to objectify.
   * @return The cleared expression.
   */
  private def clearExpression(expression: Expression): Expression = super.transExpression(expression)
  private def clearType(typ: Type): Type = super.transType(typ)

  /**
   * Traverse the expression hierarchy to sanitize all set to objects if required.
   * @param expression The expression to sanitize.
   * @param requiresTrueSet Some expression always require a true set at certain position (e.g. the member of a
   *                        SetComprehension). Set this flag to true, to not transform these expressions.
   * @return Expression that is replaced by an object if required (that means it is a set and allowsTrueSet is false).
   */
  private def sanitize(expression: Expression, requiresTrueSet: Boolean = false): Expression = expression match {
    case FieldReadExpr(recv, targetName) if requiresTrueSet =>
      apply(FieldReadExpr(sanitize(recv), targetName), expression.typ)
    case FieldReadExpr(recv, targetName) =>
      FieldReadExpr(sanitize(recv), targetName)
    case VarReadExpr(targetName) if requiresTrueSet =>
      apply(VarReadExpr(targetName), expression.typ)

    case ConstructorExpr(ClassRef(name), args) =>
      ConstructorExpr(ClassRef(name), args.map(sanitize(_)))
    case SuperExpr(args) =>
      SuperExpr(args.map(sanitize(_)))
    case MethodCallExpr(recv, fun, args) =>
      unapply(MethodCallExpr(sanitize(recv), fun, args.map(sanitize(_))), requiresTrueSet, expression.typ)
    case TypeCastExpr(recv, toTyp) =>
      TypeCastExpr(sanitize(recv), clearType(toTyp))
    case InstanceOfExpr(recv, ofTyp) =>
      InstanceOfExpr(sanitize(recv), clearType(ofTyp))
    case SetExpr(exps) =>
      unapply(SetExpr(exps.map(sanitize(_))), requiresTrueSet, expression.typ)
    case SetMemberExpr(name, recv, predicate) =>
      val pred = if (predicate.isDefined) Some(sanitize(predicate.get)) else None
      SetMemberExpr(name, sanitize(recv, requiresTrueSet = true), pred)
    case SetComprehension(member, body) =>
      unapply(SetComprehension(member.map(sanitize(_, requiresTrueSet = true)), sanitize(body)), requiresTrueSet, expression.typ)
    case BaseApplyExpr(fun, args) =>
      BaseApplyExpr(fun, args.map(sanitize(_)))
    case BaseApplyInfixExpr(left, op, right) =>
      val infixOp = BaseApplyInfixExpr(sanitize(left, requiresTrueSet = true), op, sanitize(right, requiresTrueSet = true))
      if (expression.typ.exists(_.isInstanceOf[TSet]))
        unapply(infixOp, requiresTrueSet, expression.typ)
      else
        infixOp
    case BaseApplyMethodExpr(recv, method, args) =>
      val newArgs = if (args.isDefined) Some(args.get.map(sanitize(_))) else None
      BaseApplyMethodExpr(sanitize(recv), method, newArgs)
    case BaseApplyUnaryExpr(op, exp) =>
      BaseApplyUnaryExpr(op, sanitize(exp))
    case TupleReadExpr(recv, index) =>
      TupleReadExpr(sanitize(recv), index)
    case TupleExpr(exps) =>
      TupleExpr(exps.map(sanitize(_)))
    case _ => clearExpression(expression)
  }
}
