package inca.frontend.objectoriented.lowering

import inca.frontend.objectoriented.core._
import inca.runtime.context.DataModel
import inca.util.Gensym
import truechange.SortType

object Defunctionalize {
  def transformModule(module: Module, model: DataModel): Module = {
    new Defunctionalize(module, model).transModule()
  }

  def transformModules(modules: Seq[(Module, DataModel)]): Seq[Module] =
    modules.map(mm => transformModule(mm._1, mm._2))
}

// TODO: Support tuples
//  E.g which is not correctly defunctionalized:
//  class A {}
//  class B extends A {}
//  val a: Set[(A, B)] = Set(new B(), new B())

class Defunctionalize(val module: Module, val dataModel: DataModel) extends ModuleLowering {
  private val gensym: Gensym = new Gensym(Iterable.empty)

  var auxClassDefs: Set[ClassDef] = Set()
  var defnClassDefs: Map[Type, ClassDef] = Map()

  private def genDefunClassDef(ty: Type): ClassDef = {
    val typ = clearType(ty)
    if (defnClassDefs.contains(typ))
      return defnClassDefs(typ)

    // create the inheritance hierarchy for the defun class
    val parentClassDefs = ty match {
      case TTuple(ts) =>
        ts.map(genDefunClassDef)
        Seq()
      case TClass(ref) =>
        val sortTy = SortType(ref.name.raw)
        // create a defun class for each supertype
        val superTypes = dataModel.nodeSupertypes.get(sortTy)
          .map(s => (s.name, genDefunClassDef(TClass(ClassRef(Name(s.name))))))
          .toMap
        // return a seq with all direct parent class refs
        val directSuperTypes = dataModel.directNodeSupertypes.get(sortTy).map(_.name).toSet
        superTypes.flatMap {
          case (clsName, defunClassDef) if directSuperTypes.contains(clsName) => Some(defunClassDef)
          case _ => None
        }
      case TSet(_) => throw new IllegalArgumentException("Defun classes must have a simple type, not TSet!")
      case _ => Seq()
    }

    val parentRefs = parentClassDefs.map(_.typ.ref).toSeq
    val apply = MethodDef(Seq(), Some(Private), Name("apply"), Seq(), TSet(typ), Seq())
    val constr = ConstructorDef(Seq(), None, Seq(), Seq())
    val clazz = ClassDef(Seq(), Some(Private), Name(gensym.fresh("Defun")), parentRefs, Seq(constr, apply))
    defnClassDefs += typ -> clazz
    clazz
  }

  private def genAuxDef(constrVars: Map[Name, Type], innerSetType: Type, parent: ClassRef, expr: Expression): ClassDef = {
    // rename all "this" to obj$i, since this is reserved
    val subst = constrVars.map {
      case (name@Name("this"), _) => name -> Name(gensym.fresh("obj"))
      case (name, _) => name -> name
    }
    val contentType = TSet(clearType(innerSetType))
    val fields = Seq(FieldDef(Seq(), None, Name("content"), contentType, None, immutable = true))

    // return the precomputed set
    val ret = ReturnStmt(FieldReadExpr(VarReadExpr(Name("this")), Name("content")))
    val apply = MethodDef(Seq(), Some(Private), Name("apply"), Seq(), contentType, Seq(ret))
    // create a default constructor
    val constrParams = constrVars.map { case (subst(name), typ) => Param(name, clearType(typ)) }.toSeq
    val constrBody = fields.map { f =>
      FieldAssignStmt(
        VarReadExpr(Name("this")), f.name, clearExpression(expr)
      )
    }
    val constr = ConstructorDef(Seq(), None, constrParams, constrBody)
    val clsName = Name(gensym.fresh("Aux"))
    val clazz = ClassDef(Seq(), Some(Private), clsName, Seq(parent), fields :+ constr :+ apply)
    auxClassDefs += clazz
    clazz
  }

  var usedVars: Map[Name, Type] = Map()

  override private[lowering] def transModuleInternal(module: Module): Module = {
    val Module(name, imports, classes) = module
    // make sure the defun class name is unique
    gensym.register(module.usedModuleNames.map(_.raw))
    gensym.register(classes.map(_.name.raw))
    val transClasses = classes.map(transClass)
    Module(name, imports, transClasses ++ auxClassDefs ++ defnClassDefs.values)
  }

  override private[lowering] def transMethodInternal(methodDef: MethodDef, classDef: ClassDef): MethodDef = {
    usedVars = Map(Name("this") -> classDef.typ)
    super.transMethodInternal(methodDef, classDef)
  }

  override private[lowering] def transConstructorInternal(constructorDef: ConstructorDef, classDef: ClassDef): ConstructorDef = {
    usedVars = Map(Name("this") -> classDef.typ)
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
      val newTyp = genDefunClassDef(ty).typ
      usedVars += (name -> newTyp)
      Param(name, newTyp)
    case Param(name, ty) =>
      usedVars += (name -> ty)
      Param(name, ty)
  }

  override private[lowering] def transStatementInternal(stmt: Statement): Seq[Statement] = stmt match {
    case ExprStmt(expression) =>
      Seq(ExprStmt(sanitize(expression)))
    case FieldAssignStmt(recv, name, expression) =>
      Seq(FieldAssignStmt(sanitize(recv), name, sanitize(expression)))
    // Transform: set variables to object set variables
    case VarDeclareStmt(name, TSet(ty), maybeExpression, immutable) =>
      val newTyp = genDefunClassDef(ty).typ
      usedVars += (name -> newTyp)
      val expr = if (maybeExpression.isDefined) Some(sanitize(maybeExpression.get)) else None
      Seq(VarDeclareStmt(name, newTyp, expr, immutable))
    case VarDeclareStmt(name, typ, _, _) =>
      usedVars += (name -> typ)
      super.transStatementInternal(stmt)
    case VarAssignStmt(_, _) =>
      throw new IllegalArgumentException("Defunctionalization failed! Encountered unexpected var assignment.")
    case VarPhiAssignStmt(_, _, _, _, _) =>
      super.transStatementInternal(stmt)
    case IfStmt(cnd, thn, els) =>
      super.transStatementInternal(IfStmt(sanitize(cnd), thn, els))
    case ReturnStmt(expr) =>
      Seq(ReturnStmt(sanitize(expr, requiresTrueSet = expr.typ.exists(_.isInstanceOf[TSet]))))
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
   * @param expression The expression to clear.
   * @return The cleared expression.
   */
  private def clearExpression(expression: Expression): Expression = super.transExpression(expression).head

  /**
   * Clear the target information of a type.
   * @param type The type to clear.
   * @return The cleared type.
   */
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
    case SetExpr(exps, tty) =>
      unapply(SetExpr(exps.map(sanitize(_)), if (tty.isDefined) Some(clearType(tty.get)) else tty), requiresTrueSet, expression.typ)
    case SetMemberExpr(name, recv, predicate) =>
      val pred = if (predicate.isDefined) Some(sanitize(predicate.get)) else None
      SetMemberExpr(name, sanitize(recv, requiresTrueSet = true), pred)
    case SetComprehension(member, body) =>
      unapply(SetComprehension(member.map(sanitize(_, requiresTrueSet = true)), sanitize(body)), requiresTrueSet, expression.typ)
    case BaseApplyExpr(fun, args) =>
      BaseApplyExpr(fun, args.map(sanitize(_)))
    case BaseApplyInfixExpr(left, op, right) =>
      val infixOp = BaseApplyInfixExpr(sanitize(left, requiresTrueSet = true), op, sanitize(right, requiresTrueSet = true))
      unapply(infixOp, requiresTrueSet, expression.typ)
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
