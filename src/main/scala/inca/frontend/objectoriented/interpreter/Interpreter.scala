package inca.frontend.objectoriented.interpreter

import inca.frontend.objectoriented.core._

// TODO: Currently we store everything in the store. We might optimize that
class Interpreter(module: Module) {
  private type Environment = Map[Name, Value]

  // TODO: Do we need these ? We implicitly resolved these references after typechecking
  /*val classTable: Map[Name, ClassDef] = module.classes.map(c => c.name -> c).toMap
  val methodTable: Map[(Name, Name), Seq[MethodDef]] = module.classes.flatMap { c =>
    c.methods.groupBy(_.name).map { case (methodName, methods) =>
      (c.name, methodName) -> methods
  }}.toMap*/

  var store: Store = new SimpleStore()
  var env: Environment = Map()

  private def scopedEnv[A](f: => A): A = {
    val oldenv = this.env
    try {
      val v = f
      v
    } finally {
      this.env = oldenv
    }
  }

  private def bindParams(params: Seq[Param], args: Seq[Value]): Unit = {
    if (params.size != args.size)
      throw new IllegalArgumentException(s"Expected ${params.size} arguments, but got ${args.size}")
    // bind input arguments to variables
    params.zip(args).foreach { case (Param(name, _), arg) => bindVar(name, arg) }
  }

  private def bindVar(name: Name, value: Value): Unit = {
    val addr = store.malloc()
    store.update(addr, value)
    env += name -> addr
  }

  private def updateVar(name: Name, value: Value): Unit = {
    store.update(lookupAddr(name), value)
  }

  private def lookupVar(name: Name): Value = {
    store.lookup(lookupAddr(name)) match {
      case Some(value) => value
      case None => throw new IllegalArgumentException(s"Could not lookup variable $name")
    }
  }

  private def lookupAddr(name: Name): Int = env.get(name) match {
    case Some(Address(index)) => index
    case Some(v) => throw new IllegalArgumentException(s"Expected address, but got: $v")
    case None => throw new IllegalArgumentException(s"Can not assign to undeclared variable $name")
  }

  private def resolveBool(value: Value): Boolean = value match {
    case ScalaValue(true) => true
    case ScalaValue(false) => false
    case Address(index) => resolveBool(store.lookup(index) match {
      case Some(value) => value
      case None => throw new IllegalArgumentException("")
    })
    case _ => throw new IllegalArgumentException("")
  }


  private def resolveObject(value: Value) = {

  }

  private var nextId: Int = 0

  private def freshId(): Int = {
    val newId = nextId
    nextId += 1
    newId
  }

  // TODO: Might change Unit return type for fixpoints
  def interp(main: MethodDef, args: Seq[Value]): Unit = scopedEnv {
    bindParams(main.params, args)
    main.body.foreach(interp)
  }

  def interp(stmt: Statement): Option[Value] = stmt match {
    case ExprStmt(expression) =>
      interp(expression)
      None
    case ReturnStmt(expression) =>
      Some(interp(expression))
    case FieldAssignStmt(recv, name, expression) =>
      // TODO: Assign object
      interp(recv)
      interp(expression)
      None
    /*case VarDeclareStmt(name, _, maybeExpression, true) =>
      val exp = maybeExpression.getOrElse(
        throw new IllegalArgumentException("Can not bind immutable local variable without value!")
      )
      env += name -> interp(exp)*/
    case VarDeclareStmt(name, _, maybeExpression, immutable) =>
      val expr =
        if (maybeExpression.isEmpty && immutable)
          throw new IllegalArgumentException(s"Can not bind immutable local variable $name without a value!")
        else if (maybeExpression.isEmpty)
          Address.nullPtr
        else
          interp(maybeExpression.get)
      bindVar(name, expr)
      None
    case VarAssignStmt(targetName, expression) =>
      updateVar(targetName, interp(expression))
      None
    case VarPhiAssignStmt(_, _, _, _, _) =>
      throw new IllegalStateException("Found unexpected phi node during interpretation!")
    case IfStmt(cnd, thn, els) =>
      resolveBool(interp(cnd)) match {
        case Some(true) =>
          thn.foreach(interp)
          None
        case Some(false) =>
          els.foreach(interp)
          None
        case _ =>
          throw new IllegalStateException(s"Unexpected condition value $cnd")
      }
  }

  def interp(expr: Expression): Value = expr match {
    case VarReadExpr(targetName) =>
      lookupVar(targetName)
    case FieldReadExpr(recv, targetName) =>
      interp(recv) match {
        case ObjectValue(cls, id, fvals) => ???
        case StructuralObjectValue(cls, fvals) => ???
        case _ =>
      }
      ???
    case constr@ConstructorExpr(classRef, args) =>
      constr.target match {
        case Some(ConstructorDef(_, _, params, body)) => scopedEnv {
          bindParams(params, args.map(interp))
          val obj =
            classRef.target match {
              case Some(classDef) if classDef.isCaseClass =>
                StructuralObjectValue(classRef.name.raw, params.map(_ => Address.nullPtr))
              case Some(_) =>
                ObjectValue(classRef.name.raw, freshId(), params.map(_ => Address.nullPtr))
              case None =>
                throw new IllegalArgumentException(s"Unresolved classRef $classRef")
            }
          bindVar(Name("this"), obj)
          body.foreach(interp)
          obj
        }
        case None => throw new IllegalArgumentException(s"No matching constructor found for ${classRef.name}")
      }
    case SuperExpr(args) => ???
    case MethodCallExpr(recv, fun, args, isFix) => ???
    case TypeCastExpr(recv, toTyp) => ???
    case InstanceOfExpr(recv, ofTyp) => ???
    case NullExpr() => Address.nullPtr
    case TupleReadExpr(recv, index) => ???
    case TupleExpr(exps) => ???
    case SetExpr(exps, tty) => ???
    case SetMemberExpr(name, recv, predicate) => ???
    case SetComprehension(member, body) => ???
    case SetFold(recv, projection, opClass, opMethod, neutral) => ???

    // Use scala reflection for those
    case BaseLitExpr(code) => ???
    case BaseApplyExpr(fun, args) => ???
    case BaseApplyInfixExpr(left, op, right) => ???
    case BaseApplyMethodExpr(recv, method, args) => ???
    case BaseApplyUnaryExpr(op, exp) => ???
  }
}
