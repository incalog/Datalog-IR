package inca.frontend.objectoriented.interpreter

import inca.frontend.objectoriented.core._

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

  def scopedEnv[A](f: => A): A = {
    val oldenv = this.env
    try {
      val v = f
      v
    } finally {
      this.env = oldenv
    }
  }

  // TODO: Might change Unit return type for fixpoints
  def interp(main: MethodDef, args: Seq[Value]): Unit = scopedEnv {
    val params = main.params
    if (params.size != args.size)
      throw new IllegalArgumentException(s"Expected ${params.size} arguments, but got ${args.size}")

    // bind input arguments to variables
    // TODO: Either bind immutable variables the same way and handle the case correctly or move this to the store
    params.zip(args).foreach {
      case (Param(name, _), v) => env += name -> v
    }

    main.body.foreach(interp)
  }

  def interp(stmt: Statement): Unit = stmt match {
    case ExprStmt(expression) => interp(expression)
    case ReturnStmt(expression) => interp(expression)
    case FieldAssignStmt(recv, name, expression) =>
      // TODO: Assign object
      interp(recv)
      interp(expression)
    /*case VarDeclareStmt(name, _, maybeExpression, true) =>
      val exp = maybeExpression.getOrElse(
        throw new IllegalArgumentException("Can not bind immutable local variable without value!")
      )
      env += name -> interp(exp)*/
    case VarDeclareStmt(name, _, maybeExpression, immutable) =>
      // TODO: Currently this stores everything in the store.
      //    Do we want to save immutable ScalaValues in the environment directly ?
      val expr =
        if (maybeExpression.isEmpty && immutable)
          throw new IllegalArgumentException(s"Can not bind immutable local variable $name without a value!")
        else if (maybeExpression.isEmpty)
          Address.nullPtr
        else
          interp(maybeExpression.get)
      // Add value to the store and update the environment
      val addr = store.malloc()
      store.update(addr, expr)
      env += name -> addr
    case VarAssignStmt(targetName, expression) =>
      val addr = env.get(targetName) match {
        case Some(Address(index)) => index
        case Some(v) => throw new IllegalArgumentException(s"Expected address, but got: $v")
        case None => throw new IllegalArgumentException(s"Can not assign to undeclared variable $targetName")
      }
      store.update(addr, interp(expression))
    case VarPhiAssignStmt(_, _, _, _, _) =>
      throw new IllegalStateException("Found unexpected phi node during interpretation!")
    case IfStmt(cnd, thn, els) =>
      interp(cnd).asBool match {
        case Some(true) => thn.foreach(interp)
        case Some(false) => els.foreach(interp)
        case _ => throw new IllegalStateException(s"Unexpected condition value $cnd")
      }
  }

  def interp(expr: Expression): Value = expr match {
    case VarReadExpr(targetName) =>
      env.get(targetName) match {
        case Some(Address(index)) =>
          store.lookup(index) match {
            case Some(value) => value
            case None => throw new IllegalArgumentException(s"Illegal reference to address $index")
          }
        case Some(value) => value
        case None => throw new IllegalArgumentException(s"Undeclared variable $targetName")
      }
    case FieldReadExpr(recv, targetName) => ???
    case constr@ConstructorExpr(classRef, args) =>
      constr.target match {
        case Some(ConstructorDef(_, _, params, body)) =>
          // TODO: bind params + interp body (inside a new scope)
          ???
        case None => throw new IllegalArgumentException(s"No matching constructor found for ${classRef.name}")
      }
    case SuperExpr(args) => ???
    case MethodCallExpr(recv, fun, args, isFix) => ???
    case TypeCastExpr(recv, toTyp) => ???
    case InstanceOfExpr(recv, ofTyp) => ???
    case NullExpr() => ???
    case TupleReadExpr(recv, index) => ???
    case TupleExpr(exps) => ???
    case SetExpr(exps, tty) => ???
    case SetMemberExpr(name, recv, predicate) => ???
    case SetComprehension(member, body) => ???
    case SetFold(recv, projection, opClass, opMethod, neutral) => ???
    case BaseLitExpr(code) => ???
    case BaseApplyExpr(fun, args) => ???
    case BaseApplyInfixExpr(left, op, right) => ???
    case BaseApplyMethodExpr(recv, method, args) => ???
    case BaseApplyUnaryExpr(op, exp) => ???
  }
}
