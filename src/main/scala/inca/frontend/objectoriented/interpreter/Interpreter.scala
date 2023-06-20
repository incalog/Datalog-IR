package inca.frontend.objectoriented.interpreter

import inca.frontend.objectoriented.core.{ExprStmt, Expression, FieldAssignStmt, IfStmt, MethodDef, Module, Name, Param, ReturnStmt, Statement, VarAssignStmt, VarDeclareStmt, VarPhiAssignStmt}

class Interpreter(module: Module) {
  private type Environment = Map[Name, Value]

  // TODO: Class table
  // TODO: Method table

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
    case VarDeclareStmt(name, _, maybeExpression, true) =>
      val exp = maybeExpression.getOrElse(
        throw new IllegalArgumentException("Can not bind immutable local variable without value!")
      )
      env += name -> interp(exp)
    case VarDeclareStmt(name, _, maybeExpression, false) =>
      val expr =
        if (maybeExpression.isEmpty)
          Address.nullPtr
        else
          interp(maybeExpression.get)
      // Add value to store and update environment
      val addr = store.malloc()
      store.update(addr, expr)
      env += name -> addr
    case VarAssignStmt(targetName, expression) =>
      val addr = env.get(targetName) match {
        case Some(Address(index)) => index
        case Some(v) => throw new IllegalArgumentException(s"Expected address, but got: $v")
        case None => throw new IllegalArgumentException(s"Can not assign to undeclared variable $targetName")
      }
      // It should be save to just override the value inside the store
      store.update(addr, interp(expression))

      //val addr = store.malloc()
      //store.update(addr, expr)
      //env += targetName -> interp(expression)
    case VarPhiAssignStmt(_, _, _, _, _) =>
      throw new IllegalStateException("Found unexpected phi node during interpretation!")
    case IfStmt(cnd, thn, els) =>
      interp(cnd) match {
        case ScalaValue(true) => thn.foreach(interp)
        case ScalaValue(false) => els.foreach(interp)
        case _ => throw new IllegalStateException(s"Unexpected condition value $cnd")
      }
  }

  def interp(expr: Expression): Value = ???
}
