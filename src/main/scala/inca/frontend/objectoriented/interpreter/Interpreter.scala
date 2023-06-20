package inca.frontend.objectoriented.interpreter

import inca.frontend.objectoriented.core.{ExprStmt, Expression, FieldAssignStmt, IfStmt, MethodDef, Module, Name, Param, ReturnStmt, Statement, VarAssignStmt, VarDeclareStmt, VarPhiAssignStmt}

trait Interpreter {
  private type Environment = Map[Name, Value]

  var store: Store
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

  def interp(module: Module, input: Seq[Value]): Unit = {
    // Note: Our Datalog translation does currently only support scala values as input
    val mainMethods = module.classes.flatMap { c =>
      c.methods.filter { m =>
        m.isMain
      }
    }
    if (mainMethods.isEmpty)
      throw new IllegalArgumentException("Missing program entry point!")
    if (mainMethods.size > 1)
      throw new IllegalArgumentException("Ambiguous program entry point!")
    interp(mainMethods.head, input)
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
      val idx = store.malloc()
      store.updated(idx, expr)
      env += name -> idx
    case VarAssignStmt(targetName, expression) =>
      // TODO: Var assign
      interp(expression)
    case VarPhiAssignStmt(name, typ, ifStmt, thnName, elsName) =>
      throw new IllegalStateException("Found unexpected phi node during interpretation!")
    case IfStmt(cnd, thn, els) => ???
  }

  def interp(expr: Expression): Value = ???
}
