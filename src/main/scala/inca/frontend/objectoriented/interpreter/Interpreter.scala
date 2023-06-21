package inca.frontend.objectoriented.interpreter

import inca.frontend.objectoriented.core._

import scala.annotation.tailrec

// TODO: Currently we store everything in the store. We might optimize that
class Interpreter(module: Module) {
  private type Environment = Map[Name, Value]

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

  var store: Store = new SimpleStore()

  // lookup table for dynamic dispatching
  private val dispatchTable = DispatchTable(module.classes)

  private def bindParams(params: Seq[Param], args: Seq[Value]): Unit = {
    if (params.size != args.size)
      throw new IllegalArgumentException(s"Expected ${params.size} arguments, but got ${args.size}")
    // bind input arguments to variables
    params.zip(args).foreach { case (Param(name, _), arg) =>
      arg match {
        case obj@(ObjectValue(_, _, _) | StructuralObjectValue(_ , _)) =>
          throw new RuntimeException(s"Debugging: binding parameter object $obj")
        case _ =>
          env += name -> arg
      }
    }
  }

  @tailrec
  private def resolve(value: Value): Value = value match {
    case Address(index) => resolve(store.lookup(index) match {
      case Some(value) => value
      case None => throw new IllegalStateException(s"Could not resolve address $index")
    })
    case _ => value
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

  def interp(stmts: Seq[Statement]): Option[Value] = stmts match {
    case Nil =>
      None
    case s :: rest =>
      val res = interp(s)
      if (res.isDefined)
        res
      else
        interp(rest)
  }

  def interp(stmt: Statement): Option[Value] = stmt match {
    case ExprStmt(expression) =>
      interp(expression)
      None
    case ReturnStmt(expression) =>
      Some(interp(expression))
    case FieldAssignStmt(recv, name, expression) =>
      val addr = interp(recv) match {
        case a: Address => a
        case v => throw new IllegalArgumentException(s"Expected address but found $v")
      }
      val assignValue = interp(expression)
      resolve(addr) match {
        case obj@(ObjectValue(_, _, _) | StructuralObjectValue(_, _)) => obj.updateObject(name.raw, assignValue)
        case v => throw new IllegalStateException(s"Expected object but found $v")
      }
      None
    case VarDeclareStmt(name, _, maybeExpression, immutable) =>
      if (maybeExpression.isEmpty && immutable)
        throw new IllegalArgumentException(s"Can not bind immutable local variable $name without a value!")
      else if (maybeExpression.isEmpty)
        env += name -> Address.nullPtr
      else
        interp(maybeExpression.get) match {
          case obj@(ObjectValue(_, _, _) | StructuralObjectValue(_, _)) =>
            throw new RuntimeException(s"Debugging: Should not happen: $obj")
          case value => env += name -> value
        }
      None
    case VarAssignStmt(name, expression) =>
      env += name -> interp(expression)
      None
    case VarPhiAssignStmt(_, _, _, _, _) =>
      throw new IllegalStateException("Found unexpected phi node during interpretation!")
    case IfStmt(cnd, thn, els) =>
      interp(cnd).asBoolean match {
        case Some(true) => interp(thn)
        case Some(false) => interp(els)
        case None => throw new IllegalStateException(s"Expected a boolean condition!")
      }
  }

  def interp(expr: Expression): Value = expr match {
    case VarReadExpr(name) => env.get(name) match {
        case Some(v) => v
        case None => throw new IllegalArgumentException(s"Can not read undeclared variable $name")
      }
    case FieldReadExpr(recv, targetName) =>
      val addr = interp(recv) match {
        case a: Address => a
        case v => throw new IllegalArgumentException(s"Expected address but found $v")
      }
      resolve(addr).asObject match {
        case Some((cls, _, fields)) => fields.get(targetName.raw) match {
          case Some(value) => value
          case None => throw new IllegalStateException(s"Field not found $targetName for instance of class $cls")
        }
        case v => throw new IllegalStateException(s"Expected object but found $v")
      }
    case constr@ConstructorExpr(classRef, args) =>
      // we can lookup the constructor directly without using the dispatch table
      constr.target match {
        case Some(ConstructorDef(_, _, params, body)) => scopedEnv {
          bindParams(params, args.map(interp))
          val obj =
            classRef.target match {
              case Some(classDef) if classDef.isCaseClass =>
                StructuralObjectValue(classRef.name.raw, params.map(p => p.name.raw -> Address.nullPtr).toMap)
              case Some(_) =>
                ObjectValue(classRef.name.raw, freshId(), params.map(p => p.name.raw -> Address.nullPtr).toMap)
              case None =>
                throw new IllegalArgumentException(s"Unresolved classRef $classRef")
            }
          // store the object
          val index = store.malloc()
          store.update(index, obj)
          val addr = Address(index)
          // bind this and interpret the constructor body
          env += Name("this") -> addr
          interp(body)
          // return the address to the object
          addr
        }
        case _ => throw new IllegalArgumentException(s"No matching constructor found for ${classRef.name}")
      }
    case SuperExpr(args) => ???
    case MethodCallExpr(recv, fun, args, isFix) =>
      val className = interp(recv).asObject match {
        case Some((cls, _, _)) => Name(cls)
        case None => throw new IllegalArgumentException(s"Expected address but found $v")
      }
      dispatchTable.lookup(className, fun) match {
        case Some(MethodDef(_, _, _, params, _, body)) => scopedEnv {
          bindParams(params, args.map(interp))
          // well-typed programs always return a value
          interp(body) match {
            case Some(value) => value
            case None => throw new IllegalArgumentException(s"Missing return value for method $fun")
          }
        }
        case None => throw new IllegalArgumentException(s"No matching method found with name $fun")
      }
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
