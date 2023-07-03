package inca.frontend.objectoriented.interpreter

import inca.frontend.objectoriented.core._
import inca.frontend.objectoriented.interpreter

import scala.meta.{XtensionQuasiquoteTermParam, XtensionQuasiquoteType}

final case class TypeCastException(obj: Value, typ: String) extends RuntimeException(s"Could not cast $obj to type $typ!")

class Interpreter(module: Module) {
  println("The module: ", module)
  // FIXME: Is fixpoint correctly implemented with the exception (I know that we don't need this for sets) ?
  // TODO: Support MonoMap

  private val scalaInterpreter = new ScalaInterpreter {}

  def join(v1: Value, v2: Value): MaybeChanged[Value] = {
    (v1, v2) match {
      case (SetValue(s1), SetValue(s2)) =>
        val res = SetValue(s1 ++ s2)
        if (res == v1)
          Unchanged(res)
        else
          Changed(res)
      case (Value.UNIT, Value.UNIT) =>
        Unchanged(Value.UNIT)
      case (_, _) =>
        throw new IllegalStateException(s"Can not join $v1 and $v2")
    }
  }
  private val stack = new StackImpl[(Name, Seq[Value]), Value]()(join)

  private type Environment = Map[Name, Value]

  var env: Environment = Map()

  private def newScope[A](f: => A): A = {
    val oldenv = this.env
    this.env = Map()
    try {
      val v = f
      v
    } finally {
      this.env = oldenv
    }
  }

  private val classTable = ClassTable(module.classes)
  private val dispatchTable = DispatchTable(module.classes)

  val monoStateVarName = "state"

  private def hasMonoType(expr: Expression): Boolean = {
    expr.typ match {
      case Some(TClass(ref)) => ref.target match {
        case Some(classDef) => classDef.isMonotoneClass
        case None => throw new IllegalStateException(s"Unresolved classRef $ref")
      }
      case None => throw new IllegalStateException(s"Untyped expression $expr")
    }
  }

  private def hasMonoMapType(expr: Expression): Boolean = {
    expr.typ match {
      case Some(TClass(ref)) => ref.target match {
        case Some(classDef) => classDef.isMonotoneMapClass
        case None => throw new IllegalStateException(s"Unresolved classRef $ref")
      }
      case None => throw new IllegalStateException(s"Untyped expression $expr")
    }
  }

  private def bindParams(params: Seq[Param], args: Seq[Value]): Unit = {
    if (params.size != args.size)
      throw new IllegalArgumentException(s"Expected ${params.size} arguments, but got ${args.size}")
    // bind input arguments to variables
    params.zip(args).foreach {
      case (Param(name, _), arg) => env += name -> arg
    }
  }

  private var nextId: Int = 0

  // Always start with id 1. Structural objects have id 0.
  private def freshId(): Int = {
    nextId += 1
    nextId
  }

  def run(main: MethodDef, args: Seq[Value]): Value = newScope {
    bindParams(main.params, args)
    run(main.body).getOrElse(Value.UNIT)
  }

  def run(stmts: Seq[Statement]): Option[Value] = stmts match {
    case Nil =>
      None
    case s :: rest =>
      val res = run(s)
      if (res.isDefined)
        res
      else
        run(rest)
  }

  def run(stmt: Statement): Option[Value] = stmt match {
    case ExprStmt(expression) =>
      eval(expression)
      None
    case ReturnStmt(expression) =>
      Some(eval(expression))
    case FieldAssignStmt(recv, name, expression) =>
      val obj = eval(recv)
      obj.asObject match {
        case _ => obj.updateObject(name.raw, eval(expression))
      }
      None
    case VarDeclareStmt(name, _, maybeExpression, immutable) =>
      if (maybeExpression.isEmpty && immutable)
        throw new IllegalArgumentException(s"Can not bind immutable local variable $name without a value!")
      else if (maybeExpression.isEmpty)
        env += name -> Value.NULL
      else
        env += name -> eval(maybeExpression.get)
      None
    case VarAssignStmt(name, expression) =>
      env += name -> eval(expression)
      None
    case VarPhiAssignStmt(_, _, _, _, _) =>
      throw new IllegalStateException("Found unexpected phi node during interpretation!")
    case IfStmt(cnd, thn, els) =>
      if (eval(cnd).asBoolean)
        run(thn)
      else
        run(els)
  }

  def monoJoin(monoClassName: Name, v1: Value, v2: Value): Value = {
    val method = dispatchTable.lookup(monoClassName, Name("join"))
    newScope {
      bindParams(method.params, Seq(v1, v2))
      run(method.body).get
    }
  }

  def eval(expr: Expression): Value = expr match {
    // Handle mono map
    case MethodCallExpr(recv, Name("get"), Seq(keyExpr), _) if hasMonoMapType(recv) =>
      val (className, _, fvals) = eval(recv).asObject
      val classDef = classTable.lookup(Name(className))
      val Some((_, TClass(ClassRef(monoValueClass)))) = classDef.montoneTypes

      val keyValue = eval(keyExpr)
      val stateValues = fvals(monoStateVarName).asSet.flatMap {
        // TODO: We only support Map with a single value type
        case TupleValue(key :: Seq(value)) if key == keyValue => Some(value)
        case _ => None
      }
      val initMethod = dispatchTable.lookup(monoValueClass, Name("init"))
      val initValue = newScope { run(initMethod.body).get }
      stateValues.fold(initValue) { case (agg, cur) => monoJoin(monoValueClass, agg, cur) }

    case MethodCallExpr(recv, Name("keys"), args, _) if hasMonoMapType(recv) =>
      val (_, _, fvals) = eval(recv).asObject
      SetValue(fvals(monoStateVarName).asSet.map { case TupleValue(key :: _) => key })

    // Handle the mono cases first
    case FieldReadExpr(recv, Name("result")) if hasMonoType(recv) =>
      val recvObj = eval(recv)
      val (className, _, fvals) = recvObj.asObject

      val stateValues = fvals(monoStateVarName).asSet
      val initMethod = dispatchTable.lookup(Name(className), Name("init"))
      val initValue = newScope { run(initMethod.body).get }
      stateValues.fold(initValue) { case (agg, cur) => monoJoin(Name(className), agg, cur) }
    case MethodCallExpr(recv, meth@Name("__plus__"), args, _) if hasMonoType(recv) =>
      val recvObj = eval(recv)
      val (className, _, fvals) = recvObj.asObject

      def monoLift(args: Seq[Value]): Value = if (hasMonoMapType(recv)) {
        args.head
      } else {
        val method = dispatchTable.lookup(Name(className), Name("lift"))
        newScope {
          bindParams(method.params, args)
          env += Name("this") -> recvObj
          run(method.body).get
        }
      }

      val addMethod = dispatchTable.lookup(Name(className), meth)
      val argVals = args.map(eval)

      newScope {
        bindParams(addMethod.params, argVals)
        env += Name("this") -> recvObj
        val newState = fvals(monoStateVarName).asSet + monoLift(argVals)
        recvObj.updateObject(monoStateVarName, SetValue(newState))
      }
      Value.UNIT

    case VarReadExpr(name) => env.get(name) match {
        case Some(v) => v
        case None => throw new IllegalArgumentException(s"Can not read undeclared variable $name")
      }
    case FieldReadExpr(recv, targetName) =>
      val (cls, _, fields) = eval(recv).asObject
      fields.get(targetName.raw) match {
        case Some(value) => value
        case None => throw new IllegalStateException(s"Field not found $targetName for instance of class $cls")
      }
    case constr@ConstructorExpr(classRef, args) =>
      // we can lookup the constructor directly without using the dispatch table
      constr.target match {
        case Some(ConstructorDef(_, _, params, body)) =>
          val argVals = args.map(eval)
          newScope {
            val className = classRef.name
            val fields = classTable.transitiveCollectFields(className)
            val fieldVals = fields.map { f =>
              f.name.raw -> (if (f.body.isDefined) eval(f.body.get) else Value.NULL)
            }.toMap

            val classDef = classTable.lookup(className)

            // Add monotone result field
            val monoField = if (classDef.isMonotoneClass) {
              Some(monoStateVarName -> SetValue())
            } else {
              None
            }

            val id = if (classDef.isCaseClass) 0 else freshId()
            val obj = ObjectValue(className.raw, id, fieldVals ++ monoField)

            if (classDef.isMonotoneClass)
              obj.isMono = true

            bindParams(params, argVals)
            env += Name("this") -> obj

            run(body)
            obj
          }
        case _ => throw new IllegalArgumentException(s"No matching constructor found for ${classRef.name}")
      }
    case superExpr@SuperExpr(args) =>
      superExpr.target match {
        case Some((_, ConstructorDef(_, _, params, body))) =>
          val argVals = args.map(eval)

          val thisObj = env.get(Name("this")) match {
            case Some(value) => value
            case None => throw new IllegalStateException("'this' is not bound in super expr")
          }

          newScope {
            bindParams(params, argVals)
            env += Name("this") -> thisObj
            run(body)
            thisObj
          }
        case None =>
          throw new IllegalArgumentException(s"Unresolved constructor $superExpr")
      }
    case MethodCallExpr(recv, fun, args, isFix) =>
      val recvObj = eval(recv)
      val (className, _, _) = recvObj.asObject
      val initialArgs = args.map(eval)

      dispatchTable.lookup(Name(className), fun) match {
        case methodDef@MethodDef(_, _, _, params, outType, body) =>

          def runMethod(recvObj: Value, argVals: Seq[Value]): Value = newScope {
            bindParams(params, argVals)
            if (!methodDef.isStatic)
              env += Name("this") -> recvObj
            // well-typed programs always return a value
            run(body) match {
              case Some(value) => value
              case None => throw new IllegalStateException(s"Method $fun needs to return a value !")
            }
          }

          /*
          try {
            val result = stack.fix((fun, recvObj +: initialArgs), if (isFix) throw RecurrentCall() else Set()) {
              case (_, recvVal :: argVals) => newScope {
                bindParams(params, argVals)

                if (!methodDef.isStatic)
                  env += Name("this") -> recvVal

                val res = run(body)

                // well-typed programs always return a value
                res match {
                  case Some(SetValue(values)) => values
                  case Some(value) => Set(value)
                  case None => throw new IllegalArgumentException(s"Missing return value for method $fun")
                }
              }
            }

            if (outType.isInstanceOf[TSet])
              SetValue(result)
            else
              result.head
          } catch {
            case RecurrentCall() => Value.UNIT
            case e => throw e
          }
           */

          val isUnitFixPoint = isFix && outType.isUnit
          val needsFix = isUnitFixPoint || outType.isInstanceOf[TSet]
          val default = if (isUnitFixPoint) Value.UNIT else SetValue()
          try {
            if (needsFix)
              stack.fix((fun, recvObj +: initialArgs), throw RecurrentCall(default)) {
                case (_, recvVal :: argVals) => runMethod(recvVal, argVals)
              }
            else
              runMethod(recvObj, initialArgs)
          } catch {
            case RecurrentCall(default: Value) => default
            case e => throw e
          }
      }
    case TypeCastExpr(recv, TClass(ClassRef(ofName))) =>
      eval(recv) match {
        case Value.NULL => Value.NULL
        case obj => obj.asObject match {
          case (cls, _, _) if classTable.isSubclassOf(Name(cls), ofName) => obj
          case _ => throw TypeCastException(obj, ofName.raw)
        }
      }
    case TypeCastExpr(_, ofTyp) =>
      throw new UnsupportedOperationException(s"asInstanceOf is only supported for class types, but got $ofTyp")
    case InstanceOfExpr(recv, TClass(ClassRef(ofName))) =>
      eval(recv) match {
        case Value.NULL => Value.TRUE
        case obj =>
          val (clsName, _, _) = obj.asObject
          val isSubclass = classTable.isSubclassOf(Name(clsName), ofName)
          ScalaValue(isSubclass)
      }
    case InstanceOfExpr(_, ofTyp) =>
      throw new UnsupportedOperationException(s"isInstanceOf is only supported for class types, but got $ofTyp")
    case NullExpr() =>
      Value.NULL
    case TupleReadExpr(recv, Index(ix)) =>
      eval(recv).asTuple match {
        case values if ix > 0 && ix <= values.size => values(ix-1)
        case _ => throw new IllegalArgumentException(s"Index $ix ouf of bounds for tuple $recv")
      }
    case TupleExpr(exps) =>
      val argVals = exps.map(eval)
      TupleValue(argVals)

    case SetExpr(exps, _) =>
      SetValue(exps.map(eval).toSet)

    case SetComprehension(Seq(), _) =>
      SetValue(Set())
    case SetComprehension(Seq(SetMemberExpr(name, set, None)), body) =>
      val vals = eval(set).asSet
      // Note: Do not open a new scope, since our Datalog compiler does not support scoping here
      val computedVals = for (v <- vals) yield {
        env += name -> v
        // flatten potential nested set results
        eval(body) match {
          case SetValue(values) => values
          case v => Seq(v)
        }
      }.toSeq
      SetValue(computedVals.flatten)
    case SetComprehension(Seq(SetMemberExpr(name, set, Some(pred))), body) =>
      val vals = eval(set).asSet
      def filter(v: Value): Boolean = {
        env += name -> v
        eval(pred).asBoolean
      }

      val computedVals = for (v <- vals if filter(v)) yield {
        env += name -> v
        eval(body) match {
          case SetValue(values) => values
          case v => Seq(v)
        }
      }.toSeq
      SetValue(computedVals.flatten)
    case SetComprehension(mem::rest, body) =>
      eval(SetComprehension(Seq(mem), SetComprehension(rest, body)))

    case SetFold(recv, projection, opClass, opMethod, neutral) =>
      // TODO: projection
      val classDef = classTable.lookup(opClass.name)
      val methods = classDef.methods.filter(_.name == opMethod)
      if (methods.size > 1)
        throw new IllegalStateException(s"Ambiguous method $opMethod for class $opClass in fold $expr")
      else if (methods.size < 1)
        throw new IllegalStateException(s"No method $opMethod found for class $opClass in fold $expr")

      val foldMethod = methods.head
      val setVals = eval(recv).asSet
      setVals.fold(eval(neutral)) { case (acc, current) =>
        newScope {
          bindParams(foldMethod.params, Seq(acc, current))
          run(foldMethod.body) match {
            case Some(value) => value
            case None => throw new IllegalStateException("Can not fold over Unit values!")
          }
        }
      }
    case BaseLitExpr(code) =>
      packInScalaValue(scalaInterpreter.interp(code.syntax))
    case BaseApplyExpr(fun, args) =>
      val argVals = args.map(e => eval(e).asScala)
      val paramsTyped = args.zipWithIndex.map { case (arg, ix) =>
        val argTyp = arg.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $arg"))
        ("arg$" + ix, s"${typeToScala(argTyp)}")
      }.toList
      val scalaArgs = paramsTyped.map(_._1)
      val code = s"((${paramsTyped.map(p => s"${p._1}: ${p._2}").mkString(", ")}) => $fun(${scalaArgs.mkString(", ")}))"
      packInScalaValue(scalaInterpreter.interpClosure(code, argVals:_*))
    case BaseApplyInfixExpr(left, op, right)
      if op.tree.value == "++" && left.typ.exists(_.isInstanceOf[TSet]) && right.typ.exists(_.isInstanceOf[TSet]) =>
      SetValue(eval(left).asSet ++ eval(right).asSet)
    case BaseApplyInfixExpr(left, op, right) =>
      val lhs = eval(left).asScala
      val rhs = eval(right).asScala
      val lhsTy = typeToScala(left.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $left")))
      val rhsTy = typeToScala(right.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $right")))
      val code = s"((left: $lhsTy, right: $rhsTy) => left $op right)"
      packInScalaValue(scalaInterpreter.interpClosure(code, lhs, rhs))
    case BaseApplyMethodExpr(recv, method, args) =>
      val argVals = (recv +: args.getOrElse(Seq())).map(e => eval(e).asScala)
      val paramsTyped = (recv +: args.getOrElse(Seq())).zipWithIndex.map { case (arg, ix) =>
        val argTyp = arg.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $arg"))
        ("arg$" + ix, s"${typeToScala(argTyp)}")
      }.toList
      val scalaArgs = paramsTyped.map(_._1)
      val closureParams = s"(${paramsTyped.map(p => s"${p._1}: ${p._2}").mkString(", ")})"
      val closureBody = if (args.isDefined) {
        val argS = scalaArgs.tail.mkString("(", ", ", ")")
        s"${scalaArgs.head}.${method.raw}$argS"
      } else {
        s"${scalaArgs.head}.${method.raw}"
      }
      packInScalaValue(scalaInterpreter.interpClosure(s"($closureParams => $closureBody)", argVals:_*))
    case BaseApplyUnaryExpr(op, exp) =>
      val value = eval(exp).asScala
      val valueTy = typeToScala(exp.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $exp")))
      val code = s"((value: $valueTy) => ${op.syntax} value)"
      packInScalaValue(scalaInterpreter.interpClosure(code, value))
  }

  // only pack 'pure' scala values aka. no values inside a ScalaValue
  private def packInScalaValue(value: Any) = value match {
    case value: Value => value
    case _ => ScalaValue(value)
  }

  private def typeToScala(typ: Type): meta.Type = {
    // TODO: We might want to make this more precise and refactor it
    //  This is basically the asScala method of a Type but for our Interpreter and not for PSystem
    typ match {
      case TScala(_) => typ.asScala
      case TClass(_) | TNull | TAny => t"Any"
      case TTuple(ts) => meta.Type.Tuple(ts.map(typeToScala).toList)
      case TSet(_) => t"Set[Any]"
    }
  }
}
