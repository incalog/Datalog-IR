package inca.frontend.objectoriented.interpreter

import inca.frontend.objectoriented.core._

import scala.annotation.tailrec
import scala.meta.{XtensionQuasiquoteTermParam, XtensionQuasiquoteType}

final case class TypeCastException(obj: Value, typ: String) extends RuntimeException(s"Could not cast $obj to type $typ!")

class Interpreter(module: Module) {
  // TODO: Support fixpoint
  // TODO: Support mono types

  private val scalaInterpreter = new ScalaInterpreter {}

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

  def interp(main: MethodDef, args: Seq[Value]): Value = newScope {
    bindParams(main.params, args)
    interp(main.body).getOrElse(Value.UNIT)
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
      val obj = interp(recv)
      obj.asObject match {
        case _ => obj.updateObject(name.raw, interp(expression))
      }
      None
    case VarDeclareStmt(name, _, maybeExpression, immutable) =>
      if (maybeExpression.isEmpty && immutable)
        throw new IllegalArgumentException(s"Can not bind immutable local variable $name without a value!")
      else if (maybeExpression.isEmpty)
        env += name -> Value.NULL
      else
        env += name -> interp(maybeExpression.get)
      None
    case VarAssignStmt(name, expression) =>
      env += name -> interp(expression)
      None
    case VarPhiAssignStmt(_, _, _, _, _) =>
      throw new IllegalStateException("Found unexpected phi node during interpretation!")
    case IfStmt(cnd, thn, els) =>
      if (interp(cnd).asBoolean)
        interp(thn)
      else
        interp(els)
  }

  def interp(expr: Expression): Value = expr match {
    case VarReadExpr(name) => env.get(name) match {
        case Some(v) => v
        case None => throw new IllegalArgumentException(s"Can not read undeclared variable $name")
      }
    case FieldReadExpr(recv, targetName) =>
      val (cls, _, fields) = interp(recv).asObject
      fields.get(targetName.raw) match {
        case Some(value) => value
        case None => throw new IllegalStateException(s"Field not found $targetName for instance of class $cls")
      }
    case constr@ConstructorExpr(classRef, args) =>
      // we can lookup the constructor directly without using the dispatch table
      constr.target match {
        case Some(ConstructorDef(_, _, params, body)) =>
          val argVals = args.map(interp)

          newScope {
            val className = classRef.name
            val fields = classTable.transitiveCollectFields(className)
            val fieldVals = fields.map { f =>
              f.name.raw -> (if (f.body.isDefined) interp(f.body.get) else Value.NULL)
            }.toMap

            val id = classTable.lookup(className) match {
              case Some(classDef) if classDef.isCaseClass => 0
              case Some(_) => freshId()
              case None => throw new IllegalArgumentException(s"Unresolved class $className")
            }
            val obj = ObjectValue(className.raw, id, fieldVals)

            bindParams(params, argVals)
            env += Name("this") -> obj

            interp(body)
            obj
          }
        case _ => throw new IllegalArgumentException(s"No matching constructor found for ${classRef.name}")
      }
    case superExpr@SuperExpr(args) =>
      superExpr.target match {
        case Some((_, ConstructorDef(_, _, params, body))) =>
          val argVals = args.map(interp)

          val thisObj = env.get(Name("this")) match {
            case Some(value) => value
            case None => throw new IllegalStateException("'this' is not bound in super expr")
          }

          newScope {
            bindParams(params, argVals)
            env += Name("this") -> thisObj
            interp(body)
            thisObj
          }
        case None =>
          throw new IllegalArgumentException(s"Unresolved constructor $superExpr")
      }
    case MethodCallExpr(recv, fun, args, isFix) =>
      // TODO: Support isFix
      val recvObj = interp(recv)
      val (className, _, _) = recvObj.asObject
      dispatchTable.lookup(Name(className), fun) match {
        case Some(MethodDef(_, _, _, params, _, body)) =>
          val argVals = args.map(interp)
          newScope {
            bindParams(params, argVals)
            env += Name("this") -> recvObj
            // well-typed programs always return a value

            interp(body) match {
              case Some(value) => value
              case None => throw new IllegalArgumentException(s"Missing return value for method $fun")
            }

            // TODO: Put this in a function
            /*push(???)
            if (continue ...)
            try {
              interp(body) match {
                case Some(value) => value
                case None => throw new IllegalArgumentException(s"Missing return value for method $fun")
              }
            } finally {
              pop(???)
              if (unstable)
                interp(body) again // TODO: Call the function here
            }*/


          }
        case None => throw new IllegalArgumentException(s"No matching method found with name $fun")
      }
    case TypeCastExpr(recv, TClass(ClassRef(ofName))) =>
      interp(recv) match {
        case Value.NULL => Value.NULL
        case obj => obj.asObject match {
          case (cls, _, _) if classTable.isSubclassOf(Name(cls), ofName) => obj
          case _ => throw TypeCastException(obj, ofName.raw)
        }
      }
    case TypeCastExpr(_, ofTyp) =>
      throw new UnsupportedOperationException(s"asInstanceOf is only supported for class types, but got $ofTyp")
    case InstanceOfExpr(recv, TClass(ClassRef(ofName))) =>
      interp(recv) match {
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
      interp(recv).asTuple match {
        case values if ix > 0 && ix <= values.size => values(ix-1)
        case _ => throw new IllegalArgumentException(s"Index $ix ouf of bounds for tuple $recv")
      }
    case TupleExpr(exps) =>
      val argVals = exps.map(interp)
      TupleValue(argVals)

    case SetExpr(exps, _) =>
      SetValue(exps.map(interp).toSet)

    case SetComprehension(Seq(), _) =>
      SetValue(Set())
    case SetComprehension(Seq(SetMemberExpr(name, set, None)), body) =>
      val vals = interp(set).asSet
      // Note: Do not open a new scope, since our Datalog compiler does not support scoping here
      val computedVals = for (v <- vals) yield {
        env += name -> v
        // flatten potential nested set results
        interp(body) match {
          case SetValue(values) => values
          case v => Seq(v)
        }
      }.toSeq
      SetValue(computedVals.flatten)
    case SetComprehension(Seq(SetMemberExpr(name, set, Some(pred))), body) =>
      val vals = interp(set).asSet
      def filter(v: Value): Boolean = {
        env += name -> v
        interp(pred).asBoolean
      }

      val computedVals = for (v <- vals if filter(v)) yield {
        env += name -> v
        interp(body) match {
          case SetValue(values) => values
          case v => Seq(v)
        }
      }.toSeq
      SetValue(computedVals.flatten)
    case SetComprehension(mem::rest, body) =>
      interp(SetComprehension(Seq(mem), SetComprehension(rest, body)))

    case SetFold(recv, projection, opClass, opMethod, neutral) =>
      // TODO: projection
      val classDef = classTable.lookup(opClass.name).getOrElse(throw new IllegalStateException(s"Class not found $opClass"))
      val methods = classDef.methods.filter(_.name == opMethod)
      if (methods.size > 1)
        throw new IllegalStateException(s"Ambiguous method $opMethod for class $opClass in fold $expr")
      else if (methods.size < 1)
        throw new IllegalStateException(s"No method $opMethod found for class $opClass in fold $expr")

      val foldMethod = methods.head
      val setVals = interp(recv).asSet
      setVals.fold(interp(neutral)) { case (acc, current) =>
        newScope {
          bindParams(foldMethod.params, Seq(acc, current))
          interp(foldMethod.body) match {
            case Some(value) => value
            case None => throw new IllegalStateException("Can not fold over Unit values!")
          }
        }
      }
    case BaseLitExpr(code) =>
      ScalaValue(scalaInterpreter.interp(code.syntax))
    case BaseApplyExpr(fun, args) =>
      val argVals = args.map(e => interp(e).asScala)
      val paramsTyped = args.zipWithIndex.map { case (arg, ix) =>
        val argTyp = arg.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $arg"))
        ("arg$" + ix, s"${typeToScala(argTyp)}")
      }.toList
      val scalaArgs = paramsTyped.map(_._1)
      val code = s"((${paramsTyped.map(p => s"${p._1}: ${p._2}").mkString(", ")}) => $fun(${scalaArgs.mkString(", ")}))"
      ScalaValue(scalaInterpreter.interpClosure(code, argVals:_*))
    case BaseApplyInfixExpr(left, op, right)
      if op.tree.value == "++" && left.typ.exists(_.isInstanceOf[TSet]) && right.typ.exists(_.isInstanceOf[TSet]) =>
      (interp(left), interp(right)) match {
        case (SetValue(v1), SetValue(v2)) => SetValue(v1 ++ v2)
        case _ => throw new IllegalStateException("Union on unsupported values!")
      }
    case BaseApplyInfixExpr(left, op, right) =>
      val lhs = interp(left).asScala
      val rhs = interp(right).asScala
      val lhsTy = typeToScala(left.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $left")))
      val rhsTy = typeToScala(right.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $right")))
      val code = s"((left: $lhsTy, right: $rhsTy) => left $op right)"
      ScalaValue(scalaInterpreter.interpClosure(code, lhs, rhs))
    case BaseApplyMethodExpr(recv, method, args) =>
      val argVals = (recv +: args.getOrElse(Seq())).map(e => interp(e).asScala)
      val paramsTyped = (recv +: args.getOrElse(Seq())).zipWithIndex.map { case (arg, ix) =>
        val argTyp = arg.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $arg"))
        ("arg$" + ix, s"${typeToScala(argTyp)}")
      }.toList
      val scalaArgs = paramsTyped.map(_._1)
      val closureParams = s"(${paramsTyped.map(p => s"${p._1}: ${p._2}").mkString(", ")})"
      val closureBody = s"${scalaArgs.head}.${method.raw}(${scalaArgs.tail.mkString(", ")}"
      val code = s"($closureParams => $closureBody)"
      ScalaValue(scalaInterpreter.interpClosure(code, argVals:_*))
    case BaseApplyUnaryExpr(op, exp) =>
      val value = interp(exp).asScala
      val valueTy = typeToScala(exp.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $exp")))
      val code = s"((value: $valueTy) => ${op.syntax} value)"
      ScalaValue(scalaInterpreter.interpClosure(code, value))
  }

  private def typeToScala(typ: Type): meta.Type = {
    // TODO: We want to make this more precise and refactor it
    //  This is basically the asScala method of a Type but for our Interpreter and not for PSystem
    typ match {
      case TScala(_) => typ.asScala
      case TClass(_) | TNull | TAny => t"Any"
      case TTuple(_) => t"Seq[Any]"
      case TSet(_) => t"Set[Any]"
    }
  }
}
