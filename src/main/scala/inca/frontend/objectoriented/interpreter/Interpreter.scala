package inca.frontend.objectoriented.interpreter

import inca.frontend.objectoriented.core._
import inca.util.TupleOps

import scala.annotation.tailrec
import scala.meta.{XtensionQuasiquoteTermParam, XtensionQuasiquoteType}

final case class TypeCastException(obj: Value, typ: String) extends RuntimeException(s"Could not cast $obj to type $typ!")

class Interpreter(module: Module) {
  // TODO: Support fixpoint
  // TODO: Support mono types

  private val scalaInterpreter = new ScalaInterpreter {}

  private type Environment = Map[Name, Value]

  var env: Environment = Map()

  private def newCallframe[A](f: => A): A = {
    val oldenv = this.env
    this.env = Map()
    try {
      val v = f
      v
    } finally {
      this.env = oldenv
    }
  }

  var store: Store = new SimpleStore()

  private val classTable = ClassTable(module.classes)
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
    case Address.nullPtr => value
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

  def interp(main: MethodDef, args: Seq[Value]): Value = newCallframe {
    bindParams(main.params, args)
    interp(main.body).getOrElse(Value.unit)
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
        case Some(ConstructorDef(_, _, params, body)) =>
          val argVals = args.map(interp)

          newCallframe {
            val className = classRef.name
            val fields = classTable.transitiveCollectFields(className)
            val fieldVals = fields.map { f =>
              f.name.raw -> (if (f.body.isDefined) interp(f.body.get) else Address.nullPtr)
            }.toMap

            val obj = classTable.lookup(className) match {
              case Some(classDef) if classDef.isCaseClass => StructuralObjectValue(className.raw, fieldVals)
              case Some(_) => ObjectValue(className.raw, freshId(), fieldVals)
              case None => throw new IllegalArgumentException(s"Unresolved class $className")
            }
            bindParams(params, argVals)

            val index = store.malloc()
            store.update(index, obj)
            val addr = Address(index)
            env += Name("this") -> addr

            interp(body)
            addr
          }
        case _ => throw new IllegalArgumentException(s"No matching constructor found for ${classRef.name}")
      }
    case superExpr@SuperExpr(args) =>
      superExpr.target match {
        case Some((_, ConstructorDef(_, _, params, body))) =>
          val argVals = args.map(interp)

          val thisAddr = env.get(Name("this")) match {
            case Some(value) => value
            case None => throw new IllegalStateException("'this' is not bound in super expr")
          }

          newCallframe {
            bindParams(params, argVals)
            env += Name("this") -> thisAddr
            interp(body)
            thisAddr
          }
        case None =>
          throw new IllegalArgumentException(s"Unresolved constructor $superExpr")
      }
    case MethodCallExpr(recv, fun, args, isFix) =>
      // TODO: Support isFix
      val recvAddr = interp(recv)
      val className = resolve(recvAddr).asObject match {
        case Some((cls, _, _)) => Name(cls)
        case None => throw new IllegalArgumentException(s"Expected address but found $recv")
      }
      dispatchTable.lookup(className, fun) match {
        case Some(MethodDef(_, _, _, params, _, body)) =>
          val argVals = args.map(interp)
          newCallframe {
            bindParams(params, argVals)
            env += Name("this") -> recvAddr
            // well-typed programs always return a value
            interp(body) match {
              case Some(value) => value
              case None => throw new IllegalArgumentException(s"Missing return value for method $fun")
            }
          }
        case None => throw new IllegalArgumentException(s"No matching method found with name $fun")
      }
    case TypeCastExpr(recv, TClass(ClassRef(ofName))) =>
      interp(recv) match {
        case addr@Address.nullPtr => addr
        case addr =>
          val obj = resolve(addr)
          obj.asObject match {
            case Some((cls, _, _)) if classTable.isSubclassOf(Name(cls), ofName) => addr
            case Some(_) => throw TypeCastException(obj, ofName.raw)
            case None => throw new IllegalArgumentException(s"Can not call asInstanceOf on none object expression $recv")
          }
      }
    case TypeCastExpr(_, ofTyp) =>
      throw new UnsupportedOperationException(s"asInstanceOf is only supported for class types, but got $ofTyp")
    case InstanceOfExpr(recv, TClass(ClassRef(ofName))) =>
      interp(recv) match {
        case Address.nullPtr => ScalaValue(true)
        case addr => resolve(addr).asObject match {
          case Some((cls, _, _)) => ScalaValue(classTable.isSubclassOf(Name(cls), ofName))
          case None => throw new IllegalArgumentException(s"Can not call isInstanceOf on none object expression $recv")
        }
      }
    case InstanceOfExpr(_, ofTyp) =>
      throw new UnsupportedOperationException(s"isInstanceOf is only supported for class types, but got $ofTyp")
    case NullExpr() =>
      Address.nullPtr
    case TupleReadExpr(recv, Index(ix)) =>
      resolve(interp(recv)) match {
        case TupleValue(values) if ix > 0 && ix <= values.size => values(ix-1)
        case TupleValue(_) => throw new IllegalArgumentException(s"Index $ix ouf of bounds for tuple $recv")
        case other => throw new IllegalStateException(s"Expected tuple but got $other")
      }
    case TupleExpr(exps) =>
      val argVals = exps.map(interp)
      TupleValue(argVals)

    case SetExpr(exps, _) =>
      SetValue(exps.map(interp))
    case SetMemberExpr(name, recv, predicate) =>
      val values = resolve(interp(recv)) match {
        case SetValue(vals) => vals
        case v => throw new IllegalStateException(s"Set expected, but $v found")
      }
      val bindings = values.flatMap { v =>
        env += name -> v

        val cond = if (predicate.isDefined) interp(predicate.get) else ScalaValue(true)
        resolve(cond).asBoolean match {
          case Some(true) => Some((name, v))
          case Some(false) => None
          case _ => throw new IllegalStateException(s"Boolean condition expected for set member $expr")
        }
      }
      ScalaValue(bindings)
    case SetComprehension(member, body) =>
      def processMember(memExprs: Seq[Expression]): Seq[Value] = {
        memExprs match {
          case Nil => Seq(interp(body))
          case head::tail =>
            interp(head) match {
              case ScalaValue(bindings: Seq[(Name, Value)]) =>
                bindings.flatMap { case (name, value) =>
                  // Important: bind the first variable, before we interp the next member
                  env += name -> value
                  processMember(tail)
                }
              case _ => throw new IllegalStateException(s"Unexpected interpretation of SetMember $head")
            }
        }
      }

      SetValue(processMember(member))
    case SetFold(recv, projection, opClass, opMethod, neutral) =>
      // TODO: projection
      val classDef = classTable.lookup(opClass.name).getOrElse(throw new IllegalStateException(s"Class not found $opClass"))
      val methods = classDef.methods.filter(_.name == opMethod)
      if (methods.size > 1)
        throw new IllegalStateException(s"Ambiguous method $opMethod for class $opClass in fold $expr")
      else if (methods.size < 1)
        throw new IllegalStateException(s"No method $opMethod found for class $opClass in fold $expr")

      val foldMethod = methods.head
      val neutralVal = interp(neutral)
      val recvVal = resolve(interp(recv)) match {
        case s: SetValue => s
        case v => throw new IllegalStateException(s"Set expected, but $v found")
      }
      recvVal.values.fold(neutralVal) { case (acc, current) =>
        newCallframe {
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
      val argVals = args.map(e => resolve(interp(e)).asScala)
      val paramsTyped = args.zipWithIndex.map { case (arg, ix) =>
        val argTyp = arg.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $arg"))
        ("arg$" + ix, s"${typeToScala(argTyp)}")
      }.toList
      val scalaArgs = paramsTyped.map(_._1)
      val code = s"((${paramsTyped.map(p => s"${p._1}: ${p._2}").mkString(", ")}) => $fun(${scalaArgs.mkString(", ")}))"
      ScalaValue(scalaInterpreter.interpClosure(code, argVals:_*))
    case BaseApplyInfixExpr(left, op, right)
      if op.tree.value == "++" && left.typ.exists(_.isInstanceOf[TSet]) && right.typ.exists(_.isInstanceOf[TSet]) =>
      (resolve(interp(left)), resolve(interp(right))) match {
        case (SetValue(v1), SetValue(v2)) => SetValue((v1.toSet ++ v2.toSet).toSeq)
        case _ => throw new IllegalStateException("Union on unsupported values!")
      }
    case BaseApplyInfixExpr(left, op, right) =>
      val lhs = resolve(interp(left)).asScala
      val rhs = resolve(interp(right)).asScala
      val lhsTy = typeToScala(left.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $left")))
      val rhsTy = typeToScala(right.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $right")))
      val code = s"((left: $lhsTy, right: $rhsTy) => left $op right)"
      ScalaValue(scalaInterpreter.interpClosure(code, lhs, rhs))
    case BaseApplyMethodExpr(recv, method, args) =>
      val argVals = (recv +: args.getOrElse(Seq())).map(e => resolve(interp(e)).asScala)
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
      val value = resolve(interp(exp)).asScala
      val valueTy = typeToScala(exp.typ.getOrElse(throw new IllegalStateException(s"Untyped expression $exp")))
      val code = s"((value: $valueTy) => ${op.syntax} value)"
      ScalaValue(scalaInterpreter.interpClosure(code, value))
  }

  private def typeToScala(typ: Type): meta.Type = {
    // TODO: We might want to make this more precise and refactor it
    //  This is basically the asScala method of a Type but for our Interpreter and not for PSystem
    typ match {
      case TScala(_) => typ.asScala
      case TClass(_) | TNull | TAny => t"Any"
      case TTuple(_) => t"Seq[Any]"
      case TSet(_) => t"Set[Any]"
    }
  }
}
