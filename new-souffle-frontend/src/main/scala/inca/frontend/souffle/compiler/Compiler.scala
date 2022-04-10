package inca.frontend.souffle.compiler

import inca.backend.ir.Datalog
import inca.frontend.constraint.compiler.ConstraintOptions
import inca.frontend.souffle.Syntax._
import inca.frontend.souffle.{FreeVars, PrettyPrinter, Syntax}
import inca.runtime.aggregate.Aggregation
import inca.util.{Gensym, Scala}
import inca.runtime.context.DataModel

import scala.collection.mutable.{Map => MutableMap}
import scala.meta.{XtensionQuasiquoteTerm, XtensionQuasiquoteInit, XtensionQuasiquoteType, Term => ScalaTerm}

class Compiler {

  val gensym: Gensym = new Gensym(Seq())

  val relationDecls: MutableMap[QualifiedName, RelationDecl] = MutableMap.empty
  val patterns: MutableMap[QualifiedName, Datalog.Pattern] = MutableMap.empty

  var inputs: Seq[QualifiedName] = Seq.empty
  var outputs: Seq[QualifiedName] = Seq.empty
  var printSizes: Seq[QualifiedName] = Seq.empty
  var limitSizes: Map[QualifiedName, Int] = Map.empty

  // bound computed argument
  var boundComputedArguments: Map[Datalog.Var, Datalog.Computed] = Map.empty
  var boundArgumentList: Map[Datalog.Var, Seq[Datalog.Term]] = Map.empty
  var boundFunctorCall: Map[Datalog.Var, Datalog.Computed] = Map.empty

  case class IntrinsicFunctor(name: String, argTypes: Seq[TypeName], returnType: TypeName)

  val intrinsicFunctors: Map[String, IntrinsicFunctor] = Map(
    "ord" -> IntrinsicFunctor("ord", Seq(SymbolType), UnsignedType),
    "to_float" -> IntrinsicFunctor("to_float", Seq(SymbolType), FloatType),
    "to_number" -> IntrinsicFunctor("to_number", Seq(SymbolType), NumberType),
    "to_string" -> IntrinsicFunctor("to_string", Seq(NumberType), SymbolType),
    "to_unsigned" -> IntrinsicFunctor("to_unsigned", Seq(SymbolType), UnsignedType),
    "cat" -> IntrinsicFunctor("cat", Seq(SymbolType, SymbolType), SymbolType),
    "strlen" -> IntrinsicFunctor("strlen", Seq(SymbolType), NumberType),
    "substr" -> IntrinsicFunctor("substr", Seq(SymbolType, UnsignedType, UnsignedType), SymbolType),
  )

  // <subtype> -> <direct supertypes>
  val subTypes: MutableMap[TypeName, Set[TypeName]] = MutableMap(
    UnsignedType -> Set(NumberType, AnyType),
    NumberType -> Set(FloatType, AnyType),
    FloatType -> Set(AnyType),
    SymbolType -> Set(AnyType)
  )

  // .type <ident> = <ident-1> | <ident-2> | ... | <ident-k>
  val unionTypes: MutableMap[TypeName, Set[TypeName]] = MutableMap()

  // .type <new-record> = [ <name_1>: <type_1>, ..., <name_k>: <type_k> ]
  val recordTypes: MutableMap[TypeName, Seq[Attribute]] = MutableMap()

  // .type <new-adt> = <branch-id> { <name_1>: <type_1>, ..., <name_k>: <type_k> } | ...
  val algebraicDataTypes: MutableMap[TypeName, Seq[ADTBranch]] = MutableMap()

  def isSubtype(ty1: TypeName, ty2: TypeName): Boolean = {
    ty1 == ty2 || ty2 == AnyType || {
      val supers = subTypes.getOrElse(ty1, return false)
      if (supers contains ty2)
        true
      else
        supers.foldLeft(false)((acc, ty) => acc || isSubtype(ty, ty2))
    }
  }

  def compileConstant(c: Constant): Datalog.Constant = c match {
    case ConstantString(value) => Datalog.Constant(Datalog.StringLiteral(value))
    case ConstantNumber(value) => Datalog.Constant(Datalog.IntLiteral(value))
    case ConstantUnsigned(value) => Datalog.Constant(Datalog.IntLiteral(value))
    case ConstantFloat(value) => Datalog.Constant(Datalog.DoubleLiteral(value.toDouble))
  }

  def compileFact(fact: Fact): Unit = {
    /*
    * A(0, 1) => A(x, y) :- x = 0, y = 1
    * A(x, 1) => throw error
    * */

    fact.atom.args.foreach(arg =>
      assert(arg.isInstanceOf[ArgumentConstant], "All arguments of facts have to be constants!"))

    val relationDecl = relationDecls.getOrElse(
      fact.atom.name,
      throw new Exception(s"Unknown relation '${fact.atom.name}' in fact!"))

    assert(
      relationDecl.attributes.length == fact.atom.args.length,
      s"Invalid number of arguments in fact '${PrettyPrinter.stringify(fact.atom.name)}'!\n" +
      s"Expected ${relationDecl.attributes.length} argument(s), got ${fact.atom.args.length}!")

    val body = Datalog.Body((relationDecl.attributes zip fact.atom.args).map {
      case (attr, arg) =>
        assert(
          isSubtype(arg.getType, attr.ty),
          s"Type '${PrettyPrinter.stringify(arg.getType)}' is not a subtype of type '${PrettyPrinter.stringify(attr.ty)}'!")

        assert(arg.isInstanceOf[ArgumentConstant], "Arguments of facts have to be constant!")
        val argConstant = arg.asInstanceOf[ArgumentConstant]

        Datalog.Eq(Datalog.Var(attr.name), compileConstant(argConstant.value))
    })

    // update pattern with new body
    val pattern = patterns(relationDecl.name)
    patterns += QualifiedName(pattern.name) -> Datalog.Pattern(
      pattern.vis,
      pattern.name,
      pattern.params,
      pattern.bodies :+ body
    )
  }

  def compileTypeName(ty: TypeName): Datalog.Type = ty match {
    case DeclaredType(name) => Datalog.TData(name)
    case AnyType => Datalog.TAny
    case NilType => throw new Exception("Cannot compile nil type to datalog type!")
    case primitiveType: PrimitiveType => primitiveType match {
      case SymbolType => Datalog.TLiteral.String
      case NumberType => Datalog.TLiteral.Int
      case UnsignedType => Datalog.TLiteral.Int
      case FloatType => Datalog.TLiteral.Double
    }
  }

  def compileAttribute(attribute: Attribute): Datalog.Param =
    Datalog.Param(attribute.name, compileTypeName(attribute.ty))

  def compileRelationDecl(decl: RelationDecl): Unit = {
    /*
    * extend environment `relationDecls`
    * extend environment `patterns`
    * */
    val name = QualifiedName(decl.name)
    relationDecls += name -> decl
    patterns += name -> Datalog.Pattern(None, name.toString, decl.attributes.map(compileAttribute), Seq.empty)
  }

  def compileRule(rule: EliminateRuleDisjunction.Rule): Unit = {
    /*
    * .decl A(x: number, y: number)
    * A(x, y) :- x + 1 = y
    * */

    val name = rule.head.name
    assert(relationDecls.isDefinedAt(name), s"Relation '$name' is not declared!")
    val relationDecl = relationDecls(name)

    assert(
      relationDecl.attributes.length == rule.head.args.length,
      s"Invalid number of arguments in fact '${PrettyPrinter.stringify(rule.head.name)}'!\n" +
      s"Expected ${relationDecl.attributes.length} argument(s), got ${rule.head.args.length}!")

    val constantConstraints: Seq[Datalog.Atom] = (relationDecl.attributes zip rule.head.args).collect {
      case (attr, arg: ArgumentConstant) =>
        Datalog.Eq(Datalog.Var(attr.name), compileConstant(arg.value))
    }

    val terms = rule.conjunction.terms.map(compileTerm)

    val body = Datalog.Body(constantConstraints ++ terms)

    // update pattern with new body
    val pattern = patterns(relationDecl.name)
    patterns += QualifiedName(pattern.name) -> Datalog.Pattern(
      pattern.vis,
      pattern.name,
      pattern.params,
      pattern.bodies :+ body
    )

//    val name = rule.head.name.toString
//    val params = rule.head.args.map(compileArgument)
//    Datalog.Pattern(None, name, params, bodies)
  }

  def compileArgument(argument: Argument): Datalog.Term = argument match {
    case ArgumentConstant(constant) => compileConstant(constant)
    case ArgumentVariable(name) => Datalog.Var(name)
    case Syntax.ArgumentNil => ???
    case ArgumentList(args) =>
      val bound = Datalog.Var(gensym.fresh("bound"))
      val argumentList: Seq[Datalog.Term] = args.map(compileArgument)

      boundArgumentList += bound -> argumentList
      bound

    case ArgumentDollarFunctor(name, args) => ???
    case ArgumentSingle(arg) => compileArgument(arg)
    case ArgumentAlias(arg, ty) => ???
    case ArgumentFunctorCall(name, arguments) =>
      intrinsicFunctors.get(name) match {
        case Some(value) =>
          assert(value.argTypes.length == arguments.length,
            s"invalid number of arguments, ${value.argTypes.length} required.")

          val scalaFunction = value.name match {
            case "ord" =>
              q"(x: String) => x.hashCode"
            case "to_float" =>
              q"(x: String) => x.toFloat"
            case "to_number" =>
              q"(x: String) => x.toInt"
            case "to_unsigned" =>
              q"(x: String) => x.toInt"
            case "to_string" =>
              q"(x: Int) => x.toString"
            case "cat" =>
              q"(x: String, y: String) => x + y"
            case "strlen" =>
              q"(x: String) => x.length"
            case "substr" =>
              q"(s: String, i: Int, n: Int) => s.substr(i, i + n)"
          }

          val returnType = value.returnType match {
            case DeclaredType(name) => ???
            case Syntax.AnyType => ???
            case Syntax.NilType => ???
            case primitiveType: PrimitiveType => primitiveType match {
              case Syntax.SymbolType => Datalog.TScalaString
              case Syntax.NumberType => Datalog.TScalaInt
              case Syntax.UnsignedType => Datalog.TScalaInt
              case Syntax.FloatType => Datalog.TScalaDouble
            }
          }

          val bound = Datalog.Var(gensym.fresh("bound"))

          boundFunctorCall +=
            bound -> Datalog.Computed(
              bound,
              Datalog.Evaluation(
                arguments.map(arg => (compileArgument(arg), compileType(arg.getType))),
                returnType,
                Scala[ScalaTerm.Function](scalaFunction)
              )
            )

          // return bound variable
          bound

        case None => ??? // TODO: Check if User-Defined Functor
      }
    case ArgumentAggregator(aggregator) => aggregator match {
      case AggregatorMin(argument, cond) => ???
      case AggregatorMax(argument, cond) => ???
      case AggregatorMean(argument, cond) => ???
      case AggregatorSum(argument, cond) => ???
      case AggregatorCount(cond) => ???
      case AggregatorRange(arg1, arg2, arg3) => ???
    }
    case ArgumentUnOp(op, argument) =>
      val ty = compileTypeName(argument.getType)

      def throwError =
        throw new Exception(s"Cannot compute unary operation '$op' of argument of type '${argument.getType}'!")

      val resultType = argument.getType match {

        case primitiveType: PrimitiveType => primitiveType match {
          case Syntax.NumberType => op match {
            case Syntax.UnOpMinus => Datalog.TScalaInt
            case Syntax.UnOpBNot => Datalog.TScalaBoolean
            case Syntax.UnOpLNot => Datalog.TScalaBoolean
          }
          case Syntax.UnsignedType => op match {
            case Syntax.UnOpMinus => Datalog.TScalaInt
            case Syntax.UnOpBNot => Datalog.TScalaBoolean
            case Syntax.UnOpLNot => Datalog.TScalaBoolean
          }
          case Syntax.FloatType => op match {
            case Syntax.UnOpMinus => Datalog.TScalaDouble
            case _ => throwError
          }
          case _ => throwError
        }
        case _ => throwError
      }

      val argType = argument.getType match {
        case DeclaredType(_) => ???
        case Syntax.AnyType => ???
        case Syntax.NilType => ???
        case primitiveType: PrimitiveType => primitiveType match {
          case Syntax.SymbolType => meta.Type.Name("String")
          case Syntax.NumberType => meta.Type.Name("Int")
          case Syntax.UnsignedType => meta.Type.Name("Int")
          case Syntax.FloatType => meta.Type.Name("Float")
        }
      }

      val scalaOp = op match {
        case Syntax.UnOpMinus =>
          meta.Term.ApplyUnary(meta.Term.Name("-"), meta.Term.Name("arg"))
        case Syntax.UnOpBNot =>
          meta.Term.ApplyUnary(meta.Term.Name("~"), meta.Term.Name("arg"))
        case Syntax.UnOpLNot =>
          meta.Term.ApplyUnary(meta.Term.Name("!"), meta.Term.Name("arg"))
      }

      // Souffle: A(x) :- B(-x).
      // Datalog: A(x) :- temp = Computed(...), Call(temp)

      val bound = Datalog.Var(gensym.fresh("bound"))

      boundComputedArguments += bound -> Datalog.Computed(
        bound,
        Datalog.Evaluation(
          Seq((compileArgument(argument), ty)),
          resultType,
          Scala[ScalaTerm.Function](q"(arg: $argType) => $scalaOp")
        )
      )

      // return bound variable
      bound

    case ArgumentBinOp(op, l, r) =>
      val lty = compileTypeName(l.getType)
      val rty = compileTypeName(r.getType)

      def throwError =
        throw new Exception(s"Cannot compute binary operation '$op' on arguments of types '${l.getType}' and '${r.getType}'!")

      assert(l.getType == r.getType, s"Arguments $l and $r have to be of same type")

      val resultType = l.getType match {
        case primitiveType: PrimitiveType => primitiveType match {
          case Syntax.NumberType | Syntax.UnsignedType => op match {
            case Syntax.BinOpAdd => Datalog.TScalaInt
            case Syntax.BinOpMinus => Datalog.TScalaInt
            case Syntax.BinOpMult => Datalog.TScalaInt
            case Syntax.BinOpDiv => Datalog.TScalaInt
            case Syntax.BinOpMod => Datalog.TScalaInt
            case Syntax.BinOpPow => Datalog.TScalaInt
            case Syntax.BinOpLAnd => Datalog.TScalaBoolean
            case Syntax.BinOpLOr => Datalog.TScalaBoolean
            case Syntax.BinOpLXor => Datalog.TScalaBoolean
            case Syntax.BinOpBAnd => Datalog.TScalaInt
            case Syntax.BinOpBOr => Datalog.TScalaInt
            case Syntax.BinOpBXor => Datalog.TScalaInt
            case Syntax.BinOpBShl => Datalog.TScalaInt
            case Syntax.BinOpBShr => Datalog.TScalaInt
            case Syntax.BinOpBShrU => Datalog.TScalaInt
          }
          case Syntax.FloatType => op match {
            case Syntax.BinOpAdd => Datalog.TScalaDouble
            case Syntax.BinOpMinus => Datalog.TScalaDouble
            case Syntax.BinOpMult => Datalog.TScalaDouble
            case Syntax.BinOpDiv => Datalog.TScalaDouble
            case Syntax.BinOpMod => Datalog.TScalaDouble
            case Syntax.BinOpPow => Datalog.TScalaDouble
            case Syntax.BinOpLAnd => Datalog.TScalaBoolean
            case Syntax.BinOpLOr => Datalog.TScalaBoolean
            case Syntax.BinOpLXor => Datalog.TScalaBoolean
            case Syntax.BinOpBAnd => Datalog.TScalaDouble
            case Syntax.BinOpBOr => Datalog.TScalaDouble
            case Syntax.BinOpBXor => Datalog.TScalaDouble
            case Syntax.BinOpBShl => Datalog.TScalaDouble
            case Syntax.BinOpBShr => Datalog.TScalaDouble
            case Syntax.BinOpBShrU => Datalog.TScalaDouble
          }
          case _ => throwError
        }
        case _ => throwError
      }

      val lType = l.getType match {
        case DeclaredType(name) => ???
        case Syntax.AnyType => ???
        case Syntax.NilType => ???
        case primitiveType: PrimitiveType => primitiveType match {
          case Syntax.SymbolType => meta.Type.Name("String")
          case Syntax.NumberType => meta.Type.Name("Int")
          case Syntax.UnsignedType => meta.Type.Name("Int")
          case Syntax.FloatType => meta.Type.Name("Float")
        }
      }
      val rType = r.getType match {
        case DeclaredType(name) => ???
        case Syntax.AnyType => ???
        case Syntax.NilType => ???
        case primitiveType: PrimitiveType => primitiveType match {
          case Syntax.SymbolType => meta.Type.Name("String")
          case Syntax.NumberType => meta.Type.Name("Int")
          case Syntax.UnsignedType => meta.Type.Name("Int")
          case Syntax.FloatType => meta.Type.Name("Float")
        }
      }

      // a + b ; scala.math.pow(a,b)

      val scalaOp = op match {
        case Syntax.BinOpAdd =>
          meta.Term.ApplyInfix(meta.Term.Name("l"), meta.Term.Name("+"), Nil, List(meta.Term.Name("r")))
        case Syntax.BinOpMinus =>
          meta.Term.ApplyInfix(meta.Term.Name("l"), meta.Term.Name("-"), Nil, List(meta.Term.Name("r")))
        case Syntax.BinOpMult =>
          meta.Term.ApplyInfix(meta.Term.Name("l"), meta.Term.Name("*"), Nil, List(meta.Term.Name("r")))
        case Syntax.BinOpDiv =>
          meta.Term.ApplyInfix(meta.Term.Name("l"), meta.Term.Name("/"), Nil, List(meta.Term.Name("r")))
        case Syntax.BinOpMod =>
          meta.Term.ApplyInfix(meta.Term.Name("l"), meta.Term.Name("%"), Nil, List(meta.Term.Name("r")))
        case Syntax.BinOpPow =>
          meta.Term.Apply(meta.Term.Name("scala.math.pow"), List(meta.Term.Name("l"), meta.Term.Name("r")))
        case Syntax.BinOpLAnd =>
          meta.Term.ApplyInfix(meta.Term.Name("l"), meta.Term.Name("&&"), Nil, List(meta.Term.Name("r")))
        case Syntax.BinOpLOr =>
          meta.Term.ApplyInfix(meta.Term.Name("l"), meta.Term.Name("||"), Nil, List(meta.Term.Name("r")))
        // a xor b = (a and !b) or (!a and b)
        case Syntax.BinOpLXor =>
          val notL = meta.Term.ApplyUnary(meta.Term.Name("!"), meta.Term.Name("l"))
          val notR = meta.Term.ApplyUnary(meta.Term.Name("!"), meta.Term.Name("r"))
          val firstAnd = meta.Term.ApplyInfix(meta.Term.Name("l"), meta.Term.Name("&&"), Nil, List(notR))
          val secondAnd = meta.Term.ApplyInfix(notL, meta.Term.Name("&&"), Nil, List(meta.Term.Name("r")))
          meta.Term.ApplyInfix(firstAnd, meta.Term.Name("||"), Nil, List(secondAnd))
        case Syntax.BinOpBAnd =>
          meta.Term.ApplyInfix(meta.Term.Name("l"), meta.Term.Name("&"), Nil, List(meta.Term.Name("r")))
        case Syntax.BinOpBOr =>
          meta.Term.ApplyInfix(meta.Term.Name("l"), meta.Term.Name("|"), Nil, List(meta.Term.Name("r")))
        case Syntax.BinOpBXor =>
          meta.Term.ApplyInfix(meta.Term.Name("l"), meta.Term.Name("^"), Nil, List(meta.Term.Name("r")))
        case Syntax.BinOpBShl =>
          meta.Term.ApplyInfix(meta.Term.Name("l"), meta.Term.Name("<<"), Nil, List(meta.Term.Name("r")))
        case Syntax.BinOpBShr =>
          meta.Term.ApplyInfix(meta.Term.Name("l"), meta.Term.Name(">>"), Nil, List(meta.Term.Name("r")))
        case Syntax.BinOpBShrU =>
          meta.Term.ApplyInfix(meta.Term.Name("l"), meta.Term.Name(">>>"), Nil, List(meta.Term.Name("r")))
      }

      val bound = Datalog.Var(gensym.fresh("bound"))

      boundComputedArguments += bound -> Datalog.Computed(
        bound,
        Datalog.Evaluation(
          Seq((compileArgument(l), lty), (compileArgument(r), rty)),
          resultType,
          Scala[ScalaTerm.Function](q"(l: $lType, r: $rType) => $scalaOp")
        )
      )

      bound
  }

  def compileType(ty: TypeName): Datalog.Type = ty match {
    case DeclaredType(name) => Datalog.TData(name)
    case AnyType => Datalog.TAny
    case NilType => ???
    case SymbolType => Datalog.TScalaString
    case NumberType => Datalog.TScalaInt
    case UnsignedType => Datalog.TScalaInt
    case FloatType => Datalog.TScalaDouble
  }

  def compileConstraint(constraint: Constraint, isNegated: Boolean = false): Datalog.Atom = {
    def compileCmp(l: Argument, r: Argument, op: String): Datalog.Atom = {
      assert(
        l.getType.isPrimitive && r.getType.isPrimitive,
        s"Both arguments for comparison have to be of primitive type! " +
        s"Got '${PrettyPrinter.stringify(l.getType)}' and ${PrettyPrinter.stringify(r.getType)}!")

      assert(
        l.getType == r.getType,
        s"Both arguments for comparison have to be of equal type! " +
        s"Got '${PrettyPrinter.stringify(l.getType)}' and ${PrettyPrinter.stringify(r.getType)}!")

      val cl = compileArgument(l)
      val cr = compileArgument(r)

      val scalaType = l.getType match {
        case primitiveType: PrimitiveType => primitiveType match {
          case SymbolType => "String"
          case NumberType => "Int"
          case UnsignedType => "Int"
          case FloatType => "Double"
        }
        case _ => throw new Exception("Wie zur Hölle bist du hier hin gekommen??")
      }

      val f = ScalaTerm.Function(
        List(
          ScalaTerm.Param(List.empty, meta.Name("l"), Some(meta.Type.Name(scalaType)), None),
          ScalaTerm.Param(List.empty, meta.Name("r"), Some(meta.Type.Name(scalaType)), None),
        ),
        q"l ${ScalaTerm.Name(op)} r"
      )

      // Souffle: A(x) :- x > 42.
      // Datalog: A(x) :- true = Computation(...).
      Datalog.Computed(
        if (isNegated) Datalog.False else Datalog.True,
        Datalog.Evaluation(
          Seq((cl, compileType(l.getType)), (cr, compileType(r.getType))),
          Datalog.TScalaBoolean,
          Scala[ScalaTerm.Function](f)
        )
      )
    }

    def compileStringConstraint(l: Argument, r: Argument, _op: String): Datalog.Atom = {
      assert(
        l.getType == SymbolType && r.getType == SymbolType,
        s"Arguments to string constraint have to be symbols! " +
        s"Got '${PrettyPrinter.stringify(l.getType)}' and '${PrettyPrinter.stringify(r.getType)}'!")

      val cl = compileArgument(l)
      val cr = compileArgument(r)

      val op = _op match {
        case "match" => "matches"
        case "contains" => "contains"
        case other => throw new Exception(s"Unknown string constraint '$other'!")
      }

      // Souffle: A(x) :- contains(x, "abc").
      // Datalog: A(x) :- true = Evaluation(x, "abc").
      Datalog.Computed(
        Datalog.True,
        Datalog.Evaluation(
          Seq((cl, compileType(l.getType)), (cr, compileType(r.getType))),
          Datalog.TScalaBoolean,
          if (isNegated)
            Scala[ScalaTerm.Function](q"(l: String, r: String) => !r.${ScalaTerm.Name(op)}(l)")
          else
            Scala[ScalaTerm.Function](q"(l: String, r: String) => r.${ScalaTerm.Name(op)}(l)")
        )
      )
    }

    constraint match {
      case ConstraintCmp(cmpOp, l, r) => cmpOp match {
        case ConstraintCmpOp.Lt => compileCmp(l, r, "<")
        case ConstraintCmpOp.Gt => compileCmp(l, r, ">")
        case ConstraintCmpOp.Leq => compileCmp(l, r, "<=")
        case ConstraintCmpOp.Geq => compileCmp(l, r, ">=")
        case ConstraintCmpOp.Neq => Datalog.Neq(compileArgument(l), compileArgument(r))

        case ConstraintCmpOp.Eq =>
          def compileAggregation(bound: ArgumentVariable, aggregator: ArgumentAggregator): Datalog.Atom = {




            Datalog.Computed(
              Datalog.Var(bound.name),
              aggregator.aggregator match {
                case AggregatorMin(argument, cond) => cond match {
                    case AggregatorConditionAtom(atom) =>
                      val ty: TypeName = {
                        if (argument.getType.isPrimitive) argument.getType
                        else {
                          // find attribute of relation that matches argument name
                          val attribute = relationDecls(atom.name).attributes.collectFirst {
                            case Attribute(name, ty) if name == atom.name.toString => ty
                          }

                          attribute match {
                            case Some(value) => value
                            case None => throw new Exception(s"Aggregator argument '$argument' not present in relation '${atom.name}'!")
                          }
                        }
                      }

                      val scalaType: meta.Type = ty match {
                        case DeclaredType(name) => ???
                        case Syntax.AnyType => ???
                        case Syntax.NilType => ???
                        case primitiveType: PrimitiveType => primitiveType match {
                          case Syntax.SymbolType => ???
                          case Syntax.NumberType => t"Int"
                          case Syntax.UnsignedType => t"Int"
                          case Syntax.FloatType => t"Float"
                        }
                      }

                      def genAggregation(name: String, init: meta.Term, op: meta.Term, typ: meta.Type): meta.Term = {
                        val tyAggregation = inca.util.Scala.typeOf[Aggregation[_]]
                        val initAggregation = init"${meta.Type.Apply(tyAggregation, List(typ))}()"

                        q"""
                         new $initAggregation {
                           override val name = $name
                           override def init: $typ = $init
                           override def join(v1: $typ, v2: $typ): $typ = $op(v1, v2)
                           override val isAssociative = true
                           override val isCommutative = true
                         }"""
                      }

                      val init: meta.Term = scalaType match {
                        case meta.Type.Name("Int") => q"Int.MinValue"
                        case meta.Type.Name("Float") => q"Float.MinValue"
                        case _ => ???
                      }

                      val agg: meta.Term = genAggregation(
                        "min",
                        init,
                        q"scala.math.min",
                        scalaType
                      )

                      // TODO ?
                      val column: Int = 0

                      Datalog.CustomAggregation(
                        compileTypeName(ty),
                        None,
                        Scala[meta.Term](agg),
                        atom.name.toString,
                        atom.args.map(compileArgument),
                        column
                      )

                    case AggregatorConditionDisjunction(disjunction) =>
                      // 1) discard all atoms that do not contain the argument:
                      // min x : { A(x), B(y) } ==> min x : A(x)

                      // 2) factor out disjunction into own rule:
                      // min x : { A(x), B(x) }
                      // ==>
                      // rule(x) :- A(x), B(x).
                      // min x : rule(x)

                      def collect[A](l: Seq[Option[A]]): Seq[A] =
                        l.filter(_.isDefined).map {
                          case Some(value) => value
                          case None => ???
                        }

                      def filterTerm(t: Term): Option[Term] = t match {
                        case TermConjunction(terms, isNegated) =>
                          Some(TermConjunction(collect(terms.map(filterTerm)), isNegated))
                        case TermDisjunction(terms, isNegated) =>
                          Some(TermDisjunction(collect(terms.map(filterTerm)), isNegated))
                        case t@TermAtom(atom, isNegated) =>
                          if (atom.args.contains(argument) || atom.args.exists(_.isInstanceOf[ArgumentConstant]))
                            Some(t)
                          else
                            None
                        case t@TermConstraint(constraint, isNegated) =>
                          val keep = constraint match {
                            case ConstraintCmp(_, l, r) => l == argument || r == argument
                            case ConstraintMatch(_, arg) => argument == arg
                            case ConstraintContains(_, arg) => argument == arg
                            case Syntax.ConstraintTrue => true
                            case Syntax.ConstraintFalse => true
                          }

                          if (keep) Some(t)
                          else None
                      }

                      val filtered: Seq[Term] = collect(disjunction.terms.map(filterTerm))

                      val ruleName: String = gensym.fresh("rule")
                      val freeVars = FreeVars.freeVars(disjunction)

                      compileRule(Rule(
                        Seq(Atom(ruleName, freeVars)),
                        disjunction
                      ))
                  }

                case AggregatorMax(argument, cond) => ???
                case AggregatorMean(argument, cond) => ???
                case AggregatorSum(argument, cond) => ???
                case AggregatorRange(arg1, arg2, arg3) => ???
                case AggregatorCount(cond) => cond match {
                  case AggregatorConditionDisjunction(disjunction) =>
                    // A(a, b) :- x = count : { B(a, 0), C(b), D(b) }
                    // ==>
                    // Temp(a, b) :- B(a, 0), C(b), D(b).
                    // A(a, b) :- x = count : Temp(a, b)

                    val freeVars = FreeVars.freeVars(disjunction)

                    // TODO
//                    compileRule(Rule(
//                      Seq(Atom(gensym.fresh("temp"),
//                          freeVars.map(ArgumentVariable.apply))),
//                          disjunction
//                    ))

                    // TODO
                  null

                  case AggregatorConditionAtom(atom) =>
                    Datalog.CountAggregation(atom.name.toString, atom.args.map(compileArgument))
                }
              }
            )
          }

          (l, r) match {
            case (l: ArgumentAggregator, r: ArgumentVariable) => compileAggregation(r, l)
            case (l: ArgumentVariable, r: ArgumentAggregator) => compileAggregation(l, r)
            case _ => Datalog.Eq(compileArgument(l), compileArgument(r))
          }
      }
      case ConstraintMatch(pattern, argument) =>
        // comparable to SQL 'like'
        // example: match("a.*", <someString>)
        compileStringConstraint(pattern, argument, "matches")
      case ConstraintContains(substring, argument) =>
        compileStringConstraint(substring, argument, "contains")
      case ConstraintTrue =>
        // 0 == 0
        Datalog.Eq(Datalog.Constant(Datalog.IntLiteral(0)), Datalog.Constant(Datalog.IntLiteral(0)))
      case ConstraintFalse =>
        // 0 == 1
        Datalog.Eq(Datalog.Constant(Datalog.IntLiteral(0)), Datalog.Constant(Datalog.IntLiteral(1)))
    }
  }

  def compileTerm(term: Syntax.Term): Datalog.Atom = term match {
    case TermAtom(atom, isNegated) =>
      if (inputs.contains(atom.name)) {
        // extensional call
        Datalog.ExtensionalCall(atom.name.toString, atom.args.map(compileArgument), neg = isNegated)
      }
      else {
        // intensional call
        Datalog.Call(atom.name.toString, atom.args.map(compileArgument), neg = isNegated)
      }
    case TermConstraint(constraint, isNegated) =>
      if (isNegated && constraint.canBeNegated)
        compileConstraint(constraint.negated)
      else compileConstraint(constraint, isNegated)
    case TermDisjunction(_, _) =>
      throw new Exception("There should be no disjunctions at this point.")
  }

  def compileRule(rule: Rule): Unit =
    EliminateRuleDisjunction
      .eliminateRuleDisjunction(rule)
      .foreach(compileRule)

  def compileComponentDecl(componentDecl: ComponentDecl): Unit = ???
  def compileComponentInit(componentInit: ComponentInit): Unit = ???

  def compileTypeDecl(typeDecl: TypeDecl): Unit = typeDecl match {
    case TypeDeclSubtype(name, superType) =>
      val subtype = DeclaredType(name)
      assert(superType.isPrimitive, "Supertype has to be PrimitiveType")
      if(subTypes.contains(subtype)) {
        subTypes+=subtype->(subTypes(subtype)++Set(superType))
      }
      else {
        subTypes+=subtype->(Set(superType, AnyType))
      }
    case TypeDeclUnion(name, types) =>
      val ty = DeclaredType(name)
      var primTy: TypeName = AnyType;
      def checkUnionType(ty: TypeName): Boolean = {
        if(ty.isPrimitive) {
          if(primTy == AnyType) primTy = ty
          else {
            if(primTy != ty) return false
          }
          true
        }
        else if(ty.isInstanceOf[DeclaredType]) {
          assert(unionTypes.contains(ty))
          val unionSet = unionTypes(ty)
          for(id<-unionSet)
            if(!checkUnionType(id)) return false
          true
        }
        else false
      }
      types.foreach(x => assert(checkUnionType(x), "Invalid unitType declaration"))
      unionTypes+=ty->(types.toSet)
    case TypeDeclRecord(name, records) =>
      assert(!recordTypes.contains(DeclaredType(name)))
      recordTypes+=DeclaredType(name)->records

    case TypeDeclADT(name, branches) =>
      assert(!algebraicDataTypes.contains(DeclaredType(name)))
      for(adt<-algebraicDataTypes.values)
        branches.foreach(x => adt.foreach(y => assert(y.branchId != x.branchId)))
      algebraicDataTypes+=DeclaredType(name)->branches
      for((br1, idx1)<-branches.zipWithIndex)
        for((br2, idx2)<-branches.zipWithIndex){
          if(idx1 != idx2) assert(br1.branchId != br2.branchId)
        }
  }

  def compileDirective(directive: Directive): Unit = directive.qualifier match {
    case Syntax.DirectiveQualifierInput =>
      assert(directive.qualifiedNames.length == 1, "Input directive must have one relation argument!")
      assert(directive.params.isEmpty, "Input directives must not have any parameters!")

      // extend list of inputs
      inputs :+= directive.qualifiedNames.head

    case Syntax.DirectiveQualifierPrintsize =>
      assert(directive.qualifiedNames.length == 1, "Printsize directive must have one relation argument!")
      assert(directive.params.isEmpty, "Printsize directives must not have any parameters!")

      // extend list of printSizes
      printSizes :+= directive.qualifiedNames.head

    case Syntax.DirectiveQualifierOutput =>
      assert(directive.qualifiedNames.length == 1, "Output directive must have one relation argument!")
      assert(directive.params.isEmpty, "Output directives must not have any parameters!")

      // extend list of outputs
      outputs :+= directive.qualifiedNames.head

    case Syntax.DirectiveQualifierLimitsize =>
      assert(directive.qualifiedNames.length == 1, "Limitsize directive must have one relation argument!")
      assert(directive.params.size == 1, "Limitsize directive must have exactly one parameter!")
      assert(directive.params.head._1 == "n", "Limitsize directive must have 'n' sa parameter!")
      assert(directive.params.head._2.isInstanceOf[DirectiveValueNumber], "Limitsize parameter value must be an integer!")

      val name = directive.qualifiedNames.head
      val n = directive.params.head._2.asInstanceOf[DirectiveValueNumber].value

      // extend list of limitSizes
      limitSizes += name -> n
  }

  // MODULE

  def compileStatement(statement: SouffleStatement): Unit = statement match {
    case e: TypeDecl => compileTypeDecl(e)
    case e@RelationDecl(_,_,_,_) => compileRelationDecl(e)
    case e@Rule(_,_,_) => compileRule(e)
    case e@Fact(_) => compileFact(e)
    case e@ComponentDecl(_,_,_) => compileComponentDecl(e)
    case e@ComponentInit(_,_) => compileComponentInit(e)
    case e@Directive(_,_,_) => compileDirective(e)
    case FunctorDecl(_,_,_,_) => throw new Exception("Functor declarations are not supported for compilation!")
    case Pragma(_,_) => throw new Exception("Pragmas are not supported for compilation!")
  }

  def compileProgram(program: SouffleProgram, name: String = "module"): CompiledSouffleModule = {
    // compile program
    program.foreach(compileStatement)

    // create module
    val scalaContent = Seq.empty
    val module = Datalog.Module(name, Seq.empty, patterns.values.toSeq, scalaContent)

    CompiledSouffleModule(
      module,
      inputs.map(relationDecls.apply),
      printSizes.map(relationDecls.apply),
      new DataModel(),
      ConstraintOptions()
    )
  }
}
