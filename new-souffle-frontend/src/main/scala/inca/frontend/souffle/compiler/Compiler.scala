package inca.frontend.souffle.compiler

import inca.backend.ir.Datalog
import inca.frontend.constraint.compiler.ConstraintOptions
import inca.frontend.souffle.Syntax._
import inca.frontend.souffle.{PrettyPrinter, Syntax}
import inca.util.Scala
import inca.runtime.context.DataModel

import scala.collection.mutable.{Map => MutableMap}
import scala.meta.{Term, XtensionQuasiquoteTerm}

class Compiler {

  val relationDecls: MutableMap[QualifiedName, RelationDecl] = MutableMap.empty
  val patterns: MutableMap[QualifiedName, Datalog.Pattern] = MutableMap.empty

  var inputs: Seq[QualifiedName] = Seq.empty
  var printSizes: Seq[QualifiedName] = Seq.empty

  // <subtype> -> <direct supertypes>
  val subTypes: MutableMap[TypeName, Set[TypeName]] = MutableMap(
    UnsignedType -> Set(NumberType, AnyType),
    NumberType -> Set(FloatType, AnyType),
    FloatType -> Set(AnyType),
    SymbolType -> Set(AnyType)
  )

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
    case ArgumentList(args) => ???
    case ArgumentDollarFunctor(name, args) => ???
    case ArgumentSingle(arg) => ???
    case ArgumentAlias(arg, ty) => ???
    case ArgumentFunctorCall(name, arguments) => ???
    case ArgumentAggregator(aggregator) => ???
    case ArgumentUnOp(op, argument) => ???
      //Datalog.Computed(compileArgument(argument), Datalog.Evaluation())
    case ArgumentBinOp(op, l, r) => ???
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

  def compileConstraint(constraint: Constraint): Datalog.Atom = {
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

      val f = Term.Function(
        List(
          Term.Param(List.empty, meta.Name("l"), Some(meta.Type.Name(scalaType)), None),
          Term.Param(List.empty, meta.Name("r"), Some(meta.Type.Name(scalaType)), None),
        ),
        q"l ${Term.Name(op)} r"
      )

      Datalog.Computed(
        cl,
        Datalog.Evaluation(
          Seq((cr, compileType(r.getType))),
          Datalog.TScalaBoolean,
          Scala[Term.Function](f)
        )
      )
    }

    def compileStringConstraint(l: Argument, r: Argument, op: String): Datalog.Atom = {
      assert(
        l.getType == SymbolType && r.getType == SymbolType,
        s"Arguments to match have to be symbols! Got '${PrettyPrinter.stringify(l.getType)}' and '${PrettyPrinter.stringify(r.getType)}'!")

      val cl = compileArgument(l)
      val cr = compileArgument(r)

      Datalog.Computed(
        cl,
        Datalog.Evaluation(
          Seq((cr, compileType(r.getType))),
          Datalog.TScalaBoolean,
          Scala[Term.Function](q"(l: String, r: String) => r.${Term.Name(op)}(l)")
        )
      )
    }

    constraint match {
      case ConstraintCmp(ty, l, r) => ty match {
        case ConstraintCmpOp.Lt => compileCmp(l, r, "<")
        case ConstraintCmpOp.Gt => compileCmp(l, r, ">")
        case ConstraintCmpOp.Leq => compileCmp(l, r, "<=")
        case ConstraintCmpOp.Geq => compileCmp(l, r, ">=")
        case ConstraintCmpOp.Eq => Datalog.Eq(compileArgument(l), compileArgument(r))
        case ConstraintCmpOp.Neq => Datalog.Neq(compileArgument(l), compileArgument(r))
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

  def compileTerm(term: ConjunctionTerm): Datalog.Atom = term match {
    case ConjunctionTermAtom(isNegated, atom) => ???
    case term@ConjunctionTermConstraint(isNegated, constraint) =>
      if (isNegated)
        compileConstraint(term.applyDeMorgan().constraint)
      else
        compileConstraint(constraint)
    case ConjunctionTermDisjunction(isNegated, disjunction) => ???
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
    case TypeDeclUnion(name, types) => ???
    case TypeDeclRecord(name, records) => ???
    case TypeDeclADT(name, branches) => ???
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

    case Syntax.DirectiveQualifierOutput => ???
    case Syntax.DirectiveQualifierLimitsize => ???
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
