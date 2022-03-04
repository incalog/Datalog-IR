package inca.frontend.souffle

import Syntax._
import com.sun.tools.javac.code.Type
import inca.backend.ir.{Datalog, DatalogPrinter}

import javax.lang.model.`type`.PrimitiveType

object PrettyPrinter {
  def stringifyDatalog(e: Any)(implicit verbose: Boolean = true): String = e match {
    case e: Datalog.Module => DatalogPrinter.prettyModule(e)
    case e: Datalog.Pattern => DatalogPrinter.prettyPattern(e)
    case e: Datalog.Visibility => DatalogPrinter.prettyVis(Some(e))
    case e: Option[Datalog.Visibility] => DatalogPrinter.prettyVis(e)
    case e: Datalog.Param => DatalogPrinter.prettyParam(e)
    case e: Datalog.Type => DatalogPrinter.prettyType(e)
    case e: Datalog.Body => DatalogPrinter.prettyBody(e)
    case e: Datalog.Atom => DatalogPrinter.prettyAtom(e)
    case e: Datalog.Link => DatalogPrinter.prettyLink(e)
    case e: Datalog.Term => DatalogPrinter.prettyTerm(e)
    case e: Datalog.Comparator => DatalogPrinter.prettyComparator(e)
    case _ => e.toString
  }

  def printDatalog(e: Any)(implicit verbose: Boolean = true): Unit =
    println(stringifyDatalog(e)(verbose))

  def stringify(e: Any): String = e match {
    case l: Iterable[Any] => l.map(stringify).mkString("\n")
    case e: SouffleProgram => e.map(stringify).mkString("\n")

    case e: Expression => e match {
      case Variable(name) => name
      case StringValue(value) => "\"" + value + "\""
      case NumberValue(value) => value.toString
      case FloatValue(value) => value.toString
      case Wildcard => "_"
    }

    case e: TypeName => e match {
      case DeclaredType(name) => name
      case Syntax.AnyType => "Any"
      case Syntax.NilType => "Nil"
      case SymbolType => "symbol"
      case NumberType => "number"
      case UnsignedType => "unsigned"
      case FloatType => "float"
    }

    case e: DirectiveValue => e match {
      case DirectiveValueString(value) => "\"" + value + "\""
      case DirectiveValueIdent(value) => value
      case DirectiveValueNumber(value) => value.toString
      case DirectiveValueBool(value) => value.toString
    }

    case Directive(qualifier, qualifiedNames, params) =>
      s"${stringify(qualifier)} ${stringify(qualifiedNames).mkString(", ")}" + {
        if (params.nonEmpty) s"(${params.map { case (id, v) => s"$id = ${stringify(v)}" }.mkString(", ")})"
        else ""
      }

    case RelationDecl(name, attributes, qualifiers, choiceDomain) =>
      s".decl $name(${attributes.map(stringify).mkString(", ")})" + {
        if (qualifiers.nonEmpty) " " + qualifiers.map(stringify).mkString(" ") else ""
      } + {
        choiceDomain match {
          case Some(value) => s" choice-domain ${stringify(value)}"
          case None => ""
        }
      }

    case Attribute(name, ty) => s"${stringify(name)}: ${stringify(ty)}"

    case e: RelationQualifier => e match {
      case BtreeQualifier => "btree"
      case BrieQualifier => "brie"
      case EquivalenceQualifier => "eqrel"
      case OverrideQualifier => "override"
      case InlineQualifier => "inline"
      case NoInlineQualifier => "no_inline"
      case MagicQualifier => "magic"
      case NoMagicQualifier => "no_magic"
    }

    case e: DirectiveQualifier => e match {
      case DirectiveQualifierInput => ".input"
      case DirectiveQualifierOutput => ".output"
      case DirectiveQualifierPrintsize => ".printsize"
      case DirectiveQualifierLimitsize => ".limitsize"
    }

    case ChoiceDomain(body) => body.map(stringify).mkString(", ")

    case Rule(atoms, disjunction, queryPlan) =>
      s"${atoms.map(stringify).mkString(", ")} :- ${stringify(disjunction)}." + {
        if (queryPlan.isDefined) " " + stringify(queryPlan.get)
        else ""
      }


    case SubsumptiveRule(atom1, atom2, disjunction, queryPlan) =>
      queryPlan match {
        case Some(value) => s"${stringify(atom1)} <= ${stringify(atom2)} :- ${stringify(disjunction)} ${stringify(value)}"
        case None => s"${stringify(atom1)} <= ${stringify(atom2)} :- ${stringify(disjunction)}"
      }


    case EliminateRuleDisjunction.Rule(atom, conjunction, queryPlan) =>
      s"${stringify(atom)} :- ${stringify(conjunction)}." + {
        if (queryPlan.isDefined) " " + stringify(queryPlan.get)
        else ""
      }

    case QualifiedName(identifiers) => identifiers.mkString(".")

    case Atom(name, args) => s"${stringify(name)}(${args.map(stringify).mkString(", ")})"
    case Fact(atom) => stringify(atom) + "."

    case Disjunction(conjunctions) => conjunctions.map(stringify).mkString("; ")
    case Conjunction(terms) => terms.map(stringify).mkString(", ")

    case e: ConjunctionTerm => e match {
      case ConjunctionTermAtom(negated, atom) =>
        s"${if (negated) "!" else ""}${stringify(atom)}"
      case ConjunctionTermConstraint(negated, constraint) =>
        s"${if (negated) "!" else ""}${stringify(constraint)}"
      case ConjunctionTermDisjunction(negated, disjunction) =>
        s"${if (negated) "!" else ""}(${stringify(disjunction)})"
    }

    // TODO: QueryPlan
    // case QueryPlan(body) => ???

    case e: Constant => e match {
      case ConstantString(value) => "\"" + value + "\""
      case ConstantNumber(value) => value.toString
      case ConstantUnsigned(value) => value.toString
      case ConstantFloat(value) => value.toString
    }


    case e: Argument => e match {
      case ArgumentConstant(value) => stringify(value)
      case ArgumentVariable(name) => stringify(name)
      case Syntax.ArgumentNil => "nil"
      case ArgumentList(args) => s"${args.map(stringify).mkString(", ")}"
      case ArgumentDollarFunctor(name, args) => s"$$ $name ( ${args.map(stringify).mkString(", ")} )"
      case ArgumentSingle(arg) => s"( ${stringify(arg)} )"
      case ArgumentAlias(arg, ty) => s"as ( ${stringify(arg)}, ${stringify(ty)} )"
      case ArgumentFunctorCall(name, args) => s"$name ( ${args.map(stringify).mkString(", ")} )"
      case ArgumentAggregator(aggregator) => stringify(aggregator)
      case ArgumentUnOp(op, arg) => stringify(op) + " " + stringify(arg)
      case ArgumentBinOp(op, l, r) => stringify(l) + " " + stringify(op) + " " + stringify(r)
    }

    case e: UnOp => e match {
      case Syntax.UnOpMinus => "-"
      case Syntax.UnOpBNot => "bnot"
      case Syntax.UnOpLNot => "lnot"
    }

    case e: BinOp => e match {
      case Syntax.BinOpAdd => "+"
      case Syntax.BinOpMinus => "-"
      case Syntax.BinOpMult => "*"
      case Syntax.BinOpDiv => "/"
      case Syntax.BinOpMod => "%"
      case Syntax.BinOpPow => "^"
      case Syntax.BinOpLAnd => "land"
      case Syntax.BinOpLOr => "lor"
      case Syntax.BinOpLXor => "lxor"
      case Syntax.BinOpBAnd => "band"
      case Syntax.BinOpBOr => "bor"
      case Syntax.BinOpBXor => "bxor"
      case Syntax.BinOpBShl => "bshl"
      case Syntax.BinOpBShr => "bshr"
      case Syntax.BinOpBShrU => "bshru"
    }


    case e: Aggregator => e match {
      case AggregatorMin(argument, cond) =>
        cond match {
          case AggregatorConditionAtom(atom) => s"min ${stringify(argument)}: ${stringify(atom)}"
          case AggregatorConditionDisjunction(disjunction) => s"min ${stringify(argument)}: {${stringify(disjunction)}}"
        }
      case AggregatorMax(argument, cond) =>
        cond match {
          case AggregatorConditionAtom(atom) => s"max ${stringify(argument)}: ${stringify(atom)}"
          case AggregatorConditionDisjunction(disjunction) => s"max ${stringify(argument)}: {${stringify(disjunction)}}"
        }
      case AggregatorMean(argument, cond) =>
        cond match {
          case AggregatorConditionAtom(atom) => s"mean ${stringify(argument)}: ${stringify(atom)}"
          case AggregatorConditionDisjunction(disjunction) => s"mean ${stringify(argument)}: {${stringify(disjunction)}}"
        }
      case AggregatorSum(argument, cond) =>
        cond match {
          case AggregatorConditionAtom(atom) => s"sum ${stringify(argument)}: ${stringify(atom)}"
          case AggregatorConditionDisjunction(disjunction) => s"sum ${stringify(argument)}: {${stringify(disjunction)}}"
        }
      case AggregatorCount(cond) =>
        cond match {
          case AggregatorConditionAtom(atom) => s"count: ${stringify(atom)}"
          case AggregatorConditionDisjunction(disjunction) => s"count: {${stringify(disjunction)}}"
        }
      case AggregatorRange(arg1, arg2, arg3) => s"range( ${stringify(arg1)}, ${stringify(arg2)}, ${stringify(arg3)} )"
    }


    case e: AggregatorCondition => e match {
      case AggregatorConditionAtom(atom) => stringify(atom)
      case AggregatorConditionDisjunction(disjunction) => s"{ ${stringify(disjunction)} }"
    }


    case ComponentDecl(ty, supers, bodies) =>
      s"${stringify(ty)}: ${supers.map(stringify).mkString(", ")} { ${bodies.map(stringify).mkString(" ")} }"


    case e: ComponentBody => e match {
      case ComponentBodyType(ty) => stringify(ty)
      case ComponentBodyRelation(relation) => stringify(relation)
      case ComponentBodyRule(rule) => stringify(rule)
      case ComponentBodyFact(fact) => stringify(fact)
      case ComponentBodyDirective(directive) => stringify(directive)
      case ComponentBodyOverride(identifier) => s".override ${stringify(identifier)}"
      case ComponentBodyComponentInit(init) => stringify(init)
      case ComponentBodyComponentDecl(decl) => stringify(decl)
    }


    case ComponentInit(name, ty) => s".init ${stringify(name)} = ${stringify(ty)}"


    case ComponentType(name, arguments) => s"${stringify(name)} < ${arguments.map(stringify).mkString(", ")} >"


    case FunctorDecl(name, attributes, returnType, isStateful) =>
      s".functor ${stringify(name)} ( ${attributes.map(stringify).mkString(", ")} ): " +
        s"${stringify(returnType)} ${if(isStateful) "stateful" else ""}"


    case Pragma(param, parameterValue) => s".pragma ${stringify(param)} ${stringify(parameterValue)}"


    case ADTBranch(branchId, attributes) => s"$branchId { ${attributes.map(stringify).mkString(", ")} }"

    case e: TypeDecl => e match {
      case TypeDeclSubtype(name, superType) => s".type $name <: ${stringify(superType)}"
      case TypeDeclUnion(name, types) => s".type $name = ${types.map(stringify).mkString(" | ")}"
      case TypeDeclRecord(name, records) => s".type $name = [ ${records.map(stringify).mkString(", ")} ]"
      case TypeDeclADT(name, branches) => s".type $name = ${branches.map(stringify).mkString(" | ")}"
    }

    case e: Constraint => e match {
      case ConstraintCmp(ty, l, r) =>
        ty match {
          case ConstraintCmpOp.Lt => s"${stringify(l)} < ${stringify(r)}"
          case ConstraintCmpOp.Gt => s"${stringify(l)} > ${stringify(r)}"
          case ConstraintCmpOp.Leq => s"${stringify(l)} <= ${stringify(r)}"
          case ConstraintCmpOp.Geq => s"${stringify(l)} >= ${stringify(r)}"
          case ConstraintCmpOp.Eq => s"${stringify(l)} = ${stringify(r)}"
          case ConstraintCmpOp.Neq => s"${stringify(l)} != ${stringify(r)}"
          case ConstraintCmpOp.Match => s"match ( ${stringify(l)}, ${stringify(r)} )"
          case ConstraintCmpOp.Contains => s"contains ( ${stringify(l)}, ${stringify(r)} )"
        }
      case ConstraintTrue => "true"
      case ConstraintFalse => "false"
    }

    case e: Functor => e match {
      case UserDefinedFunctor(name) => s"@${stringify(name)}"
      case e: IntrinsicFunctor => e match {
        case Syntax.IntrinsicFunctorOrd => "ord"
        case Syntax.IntrinsicFunctorToFloat => "to_float"
        case Syntax.IntrinsicFunctorToNumber => "to_number"
        case Syntax.IntrinsicFunctorToString => "to_string"
        case Syntax.IntrinsicFunctorToUnsigned => "to_unsigned"
        case Syntax.IntrinsicFunctorCat => "cat"
        case Syntax.IntrinsicFunctorStrLen => "strlen"
        case Syntax.IntrinsicFunctorSubStr => "substr"
        case Syntax.IntrinsicFunctorAutoInc => "autoinc"
      }
    }


    case _ => stringifyDatalog(e)
  }

  def print(e: Any): Unit = println(stringify(e))
}
