package inca.frontend.souffle

import Syntax._
import inca.backend.ir.{Datalog, DatalogPrinter}

object PrettyPrinter {
  def stringifyDatalog(e: Any)(implicit verbose: Boolean = true): String = e match {
    case e: Datalog.Module => DatalogPrinter.prettyModule(e)
    case e: Datalog.Pattern => DatalogPrinter.prettyPattern(e)
    case e: Datalog.Visibility => DatalogPrinter.prettyVis(Some(e))
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
      case OverridableQualifier => "overridable"
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
        case Some(value) => s"${stringify(atom1)} <= ${stringify(atom2)} :- ${stringify(disjunction)}. ${stringify(value)}"
        case None => s"${stringify(atom1)} <= ${stringify(atom2)} :- ${stringify(disjunction)}."
      }

    case compiler.EliminateRuleDisjunction.Rule(atom, conjunction, queryPlan) =>
      s"${stringify(atom)} :- ${stringify(conjunction)}." + {
        if (queryPlan.isDefined) " " + stringify(queryPlan.get)
        else ""
      }

    case QualifiedName(identifiers) => identifiers.mkString(".")

    case Atom(name, args) => s"${stringify(name)}(${args.map(stringify).mkString(", ")})"
    case Fact(atom) => stringify(atom) + "."

    case TermDisjunction(terms, isNegated) =>
      { if (isNegated) "!" else "" } +  terms.map(stringify).mkString("; ")
    case TermConjunction(terms, isNegated) =>
      val s = terms.map(stringify).mkString(", ")
      if (isNegated) s"!($s)"
      else s
    case TermAtom(atom, isNegated) => s"${if (isNegated) "!" else ""}${stringify(atom)}"
    case TermConstraint(constraint, isNegated) => s"${if (isNegated) "!" else ""}${stringify(constraint)}"

    case QueryPlan(body) =>
      ".plan " + body.map { case (i, value) => s"$i : (${value.mkString(", ")})" }.mkString(", ")

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
      case ArgumentList(args) => s"[${args.map(stringify).mkString(", ")}]"
      case ArgumentBranchConstructor(name, args) =>
        if (args.isEmpty) s"$$ $name "
        else s"$$ $name ( ${args.map(stringify).mkString(", ")} )"
      case ArgumentSingle(arg) => s"( ${stringify(arg)} )"
      case ArgumentAlias(arg, ty) => s"as ( ${stringify(arg)}, ${stringify(ty)} )"
      case ArgumentUserDefinedFunc(name, args) => s"@$name ( ${args.map(stringify).mkString(", ")} )"
      case ArgumentAggregator(aggregator) => stringify(aggregator)
      case ArgumentUnOp(op, arg) =>
        stringify(op) + {
          op match {
            case UnOpMinus => ""
            case _ => " "
          }
        } +
        stringify(arg)
      case ArgumentBinOp(op, l, r) => stringify(l) + " " + stringify(op) + " " + stringify(r)
      case ArgumentIntrinsicFunc(func, args) => {
        func match {
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
      } + {
        if (args.isEmpty) "" else s"(${args.map(stringify).mkString(", ")})"
      }
    }

    case e: UnOp => e match {
      case UnOpMinus => "-"
      case UnOpBNot => "bnot"
      case UnOpLNot => "lnot"
    }

    case e: BinOp => e match {
      case BinOpAdd => "+"
      case BinOpMinus => "-"
      case BinOpMult => "*"
      case BinOpDiv => "/"
      case BinOpMod => "%"
      case BinOpPow => "^"
      case BinOpLAnd => "land"
      case BinOpLOr => "lor"
      case BinOpLXor => "lxor"
      case BinOpBAnd => "band"
      case BinOpBOr => "bor"
      case BinOpBXor => "bxor"
      case BinOpBShl => "bshl"
      case BinOpBShr => "bshr"
      case BinOpBShrU => "bshru"
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
      s".comp ${stringify(ty)}: ${supers.map(stringify).mkString(", ")} {\n" +
      s"${bodies.map("\t" + stringify(_)).mkString("\n")}" +
      "\n}"

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

    case ComponentType(name, arguments) =>
      stringify(name) + {
        if (arguments.isEmpty) ""
        else "<" + arguments.map(stringify).mkString(", ") + ">"
      }

    case FunctorDecl(name, attributes, returnType, isStateful) =>
      s".functor ${stringify(name)} ( ${attributes.map(stringify).mkString(", ")} ): " +
        s"${stringify(returnType)} ${if(isStateful) "stateful" else ""}"

    case Pragma(param, parameterValue) =>
      ".pragma \"" + param + "\"" + {
        if (parameterValue.isDefined) " \"" + parameterValue.get + "\""
        else ""
      }

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
        }
      case ConstraintMatch(l, r) => s"match(${stringify(l)}, ${stringify(r)})"
      case ConstraintContains(l, r) => s"contains(${stringify(l)}, ${stringify(r)})"
      case ConstraintTrue => "true"
      case ConstraintFalse => "false"
    }

    case UserDefinedFunctor(name) => s"@${name}"
    case e: Functor => e match {
      case e: IntrinsicFunctor => e match {
        case IntrinsicFunctorOrd => "ord"
        case IntrinsicFunctorToFloat => "to_float"
        case IntrinsicFunctorToNumber => "to_number"
        case IntrinsicFunctorToString => "to_string"
        case IntrinsicFunctorToUnsigned => "to_unsigned"
        case IntrinsicFunctorCat => "cat"
        case IntrinsicFunctorStrLen => "strlen"
        case IntrinsicFunctorSubStr => "substr"
        case IntrinsicFunctorAutoInc => "autoinc"
      }
    }

    case _ => stringifyDatalog(e)
  }

  def print(e: Any): Unit = println(stringify(e))
}
