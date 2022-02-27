package inca.frontend.souffle

import inca.frontend.souffle.Syntax.{ArgumentConstant, ArgumentNil, ArgumentVariable, Atom, BrieQualifier, BtreeQualifier, ChoiceDomain, Conjunction, ConjunctionBodyAtom, ConjunctionBodyConstraint, ConjunctionBodyDisjunction, ConjunctionTerm, ConstantFloat, ConstantNumber, ConstantString, ConstantUnsigned, DeclaredType, Disjunction, EquivalenceQualifier, Fact, FalseDirectiveValue, FloatType, FloatValue, IdentDirectiveValue, InlineQualifier, InputQualifier, LimitsizeQualifier, MagicQualifier, NoInlineQualifier, NoMagicQualifier, NumberDirectiveValue, NumberType, NumberValue, OutputQualifier, OverrideQualifier, PrintsizeQualifier, QualifiedName, Relation, RelationAttribute, Rule, StringDirectiveValue, StringValue, SymbolType, TrueDirectiveValue, UnsignedType, Variable, Wildcard}

object PrettyPrinter {
  def print(e: Any): String = e match {
    case l: Seq[Any] => l.map(print).mkString("\n")

    case Variable(name) => name
    case StringValue(value) => "\"" + value + "\""
    case NumberValue(value) => value.toString
    case FloatValue(value) => value.toString
    case Wildcard => "_"

    case DeclaredType(name) => name
    case SymbolType => "symbol"
    case NumberType => "number"
    case UnsignedType => "unsigned"
    case FloatType => "float"

    case StringDirectiveValue(value) => "\"" + value + "\""
    case IdentDirectiveValue(value) => value
    case NumberDirectiveValue(value) => value.toString
    case TrueDirectiveValue => "true"
    case FalseDirectiveValue => "false"

    case Relation(name, attributes, qualifiers, choiceDomain) =>
      s".decl $name(${attributes.map(print).mkString(", ")})" + {
        if (qualifiers.nonEmpty) " " + qualifiers.map(print).mkString(" ") else ""
      } + {
        choiceDomain match {
          case Some(value) => s" choice-domain ${print(value)}"
          case None => ""
        }
      }

    case RelationAttribute(name, ty) => s"${print(name)}: ${print(ty)}"

    case BtreeQualifier => "btree"
    case BrieQualifier => "brie"
    case EquivalenceQualifier => "eqrel"
    case OverrideQualifier => "override"
    case InlineQualifier => "inline"
    case NoInlineQualifier => "no_inline"
    case MagicQualifier => "magic"
    case NoMagicQualifier => "no_magic"

    case InputQualifier => ".input"
    case OutputQualifier => ".output"
    case PrintsizeQualifier => ".printsize"
    case LimitsizeQualifier => ".limitsize"

    case ChoiceDomain(body) => body.map(print).mkString(", ")

    case Rule(atoms, disjunction, queryPlan) =>
      s"${atoms.map(print).mkString(", ")} :- ${print(disjunction)}." + {
        if (queryPlan.isDefined) " " + print(queryPlan.get)
        else ""
      }

    case QualifiedName(identifiers) => identifiers.mkString(".")

    case Atom(name, args) => s"${print(name)}(${args.map(print).mkString(", ")})"
    case Fact(atom) => print(atom) + "."

    case Disjunction(conjunctions) => conjunctions.map(print).mkString("; ")
    case Conjunction(terms) => terms.map(print).mkString(", ")
    case ConjunctionTerm(negated, body) => s"${if (negated) "!" else ""}${print(body)}"
    case ConjunctionBodyAtom(atom) => print(atom)
    case ConjunctionBodyConstraint(constraint) => print(constraint)
    case ConjunctionBodyDisjunction(disjunction) => "(" + print(disjunction) + ")"

    case ConstantString(value) => "\"" + value + "\""
    case ConstantNumber(value) => value.toString
    case ConstantUnsigned(value) => value.toString
    case ConstantFloat(value) => value.toString

    case ArgumentConstant(value) => print(value)
    case ArgumentVariable(name) => print(name)
    case ArgumentNil => "nil"

    case _ => e.toString
  }
}
