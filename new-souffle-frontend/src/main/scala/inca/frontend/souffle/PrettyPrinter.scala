package inca.frontend.souffle

import Syntax._
import inca.backend.ir.{Datalog, DatalogPrinter}

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
    case l: Seq[Any] => l.map(stringify).mkString("\n")
    case e: SouffleProgram => e.map(stringify).mkString("\n")

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

    case DirectiveValueString(value) => "\"" + value + "\""
    case DirectiveValueIdent(value) => value
    case DirectiveValueNumber(value) => value.toString
    case DirectiveValueBool(value) => value.toString

    case Directive(qualifier, qualifiedNames, params) =>
      s"${stringify(qualifier)} ${stringify(qualifiedNames).mkString(", ")}" + {
        if (params.isDefined) s"(${params.get.map { case (id, v) => s"$id = ${stringify(v)}" }.mkString(", ")})"
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

    case BtreeQualifier => "btree"
    case BrieQualifier => "brie"
    case EquivalenceQualifier => "eqrel"
    case OverrideQualifier => "override"
    case InlineQualifier => "inline"
    case NoInlineQualifier => "no_inline"
    case MagicQualifier => "magic"
    case NoMagicQualifier => "no_magic"

    case DirectiveQualifierInput => ".input"
    case DirectiveQualifierOutput => ".output"
    case DirectiveQualifierPrintsize => ".printsize"
    case DirectiveQualifierLimitsize => ".limitsize"

    case ChoiceDomain(body) => body.map(stringify).mkString(", ")

    case Rule(atoms, disjunction, queryPlan) =>
      s"${atoms.map(stringify).mkString(", ")} :- ${stringify(disjunction)}." + {
        if (queryPlan.isDefined) " " + stringify(queryPlan.get)
        else ""
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
    case ConjunctionTermAtom(negated, atom) =>
      s"${if (negated) "!" else ""}${stringify(atom)}"
    case ConjunctionTermConstraint(negated, constraint) =>
      s"${if (negated) "!" else ""}${stringify(constraint)}"
    case ConjunctionTermDisjunction(negated, disjunction) =>
      s"${if (negated) "!" else ""}(${stringify(disjunction)})"

    case ConstantString(value) => "\"" + value + "\""
    case ConstantNumber(value) => value.toString
    case ConstantUnsigned(value) => value.toString
    case ConstantFloat(value) => value.toString

    case ArgumentConstant(value) => stringify(value)
    case ArgumentVariable(name) => stringify(name)
    case ArgumentNil => "nil"

    case ADTBranch(branchId, attributes) => s"$branchId { ${attributes.map(stringify).mkString(", ")} }"

    case TypeDeclSubtype(name, superType) => s".type $name <: ${stringify(superType)}"
    case TypeDeclUnion(name, types) => s".type $name = ${types.map(stringify).mkString(" | ")}"
    case TypeDeclRecord(name, records) => s".type $name = [ ${records.map(stringify).mkString(", ")} ]"
    case TypeDeclADT(name, branches) => s".type $name = ${branches.map(stringify).mkString(" | ")}"

    case _ => stringifyDatalog(e)
  }

  def print(e: Any): Unit = println(stringify(e))
}
