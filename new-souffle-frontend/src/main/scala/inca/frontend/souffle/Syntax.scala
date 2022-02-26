package inca.frontend.souffle

object Syntax {

  // EXPRESSIONS

  sealed trait Expression
  case class Variable(name: String) extends Expression {
    //override def toString: String = cleanVarName(name)
  }
  case class StringValue(value: String) extends Expression {
    override def toString: String = "\"" + value + "\""
  }
  case class NumberValue(value: Int) extends Expression {
    override def toString: String = value.toString
  }
  case class FloatValue(value: Float) extends Expression {
    override def toString: String = value.toString
  }
  case object Wildcard extends Expression {
    override def toString: String = "_"
  }

  // TYPES

  sealed trait Type
  case class DeclaredType(name: String) extends Type {
    override def toString: String = name
  }

  sealed trait PrimitiveType extends Type
  case object SymbolType extends PrimitiveType {
    override def toString: String = "symbol"
  }
  case object NumberType extends PrimitiveType {
    override def toString: String = "number"
  }
  case object UnsignedType extends PrimitiveType {
    override def toString: String = "unsigned"
  }
  case object FloatType extends PrimitiveType {
    override def toString: String = "float"
  }

  // RELATIONS
  // relation_decl ::=
  //    '.decl' IDENT ( ',' IDENT )* '(' attribute ( ',' attribute )* ')'
  //    ( 'override' | 'inline' | 'no_inline' | 'magic' | 'no_magic' | 'brie' | 'btree' | 'eqrel' )*
  //    choice_domain

  case class Relation(name: String, attributes: Seq[RelationAttribute], qualifiers: Seq[RelationQualifier] = Seq(), choiceDomain: Option[ChoiceDomain] = None) {
    override def toString: String =
      s".decl $name(${attributes.mkString(", ")})" + {
        if (qualifiers.nonEmpty) " " + qualifiers.mkString(" ") else ""
      } + {
        choiceDomain match {
          case Some(value) => s" choice-domain $value"
          case None => ""
        }
      }

    def isNullary: Boolean = attributes.isEmpty
  }

  // attribute ::= IDENT ":" type_name
  case class RelationAttribute(name: String, ty: Type) {
    override def toString: String = s"$name: $ty"
  }

  sealed trait RelationQualifier
  case object BtreeQualifier extends RelationQualifier {
    override def toString: String = "btree"
  }
  case object BrieQualifier extends RelationQualifier {
    override def toString: String = "brie"
  }
  case object EquivalenceQualifier extends RelationQualifier {
    override def toString: String = "eqrel"
  }
  case object OverrideQualifier extends RelationQualifier {
    override def toString: String = "override"
  }
  case object InlineQualifier extends RelationQualifier {
    override def toString: String = "inline"
  }
  case object NoInlineQualifier extends RelationQualifier {
    override def toString: String = "no_inline"
  }
  case object MagicQualifier extends RelationQualifier {
    override def toString: String = "magic"
  }
  case object NoMagicQualifier extends RelationQualifier {
    override def toString: String = "no_magic"
  }

  // CHOICE DOMAIN
  // choice_domain ::=
  //    ( 'choice-domain' ( IDENT | '(' IDENT ( ',' IDENT )* ')' ) ( ',' ( IDENT | '(' IDENT ( ',' IDENT )* ')' ) )* )?
  case class ChoiceDomain(body: Seq[String]) {
    override def toString: String = body.mkString(", ")
  }

  // RULES
  // rule ::= atom ( ',' atom )* ':-' disjunction '.' query_plan?
  case class Rule(atoms: Seq[Atom], disjunction: Seq[Conjunction], queryPlan: Option[QueryPlan] = None)

  // qualified_name ::= IDENT ( '.' IDENT )*
  case class QualifiedName(identifiers: Seq[String])

  // atom ::= qualified_name '(' ( argument ( ',' argument )* )? ')'
  case class Atom(name: QualifiedName, args: Seq[Argument])

  // disjunction ::= conjunction ( ';' conjunction )*
  case class Disjunction(conjunctions: Seq[Conjunction])

  // conjunction ::= '!'* ( atom | constraint | '(' disjunction ')' ) ( ',' '!'* ( atom | constraint | '(' disjunction ')' ) )*
  // example: a(x, y, z), !b, !!!!c, (g + 1; h == 1)
  case class Conjunction(body: Seq[ConjunctionBody])

  sealed trait ConjunctionBody
  case class ConjunctionBodyAtom(atom: Atom) extends ConjunctionBody
  case class ConjunctionBodyConstraint(constraint: Constraint) extends ConjunctionBody
  case class ConjunctionBodyDisjunction(disjunction: Disjunction) extends ConjunctionBody

  // query_plan ::= '.plan' NUMBER ':' '(' ( NUMBER ( ',' NUMBER )* )? ')' ( ',' NUMBER ':' '(' ( NUMBER ( ',' NUMBER )* )? ')' )*
  case class QueryPlan(body: Seq[(NumberValue, Seq[NumberValue])])

  // TODO: CONSTRAINTS
  type Constraint

  // TODO: ARGUMENTS
  // argument ::=
  //      constant
  //    | variable
  //    | 'nil'
  //    | '[' argument_list ']'
  //    | '$' IDENT ( '(' argument_list ')' )?
  //    | '(' argument ')'
  //    | 'as' '(' argument ',' type_name ')'
  //    | ( userdef_functor | intrinsic_functor ) '(' argument_list ')'
  //    | aggregator
  //    | ( unary_operation | argument binary_operation ) argument
  type Argument
}
