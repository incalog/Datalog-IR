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

  // DIRECTIVE VALUES
  // directive_value ::= STRING | IDENT | NUMBER | 'true' | 'false'
  // TODO: Pretty sure this is not exactly correct, seems weird

  sealed trait DirectiveValue
  case class StringDirectiveValue extends DirectiveValue {
    override def toString: String = "STRING"
  }
  case class IdentDirectiveValue extends DirectiveValue {
    override def toString: String = "IDENT"
  }
  case class NumberDirectiveValue extends DirectiveValue {
    override def toString: String = "NUMBER"
  }
  case class TrueDirectiveValue extends DirectiveValue {
    override def toString: String = "true"
  }
  case class FalseDirectiveValue extends DirectiveValue {
    override def toString: String = "false"
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

  // DirectiveQualifier
  // directive_qualifier  ::= '.input' | '.output' | '.printsize' | '.limitsize'
  sealed trait DirectiveQualifier
  case object InputQualifier extends DirectiveQualifier {
    override def toString: String = ".input"
  }
  case object OutputQualifier extends DirectiveQualifier {
    override def toString: String = ".output"
  }
  case object PrintsizeQualifier extends DirectiveQualifier {
    override def toString: String = ".printsize"
  }
  case object LimitsizeQualifier extends DirectiveQualifier {
    override def toString: String = ".limitsize"
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

  // SUBSUMPTIVE RULE
  // rule ::= atom '<=' atom ':-' disjunction '.' query_plan?
  case class SubsumptiveRule(atom1: Atom, atom2: Atom, disjunction: Disjunction, queryPlan: Option[QueryPlan] = None)

  // qualified_name ::= IDENT ( '.' IDENT )*
  case class QualifiedName(identifiers: Seq[String])

  // atom ::= qualified_name '(' ( argument ( ',' argument )* )? ')'
  case class Atom(name: QualifiedName, args: Seq[Argument])

  // fact ::= atom '.'
  case class Fact(atom: Atom)

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
  // constraint ::= argument ( '<' | '>' | '<=' | '>=' | '=' | '!=' ) argument
  //           | ( 'match' | 'contains' ) '(' argument ',' argument ')'
  //           | 'true'
  //           | 'false'
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

  // TODO: AGGREGATOR
  // aggregator  ::= (( ( 'max' | 'mean' | 'min' | 'sum' ) argument | 'count' ) ':' ( '{' disjunction '}' | atom )) |
  //                'range' '(' argument ',' argument (',' argument)? ')'

  // TODO: COMPONENT DECLARATION
  // component_decl ::=
  //  '.comp' component_type ( ( ':' | ',' ) component_type )*
  //    '{'
  //        ( type_decl | relation_decl | rule | fact | directive | '.override' IDENT | component_init | component_decl )*
  //    '}'

  // TODO: COMPONENT INITIALISATION
  // component_init ::= '.init' IDENT '=' component_type

  // TODO: COMPONENT TYPE
  // component_type ::= IDENT ( '<' IDENT ( ',' IDENT )* '>' )?

  // DIRECTIVE
  // directive ::= directive_qualifier qualified_name ( ',' qualified_name )* ( '(' ( IDENT '=' directive_value ( ',' IDENT '=' directive_value )* )? ')' )?
  case class Directive(dirQualifier: DirectiveQualifier, qualNames: Seq[QualifiedName], params: (identifier: Seq[String], directiveValue: Seq[DirectiveValue]))

  // USER-DEFINED FUNCTORS
  // functor_decl
  //         ::= '.functor' IDENT '(' ( attribute ( ',' attribute )* )? ')' ':' type_name 'stateful'?
  // TODO: Find out what to do with the optional 'stateful'
  case class Functor(name: String, attributes: Seq[RelationAttribute], returnType: Type)

  // PRAGMAS
  // pragma   ::= '.pragma' STRING STRING?
  case class Pragma(param: String, parameterValue: Option[String] = None)


}
