package inca.frontend.souffle

object Syntax {

  // EXPRESSIONS

  sealed trait Expression
  case class Variable(name: String) extends Expression
  case class StringValue(value: String) extends Expression
  case class NumberValue(value: Int) extends Expression
  case class FloatValue(value: Float) extends Expression
  case object Wildcard extends Expression

  // TYPES

  sealed trait Type
  case class DeclaredType(name: String) extends Type

  sealed trait PrimitiveType extends Type
  case object SymbolType extends PrimitiveType
  case object NumberType extends PrimitiveType
  case object UnsignedType extends PrimitiveType
  case object FloatType extends PrimitiveType

  // DIRECTIVE VALUES
  // directive_value ::= STRING | IDENT | NUMBER | 'true' | 'false'

  sealed trait DirectiveValue
  case class StringDirectiveValue(value: String) extends DirectiveValue
  case class IdentDirectiveValue(value: String) extends DirectiveValue
  case class NumberDirectiveValue(value: Int) extends DirectiveValue
  case object TrueDirectiveValue extends DirectiveValue
  case object FalseDirectiveValue extends DirectiveValue

  // RELATIONS
  // relation_decl ::=
  //    '.decl' IDENT ( ',' IDENT )* '(' attribute ( ',' attribute )* ')'
  //    ( 'override' | 'inline' | 'no_inline' | 'magic' | 'no_magic' | 'brie' | 'btree' | 'eqrel' )*
  //    choice_domain

  case class Relation(name: String, attributes: Seq[RelationAttribute], qualifiers: Seq[RelationQualifier] = Seq(), choiceDomain: Option[ChoiceDomain] = None) {
    def isNullary: Boolean = attributes.isEmpty
  }

  // attribute ::= IDENT ":" type_name
  case class RelationAttribute(name: String, ty: Type)

  // relation qualifiers
  sealed trait RelationQualifier
  case object BtreeQualifier extends RelationQualifier
  case object BrieQualifier extends RelationQualifier
  case object EquivalenceQualifier extends RelationQualifier
  case object OverrideQualifier extends RelationQualifier
  case object InlineQualifier extends RelationQualifier
  case object NoInlineQualifier extends RelationQualifier
  case object MagicQualifier extends RelationQualifier
  case object NoMagicQualifier extends RelationQualifier

  // DirectiveQualifier
  // directive_qualifier  ::= '.input' | '.output' | '.printsize' | '.limitsize'
  sealed trait DirectiveQualifier
  case object InputQualifier extends DirectiveQualifier
  case object OutputQualifier extends DirectiveQualifier
  case object PrintsizeQualifier extends DirectiveQualifier
  case object LimitsizeQualifier extends DirectiveQualifier

  // CHOICE DOMAIN
  // choice_domain ::=
  //    ( 'choice-domain' ( IDENT | '(' IDENT ( ',' IDENT )* ')' ) ( ',' ( IDENT | '(' IDENT ( ',' IDENT )* ')' ) )* )?
  case class ChoiceDomain(body: Seq[String])

  // RULES
  // rule ::= atom ( ',' atom )* ':-' disjunction '.' query_plan?
  case class Rule(atoms: Seq[Atom], disjunction: Disjunction, queryPlan: Option[QueryPlan] = None) {
    override def toString: String =
      s"${atoms.mkString(", ")} :- $disjunction." + {
        if (queryPlan.isDefined) " " + queryPlan.get.toString
        else ""
      }
  }

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
  case class Disjunction(conjunctions: Seq[Conjunction]) {
    override def toString: String = conjunctions.mkString("; ")
  }

  // conjunction ::=
  //    '!'* ( atom | constraint | '(' disjunction ')' )
  //        ( ',' '!'* ( atom | constraint | '(' disjunction ')' ) )*
  // example: a(x, y, z), !b, !!!!c, age > 50
  case class Conjunction(terms: Seq[ConjunctionTerm])

  case class ConjunctionTerm(negated: Boolean, body: ConjunctionBody)

  sealed trait ConjunctionBody
  case class ConjunctionBodyAtom(atom: Atom) extends ConjunctionBody
  case class ConjunctionBodyConstraint(constraint: Constraint) extends ConjunctionBody
  case class ConjunctionBodyDisjunction(disjunction: Disjunction) extends ConjunctionBody

  // query_plan ::= '.plan' NUMBER ':' '(' ( NUMBER ( ',' NUMBER )* )? ')' ( ',' NUMBER ':' '(' ( NUMBER ( ',' NUMBER )* )? ')' )*
  case class QueryPlan(body: Seq[(Int, Seq[Int])])

  // TODO: CONSTRAINTS
  // constraint ::= argument ( '<' | '>' | '<=' | '>=' | '=' | '!=' ) argument
  //           | ( 'match' | 'contains' ) '(' argument ',' argument ')'
  //           | 'true'
  //           | 'false'
  type Constraint

  // constant ::= STRING | NUMBER | UNSIGNED | FLOAT
  sealed trait Constant
  case class ConstantString(value: String) extends Constant
  case class ConstantNumber(value: Int) extends Constant
  case class ConstantUnsigned(value: Int) extends Constant {
    assert(value >= 0)
  }
  case class ConstantFloat(value: Float) extends Constant

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
  // TODO: extend
  sealed trait Argument
  case class ArgumentConstant(value: Constant) extends Argument
  case class ArgumentVariable(name: String) extends Argument {
    def isWildcard: Boolean = name == "_"
  }
  case object ArgumentNil extends Argument

  // TODO: AGGREGATOR
  // aggregator  ::= (( ( 'max' | 'mean' | 'min' | 'sum' ) argument | 'count' ) ':' ( '{' disjunction '}' | atom )) |
  //                'range' '(' argument ',' argument (',' argument)? ')'

  // TODO: COMPONENT DECLARATION
  // component_decl ::=
  //  '.comp' component_type ( ( ':' | ',' ) component_type )*
  //    '{'
  //        ( type_decl | relation_decl | rule | fact | directive | '.override' IDENT | component_init | component_decl )*
  //    '}'

  // component_init ::= '.init' IDENT '=' component_type
  case class ComponentInit(name: String, ty: ComponentType)

  // component_type ::= IDENT ( '<' IDENT ( ',' IDENT )* '>' )?
  case class ComponentType(name: String, arguments: Seq[String])

  // DIRECTIVE
  // directive ::=
  //    directive_qualifier qualified_name ( ',' qualified_name )*
  //        ( '(' ( IDENT '=' directive_value ( ',' IDENT '=' directive_value )* )? ')' )?
  case class Directive(qualifier: DirectiveQualifier,
                       qualifiedNames: Seq[QualifiedName],
                       params: Map[String, DirectiveValue])

  // USER-DEFINED FUNCTORS
  // functor_decl
  //         ::= '.functor' IDENT '(' ( attribute ( ',' attribute )* )? ')' ':' type_name 'stateful'?
  case class Functor(name: String, attributes: Seq[RelationAttribute], returnType: Type, isStateful: Boolean = false)

  // PRAGMAS
  // pragma   ::= '.pragma' STRING STRING?
  case class Pragma(param: String, parameterValue: Option[String] = None)
}
