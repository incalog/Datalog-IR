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
  case class DirectiveValueString(value: String) extends DirectiveValue
  case class DirectiveValueIdent(value: String) extends DirectiveValue
  case class DirectiveValueNumber(value: Int) extends DirectiveValue
  case class DirectiveValueBool(value: Boolean) extends DirectiveValue

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
  case object DirectiveQualifierInput extends DirectiveQualifier
  case object DirectiveQualifierOutput extends DirectiveQualifier
  case object DirectiveQualifierPrintsize extends DirectiveQualifier
  case object DirectiveQualifierLimitsize extends DirectiveQualifier

  // CHOICE DOMAIN
  // choice_domain ::=
  //    ( 'choice-domain' ( IDENT | '(' IDENT ( ',' IDENT )* ')' ) ( ',' ( IDENT | '(' IDENT ( ',' IDENT )* ')' ) )* )?
  case class ChoiceDomain(body: Seq[String])

  // RULES
  // rule ::= atom ( ',' atom )* ':-' disjunction '.' query_plan?
  case class Rule(atoms: Seq[Atom], disjunction: Disjunction, queryPlan: Option[QueryPlan] = None)

  // A(x) :- B(x); C(x)
  // A(x) :- B(x)
  // A(x) :- C(x)

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

  // conjunction ::=
  //    '!'* ( atom | constraint | '(' disjunction ')' )
  //        ( ',' '!'* ( atom | constraint | '(' disjunction ')' ) )*
  // example: a(x, y, z), !b, !!!!c, age > 50, (true; false)
  case class Conjunction(terms: Seq[ConjunctionTerm]) {
    def ++(other: Conjunction): Conjunction = Conjunction(terms ++ other.terms)
  }

  // conjunction_term ::= atom | constraint | '(' disjunction ')'
  sealed abstract class ConjunctionTerm {
    val isNegated: Boolean
    def negated: ConjunctionTerm
  }
  case class ConjunctionTermAtom(override val isNegated: Boolean, atom: Atom) extends ConjunctionTerm {
    override def negated: ConjunctionTerm = ConjunctionTermAtom(!isNegated, atom)
  }
  case class ConjunctionTermConstraint(override val isNegated: Boolean, constraint: Constraint) extends ConjunctionTerm {
    override def negated: ConjunctionTerm = ConjunctionTermConstraint(!isNegated, constraint)
  }
  case class ConjunctionTermDisjunction(override val isNegated: Boolean, disjunction: Disjunction) extends ConjunctionTerm {
    override def negated: ConjunctionTerm = ConjunctionTermDisjunction(!isNegated, disjunction)
  }

  // query_plan ::= '.plan' NUMBER ':' '(' ( NUMBER ( ',' NUMBER )* )? ')' ( ',' NUMBER ':' '(' ( NUMBER ( ',' NUMBER )* )? ')' )*
  case class QueryPlan(body: Seq[(Int, Seq[Int])])

  // constraint ::= argument ( '<' | '>' | '<=' | '>=' | '=' | '!=' ) argument
  //           | ( 'match' | 'contains' ) '(' argument ',' argument ')'
  //           | 'true'
  //           | 'false'

  sealed trait ConstraintCmpType
  object ConstraintCmp {
    case object Lt extends ConstraintCmpType
    case object Gt extends ConstraintCmpType
    case object Leq extends ConstraintCmpType
    case object Geq extends ConstraintCmpType
    case object Eq extends ConstraintCmpType
    case object Neq extends ConstraintCmpType
    case object Match extends ConstraintCmpType
    case object Contains extends ConstraintCmpType
  }

  sealed trait Constraint
  case class ConstraintCmp(ty: ConstraintCmpType, l: Argument, r: Argument) extends Constraint
  case object ConstraintTrue extends Constraint
  case object ConstraintFalse extends Constraint

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
  sealed trait Argument
  case class ArgumentConstant(value: Constant) extends Argument
  case class ArgumentVariable(name: String) extends Argument {
    def isWildcard: Boolean = name == "_"
  }
  case object ArgumentNil extends Argument
  case class ArgumentList(args: Seq[Argument]) extends Argument
  case class ArgumentDollarFunctor(name: String, args: Seq[Argument]) extends Argument
  case class ArgumentSingle(arg: Argument) extends Argument
  case class ArgumentAlias(arg: Argument, ty: Type) extends Argument
  case class ArgumentFunctorCall(name: String, arguments: Seq[Argument]) extends Argument
  case class ArgumentAggregator(aggregator: Aggregator) extends Argument
  case class ArgumentUnOp(op: UnOp, argument: Argument) extends Argument
  case class ArgumentBinOp(op: BinOp, l: Argument, r: Argument) extends Argument

  // unary_operation ::= '-' | 'bnot' | 'lnot'
  sealed trait UnOp
  case object UnOpMinus extends UnOp
  case object UnOpBNot extends UnOp
  case object UnOpLNot extends UnOp

  // binary_operation ::=
  //  '+' | '-' | '*' | '/' | '%' | '^' | 'land' | 'lor' | 'lxor' | 'band' | 'bor' | 'bxor' | 'bshl' | 'bshr' | 'bshru'
  sealed trait BinOp
  case object BinOpAdd extends BinOp
  case object BinOpMinus extends BinOp
  case object BinOpMult extends BinOp
  case object BinOpDiv extends BinOp
  case object BinOpMod extends BinOp
  case object BinOpPow extends BinOp
  case object BinOpLAnd extends BinOp
  case object BinOpLOr extends BinOp
  case object BinOpLXor extends BinOp
  case object BinOpBAnd extends BinOp
  case object BinOpBOr extends BinOp
  case object BinOpBXor extends BinOp
  case object BinOpBShl extends BinOp
  case object BinOpBShr extends BinOp
  case object BinOpBShrU extends BinOp

  // aggregator  ::= (( ( 'max' | 'mean' | 'min' | 'sum' ) argument | 'count' ) ':' ( '{' disjunction '}' | atom )) |
  //                'range' '(' argument ',' argument (',' argument)? ')'
  sealed trait Aggregator
  case class AggregatorMin(argument: Argument, cond: AggregatorCondition) extends Aggregator
  case class AggregatorMax(argument: Argument, cond: AggregatorCondition) extends Aggregator
  case class AggregatorMean(argument: Argument, cond: AggregatorCondition) extends Aggregator
  case class AggregatorSum(argument: Argument, cond: AggregatorCondition) extends Aggregator
  case class AggregatorCount(cond: AggregatorCondition) extends Aggregator
  case class AggregatorRange(arg1: Argument, arg2: Argument, arg3: Option[Argument]) extends Aggregator

  sealed trait AggregatorCondition
  case class AggregatorConditionAtom(atom: Atom) extends AggregatorCondition
  case class AggregatorConditionDisjunction(disjunction: Disjunction) extends AggregatorCondition

  // component_decl ::=
  //  '.comp' component_type ( ( ':' | ',' ) component_type )*
  //    '{'
  //        ( type_decl | relation_decl | rule | fact | directive | '.override' IDENT | component_init | component_decl )*
  //    '}'
  case class Component(ty: ComponentType, supers: Seq[ComponentType], bodies: Seq[ComponentBody])

  sealed trait ComponentBody
  case class ComponentBodyType(ty: Type) extends ComponentBody
  case class ComponentBodyRelation(relation: Relation) extends ComponentBody
  case class ComponentBodyRule(rule: Rule) extends ComponentBody
  case class ComponentBodyFact(fact: Fact) extends ComponentBody
  case class ComponentBodyDirective(directive: Directive) extends ComponentBody
  case class ComponentBodyOverride(identifier: String) extends ComponentBody
  case class ComponentBodyComponentInit(init: ComponentInit) extends ComponentBody
  case class ComponentBodyComponentDecl(decl: Component) extends ComponentBody

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
                       params: Option[Map[String, DirectiveValue]])

  // FUNCTORS
  // functor_decl
  //         ::= '.functor' IDENT '(' ( attribute ( ',' attribute )* )? ')' ':' type_name 'stateful'?
  case class UserDefinedFunctor(name: String, attributes: Seq[RelationAttribute], returnType: Type, isStateful: Boolean = false)

  // INTRINSIC FUNCTOR
  // intrinsic_functor ::= 'ord' | 'to_float' | 'to_number' | 'to_string' | 'to_unsigned' | 'cat' | 'strlen' | 'substr' | 'autoinc'
  sealed trait IntrinsicFunctor
  case object IntrinsicFunctorOrd extends IntrinsicFunctor
  case object IntrinsicFunctorToFloat extends IntrinsicFunctor
  case object IntrinsicFunctorToNumber extends IntrinsicFunctor
  case object IntrinsicFunctorToString extends IntrinsicFunctor
  case object IntrinsicFunctorToUnsigned extends IntrinsicFunctor
  case object IntrinsicFunctorCat extends IntrinsicFunctor
  case object IntrinsicFunctorStrLen extends IntrinsicFunctor
  case object IntrinsicFunctorSubStr extends IntrinsicFunctor
  case object IntrinsicFunctorAutoInc extends IntrinsicFunctor

  // PRAGMAS
  // pragma   ::= '.pragma' STRING STRING?
  case class Pragma(param: String, parameterValue: Option[String] = None)

  // ADT DECLARATION
  // adt_branch ::= IDENT "{" (attribute ( "," attribute)*)? "}"
  case class AdtBranch(branchId: String, attributeList: Seq[RelationAttribute])

  // RECORD DECLARATION
  // record_list ::= "[" attribute ( "," attribute)* "]"
  case class RecordList(attributeList: Seq[RelationAttribute])




}
