package inca.frontend.souffle

object Syntax {
  // PROGRAM

  // program  ::=
  // ( pragma |
  //   functor_decl |
  //   component_decl |
  //   component_init |
  //   directive |
  //   rule |
  //   fact |
  //   relation_decl |
  //   type_decl )*

  sealed trait SouffleStatement
  type SouffleProgram = Seq[SouffleStatement]

  // EXPRESSIONS

  sealed trait Expression
  case class Variable(name: String) extends Expression
  case class StringValue(value: String) extends Expression
  case class NumberValue(value: Int) extends Expression
  case class FloatValue(value: Float) extends Expression
  case object Wildcard extends Expression

  // TYPES

  sealed trait TypeName
  case class DeclaredType(name: String) extends TypeName
  case object AnyType extends TypeName
  case object NilType extends TypeName

  sealed trait PrimitiveType extends TypeName
  case object SymbolType extends PrimitiveType
  case object NumberType extends PrimitiveType
  case object UnsignedType extends PrimitiveType
  case object FloatType extends PrimitiveType

  /*   "A type declaration binds a name with a new type.
  *     The type is either a subtype, an equivalence/union type, a record type, of an ADT."
  *
  * type_decl ::= TYPE IDENT ("<:" type_name | "=" ( type_name ( "|" type_name )* | record_list | adt_branch ( "|" adt_branch )* ))
  *
  *     or:
  *
  * type_decl ::= TYPE IDENT "<:" type_name
  * type_decl ::= TYPE IDENT "=" ( type_name ( "|" type_name )* )
  * type_decl ::= TYPE IDENT "=" record_list
  * type_decl ::= TYPE IDENT "=" adt_branch ( "|" adt_branch )*
  * */
  sealed trait TypeDecl extends SouffleStatement
  case class TypeDeclSubtype(name: String, superType: TypeName) extends TypeDecl
  case class TypeDeclUnion(name: String, types: Seq[TypeName]) extends TypeDecl
  case class TypeDeclRecord(name: String, records: Seq[Attribute]) extends TypeDecl
  case class TypeDeclADT(name: String, branches: Seq[ADTBranch]) extends TypeDecl

  // ADT DECLARATION
  // adt_branch ::= IDENT "{" (attribute ( "," attribute)*)? "}"
  case class ADTBranch(branchId: String, attributes: Seq[Attribute])

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

  case class RelationDecl(name: String,
                          attributes: Seq[Attribute],
                          qualifiers: Seq[RelationQualifier] = Seq(),
                          choiceDomain: Option[ChoiceDomain] = None) extends SouffleStatement {
    def isNullary: Boolean = attributes.isEmpty
  }

  // attribute ::= IDENT ":" type_name
  case class Attribute(name: String, ty: TypeName)

  // relation qualifiers
  // relation_qualifier ::= 'override' | 'inline' | 'no_inline' | 'magic' | 'no_magic' | 'brie' | 'btree' | 'eqrel'
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
  // directive_qualifier ::= '.input' | '.output' | '.printsize' | '.limitsize'
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
  case class Rule(atoms: Seq[Atom], disjunction: Disjunction, queryPlan: Option[QueryPlan] = None) extends SouffleStatement

  // SUBSUMPTIVE RULE
  // rule ::= atom '<=' atom ':-' disjunction '.' query_plan?
  case class SubsumptiveRule(atom1: Atom, atom2: Atom, disjunction: Disjunction, queryPlan: Option[QueryPlan] = None)

  // qualified_name ::= IDENT ( '.' IDENT )*
  case class QualifiedName(identifiers: Seq[String]) {
    override def toString: String = identifiers.mkString(".")
  }

  object QualifiedName {
    def apply(identifier: String): QualifiedName = QualifiedName(identifier.split('.'))

    import scala.language.implicitConversions
    implicit def stringToQualifiedName(s: String): QualifiedName =
      QualifiedName(s.split('.'))
  }

  // atom ::= qualified_name '(' ( argument ( ',' argument )* )? ')'
  case class Atom(name: QualifiedName, args: Seq[Argument])

  // fact ::= atom '.'
  case class Fact(atom: Atom) extends SouffleStatement

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

    def applyDeMorgan(): ConjunctionTermConstraint =
      // !(a > b) -> a <= b
    ConjunctionTermConstraint(!isNegated, constraint.negated)
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

  sealed trait ConstraintCmpOp
  object ConstraintCmpOp {
    case object Lt extends ConstraintCmpOp
    case object Gt extends ConstraintCmpOp
    case object Leq extends ConstraintCmpOp
    case object Geq extends ConstraintCmpOp
    case object Eq extends ConstraintCmpOp
    case object Neq extends ConstraintCmpOp
  }

  sealed trait Constraint {
    def negated: Constraint
  }
  case class ConstraintCmp(ty: ConstraintCmpOp, l: Argument, r: Argument) extends Constraint {
    override def negated: ConstraintCmp =
      ConstraintCmp(
        ty match {
          case ConstraintCmpOp.Lt => ConstraintCmpOp.Geq
          case ConstraintCmpOp.Gt => ConstraintCmpOp.Leq
          case ConstraintCmpOp.Leq => ConstraintCmpOp.Gt
          case ConstraintCmpOp.Geq => ConstraintCmpOp.Lt
          case ConstraintCmpOp.Eq => ConstraintCmpOp.Neq
          case ConstraintCmpOp.Neq => ConstraintCmpOp.Eq
        },
        l, r
      )
  }
  case class ConstraintMatch(pattern: Argument, argument: Argument) extends Constraint {
    override def negated: Constraint =
      throw new Exception("This does not make sense here...")
  }
  case class ConstraintContains(substring: Argument, argument: Argument) extends Constraint {
    override def negated: Constraint =
      throw new Exception("This does not make sense here...")
  }
  case object ConstraintTrue extends Constraint {
    override def negated: Constraint = ConstraintFalse
  }
  case object ConstraintFalse extends Constraint {
    override def negated: Constraint = ConstraintTrue
  }

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
  sealed trait Argument {
    def getType: TypeName = AnyType
  }
  case class ArgumentConstant(value: Constant) extends Argument {
    override def getType: TypeName = value match {
      case ConstantString(_) => SymbolType
      case ConstantNumber(_) => NumberType
      case ConstantUnsigned(_) => UnsignedType
      case ConstantFloat(_) => FloatType
    }
  }
  case class ArgumentVariable(name: String) extends Argument {
    def isWildcard: Boolean = name == "_"
  }
  case object ArgumentNil extends Argument {
    override def getType: TypeName = NilType
  }
  case class ArgumentList(args: Seq[Argument]) extends Argument
  case class ArgumentDollarFunctor(name: String, args: Seq[Argument]) extends Argument
  case class ArgumentSingle(arg: Argument) extends Argument
  case class ArgumentAlias(arg: Argument, ty: TypeName) extends Argument
  case class ArgumentFunctorCall(name: String, args: Seq[Argument]) extends Argument
  case class ArgumentAggregator(aggregator: Aggregator) extends Argument
  case class ArgumentUnOp(op: UnOp, arg: Argument) extends Argument
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
  case class ComponentDecl(ty: ComponentType, supers: Seq[ComponentType], bodies: Seq[ComponentBody]) extends SouffleStatement

  sealed trait ComponentBody
  case class ComponentBodyType(decl: TypeDecl) extends ComponentBody
  case class ComponentBodyRelation(relation: RelationDecl) extends ComponentBody
  case class ComponentBodyRule(rule: Rule) extends ComponentBody
  case class ComponentBodyFact(fact: Fact) extends ComponentBody
  case class ComponentBodyDirective(directive: Directive) extends ComponentBody
  case class ComponentBodyOverride(identifier: String) extends ComponentBody
  case class ComponentBodyComponentInit(init: ComponentInit) extends ComponentBody
  case class ComponentBodyComponentDecl(decl: ComponentDecl) extends ComponentBody

  // component_init ::= '.init' IDENT '=' component_type
  case class ComponentInit(name: String, ty: ComponentType) extends SouffleStatement

  // component_type ::= IDENT ( '<' IDENT ( ',' IDENT )* '>' )?
  case class ComponentType(name: String, arguments: Seq[String])

  // DIRECTIVE
  // directive ::=
  //    directive_qualifier qualified_name ( ',' qualified_name )*
  //        ( '(' ( IDENT '=' directive_value ( ',' IDENT '=' directive_value )* )? ')' )?
  case class Directive(qualifier: DirectiveQualifier,
                       qualifiedNames: Seq[QualifiedName],
                       params: Map[String, DirectiveValue] = Map.empty) extends SouffleStatement

  // FUNCTORS
  // functor_decl ::=
  //    '.functor' IDENT '(' ( attribute ( ',' attribute )* )? ')' ':' type_name 'stateful'?
  case class FunctorDecl(name: String, attributes: Seq[Attribute], returnType: TypeName, isStateful: Boolean = false) extends SouffleStatement

  sealed trait Functor
  case class UserDefinedFunctor(name: String) extends Functor

  sealed trait IntrinsicFunctor extends Functor
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
  // pragma ::= '.pragma' STRING STRING?
  case class Pragma(param: String, parameterValue: Option[String] = None) extends SouffleStatement
}
