package inca.frontend.souffle

import Syntax._
import cats.parse.Parser.not
import Console.{RED, RESET, UNDERLINED}
import cats.parse.{Numbers, Parser => P, Parser0 => P0}

case class ParseException(message: String) extends Exception(message)

object Parser {

  // HELPERS

  object Literals {
    val quotes: P[Unit] = P.char('"')
    val colon: P[Unit] = P.char(':')
    val semicolon: P[Unit] = P.char(';')
    val underscore: P[Unit] = P.char('_')
    val comma: P[Unit] = P.char(',')
    val pipe: P[Unit] = P.char('|')

    val digit: P[Char] = cats.parse.Rfc5234.digit
    val letter: P[Char] = cats.parse.Rfc5234.alpha
    val identifier: P[String] =
      ((letter | underscore.as('_')) ~ (letter | digit | underscore.as('_')).rep0).map {
        case (c, s) => s"$c${s.mkString}"
      }

    val string: P[String] = quotes *> P.until0(quotes) <* quotes
    val unsigned: P[Int] = Numbers.nonNegativeIntString.map(_.toInt)
    val number: P[Int] = Numbers.signedIntString.map(_.toInt)

    val sign: P[String] = P.charIn("+-").map(_.toString)
    val digits: P[String] = digit.rep.map(_.toList.mkString)

    val float: P[Float] =
        // +1.5, -.42
        (sign.?.with1 ~
          (digits.?.with1 <* P.char('.')) ~
          digits
        ).backtrack
          .map { case ((a, b), c) => (a.getOrElse("") + b.getOrElse("") + "." + c).toFloat } |
        // 1e-5
        (sign.?.with1 ~
          (digits <* P.char('e')) ~
          sign.? ~
          digits
        ).map { case (((a, b), c), d) => (a.getOrElse("") + b + "e" + c.getOrElse("") + d).toFloat }
  }

  object Separators {
    val comma: P[Unit] = spaced(Literals.comma)
    val colon: P[Unit] = spaced(Literals.colon)
    val semicolon: P[Unit] = spaced(Literals.semicolon)
    val pipe: P[Unit] = spaced(Literals.pipe)
  }

  val lineComment: P[Unit] = P.string("//") *> P.charsWhile0(c => c != '\n' && c != '\r').void
  val blockComment: P[Unit] = P.string("/*") *> P.recursive[Unit](rec =>
    P.product01(P.charsWhile0(c => c != '*').void, P.string("*/") | P.char('*') ~ rec).void
  )
  val comment: P[Unit] = lineComment | blockComment
  val oneWhitespace: P[Unit] = P.charIn(" \t\n\r").void | comment
  val whitespace: P0[Unit] = oneWhitespace.rep0.void

  def spaced[A](p: P0[A]): P0[A] = p <* whitespace
  def spaced[A](p: P[A]): P[A] = p <* whitespace

  def parens[A](p: P0[A]): P[A] = spaced(P.char('(')) *> spaced(p) <* spaced(P.char(')'))
  def brackets[A](p: P0[A]): P[A] = spaced(P.char('[')) *> spaced(p) <* spaced(P.char(']'))
  def braces[A](p: P0[A]): P[A] = spaced(P.char('{')) *> spaced(p) <* spaced(P.char('}'))

  def keyword(kw: String): P[Unit] = spaced(P.string(kw) <* oneWhitespace)

  // TYPES

  val typename: P[TypeName] =
    P.string("symbol").as(SymbolType) |
    P.string("number").as(NumberType) |
    P.string("unsigned").as(UnsignedType) |
    P.string("float").as(FloatType) |
    P.defer(qualifiedName).map(name => DeclaredType(name.toString))

  val typeDeclSubtype: P[TypeDeclSubtype] = {
    (keyword(".type") *> spaced(Literals.identifier) <* keyword("<:")) ~
      spaced(typename)
  }.map { case (sub, sup) => TypeDeclSubtype(sub, sup) }

  val typeDeclUnion: P[TypeDeclUnion] = {
    (keyword(".type") *> spaced(Literals.identifier) <* keyword("=")) ~
      spaced(typename).repSep(Separators.pipe)
  }.map { case (name, tys) => TypeDeclUnion(name, tys.toList) }

  // record_list ::= "[" attribute ( "," attribute)* "]"
  val recordList: P[Seq[Attribute]] = {
    brackets(relationAttribute.repSep(Separators.comma))
      .map(_.toList)
  }

  val typeDeclRecord: P[TypeDeclRecord] = {
    (keyword(".type") *> spaced(Literals.identifier) <* keyword("=")) ~
    recordList
  }.map { case (name, records) => TypeDeclRecord(name, records) }

  val adtBranch: P[ADTBranch] = {
    spaced(Literals.identifier) ~ braces(relationAttribute.repSep(Separators.comma))
  }.map { case (id, attributes) => ADTBranch(id, attributes.toList) }

  val typeDeclADT: P[TypeDeclADT] = {
    (keyword(".type") *> spaced(Literals.identifier) <* keyword("=")) ~
    adtBranch.repSep(Separators.pipe)
  }.map { case (name, branches) => TypeDeclADT(name, branches.toList) }

  val typeDecl: P[TypeDecl] =
    typeDeclSubtype.backtrack |
    typeDeclRecord.backtrack |
    typeDeclADT.backtrack |
    typeDeclUnion

  // EXPRESSIONS

  val variable: P[Expression] =
    Literals.string.map(StringValue.apply) |
    Literals.number.map(NumberValue.apply) |
    Literals.float.map(FloatValue.apply)

  // AGGREGATOR

  val aggregatorCondition: P[AggregatorCondition] =
    braces(P.defer(disjunction)).map(AggregatorConditionDisjunction.apply) |
    P.defer(atom).map(AggregatorConditionAtom.apply)

  val prefixAggregator: P[Aggregator] = {
    (keyword("min").backtrack.as("min") |
      keyword("max").backtrack.as("max") |
      keyword("mean").backtrack.as("mean") |
      keyword("sum").backtrack.as("sum")) ~
      (spaced(P.defer(argument)) <* Separators.colon) ~ spaced(aggregatorCondition)
  }.map { case ((op, arg), cond) => op match {
    case "min" => AggregatorMin(arg, cond)
    case "max" => AggregatorMax(arg, cond)
    case "mean" => AggregatorMean(arg, cond)
    case "sum" => AggregatorSum(arg, cond)
  }}

  val aggregatorCount: P[AggregatorCount] = {
    keyword("count") *> Separators.colon *> spaced(aggregatorCondition)
  }.map(AggregatorCount.apply)

  val aggregatorRange: P[AggregatorRange] = {
    P.string("range") *>
      parens(
        (spaced(P.defer(argument)) <* Separators.comma) ~
        spaced(P.defer(argument)) ~
        (Separators.comma *> spaced(P.defer(argument))).?
      )
  }.map { case ((arg1, arg2), arg3) => AggregatorRange(arg1, arg2, arg3) }

  val aggregator: P[Aggregator] =
    prefixAggregator | aggregatorCount | aggregatorRange

  // RELATIONS

  lazy val relationAttribute: P[Attribute] =
    ((spaced(Literals.identifier) <* spaced(Literals.colon)) ~
      spaced(typename))
      .map { case (n, t) => Attribute(n, t) }

  val qualifierMap: Map[String, RelationQualifier] = Map(
    "override" -> OverrideQualifier,
    "inline" -> InlineQualifier,
    "no_inline" -> NoInlineQualifier,
    "magic" -> MagicQualifier,
    "no_magic" -> NoMagicQualifier,
    "brie" -> BrieQualifier,
    "btree" -> BtreeQualifier,
    "eqrel" -> EquivalenceQualifier,
  )

  val qualifier: P[RelationQualifier] =
    P.stringIn(qualifierMap.keys).map(qualifierMap.apply)

  val choiceDomain: P[ChoiceDomain] =
    keyword("choice-domain") *>
      spaced(
        Literals.identifier.map(Seq(_)) |
          parens(spaced(Literals.identifier).repSep(Separators.comma)).map(_.toList)
      ).repSep(Separators.comma).map(_.toList.flatten)
        .map(ChoiceDomain.apply)

  val relationDecl: P[RelationDecl] = {
    (
      /* name */ (keyword(".decl") *> spaced(Literals.identifier)) ~
      /* attributes */ spaced(parens(relationAttribute.repSep0(Separators.comma))) ~
      /* qualifiers */ spaced(qualifier).repUntil0(not(qualifier)) ~
      /* choice domain */ choiceDomain.?
    ).map {
      case (((name, attr), qualifiers), choiceDomain) =>
        RelationDecl(name, attr, qualifiers, choiceDomain)
    }
  }

  lazy val qualifiedName: P[QualifiedName] =
    Literals.identifier.repSep(P.char('.')).map(l => QualifiedName(l.toList))

  val constant: P[Constant] =
    Literals.float.backtrack.map(ConstantFloat.apply) |
    Literals.unsigned.backtrack.map(ConstantUnsigned.apply) |
    Literals.number.backtrack.map(ConstantNumber.apply) |
    Literals.string.map(ConstantString.apply)

  val argumentList: P[Seq[Argument]] =
    P.defer(spaced(argument)).repSep(Separators.comma).map(_.toList)

  val binOpMap: Map[String, BinOp] = Map(
    "+" -> BinOpAdd,
    "-" -> BinOpMinus,
    "*" -> BinOpMult,
    "/" -> BinOpDiv,
    "%" -> BinOpMod,
    "^" -> BinOpPow,
    "land" -> BinOpLAnd,
    "lor" -> BinOpLOr,
    "lxor" -> BinOpLXor,
    "band" -> BinOpBAnd,
    "bor" -> BinOpBOr,
    "bxor" -> BinOpBXor,
    "bshl" -> BinOpBShl,
    "bshr" -> BinOpBShr,
    "bshru" -> BinOpBShrU,
  )

  val binOp: P[BinOp] = P.stringIn(binOpMap.keys).map(binOpMap.apply)

  lazy val argumentAtom: P[Argument] = {
    P.string("nil").as(ArgumentNil) |
    (keyword("bnot") *> spaced(P.defer(argument))).map(ArgumentUnOp(UnOpBNot, _)) |
    (keyword("lnot") *> spaced(P.defer(argument))).map(ArgumentUnOp(UnOpLNot, _)) |
    (keyword("-") *> spaced(P.defer(argument))).map(ArgumentUnOp(UnOpMinus, _)) |
    aggregator.map(ArgumentAggregator.apply) |
    (keyword("as") *> parens((spaced(P.defer(argument)) <* Separators.comma) ~ typename))
      .map { case (arg, ty) => ArgumentAlias(arg, ty) } |
    (spaced(Literals.identifier) ~ parens(argumentList))
      .map { case (name, args) => ArgumentFunctorCall(name, args.toList) }.backtrack |
    (Literals.identifier | P.char('_').as("_")).map(ArgumentVariable.apply) |
    (P.char('$') *> Literals.identifier ~ parens(argumentList).?)
      .map { case (name, args) => ArgumentDollarFunctor(name, args.getOrElse(Seq.empty)) } |
    constant.map(ArgumentConstant.apply) |
    brackets(argumentList).map(l => ArgumentList(l.toList)) |
    parens(P.defer(argument)).map(ArgumentSingle.apply)
  }

  lazy val argument: P[Argument] = {
    spaced(P.defer(argumentAtom)) ~ (spaced(binOp) ~ spaced(P.defer(argument))).?
  }.map { case (arg1, tail) => tail match {
      case Some((op, arg2)) => ArgumentBinOp(op, arg1, arg2)
      case None => arg1
    }
  }

  lazy val atom: P[Atom] =
    (spaced(qualifiedName) ~ parens(spaced(argument).repSep(Separators.comma).?))
      .map {
        case (qn, args) => Atom(qn, if (args.isDefined) args.get.toList else Seq.empty)
      }

  val fact: P[Fact] = atom.map(Fact.apply) <* spaced(P.char('.'))

  val negation: P0[Boolean] = P.char('!').rep0.map(_.length % 2 == 1)

  val constraintCmpOpInfix: P[ConstraintCmpOp] =
    P.string("<=").as(ConstraintCmpOp.Leq) |
    P.string("<").as(ConstraintCmpOp.Lt) |
    P.string(">=").as(ConstraintCmpOp.Geq) |
    P.string(">").as(ConstraintCmpOp.Gt) |
    P.string("=").as(ConstraintCmpOp.Eq) |
    P.string("!=").as(ConstraintCmpOp.Neq)

  val constraint: P[Constraint] =
    (
      (P.string("match") | P.string("contains")).string ~
      parens((spaced(argument) <* Separators.comma) ~ spaced(argument))
    ).map { case (op, (l, r)) => op match {
      case "match" => ConstraintMatch(l, r)
      case "contains" => ConstraintContains(l, r)
    }}.backtrack |
    (spaced(argument) ~ spaced(constraintCmpOpInfix) ~ spaced(argument)).map {
      case ((l, op), r) => ConstraintCmp(op, l, r)
    }

  val conjunctionTerm: P[ConjunctionTerm] =
    (
      spaced(negation) ~
      spaced(constraint.backtrack | atom | parens(P.defer(disjunction)))
    ).map {
      case (negated, atom: Atom) => ConjunctionTermAtom(negated, atom)
      case (negated, constraint: Constraint) => ConjunctionTermConstraint(negated, constraint)
      case (negated, disjunction: Disjunction) => ConjunctionTermDisjunction(negated, disjunction)
      case _ => throw ParseException("Failed to parse conjunction term!")
    }.asInstanceOf[P[ConjunctionTerm]]

  val conjunction: P[Conjunction] =
    conjunctionTerm.repSep(Separators.comma).map(l => Conjunction(l.toList))

  lazy val disjunction: P[Disjunction] =
    spaced(conjunction).repSep(Separators.semicolon).map(l => Disjunction.apply(l.toList))

  val queryPlan: P[QueryPlan] =
    keyword(".plan") *>
    (
      (spaced(Literals.number) <* spaced(Literals.colon)) ~
        spaced(parens(Literals.number.repSep0(Separators.comma)))
    ).repSep(Separators.comma).map(l => QueryPlan(l.toList))

  // rule ::= atom ( ',' atom )* ':-' disjunction '.' query_plan?
  val rule: P[Rule] = {
    (
      (spaced(atom).repSep(Separators.comma) <* spaced(P.string(":-"))) ~
      (disjunction <* spaced(P.char('.'))) ~
      queryPlan.?
    ).map { case ((atoms, disjunction), qp) => Rule(atoms.toList, disjunction, qp) }
  }

  // DIRECTIVE

  val directiveQualifier: P[DirectiveQualifier] =
    P.string(".input").as(DirectiveQualifierInput) |
    P.string(".output").as(DirectiveQualifierOutput) |
    P.string(".printsize").as(DirectiveQualifierPrintsize) |
    P.string(".limitsize").as(DirectiveQualifierLimitsize)

  val directiveValue: P[DirectiveValue] = {
    P.string("true").as(DirectiveValueBool(true)) |
    P.string("false").as(DirectiveValueBool(false)) |
    Literals.number.map(DirectiveValueNumber.apply) |
    Literals.identifier.map(DirectiveValueIdent.apply) |
    Literals.string.map(DirectiveValueString.apply)
  }

  val directiveMapping: P[(String, DirectiveValue)] =
    (spaced(Literals.identifier) <* spaced(P.char('='))) ~ spaced(directiveValue)

  val directive: P[Directive] =
    (
      spaced(directiveQualifier) ~
      spaced(qualifiedName).repSep(Separators.comma) ~
      spaced(parens(spaced(directiveMapping).repSep(Separators.comma))).?
    ).map {
      case ((q, n), m) => Directive(q, n.toList, m match {
        case Some(l) => Map.from(l.toList)
        case None => Map.empty
      })
    }

  // COMPONENT DECL

  val componentType: P[ComponentType] = {
    Literals.identifier ~
    (spaced(P.char('<')) *> spaced(Literals.identifier).repSep(Separators.comma) <* spaced(P.char('>'))).?
  }.map { case (name, arguments) => ComponentType(name, arguments.map(_.toList).getOrElse(Seq())) }

  val componentBody: P[ComponentBody] =
    typeDecl.map(ComponentBodyType.apply) |
    relationDecl.map(ComponentBodyRelation.apply) |
    fact.backtrack.map(ComponentBodyFact.apply) |
    rule.map(ComponentBodyRule.apply) |
    directive.map(ComponentBodyDirective.apply) |
    (P.string(".override") *> Literals.identifier).map(ComponentBodyOverride.apply) |
    P.defer(componentInit).map(ComponentBodyComponentInit.apply) |
    P.defer(componentDecl).map(ComponentBodyComponentDecl.apply)

  lazy val componentDecl: P[ComponentDecl] = {
    (keyword(".comp") *> spaced(componentType)) ~
    (spaced(Literals.colon) *> spaced(componentType).repSep(Separators.comma)).? ~
    braces(spaced(componentBody).rep)
  }.map {
    case ((ty, supers), bodies) =>
      ComponentDecl(ty, supers.map(_.toList).getOrElse(Seq()), bodies.toList)
  }

  lazy val componentInit: P[ComponentInit] = {
    (keyword(".init") *> spaced(Literals.identifier) <* spaced(P.char('='))) ~
    spaced(componentType)
  }.map { case (name, ty) => ComponentInit(name, ty) }

  // SubsumptiveRule
  // rule ::= atom '<=' atom ':-' disjunction '.' query_plan?
  lazy val subsumptiveRule: P[SubsumptiveRule] = {
    (
      (
        (spaced(atom) <* spaced(P.string("<="))) ~ spaced(atom)
        ) ~
        (spaced(P.string(":-")) *> disjunction <* spaced(P.char('.'))) ~
        queryPlan.?
      ).map { case (((atom1, atom2), disjunction), qp) => SubsumptiveRule(atom1, atom2, disjunction, qp) }
  }

  //FunctorDecl
  // functor_decl ::= '.functor' IDENT '(' ( attribute ( ',' attribute )* )? ')' ':' type_name 'stateful'?
  lazy val functorDecl: P[FunctorDecl] = {
    ((keyword(".functor") *> Literals.identifier ~
      spaced(parens(relationAttribute.repSep0(Separators.comma)))) ~
      (spaced(Literals.colon) *> spaced(typename)) ~
      keyword("stateful").?
      ).map {
      case (((name, attributes), returnType), None) => FunctorDecl(name, attributes, returnType)
      case (((name, attributes), returnType), Some(())) => FunctorDecl(name, attributes, returnType, true)
    }
  }

  // PRAGMA
  val pragma: P[Pragma] =
    (keyword(".pragma") *> spaced(Literals.string) ~ spaced(Literals.string).?)
      .map { case (param, value) => Pragma(param, value) }

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

  val program: P[SouffleProgram] = (
    pragma |
    functorDecl |
    fact.backtrack |
    subsumptiveRule.backtrack |
    rule |
    relationDecl |
    directive |
    typeDecl |
    componentDecl |
    componentInit
  ).rep.map(_.toList)

  // PARSE METHODS

  def parse[A](p: P[A], source: String): A = p.surroundedBy(whitespace).parseAll(source) match {
    case Left(err) =>
      val errorChar = if (err.failedAtOffset < source.length) source(err.failedAtOffset) else " "
      System.out.print(source.substring(0, err.failedAtOffset))
      System.out.print(s"$RED$UNDERLINED$errorChar$RESET")
      if (err.failedAtOffset < source.length)
        System.out.print(source.substring(err.failedAtOffset + 1))

      System.out.print("\n")
      System.out.flush()

      throw ParseException(s"Parser error! Expected: ${err.expected}")
    case Right(value) => value
  }

  def parse(source: String): SouffleProgram = parse(program, source)

  /*  TODO: Missing parsers:
  * Intrinsic functors
  * */
}
