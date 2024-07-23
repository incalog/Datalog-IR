package inca.frontend.functional.syntax

import cats.parse.{Numbers, Parser as P, Parser0 as P0}
import inca.ir.{Name, RefByQualifiedName}
import inca.ir.util.SourceLocation

import scala.language.implicitConversions

/**
 *  Parser for TIP programs, adapted for cats-parse from https://github.com/cs-au-dk/TIP/blob/master/src/tip/parser/TipParser.scala
 */
object Parser:

  implicit class Ploc[T](p: => P[T]) {
    def mapWithLoc[U <: SourceLocation](f: T => U): P[U] =
      val pv = p
      (P.index.with1 ~ pv ~ P.index).map {
        case ((start, t), end) =>
          val u = f(t)
          u.startIndex = start
          u.endIndex = end
          u
      }
  }

  def parseModule(source: String): Module =
    (whitespaces0 *> module <* P.end).parseAll(source) match
      case Right(p) => p
      case Left(err) => throw new IllegalArgumentException(s"Parse error at ${source.slice(err.failedAtOffset, err.failedAtOffset+10)}: $err")

  /* LEXICAL */

  val lineComment: P[Unit] = P.string("//") *> P.charsWhile0(c => c != '\n' && c != '\r').void
  val blockComment: P[Unit] = P.string("/*") *> P.recursive[Unit](rec =>
    P.product01(P.charsWhile0(c => c != '*').void, P.string("*/") | P.char('*') ~ rec).void
  )
  val comment: P[Unit] = lineComment | blockComment
  val whitespace: P[Unit] = (P.charIn(" \t\r\n").void | comment)
  val whitespaces0: P0[Unit] = whitespace.rep0.void

  def spaced[A](p: P[A]): P[A] =
    p <* whitespaces0

  val keywords = Set(
    "module",
    "import",
    "dl_import",
    "as",
    "private",
    "def",
    "data",
    "if",
    "let",
    "in",
    "match",
    "case",
    "true",
    "false",
    "fold"
  )

  def keyword(s: String): P[Unit] =
    if (!keywords.contains(s))
      throw new IllegalArgumentException(s"Not a keyword $s")
    else
      spaced(P.string(s) *> P.not(letterDigit))

  val letter: P[Unit] = P.ignoreCaseCharIn('a' to 'z').void
  val digit: P[Unit] = P.charIn('0' to '9').void
  val letterDigit: P[Unit] = P.charIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Some('_')).void
  val opSymbol: P[Unit] = P.charIn("!@#$%^&*()+=<>,.:?/\\_|").void

  val id: P[String] =
    (letter ~ letterDigit.rep0)
      .string
      .filter(s => !keywords.contains(s))

  val identifier: P[Name] =
    spaced(id).mapWithLoc(Name.apply)

  val qualifiedIdentifier: P[Name] =
    spaced(id ~ (P.char('.') ~ id).rep0).mapWithLoc((a,bs) => Name((a :: bs).mkString(".")))

  val qualifiedIdentifierComponents: P[Seq[Name]] =
    spaced(id ~ (P.char('.') *> id).rep0).map((a, bs) => (a :: bs).map(Name.apply))

  def inParens[A](p: P0[A]): P[A] =
    op('(') *> p <* op(')')

  def inBraces[A](p: P0[A]): P[A] =
    op('{') *> p <* op('}')

  def inBrackets[A](p: P0[A]): P[A] =
    op('[') *> p <* op(']')

  def list0[A](p: P[A]): P0[List[A]] =
    p.repSep0(op(','))

  def list[A](p: P[A]): P[List[A]] =
    p.repSep(op(',')).map(_.toList)

  val semi: P[Unit] =
    op(';')

  def op(c: Char): P[Unit] =
    spaced(P.char(c))

  def op(s: String): P[Unit] =
    spaced(P.string(s) *> P.not(letterDigit))

  def operator(s: String): P[Unit] =
    spaced(P.string(s) <* P.not(opSymbol))

  def oneOperator(ss: List[String]): P[String] =
    P.oneOf(ss.map(s => operator(s) *> P.pure(s)))

  /** Auxiliary */

  val visibility: P0[Visibility] = keyword("private").mapWithLoc(_ => Private())

  val mainFuncAnno: P[MainFunctionAnno] = op("@main").mapWithLoc(_ => MainFunctionAnno())

  val annotation: P[Annotation] = mainFuncAnno

  /** Types */

  def simpleType[Ty <: Type](s: String, t: Ty): P[Ty] = op(s).map(_ => t)

  val recType: P[Type] = P.defer(typ)

  val tupleType: P[TTuple] = inParens(recType.repSep0(op(','))).mapWithLoc(TTuple.apply)

  val setType: P[TSet] = op("Set") *> inBrackets(recType).mapWithLoc(TSet.apply)

  val atomicType: P[Type] =
    simpleType("Nothing",TNothing) |
    simpleType("Any", TAny) |
    simpleType("Unit", TTuple(Seq())) |
    tupleType | setType | identifier.mapWithLoc(TName.apply)

  lazy val typ: P[Type] =
    atomicType.flatMap ( t =>
      op("=>") *> recType.mapWithLoc(TFun(t, _)) |
      inBrackets(recType.repSep(op(','))).mapWithLoc(u => TApply(t, u.toList)) |
      P.pure(t)
    )

  /** Expressions */

  private val recExpression: P[Expression] = P.defer(expression)
  private val recInfixExp: P[Expression] = P.defer(infixExp)

  val setPredicate: P[Expression] =
    (recExpression ~ ((op("not").?.with1 <* op("in")) ~ recInfixExp).?).mapWithLoc {
      case (e, Some((hasNot, set))) => SetMember(e, set, hasNot.isDefined)
      case (e, None) => e
    }

  def setExpMore(e: Expression): P0[Expression] =
    (op(',') *> recExpression.repSep(op(','))).mapWithLoc(es => SetExp(e +: es.toList)) |
    (op('|') *> setPredicate.repSep(op(','))).mapWithLoc(es => SetComprehension(e, es.toList)) |
      P.pure(SetExp(Seq(e)))

  lazy val setExp: P[Expression] =
    inBraces((recExpression flatMap setExpMore) | P.index.map(_ => SetExp(Seq())))

  lazy val tupleExp: P[Expression] = inParens(recExpression.repSep0(op(','))).mapWithLoc {
    case e::Nil => e
    case es => Tuple(es)
  }

  val boolLit: P[BoolLit] = spaced(
    keyword("true").mapWithLoc(_ => BoolLit(true)) |
    keyword("false").mapWithLoc(_ => BoolLit(false))
  )

  val intLit: P[IntLit] = spaced(
    Numbers.signedIntString.mapWithLoc(s => IntLit(s.toInt))
  )

  val doubleLit: P[DoubleLit] = spaced(
    (Numbers.signedIntString ~ (P.char('.') *> Numbers.nonNegativeIntString)).mapWithLoc {
      case (a,b) => DoubleLit(s"$a.$b".toDouble)
    })

  val stringLit: P[StringLit] = spaced(
    P.char('"') *> P.charsWhile0(_ != '\"') <* P.char('"')
  ).mapWithLoc(StringLit.apply)

  val lambdaVars: P[Seq[(Name, Type)]] =
    inParens((identifier ~ (op(':') *> typ)).repSep0(op(",")))

  lazy val lambdaExp: P[Lambda] =
    (lambdaVars.backtrack ~ (op("=>") *> recExpression)).mapWithLoc(Lambda.apply)

  val unaryOperator: P[String] =
    oneOperator(List("-"))

  val unaryExp: P[Expression] =
    (unaryOperator ~ recInfixExp).mapWithLoc(UnOp.apply)

  val foldExp: P[SetFold] =
    (keyword("fold") *> inBrackets(typ).? ~
      inParens(recExpression.repSep0(op(',')))).flatMap {
      case (ty, init :: op :: set :: Nil) => P.pure(SetFold(ty, init, op, set))
      case (ty, args) => P.failWith(s"Wrong number of fold arguments, expected 3 but got ${args.size}: $args")
    }
    
  val queryExp: P[DlQuery] =
    (spaced(P.char('?')) *> qualifiedIdentifierComponents <* P.string("()")).map {
      case ns => DlQuery(RefByQualifiedName(ns)) 
    }

  lazy val atomicExp: P[Expression] =
      foldExp.backtrack |
      setExp |
      queryExp |
      lambdaExp |
      tupleExp |
      boolLit |
      stringLit |
      doubleLit.backtrack |
      intLit.backtrack |
      unaryExp |
      identifier.mapWithLoc(Var.apply)

  def callExpStep(e: Expression): P[Call] =
    (inBrackets(typ.repSep(op(','))).?.with1 ~ inParens(recExpression.repSep0(op(',')))).mapWithLoc {
      case (tys, args) => Call(e, tys.map(_.toList).getOrElse(Seq.empty), args)
    }

  def callExpRec(e: Expression): P0[Expression] =
    (callExpStep(e) flatMap callExpRec) | P.pure(e)

  lazy val callExp: P[Expression] =
    atomicExp flatMap callExpRec

  val pattern: P[Pattern] =
    (identifier ~ inParens(identifier.repSep0(op(',')))).mapWithLoc { case (name, args) => ConstructorPattern(name, args.map(PatternVariable.apply).toList) }

  lazy val matchCase: P[(Pattern,Expression)] =
    (keyword("case") *> pattern ~ op("=>") ~ recExpression).map { case ((p, _), e) => (p,e) }

  def matchExp(e: Expression): P[Match] =
    (keyword("match") *> inBraces(matchCase.rep)).mapWithLoc(cases => Match(e, cases.toList))

  val compareOperator: P[String] =
    oneOperator(List("==", ">=", "<=", "!=", "<", ">"))

  val additiveOperator: P[String] =
    oneOperator(List("++", "+", "-"))

  val multiplicativeOperator: P[String] =
    oneOperator(List("*", "/", "&", "%")).backtrack

  lazy val binMultiplicativeExpression: P[Expression] =
    (callExp ~ (multiplicativeOperator ~ P.defer(binMultiplicativeExpression)).?).mapWithLoc {
      case (lhs, Some(op, rhs)) => BinOp(lhs, op, rhs)
      case (lhs, None) => lhs
    }

  lazy val binAdditiveExpression: P[Expression] =
    (binMultiplicativeExpression ~ (additiveOperator ~ P.defer(binAdditiveExpression)).?).mapWithLoc {
      case (lhs, Some(op, rhs)) => BinOp(lhs, op, rhs)
      case (lhs, None) => lhs
    }

  lazy val binCompareExpression: P[Expression] =
    (binAdditiveExpression ~ (compareOperator ~ P.defer(binCompareExpression)).?).mapWithLoc {
      case (lhs, Some(op, rhs)) => BinOp(lhs, op, rhs)
      case (lhs, None) => lhs
    }

  lazy val binBoolAndExpression: P[Expression] =
    (binCompareExpression ~ (operator("&&") ~ P.defer(binBoolAndExpression)).?).mapWithLoc {
      case (lhs, Some(op, rhs)) => BinOp(lhs, "&&", rhs)
      case (lhs, None) => lhs
    }

  lazy val binBoolOrExpression: P[Expression] =
    (binBoolAndExpression ~ (operator("||") ~ P.defer(binBoolOrExpression)).?).mapWithLoc {
      case (lhs, Some(op, rhs)) => BinOp(lhs, "||", rhs)
      case (lhs, None) => lhs
    }

  val binOpExp: P[Expression] = binBoolOrExpression

  def infixExpRec(e: Expression): P0[Expression] =
    (infixExpStep(e) flatMap infixExpRec) | P.pure(e)

  def infixExpStep(e: Expression): P[Expression] =
    matchExp(e)

  lazy val infixExp: P[Expression] =
    binOpExp flatMap infixExpRec

  lazy val ifExp: P[If] =
    (keyword("if") *> inParens(recExpression) ~ recExpression ~ op("else") ~ recExpression).mapWithLoc {
      case (((cond, thn), _), els) => If(cond, thn, els)
    }

  val letBindings: P[Seq[Name]] =
    inParens(identifier.repSep0(op(',')).map(_.toList)) | identifier.map(n => Seq(n))

  lazy val letExp: P[Let] =
    (keyword("let") *> letBindings ~ (op(':') *> typ).? ~ op('=') ~ binOpExp ~ keyword("in") ~ recExpression).mapWithLoc {
      case (((((names, ty), _), bound), _), body) => Let(names, ty, bound, body)
    }

  lazy val expression: P[Expression] = ifExp | letExp | infixExp


  /** Definitions */

  val typeParam: P[ParametricType] = identifier.mapWithLoc(ParametricType.apply)

  val typeParams: P0[Seq[ParametricType]] =
    inBrackets(typeParam.repSep0(op(","))) | P.pure(Seq())

  val param: P[Param] = (identifier ~ op(":") ~ typ).mapWithLoc {
    case ((name, _), typeAnno) => Param(name, typeAnno)
  }

  val params: P0[Seq[Param]] =
    inParens(param.repSep0(op(","))) | P.pure(Seq())

  val dataConstructor: P[DataConstructor] =
    (identifier ~ inParens(typ.repSep0(op(',')))).mapWithLoc(DataConstructor.apply)

  val dataDef: P[DataDef] =
    ((annotation.rep0 ~ visibility.?).with1 ~
      keyword("data") ~ identifier ~ typeParams ~
      op("=") ~ dataConstructor.repSep(op('|'))).mapWithLoc {
      case ((((((annos, vis), _), name), tyParams), _), constrs) => DataDef(annos, vis, name, tyParams, constrs.toList)
    }

  val functionDef: P[FunctionDef] =
    ((annotation.rep0 ~ visibility.?).with1 ~
      keyword("def") ~ identifier ~ typeParams ~ params ~
      op(":") ~ typ ~ op("=") ~ expression).mapWithLoc {
      case (((((((((annos, vis), _), name), tyParams), params), _), ty), _), body) =>
        FunctionDef(annos, vis, name, tyParams, params, ty, body)
    }

  val content: P[ModuleContent] =
    functionDef | dataDef

  val impor: P[Import] =
    keyword("import") *> qualifiedIdentifier.mapWithLoc(Import.apply)

  val relation: P[RelationDecl] = (identifier ~ params).map {
    case (n, ps) => RelationDecl(n, ps)
  }

  val dlImpor: P[DlImport] =
    (((keyword("dl_import") *> identifier) <* keyword("as")) ~ qualifiedIdentifier ~ inBraces(relation.rep0)).map {
      case ((m, n), rels) => DlImport(m, n, rels)
    }

  val module: P[Module] =
    whitespaces0.with1 *>
    keyword("module") *> (qualifiedIdentifier ~ (impor | dlImpor).rep0 ~ content.rep0)
      .mapWithLoc { case ((name, imports),contents) => Module(name, imports, contents) }
