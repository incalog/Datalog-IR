package inca.codeql.syntax

import cats.parse.{Numbers, Parser as P, Parser0 as P0}
import _root_.inca.ir.Name
import _root_.inca.ir.util.SourceLocation

object Parser:
  implicit private class ParserLocationOps[T](p: P[T]):
    def mapWithLoc[U <: SourceLocation](f: T => U): P[U] =
      (P.index.with1 ~ p ~ P.index).map { case ((start, value), end) =>
        val result = f(value)
        result.startIndex = start
        result.endIndex = end
        result
      }

  implicit private class Parser0LocationOps[T](p: P0[T]):
    def mapWithLoc0[U <: SourceLocation](f: T => U): P0[U] =
      (P.index ~ p ~ P.index).map { case ((start, value), end) =>
        val result = f(value)
        result.startIndex = start
        result.endIndex = end
        result
      }

  def parseProgram(source: String): Program =
    program.parseAll(source) match
      case Right(value) => value
      case Left(error) =>
        val near = source.slice(error.failedAtOffset, (error.failedAtOffset + 30).min(source.length))
        throw IllegalArgumentException(s"CodeQL parse error near '$near': $error")

  private val lineComment: P[Unit] = P.string("//") *> P.charsWhile0(c => c != '\n' && c != '\r').void
  private val blockComment: P[Unit] = P.string("/*") *> P.recursive[Unit] { rec =>
    P.product01(P.charsWhile0(_ != '*').void, P.string("*/") | (P.char('*') *> rec)).void
  }
  private val whitespace: P[Unit] = P.charIn(" \t\r\n").void | lineComment | blockComment
  private val whitespaces0: P0[Unit] = whitespace.rep0.void

  private def spaced[A](p: P[A]): P[A] = p <* whitespaces0
  private val letter: P[Unit] = P.ignoreCaseCharIn('a' to 'z').void
  private val letterDigit: P[Unit] = P.charIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Seq('_')).void

  private val keywords = Set(
    "abstract", "and", "as", "boolean", "class", "date", "exists", "extends", "external", "false",
    "final", "float", "from", "in", "instanceof", "int", "not", "or", "override", "predicate", "query",
    "select", "string", "this", "true", "where"
  )

  private def keyword(value: String): P[Unit] = spaced(P.string(value) <* P.not(letterDigit))
  private def symbol(value: String): P[Unit] = spaced(P.string(value))
  private val rawIdentifier: P[String] = (letter ~ letterDigit.rep0).string.filter(value => !keywords.contains(value))
  val identifier: P[Name] = spaced(rawIdentifier).mapWithLoc(Name.apply)

  private def inParens[A](p: P0[A]): P[A] = symbol("(") *> p <* symbol(")")
  private def inBraces[A](p: P0[A]): P[A] = symbol("{") *> p <* symbol("}")

  val qlType: P[QlType] =
    keyword("int").as(QlType.IntType) |
      keyword("float").as(QlType.FloatType) |
      keyword("string").as(QlType.StringType) |
      keyword("boolean").as(QlType.BooleanType) |
      keyword("date").as(QlType.DateType) |
      identifier.map(name => QlType.EntityType(name))

  val variableDecl: P[VariableDecl] = (qlType ~ identifier).mapWithLoc(VariableDecl.apply)

  private val escapedChar: P[Char] = (P.char('\\') *> P.anyChar).map {
    case 'n' => '\n'
    case 'r' => '\r'
    case 't' => '\t'
    case '"' => '"'
    case '\\' => '\\'
    case other => other
  }
  private val stringChar: P[Char] = escapedChar.backtrack | P.charWhere(c => c != '"' && c != '\\')
  private val stringLiteral: P[Literal] = spaced(P.char('"') *> stringChar.rep0.map(_.mkString) <* P.char('"'))
    .mapWithLoc(Literal.StringValue.apply)
  private val floatLiteral: P[Literal] = spaced(
    (Numbers.nonNegativeIntString ~ (P.char('.') *> Numbers.nonNegativeIntString)).string
  ).mapWithLoc(value => Literal.FloatValue(value.toDouble))
  private val intLiteral: P[Literal] = spaced(Numbers.nonNegativeIntString)
    .mapWithLoc(value => Literal.IntValue(value.toInt))
  private val booleanLiteral: P[Literal] =
    keyword("true").as(Literal.BooleanValue(true)) |
      keyword("false").as(Literal.BooleanValue(false))
  val literal: P[Literal] = stringLiteral | floatLiteral.backtrack | intLiteral | booleanLiteral

  private lazy val expressionPrimary: P[Expr] =
    literal.mapWithLoc(Expr.Constant.apply) |
      symbol("_").as(Expr.Wildcard) |
      keyword("this").mapWithLoc(_ => Expr.Var(Name("this"))) |
      (identifier ~ inParens(P.defer(expression).repSep0(symbol(","))).?).mapWithLoc {
        case (name, Some(args)) => Expr.Call(name, args)
        case (name, None) => Expr.Var(name)
      } |
      inParens(P.defer(expression))

  private lazy val memberSuffix: P[(Name, Seq[Expr])] =
    symbol(".") *> identifier ~ inParens(P.defer(expression).repSep0(symbol(",")))

  private lazy val expressionAtom: P[Expr] =
    (expressionPrimary ~ memberSuffix.backtrack.rep0).mapWithLoc { case (receiver, members) =>
      members.foldLeft(receiver) { case (current, (name, args)) => Expr.MemberCall(current, name, args) }
    }

  private lazy val unaryExpression: P[Expr] =
    ((symbol("-").as("-") | symbol("+").as("+")) ~ P.defer(unaryExpression)).mapWithLoc(Expr.Unary.apply) |
      expressionAtom

  private def binaryLevel(next: => P[Expr], operators: Seq[String]): P[Expr] =
    val op = P.oneOf(operators.sortBy(-_.length).map(value => symbol(value).as(value)).toList)
    (next ~ (op ~ next).rep0).mapWithLoc { case (head, tail) =>
      tail.foldLeft(head) { case (lhs, (operator, rhs)) => Expr.Binary(lhs, operator, rhs) }
    }

  private lazy val multiplicative: P[Expr] = binaryLevel(unaryExpression, Seq("*", "/", "%"))
  private lazy val additive: P[Expr] = binaryLevel(multiplicative, Seq("+", "-"))
  lazy val expression: P[Expr] = additive

  private val comparator: P[String] = P.oneOf(
    Seq("!=", "<=", ">=", "=", "<", ">").map(value => symbol(value).as(value)).toList
  )

  private lazy val existsFormula: P[Formula] =
    (keyword("exists") *> inParens(variableDecl.repSep(symbol(",")) ~ (symbol("|") *> P.defer(formula))))
      .mapWithLoc { case (vars, body) => Formula.Exists(vars.toList, body) }

  private lazy val formulaAtom: P[Formula] =
    existsFormula |
      inParens(P.defer(formula)) |
      (expression ~ (
        (keyword("in") *> (symbol("[") *> expression ~ (symbol("..") *> expression) <* symbol("]"))).map(Left.apply) |
          (symbol("=") *> (symbol("[") *> expression ~ (symbol("..") *> expression) <* symbol("]"))).backtrack.map(Left.apply) |
          (comparator ~ expression).map(Right.apply)
      ).?).flatMap {
        case (value, Some(Left((lower, upper)))) => P.pure(Formula.InRange(value, lower, upper))
        case (lhs, Some(Right((op, rhs)))) => P.pure(Formula.Compare(lhs, op, rhs))
        case (Expr.Call(name, args), None) => P.pure(Formula.Call(name, args))
        case (Expr.MemberCall(receiver, name, args), None) => P.pure(Formula.MemberCall(receiver, name, args))
        case (Expr.Constant(Literal.BooleanValue(value)), None) => P.pure(Formula.Truth(value))
        case _ => P.failWith("Expected a predicate call or formula comparison")
      }

  private lazy val negatedFormula: P[Formula] =
    (keyword("not") *> P.defer(negatedFormula)).mapWithLoc(Formula.Not.apply) | formulaAtom

  private lazy val conjunction: P[Formula] = negatedFormula.repSep(keyword("and")).mapWithLoc { parts =>
    if parts.tail.isEmpty then parts.head else Formula.And(parts.toList)
  }
  private lazy val disjunction: P[Formula] = conjunction.repSep(keyword("or")).mapWithLoc { parts =>
    if parts.tail.isEmpty then parts.head else Formula.Or(parts.toList)
  }
  lazy val formula: P[Formula] = disjunction

  private case class PredicateHeader(name: Name, params: Seq[VariableDecl], resultType: Option[QlType])

  private val predicateHeader: P[PredicateHeader] =
    (keyword("predicate") *> identifier ~ inParens(variableDecl.repSep0(symbol(","))))
      .map { case (name, params) => PredicateHeader(name, params, None) } |
      (qlType ~ identifier ~ inParens(variableDecl.repSep0(symbol(",")))).map {
        case ((resultType, name), params) => PredicateHeader(name, params, Some(resultType))
      }

  private val modifier: P[String] = keyword("external").as("external") | keyword("query").as("query")

  val predicateDecl: P[PredicateDecl] =
    (modifier.rep0.with1 ~ predicateHeader ~ (inBraces(formula).map(Some.apply) | symbol(";").as(None))).flatMap {
      case ((modifiers, header), body) =>
        val external = modifiers.contains("external")
        if external && body.nonEmpty then P.failWith("External predicates must end with ';'")
        else if !external && body.isEmpty then P.failWith("Only external predicates may omit their body")
        else P.pure(PredicateDecl(header.name, header.params, header.resultType, body, external, modifiers.contains("query")))
    }

  private enum ClassEntry:
    case Characteristic(body: Formula)
    case Field(field: VariableDecl)
    case Member(member: MemberPredicateDecl)

  private val classModifier: P[String] =
    keyword("abstract").as("abstract") | keyword("final").as("final")

  private val memberModifier: P[String] = keyword("override").as("override")

  private def characteristic(className: Name): P[ClassEntry] =
    (spaced(P.string(className.name)) *> inParens(P.pure(())) *> inBraces(formula))
      .map(ClassEntry.Characteristic.apply)

  private val memberPredicate: P[ClassEntry] =
    (memberModifier.rep0.with1 ~ predicateHeader ~ inBraces(formula)).map { case ((modifiers, header), body) =>
      ClassEntry.Member(MemberPredicateDecl(
        header.name,
        header.params,
        header.resultType,
        body,
        isOverride = modifiers.contains("override")
      ))
    }

  private val field: P[ClassEntry] = (variableDecl <* symbol(";")).map(ClassEntry.Field.apply)

  private def classBody(className: Name): P[(Seq[VariableDecl], Option[Formula], Seq[MemberPredicateDecl])] =
    inBraces((characteristic(className).backtrack | memberPredicate.backtrack | field).rep0).flatMap { entries =>
      val characteristics = entries.collect { case ClassEntry.Characteristic(body) => body }
      if characteristics.size > 1 then P.failWith(s"Class $className has more than one characteristic predicate")
      else P.pure((
        entries.collect { case ClassEntry.Field(value) => value },
        characteristics.headOption,
        entries.collect { case ClassEntry.Member(value) => value }
      ))
    }

  private val typeList: P[Seq[QlType]] = qlType.repSep(symbol(",")).map(_.toList)

  private val classInheritance: P0[(Seq[QlType], Seq[QlType])] =
    (keyword("extends") *> typeList).?.map(_.getOrElse(Seq.empty)) ~
      (keyword("instanceof") *> typeList).?.map(_.getOrElse(Seq.empty))

  val classDecl: P[ClassDecl] =
    (classModifier.rep0.with1 ~ (keyword("class") *> identifier)).flatMap { case (modifiers, name) =>
      (classInheritance.with1 ~ classBody(name)).mapWithLoc {
        case ((bases, instanceOf), (fields, characteristic, members)) =>
          ClassDecl(
            name,
            bases,
            instanceOf,
            fields,
            characteristic,
            members,
            isAbstract = modifiers.contains("abstract"),
            isFinal = modifiers.contains("final")
          )
      }
    }

  private val selectColumn: P[SelectColumn] =
    (expression ~ (keyword("as") *> identifier).?).mapWithLoc(SelectColumn.apply)

  private val selectTail: P[(Option[Formula], List[SelectColumn])] =
    (keyword("where") *> formula).?.with1 ~
      (keyword("select") *> selectColumn.repSep(symbol(",")).map(_.toList))

  val selectQuery: P[SelectQuery] =
    ((keyword("from") *> variableDecl.repSep(symbol(","))).?.map(_.fold(Seq.empty)(_.toList)).with1 ~ selectTail)
      .mapWithLoc {
      case (from, (where, columns)) => SelectQuery(from, where, columns)
    }

  val program: P0[Program] =
    (whitespaces0 *> (classDecl.backtrack.map(Left.apply) | predicateDecl.backtrack.map(Right.apply)).rep0 ~ selectQuery.? <* P.end).mapWithLoc0 {
      case (declarations, query) => Program(
        declarations.collect { case Right(predicate) => predicate },
        declarations.collect { case Left(clazz) => clazz },
        query
      )
    }
