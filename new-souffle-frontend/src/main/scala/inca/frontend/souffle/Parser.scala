package inca.frontend.souffle

import cats.parse.Parser.not

import Console.{RED, RESET, UNDERLINED}
import cats.parse.{Numbers, Parser => P, Parser0 => P0}
import inca.frontend.souffle.Syntax.{BrieQualifier, BtreeQualifier, ChoiceDomain, DeclaredType, EquivalenceQualifier, Expression, FloatType, FloatValue, InlineQualifier, MagicQualifier, NoInlineQualifier, NoMagicQualifier, NumberType, NumberValue, OverrideQualifier, Relation, RelationAttribute, RelationQualifier, StringValue, SymbolType, Type, UnsignedType}

object Parser {

  // HELPERS

  object Literals {
    val quotes: P[Unit] = P.char('"')
    val colon: P[Unit] = P.char(':')
    val semicolon: P[Unit] = P.char(';')
    val underscore: P[Unit] = P.char('_')
    val comma: P[Unit] = P.char(',')

    val string: P[String] = quotes *> P.until0(quotes)
    val number: P[Int] = Numbers.signedIntString.map(_.toInt)
    val float: P[Float] = Numbers.jsonNumber.map(_.toFloat)

    val digit: P[Char] = cats.parse.Rfc5234.digit
    val letter: P[Char] = cats.parse.Rfc5234.alpha
    val identifier: P[String] =
      ((letter | underscore.as('_')) ~ (letter | digit | underscore.as('_')).rep0).map {
        case (c, s) => s"$c${s.mkString}"
      }
  }

  object Separators {
    val comma: P[Unit] = spaced(Literals.comma)
    val semicolon: P[Unit] = spaced(Literals.semicolon)
  }

  val whitespace: P0[Unit] = P.until0(P.not(P.charIn(" \t\n\r"))).void

  def spaced[A](p: P[A]): P[A] = p <* whitespace

  def parens[A](p: P0[A]): P[A] = spaced(P.char('(')) *> p <* spaced(P.char(')'))

  // TYPES

  val typename: P[Type] =
    P.string("symbol").as(SymbolType) |
    P.string("number").as(NumberType) |
    P.string("unsigned").as(UnsignedType) |
    P.string("float").as(FloatType) |
    Literals.identifier.map(DeclaredType.apply)

  // EXPRESSIONS

  val variable: P[Expression] =
    Literals.string.map(StringValue.apply) |
    Literals.number.map(NumberValue.apply) |
    Literals.float.map(FloatValue.apply)

  // RELATIONS

  val relationAttribute: P[RelationAttribute] =
    ((spaced(Literals.identifier) <* spaced(Literals.colon)) ~
      spaced(typename))
      .map { case (n, t) => RelationAttribute(n, t) }

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
    spaced(P.string("choice-domain")) *>
      spaced(
        Literals.identifier.map(Seq(_)) |
          parens(spaced(Literals.identifier).repSep(Separators.comma)).map(_.toList)
      ).repSep(Separators.comma).map(_.toList.flatten)
        .map(ChoiceDomain.apply)

  val relation: P[Relation] = {
    (
      /* name */ (spaced(P.string(".decl")) *> spaced(Literals.identifier)) ~
      /* attributes */ spaced(parens(relationAttribute.repSep0(Separators.comma))) ~
      /* qualifiers */ spaced(qualifier).repUntil0(not(qualifier)) ~
      /* choice domain */ choiceDomain.?
    ).map {
      case (((name, attr), qualifiers), choiceDomain) =>
        Relation(name, attr, qualifiers, choiceDomain)
    }
  }

  // PROGRAM

  val program: P[Seq[Any]] = (
    relation
  ).rep.map(_.toList)


  // PARSE METHODS

  def parse[A](p: P[A], source: String): A = p.surroundedBy(whitespace).parseAll(source) match {
    case Left(err) =>
      System.out.print(source.substring(0, err.failedAtOffset))
      System.out.print(s"$RED$UNDERLINED${source(err.failedAtOffset)}$RESET")
      System.out.print(source.substring(err.failedAtOffset + 1))

      throw new Exception(s"Parser error! Expected: ${err.expected}")
    case Right(value) => value
  }

  def parse(source: String): Seq[Any] = parse(program, source)
}
