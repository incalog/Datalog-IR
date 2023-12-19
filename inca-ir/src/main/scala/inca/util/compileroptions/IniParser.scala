package inca.util.compileroptions

import cats.data.NonEmptyList
import cats.parse.{Numbers, Parser as P, Parser0 as P0}

object IniParser:
  private val lineComment: P[Unit] = (P.string("#") | P.string(";")) *> P.charsWhile0(c => c != '\n' && c != '\r').void
  /*val blockComment: P[Unit] = P.string("/*") *> P.recursive[Unit](rec =>
    P.product01(P.charsWhile0(c => c != '*').void, P.string("*/") | P.char('*') ~ rec).void
  )*/
  private val comment: P[Unit] = lineComment
  private val whitespace: P[Unit] = P.charIn(" \t\r\n").void | comment

  private val linebreak: P[Unit] = P.charIn("\r\n").void | comment

  private val whitespaces0: P0[Unit] = whitespace.rep0.void

  private def spaced[A](p: P[A]): P[A] = p <* whitespaces0

  private val letter: P[Unit] = P.charIn(('a' to 'z') ++ ('A' to 'Z') ++ Some('_')).void
  private val digit: P[Unit] = P.charIn('0' to '9').void
  private val letterDigit: P[Unit] = P.charIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Some('_')).void

  private val id: P[String] = (letter ~ letterDigit.rep0).string
  private val identifier: P[String] = spaced(id)

  private def op(c: Char): P[Unit] = spaced(P.char(c))
  private def inBrackets[A](p: P0[A]): P[A] = op('[') *> p <* op(']')

  private val intLit: P[Int] = spaced(
    Numbers.signedIntString.map(s => s.toInt)
  )

  private val doubleLit: P[Double] = spaced(
    (Numbers.signedIntString ~ (P.char('.') *> Numbers.nonNegativeIntString)).map {
      case (a, b) => s"$a.$b".toDouble
    })

  private val stringLit: P[String] = spaced(
    P.char('"') *> P.charsWhile0(_ != '\"') <* P.char('"')
  )

  private val fallbackStringLit: P[String] = P.anyChar.rep.string <* linebreak

  private val boolLit: P[Boolean] = spaced(
    P.string("true").map(_ => true) |
    P.string("false").map(_ => false)
  )

  private val lit: P[Any] =
    boolLit.backtrack |
      intLit.backtrack |
      doubleLit.backtrack |
      stringLit.backtrack |
      fallbackStringLit

  private val entry: P[(String, Any)] = (identifier <* op('=')) ~ lit

  private val section: P[(String, Seq[(String, Any)])] = spaced(inBrackets(identifier)) ~ entry.rep0

  def parse(source: String): Seq[(String, Seq[(String, Any)])] =
    (whitespaces0 *> section.rep0 <* P.end).parseAll(source) match
      case Left(err) => throw IllegalArgumentException(s"Parse error: $err")
      case Right(value) => value

