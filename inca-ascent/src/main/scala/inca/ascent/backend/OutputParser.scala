package inca.ascent.backend

import cats.parse.{Numbers, Parser as P, Parser0 as P0}
import inca.ascent.syntax.Term.CustomLit
import inca.ir.Name
import inca.ir.execution.Relation
import inca.ir.util.SourceLocation
import cats.Show.Shown.mat

object OutputParser:
  def parse(s:String): Seq[Relation] = {
    (relations <* P.end).parseAll(s) match
      case Right(rels) => rels
      case Left(err) => throw new IllegalArgumentException(s"Parser error: $err")
  }

  val letter: P[Unit] = P.ignoreCaseCharIn('a' to 'z').void
  val digit: P[Unit] = P.charIn('0' to '9').void
  val letterDigit: P[Unit] = P.charIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Some('_')).void

  val id: P[String] = (letter ~ letterDigit.rep0).string

  val whitespace: P[Unit]= P.charIn(" \t\r\n").void
  val whitespace0: P0[Unit]= whitespace.rep0.void
  def spaced[A](p: P[A]):P[A]= p <* whitespace0

  def op(c: Char): P[Unit] = spaced(P.char(c))

  def inParens[A](p:P[A]):P[A]= P.char('(') *> p <* P.char(')')
  def inBrackets[A](p: P0[A]): P0[A]= P.char('[') *> p <* P.char(']')

  def stringLit: P[String]= P.char('"') *> P.charsWhile0(_ !='"') <* P.char('"')
  def intLit: P[Int] = Numbers.signedIntString.map(s => s.toInt)
  def doubleLit: P[Double] = P.string("Float(")*>(Numbers.signedIntString ~ (P.string(".") *> Numbers.nonNegativeIntString)).map((a,b) => s"$a.$b".toDouble) <* P.string(")")

  def adtLit: P[String] = (id ~ (P.char('(') *> P.defer(literal).repSep0(0, sep=op(',')) <* P.char(')')).?).map {
    case ("Some", args) => args.get.head.toString
    case (name, Some(args)) => name + args.mkString("(", ",", ")")
    case (name, _) => name + "()"
  }

  private def name: P[String] = P.charsWhile(_ !=':')
  private def literal: P[Any] = stringLit | doubleLit.backtrack | intLit | adtLit
  private def tuple:P[List[Any]]= inParens(literal.repSep(1, op(',')) <* P.char(',').rep0 ).map(_.toList) //|inParens(oneValParam).map(v => List(v))
  private def list: P0[List[List[Any]]] = inBrackets(tuple.repSep0(0, op(',')).map(_.toList))

  private def relation: P[Relation] = spaced((name <* op(':')) ~ list).map{
    case (name, tuples) =>
      val arity = tuples.headOption match
        case Some(tup) => tup.size
        case _ => 0
      val params = (0 until arity).map(idx => s"param_${idx}")
      Relation.from(name, params, tuples)
  }

  private val relations: P0[List[Relation]] = relation.rep0


