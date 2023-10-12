package inca.frontend.datalog.syntax

import cats.parse.{Numbers, Parser as P, Parser0 as P0}
import inca.ir.Name
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

  implicit class P0loc[T](p: => P0[T]) {
    def mapWithLoc[U <: SourceLocation](f: T => U): P0[U] =
      val pv = p
      (P.index ~ pv ~ P.index).map {
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
    "not",
    "Int",
    "Double",
    "String"
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
    spaced(id ~ (P.char('.') ~ id).rep0).mapWithLoc((a, bs) => Name((a :: bs).mkString(".")))


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

  /* Terms */

  val intLit: P[Literal] = spaced(
    Numbers.signedIntString.mapWithLoc(s => Literal.Int(s.toInt))
  )

  val doubleLit: P[Literal] = spaced(
    (Numbers.signedIntString ~ (P.char('.') *> Numbers.nonNegativeIntString)).mapWithLoc {
      case (a,b) => Literal.Double(s"$a.$b".toDouble)
    })

  val stringLit: P[Literal] = spaced(
    P.char('"') *> P.charsWhile0(_ != '\"') <* P.char('"')
  ).mapWithLoc(Literal.String.apply)

  val literal: P[Literal] = doubleLit.backtrack | intLit | stringLit

  val atomicTerm: P[Term] =
    literal.mapWithLoc(Term.Constant.apply) |
    identifier.mapWithLoc(Term.Var.apply) |
    inParens(P.defer(term))

  val binop: P[String] =
    oneOperator(List("+","-","*","/"))

  lazy val term: P[Term] =
    (atomicTerm ~ (binop ~ P.defer(term)).?).mapWithLoc {
      case (t, None) => t
      case (t1, Some((op, t2))) => Term.BinOp(t1, op, t2)
    }

  val call: P[Atom.Call] =
    (keyword("not").?.with1 ~ identifier ~ inParens(term.repSep0(op(',')))).mapWithLoc {
      case ((not, name), args) => Atom.Call(name, args, not.isDefined)
    }

  val comparator: P[String] =
    oneOperator(List("==", "!=", "<=", "<", ">=", ">"))

  val compare: P[Atom.Compare] =
    (term ~ comparator ~ term).mapWithLoc {
      case ((lhs, op), rhs) => Atom.Compare(lhs, op, rhs)
    }

  val atom: P[Atom] =
    call.backtrack | compare

  val typ: P[Type] =
    keyword("Int").mapWithLoc(_ => Type.Int()) |
      keyword("Double").mapWithLoc(_ => Type.Double()) |
      keyword("String").mapWithLoc(_ => Type.String())

  val signature: P[(Name, Seq[Type])] =
    identifier ~ inParens(typ.repSep(op(',')).map(_.toList)) <* op('.')

  val param: P[Param] =
    (identifier ~ inParens(identifier).?).mapWithLoc {
      case (name, None) => Param.Named(name)
      case (agg, Some(name)) => Param.Aggregated(name, agg)
    } |
      literal.mapWithLoc(Param.Constant.apply)

  val head: P[(Name, Seq[Param])] =
    identifier ~ inParens(param.repSep(op(',')).map(_.toList))

  val rule: P[(Name, Seq[Rule])] =
    (head ~ (op(":-") *> atom.repSep0(op(','))).rep0 <* op('.')).map {
      case ((name, params), Nil) => name -> Seq(Rule(params, Seq()))
      case ((name, params), bodies) => name -> bodies.map(as => Rule(params, as))
    }

  val relation: P[Relation] =
    (signature ~ rule.backtrack.rep0).flatMap {
      case ((name, params), rules) =>
        if (rules.exists(_._1 != name))
          P.failWith(s"Relation name $name must be repeated in rules $rules")
        else
          P.pure(Relation(name, params, rules.flatMap(_._2)))
    }

  val module: P0[Module] =
    whitespaces0 *> relation.rep0.mapWithLoc(Module.apply)
