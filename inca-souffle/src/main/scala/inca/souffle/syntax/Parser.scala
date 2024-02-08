package inca.souffle.syntax

import cats.parse.{Numbers, Parser as P, Parser0 as P0}
import inca.ir.util.SourceLocation
import inca.ir.{Name, RefByName}
import inca.souffle.syntax.Atom.Disjunction
import inca.souffle.syntax.Parser.term
import inca.souffle.syntax.ProgramContent.*

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

  def parseSouffle(source: String): Program =
    (whitespaces0 *> module <* P.end).parseAll(source) match
      case Right(p) => p
      case Left(err) => throw new IllegalArgumentException(s"Parse error at ${source.slice(err.failedAtOffset, err.failedAtOffset + 10)}: $err")

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
    "nil",
    "not",
    "Int",
    "Double",
    "String",
    "Any"
  )

  def keyword(s: String): P[Unit] =
    if (!keywords.contains(s))
      throw new IllegalArgumentException(s"Not a keyword $s")
    else
      spaced(P.string(s) *> P.not(letterDigit))

  val letter: P[Unit] = P.ignoreCaseCharIn('_' +: ('a' to 'z')).void
  val digit: P[Unit] = P.charIn('0' to '9').void
  val letterDigit: P[Unit] = P.charIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Some('_')).void
  val opSymbol: P[Unit] = P.charIn("!@#$%^&*()+=<>,.:?/\\_|").void

  val id: P[String] =
    (letter ~ letterDigit.rep0)
      .string
      .filter(s => !keywords.contains(s))

  val identifier: P[String] =
    spaced(id)

  val varidentifier: P[String] =
    spaced(P.char('?').?.with1 ~ id).map {
      case (None, name) => name
      case (Some(_), name) => s"?$name"
    }

  val qualifiedIdentifier: P[QualifiedName] =
    spaced(id ~ (P.char('.') *> id).rep0).map((a, bs) => QualifiedName(a :: bs))

  val intnum: P[Int] =
    spaced(Numbers.signedIntString).map(_.toInt)

  def inParens[A](p: P0[A]): P[A] =
    op('(') *> p <* op(')')

  def inBraces[A](p: P0[A]): P[A] =
    op('{') *> p <* op('}')

  def inAngles[A](p: P0[A]): P[A] =
    op('<') *> p <* op('>')

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

  def oneOperator[A](ss: List[A]): P[A] = ss match
    case Nil => P.fail
    case s :: rest => op(s.toString).map(_ => s) | oneOperator(rest)

  /* Terms */

  val intLit: P[Term] = 
    intnum.mapWithLoc(s => Term.NumberLit(s.toInt))

  val doubleLit: P[Term] = spaced(
    (Numbers.signedIntString ~ (P.char('.') *> Numbers.nonNegativeIntString)).mapWithLoc {
      case (a,b) => Term.FloatLit(s"$a.$b".toDouble)
    })

  val stringLit: P[String] = spaced(
    P.char('"') *> P.charsWhile0(_ != '\"') <* P.char('"')
  )

  val literal: P[Term] = doubleLit.backtrack | intLit | stringLit.mapWithLoc(Term.StringLit.apply)

  val wildcard: P[Term] = op("_").mapWithLoc(_ => Term.Var("_"))

  val variable: P[Term] = varidentifier.mapWithLoc(Term.Var.apply) | wildcard

  val typ: P[Type] =
    oneOperator(List(Type.Number, Type.Symbol, Type.Unsigned, Type.Float)) |
    qualifiedIdentifier.map(Type.Name.apply)


  lazy val term = P.defer(termRec)

  val aggregator: P[Aggregator] = P.fail

  val intrinsicFunctor: P[IntrinsicFunctor] =
    import IntrinsicFunctor.*
    oneOperator(List(
      Ord,
      ToFloat,
      ToNumber,
      ToString,
      ToUnsigned,
      Cat,
      StrLen,
      Substr,
      Max,
      Min,
    ))

  val unop: P[UnOp] =
    import UnOp.*
    oneOperator(List(Neg, Bnot, Lnot))

  val argList: P0[List[Term]] = term.repSep0(op(','))

  val atomicTerm: P[Term] =
    literal |
    op("nil").mapWithLoc(_ => Term.Nil.apply()) |
    inBrackets(argList).mapWithLoc(Term.List.apply) |
    P.char('$') *> (qualifiedIdentifier ~ inParens(argList)).mapWithLoc((name, args) => Term.Constr(name, args)) |
    op("as") *> inParens(term ~ (op(',') *> typ)).mapWithLoc((t,ty) => Term.TypeCast(t, ty)) |
    aggregator.mapWithLoc(Term.AggregatorTerm.apply) |
    (intrinsicFunctor ~ inParens(argList)).backtrack.map((f, args) => Term.IntrinsicFunctorApp(f, args)) |
    (identifier ~ inParens(argList)).backtrack.map((f, args) => Term.UserDefFunctorApp(UserDefFunctor(f), args)) |
    (unop ~ term).map((op, arg) => Term.Unary(op, arg)) |
    inParens(term) |
    variable


  val binop: P[BinOp] =
    import BinOp.*
    oneOperator(List(
      Add,
      Sub,
      Mul,
      Div,
      Rem,
      Pow,
      Land,
      Lor,
      Lxor,
      Band,
      Bor,
      Bxor,
      Bshl,
      Bshr,
      Bshru,
    ))

  lazy val termRec: P[Term] =
    (atomicTerm ~ (binop ~ term).?).mapWithLoc {
      case (t, None) => t
      case (t1, Some((op, t2))) => Term.Binary(t1, op, t2)
    }

  val call: P[Atom.Call] =
    (qualifiedIdentifier ~ inParens(term.repSep0(op(',')))).mapWithLoc {
      case (name, args) => Atom.Call(name, args)
    }

  val comparator: P[Comparator] =
    import Comparator.*
    oneOperator(List(LT, LE, GT, GE, EQ, NEQ))

  val compare: P[Atom] =
    (term ~ comparator ~ term).mapWithLoc {
      case ((lhs, op), rhs) => Atom.Compare(lhs, op, rhs)
    }

  lazy val atom: P[Atom] =
    inParens(P.defer(disjunction)) |
    op('!') *> P.defer(atom).map(Atom.Not.apply) |
    call.backtrack |
    compare |
    // TODO match
    // TODO contains
    oneOperator(List(Atom.True, Atom.False))

  lazy val disjunction: P[Atom.Disjunction] =
    (atom.repSep(op(',')) ~ (op(';') *> P.defer(disjunction)).?).mapWithLoc {
      case (alt, None) => Atom.Disjunction(Seq(alt.toList))
      case (alt, Some(Disjunction(alts))) => Atom.Disjunction(alt.toList +: alts)
    }

//  val signature: P[(Name, Seq[Type])] =
//    identifier ~ inParens(typ.repSep(op(',')).map(_.toList)) <* op('.')
//
//  val param: P[Param] =
//    (identifier ~ inParens(identifier).?).mapWithLoc {
//      case (name, None) => Param.Named(name)
//      case (agg, Some(name)) => Param.Aggregated(name, agg)
//    } |
//      literal.mapWithLoc(Param.Constant.apply)

  val atomList: P[List[Atom]] = atom.repSep(op(',')).map(_.toList)

  val plan: P[QueryPlan] =
    op(".plan") *>
      (
        (intnum <* op(':')) ~
          inParens(intnum.repSep0(op(',')))).repSep(op(',')).map( plans =>
        QueryPlan(plans.toList.map(p => p._1 -> p._2))
      )

  val rule: P[Rule] =
    (atomList ~ (op(":-") *> (disjunction <* op('.')) ~ plan.?)).mapWithLoc { case (heads, (body, plan)) => Rule(heads, body, plan) }

  val fact: P[Fact] =
    (qualifiedIdentifier ~ inParens(argList)).mapWithLoc((name, args) => Fact(name, args))

  val attribute: P[Attribute] = ((varidentifier <* op(':')) ~ typ).map((name, ty) => Attribute(name, ty))
  val qualifier: P[Qualifier] =
    import Qualifier.*
    oneOperator(List(
      EqRel,
      BTree,
      Brie,
      NoMagic,
      Magic,
      NoInline,
      Inline,
      Override
    ))

  val decl: P[RelationDecl] =
    // TODO choice domain
    (op(".decl") *> identifier.repSep(op(',')) ~ inParens(attribute.repSep0(op(','))) ~ qualifier.rep0).mapWithLoc {
      case ((names, attrs), quals) => RelationDecl(names.toList, attrs.toList, quals, None)
    }

  val typeDeclConstraint: P[TypeDeclConstraint] =
    import TypeDeclConstraint.*
    (op("<:") *> typ).map(SubType.apply) |
    (op("=") *> typ).map(EqType.apply)

  val typeDecl: P[TypeDecl] =
    (op(".type") *> identifier ~ typeDeclConstraint).mapWithLoc((name, con) => TypeDecl(name, con))

  val directiveValue: P[DirectiveValue] =
    stringLit.map(DirectiveValue.StringLit.apply) |
    identifier.map(DirectiveValue.Id.apply) |
    intnum.map(s => DirectiveValue.Number(s)) |
    op("true").map(_ => DirectiveValue.True) |
    op("false").map(_ => DirectiveValue.False)

  val directive: P[Directive] =
    import DirectiveQualifier.*
    (oneOperator(List(Input, Output, Printsize, Limitsize))
      ~ qualifiedIdentifier.repSep(op(','))
      ~ inParens((identifier ~ (op("=") *> directiveValue)).repSep0(op(','))).?).mapWithLoc {
      case ((qual, names), attrs) => Directive(qual, names.toList, attrs.getOrElse(List()).toMap)
    }

  val compType: P[ComponentType] =
    (identifier ~ inAngles(identifier.repSep(op(','))).?).map((name, args) => ComponentType(name, args.map(_.toList).getOrElse(List())))

  val componentContent: P[ProgramContent] =
    P.defer(programContent).filter(_ => true)

  val component: P[ComponentDecl] =
    (op(".comp") *> compType ~ (op(':') *> compType.repSep(op(','))).? ~ inBraces(componentContent.rep0)).mapWithLoc {
      case ((c, sups), content) => ComponentDecl(c, sups.map(_.toList).getOrElse(List()), content)
    }

  val componentInit: P[ComponentInit] =
    (op(".init") *> identifier ~ (op('=') *> compType)).mapWithLoc((name, ty) => ComponentInit(name, ty))

  lazy val programContent: P[ProgramContent] =
    rule | fact | decl | typeDecl | directive | component | componentInit

  val module: P0[Program] =
    whitespaces0 *> programContent.rep0.map(Program.apply)
