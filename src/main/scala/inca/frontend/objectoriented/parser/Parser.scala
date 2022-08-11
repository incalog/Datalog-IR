package inca.frontend.objectoriented.parser

import inca.frontend.objectoriented.core._
import cats.parse.{Numbers, Parser => P, Parser0 => P0}
import inca.compiler.SourceLocation

import scala.language.{existentials, implicitConversions}

trait Parser {

  val lineComment: P[Unit] = P.string("//") *> P.charsWhile0(c => c != '\n' && c != '\r').void
  val blockComment: P[Unit] = P.string("/*") *> P.recursive[Unit](rec =>
    P.product01(P.charsWhile0(c => c != '*').void, P.string("*/") | P.char('*') ~ rec).void
  )
  val comment: P[Unit] = lineComment | blockComment
  val whitespace: P[Unit] = (P.charIn(" \t\r\n").void | comment)
  val whitespaces0: P0[Unit] = whitespace.rep0.void

  def inParentheses[A](p: P0[A]): P[A] =
    op('(') *> p <* op(')')

  def inBraces[A](p: P0[A]): P[A] =
    op('{') *> p <* op('}')

  def spaced[A](p: P[A]): P[A] =
    p <* whitespaces0

  def seq0[A](p: P[A], sep: Char = ','): P0[Seq[A]] =
    (p <* P.char(sep).? <* whitespaces0).rep0

  object Keyword extends Enumeration {
    val IF = "if"
    val ELSE = "else"
    val CLASS = "class"
    val DEF = "def"
    val PRIVATE = "private"
    val VAR = "var"
    val NEW = "new"
    val RETURN = "return"
  }

  val keywords: Set[String] = Keyword.values.map(k => k.toString)

  import Keyword._

  def keyword(keyword: String): P[Unit] =
    spaced(P.string(keyword) *> P.not(letterDigit))

  def op(c: Char): P[Unit] =
    spaced(P.char(c))

  def op(s: String): P[Unit] =
    spaced(P.string(s))

  val letter: P[Unit] = P.ignoreCaseCharIn('a' to 'z').void
  val digit: P[Unit] = P.charIn('0' to '9').void
  val letterDigit: P[Unit] = P.charIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).void

  val id: P[Name] =
    (letter ~ letterDigit.rep0)
      .string
      .filter(s => !keywords.contains(s)).backtrack
      .mapWithLoc(s => Name(s))

  val identifier: P[Name] =
    spaced(id)

  protected[frontend] def privateVisibility: P[Visibility] =
    keyword(PRIVATE).mapWithLoc(_ => Private)

  protected[frontend] def visibility: P[Visibility] =
    spaced(privateVisibility)

  protected[frontend] def overrideAnnotation: P[Annotation] =
    spaced(P.string(OverrideFunctionAnno.toString)).map(_ => OverrideFunctionAnno)

  protected[frontend] def methodDef: P[FunctionDef] = {
    val functionHeader = ((((overrideAnnotation.? ~ visibility.?).with1
      <* keyword(DEF)) ~ identifier ~ defParams)
      ~ (op(':') *> typeAnno).?
      ~ inBraces(exp))
    functionHeader.mapWithLoc { case (((((overrideAnnotation, visibility), funcName), params), typeAnno), content) =>
        // TODO: support expression in function body
        //println(content)
        val anno = if (overrideAnnotation.isEmpty) Seq() else Seq(overrideAnnotation.get)
        FunctionDef(anno, visibility, funcName, params, typeAnno.getOrElse(TAny), Var(Name("somevar")))
    }
  }

  protected[frontend] def exp: P0[Seq[Expression]] = {
    atomicExp.rep0
  }

  protected[frontend] def defParams: P[Seq[Param]] =
    spaced(inParentheses(paramList))

  protected[frontend] def paramList: P0[Seq[Param]] =
    seq0(param)

  protected[frontend] def param: P[Param] =
    nameWithType.mapWithLoc {
      case(name, typeAnno) => Param(name, typeAnno.getOrElse(TAny))
    }

  protected[frontend] def nameWithType: P[(Name, Option[Type])] =
    spaced(identifier ~ (op(':') *> typeAnno).?)

  protected[frontend] def typeAnno: P[Type] =
    identifier.mapWithLoc(n => TClass(n))

  /*protected[frontend] def variableDefAndAssignExp: P[(Var, Assign)] =
    ((keyword(VAR) *> nameWithType) ~ (op('=') *> atomicExp)).mapWithLoc {
      case ((name, typeAnno), exp) =>
        Var(name), Assign()
    }*/

  protected[frontend] def attrDef: P[VariableDef] =
    ((visibility.?.with1 <* keyword(VAR)) ~ nameWithType).mapWithLoc {
      case (visibility, (name, typeAnno)) => VariableDef(Seq(), visibility, name, typeAnno.getOrElse(TAny), None)
    } //~ (keyword("=") *> CallExp | ConstructorExp | ConstExp ).? or something nested... maybe atomicExp

  protected[frontend] def atomicExp: P[Expression] = {
    // var | call followed by optional .attr or .call
    (((variable <* P.not(P.char('('))).backtrack | P.defer(callExp).backtrack) ~
      (attrExp | methodCallExp).rep0
    ).map {
      case (exp, nestedExp) =>
        //println(exp, nestedExp)
        exp
    }
  }

  protected[frontend] def attrExp: P[Get] =
    (op('.') *> (identifier <* P.not(P.char('(')))).backtrack.mapWithLoc {
      name => Get(name)
    }

  protected[frontend] def methodCallExp: P[Call] =
    (op('.') *> P.defer(callExp)).backtrack

  protected[frontend] def variable: P[Var] =
    identifier.mapWithLoc(Var.apply)

  protected[frontend] def callExp: P[Call] =
    (identifier ~ inParentheses(seq0(atomicExp))).mapWithLoc {
          // TODO: Replace Var(fun) with correct and replace transitive
      case (fun, argList) => Call(Var(fun), argList, false)
    }

  /*protected[frontend] val classConstructorDef: P[Unit] = {
    P.string(" ")
  }*/

  protected[frontend] def className: P[Name] =
    spaced(keyword(CLASS) *> identifier)

  protected[frontend] def parentClassNames: P0[Seq[Name]] =
    spaced(inParentheses(seq0(identifier)))

  protected[frontend] def classDef: P[ClassDef] = {
    val header = visibility.?.with1 ~ className ~ parentClassNames.?
    val content = spaced(inBraces(classContent.rep0))

    (header ~ content).mapWithLoc { case (((visibility, name), parents), content)  =>
      ClassDef(Seq(), visibility, name, parents.getOrElse(Seq()), content)
    }
  }

  protected[frontend] def classContent: P[Content] = {
    (methodDef | attrDef)
    //| attributeDef | classConstructorDef
  }

  def moduleContent: P[Content] =
    classDef

  def module: P[Module] = {
    (op("module") *> identifier
      ~ moduleContent.rep0(0))
    .mapWithLoc { case (name, content) =>
      // TODO: imports are empty for now
      Module(name, List(), content)
    }
  }

  implicit class Ploc[T](p: => P[T]) {
    def mapWithLoc[U <: SourceLocation](f: T => U): P[U] = {
      (P.index.with1 ~ p ~ P.index).map {
        case ((start, t), end) =>
          val u = f(t)
          u.startIndex = start
          u.endIndex = end
          u
      }
    }
  }
}

object Parser {
  private lazy val parser: Parser = new Parser {}

  def parse(code: String): Module = {
    Module(Name("Dummy"), List(), List())
  }
}