package inca.frontend.objectoriented.parser

import inca.compiler.SourceLocation
import inca.frontend.objectoriented.core._
import cats.parse.{Parser => P, Parser0 => P0}

import scala.language.{existentials, implicitConversions}

trait Parser {

  val lineComment: P[Unit] = P.string("//") *> P.charsWhile0(c => c != '\n' && c != '\r').void
  val blockComment: P[Unit] = P.string("/*") *> P.recursive[Unit](rec =>
    P.product01(P.charsWhile0(c => c != '*').void, P.string("*/") | P.char('*') ~ rec).void
  )
  val comment: P[Unit] = lineComment | blockComment
  val whitespace: P[Unit] = (P.charIn(" \t\r\n").void | comment)
  val whitespaces0: P0[Unit] = whitespace.rep0.void

  val letter: P[Unit] = P.ignoreCaseCharIn('a' to 'z').void
  val digit: P[Unit] = P.charIn('0' to '9').void
  val letterDigit: P[Unit] = P.charIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).void

  def inParentheses[A](p: P0[A]): P[A] =
    op('(') *> p <* op(')')

  def inBraces[A](p: P0[A]): P[A] =
    op('{') *> p <* op('}')

  def spaced[A](p: P[A]): P[A] =
    p <* whitespaces0

  def seq0[A](p: P[A], sep: Char = ','): P0[Seq[A]] =
    (p <* P.char(sep).? <* whitespaces0).rep0

  object Keyword extends Enumeration {
    type Keyword = Value

    val IF: Value      = Value("if")
    val ELSE: Value    = Value("else")
    val CLASS: Value   = Value("class")
    val DEF: Value     = Value("def")
    val PRIVATE: Value = Value("private")
    val VAR: Value     = Value("var")
    val NEW: Value     = Value("new")
    val RETURN: Value  = Value("return")
    val INIT: Value    = Value("init")
  }

  import Keyword._

  val keywords: Set[String] = Keyword.values.map(k => k.toString)

  def keyword(keyword: Keyword): P[Unit] =
    spaced(P.string(keyword.toString) *> P.not(letterDigit))

  def op(c: Char): P[Unit] =
    spaced(P.char(c))

  def op(s: String): P[Unit] =
    spaced(P.string(s))

  val id: P[Name] = {
    (letter ~ letterDigit.rep0)
      .string
      .filter(s => !keywords.contains(s)).backtrack
      .mapWithLoc(s => Name(s))
  }

  val identifier: P[Name] =
    spaced(id)

  val privateVisibility: P[Visibility] =
    keyword(PRIVATE).mapWithLoc(_ => Private)

  val visibility: P[Visibility] =
    spaced(privateVisibility)

  val overrideAnnotation: P[Annotation] =
    spaced(P.string(OverrideFunctionAnno.toString)).map(_ => OverrideFunctionAnno)

  val typeHint: P[Type] =
    identifier.mapWithLoc(n => TClass(n))

  val nameWithType: P[(Name, Option[Type])] =
    spaced(identifier ~ (op(':') *> typeHint).?)

  protected[frontend] lazy val assignStmt: P[Statement] =
    (assignableExpr ~ (op('=') *> expr)).mapWithLoc {
      case (targetExpr, valueExpr) =>
        targetExpr match {
          case FieldReadExpr(_, previousExpr) => FieldAssignStmt(previousExpr, valueExpr)
          case VarReadExpr(name)              => VarAssignStmt(name, valueExpr)
        }
    }

  protected[frontend] lazy val ifElseStmt: P[IfStmt] = {
    val ifBlock = keyword(IF) *> inParentheses(P.defer(expr)) ~ (inBraces(P.defer(stmt).rep0) | P.defer(stmt).map(Seq(_)))
    val elseBlock = keyword(ELSE) *> (inBraces(P.defer(stmt).rep0) | P.defer(stmt).map(Seq(_)))
    (ifBlock ~ elseBlock.?).mapWithLoc {
      case ((compareExpr, thnStmt), elseStmts) => IfStmt(compareExpr, thnStmt, elseStmts.getOrElse(Seq()))
    }
  }

  protected[frontend] lazy val varDeclareStmt: P[VarDeclareStmt] = {
    (keyword(VAR) *> nameWithType ~ (op('=') *> expr).?).mapWithLoc {
      case ((name, typeHint), valueExpr) =>
        VarDeclareStmt(name, typeHint.getOrElse(TAny), valueExpr)
    }
  }

  protected[frontend] lazy val returnStmt: P[Statement] =
    (keyword(RETURN) *> expr.?).mapWithLoc(ReturnStmt)

  protected[frontend] lazy val exprStmt: P[Statement] =
    expr.mapWithLoc(ExprStmt)

  protected[frontend] lazy val stmt: P[Statement] = {
    assignStmt.backtrack | ifElseStmt | varDeclareStmt | exprStmt | returnStmt
  }

  private val variable: P[Name] =
    identifier <* P.not(P.char('('))

  private val call: P[(Name, Seq[Expression])] =
    identifier ~ inParentheses(seq0(P.defer(expr)))

  protected[frontend] val variableReadExpr: P[VarReadExpr] =
    variable.mapWithLoc(VarReadExpr).backtrack

  protected[frontend] val constructorExpr: P[ConstructorExpr] =
    (keyword(NEW) *> call).mapWithLoc { case (name, argList) => ConstructorExpr(name, argList) }.backtrack

  protected[frontend] val compareExpr: P[CompareExpr] = {
    val compareOps = P.oneOf(CompareOp.values.toList.map(o => P.string(o.toString).string))
    ((assignableExpr ~ spaced(compareOps)) ~ assignableExpr).mapWithLoc {
      case ((left, operator), right) => CompareExpr(left, right, CompareOp.withName(operator))
    }
  }

  private lazy val atom: P[Expression] =
    variableReadExpr | constructorExpr // |
    // Comment this in to allow function / method calls with implicit this.
    //call.mapWithLoc { case (name,  expressions) => MethodCallExpr(name, expressions) }.backtrack

  protected[frontend] lazy val assignableExpr: P[Expression] = {
    // atom.attr | atom.someMethod(...)
    (atom ~ (op('.') *> (variable.backtrack | call.backtrack)).rep0).mapWithLoc { case (startExpr, pathIdentifiers) =>
        pathIdentifiers.foldLeft(startExpr) { case (prev, current) =>
          current match {
            case name: Name => FieldReadExpr(name, prev)
            case (name: Name, argList: Seq[Expression]) => MethodCallExpr(name, List(prev) ++ argList)
          }
        }
    }
  }

  protected[frontend] lazy val expr: P[Expression] = {
    compareExpr.backtrack | assignableExpr
  }

  protected[frontend] val defParams: P[Seq[Param]] =
    spaced(inParentheses(paramList))

  protected[frontend] lazy val paramList: P0[Seq[Param]] =
    seq0(param)

  protected[frontend] lazy val param: P[Param] =
    nameWithType.mapWithLoc {
      case (name, typeHint) => Param(name, typeHint.getOrElse(TAny))
    }

  protected[frontend] val methodDef: P[MethodDef] = {
    val functionHeader = ((((overrideAnnotation.? ~ visibility.?).with1
      <* keyword(DEF)) ~ identifier ~ defParams)
      ~ (op(':') *> typeHint).?
      ~ inBraces(stmt.rep0))
    functionHeader.mapWithLoc { case (((((overrideAnnotation, visibility), funcName), params), typeHint), content) =>
      val anno = if (overrideAnnotation.isEmpty) Seq() else Seq(overrideAnnotation.get)
      MethodDef(anno, visibility, funcName, params, typeHint.getOrElse(TAny), content)
    }
  }

  protected[frontend] val fieldDef: P[FieldDef] = {
    ((visibility.?.with1 <* keyword(VAR)) ~ nameWithType ~ (op('=') *> assignableExpr).?).mapWithLoc {
      case ((visibility, (name, typeHint)), valueExpr) =>
        FieldDef(Seq(), visibility, name, typeHint.getOrElse(TAny), valueExpr)
    }
  }

  protected[frontend] val constructorDef: P[ConstructorDef] = {
    val functionHeader = ((((overrideAnnotation.? ~ visibility.?).with1
      <* keyword(INIT)) ~ defParams)
      ~ inBraces(stmt.rep0))
    functionHeader.mapWithLoc { case (((overrideAnnotation, visibility), params), content) =>
      val anno = if (overrideAnnotation.isEmpty) Seq() else Seq(overrideAnnotation.get)
      ConstructorDef(anno, visibility, params, content)
    }
  }

  protected[frontend] val className: P[Name] =
    spaced(keyword(CLASS) *> identifier)

  protected[frontend] val parentClassNames: P0[Seq[Name]] =
    spaced(inParentheses(seq0(identifier)))

  protected[frontend] val classContent: P[ClassContent] =
    methodDef | fieldDef | constructorDef

  protected[frontend] val classDef: P[ClassDef] = {
    val header = visibility.?.with1 ~ className ~ parentClassNames.?
    val content = spaced(inBraces(classContent.rep0))

    (header ~ content).mapWithLoc { case (((visibility, name), parents), content)  =>
      ClassDef(Seq(), visibility, name, parents.getOrElse(Seq()), content)
    }
  }

  val moduleContent: P[ClassDef] =
    classDef

  val module: P[Module] = {
    (op("module") *> identifier
      ~ moduleContent.rep0(0))
    .mapWithLoc { case (name, content) =>
      // TODO: imports are empty for now
      Module(name, List(), content)
    }
  }

  implicit class Ploc[T](p: => P[T]) {
    def mapWithLoc[U <: SourceLocation](f: T => U): P[U] = {
      ((P.index.with1 ~ p) ~ P.index).map {
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
  final case class ParseException(private val message: String = "", private val cause: Throwable = None.orNull)
    extends Exception(message, cause)

  private lazy val parser: Parser = new Parser {}

  def parse(code: String): Module =
    parser.module.parse(code) match {
      case Right((_, module)) => module
      case Left(e: cats.parse.Parser.Error) =>  throw ParseException(e.toString, null)
    }
}