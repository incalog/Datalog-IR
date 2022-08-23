package inca.frontend.objectoriented.parser

import inca.compiler.SourceLocation
import inca.frontend.objectoriented.core._
import cats.parse.{Parser => P, Parser0 => P0}
import inca.util.Scala
import scalaparse.syntax.Basic.isOpChar
import scalaparse.syntax.Identifiers.OpCharNotSlash

import scala.language.{existentials, implicitConversions}
import scala.meta.parsers.Parsed
import scala.meta.{Term, XtensionParseInputLike}

trait Parser {

  val scalaQuoteChar = '`'
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

    val CAST: Value       = Value("cast")
    val IF: Value         = Value("if")
    val ELSE: Value       = Value("else")
    val CLASS: Value      = Value("class")
    val DEF: Value        = Value("def")
    val PRIVATE: Value    = Value("private")
    val VAR: Value        = Value("var")
    val VAL: Value        = Value("val")
    val NEW: Value        = Value("new")
    val RETURN: Value     = Value("return")
    val TRUE: Value       = Value("true")
    val FALSE: Value      = Value("false")
    val NULL: Value       = Value("null")
    val EXTENDS: Value    = Value("extends")
    val INSTANCEOF: Value = Value("instanceOf")
  }

  import Keyword._

  val keywords: Set[String] = Keyword.values.map(k => k.toString)

  def keyword(keyword: Keyword): P[Unit] =
    spaced(P.string(keyword.toString) *> P.not(letterDigit))

  def op(c: Char): P[Unit] =
    spaced(P.char(c))

  def op(s: String): P[Unit] =
    spaced(P.string(s))

  def encloseBetween[T](p: P[T], c: Char): P[T] =
    spaced(P.char(c) *> p <* P.char(c))
    
  def pass[T](o: T): P0[T] =
    P.pure(o)

  def fail[T](s: String = ""): P[T] =
    if (s.isEmpty) P.fail[T] else P.failWith[T](s)

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
    spaced(P.string(OverrideAnnotation.toString)).map(_ => OverrideAnnotation)

  val mainAnnotation: P[Annotation] =
    spaced(P.string(MainAnnotation.toString)).map(_ => MainAnnotation)

  protected[frontend] val scalaTypeCore: P[Type] =
    P.charsWhile(_ != scalaQuoteChar).flatMap { raw_code =>
      raw_code.parse[meta.Type] match {
        case err: Parsed.Error  => fail(err.message)
        case Parsed.Success(ty) => pass(TScala(Scala(ty)))
      }
    }

  val noChar: P0[Unit] =
    P.not(P.charIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ "_"))

  protected[frontend] val scalaType: P[Type] =
    encloseBetween(scalaTypeCore, scalaQuoteChar) |
      (P.string("Int").string.soft <* noChar).mapWithLoc(_ => TScalaInt) |
      (P.string("Long").string.soft <* noChar).mapWithLoc(_ => TScalaLong) |
      (P.string("String").string.soft <* noChar).mapWithLoc(_ => TScalaString) |
      (P.string("Boolean").string.soft <* noChar).mapWithLoc(_ => TScalaBoolean) |
      (P.string("Double").string.soft <* noChar).mapWithLoc(_ => TScalaDouble)

  /** Helper for the Type like TAny. */
  protected[frontend] def simpleType[T <: Type](s: String, t: T): P[T] =
    (P.string(s).soft <* noChar).mapWithLoc(_ => t)

  protected[frontend] val classRef: P[ClassRef] =
    identifier.mapWithLoc(ClassRef)

  protected[frontend] val classType: P[TClass] =
    classRef.mapWithLoc(TClass)

  protected[frontend] val typeAnno: P[Type] =
    spaced(
      simpleType("Any", TAny) |
        simpleType("Null", TNull) |
        simpleType("Unit", TTuple(Seq())) |
        scalaType |
        classType
    )

  val nameWithType: P[(Name, Type)] =
    spaced(identifier ~ (op(':') *> typeAnno))

  protected[frontend] lazy val assignStmt: P[Statement] =
    (nestedAccessExpr ~ (op('=') *> expr)).backtrack.flatMapWithLoc {
      case (targetExpr, valueExpr) =>
        targetExpr match {
          case FieldReadExpr(previousExpr, name) => pass(FieldAssignStmt(previousExpr, name, valueExpr))
          case VarReadExpr(name)                 => pass(VarAssignStmt(name, valueExpr))
          case _                                 => fail(s"Can not assign a value to expression: $targetExpr")
        }
    }

  protected[frontend] lazy val ifElseStmt: P[IfStmt] = {
    val ifBlock = keyword(IF) *> inParentheses(P.defer(expr)) ~ (inBraces(P.defer(stmt).rep0) | P.defer(stmt).map(Seq(_)))
    val elseBlock = keyword(ELSE) *> (inBraces(P.defer(stmt).rep0) | P.defer(stmt).map(Seq(_)))
    (ifBlock ~ elseBlock.?).mapWithLoc {
      case ((compareExpr, thnStmt), elseStmts) => IfStmt(compareExpr, thnStmt, elseStmts.getOrElse(Seq()))
    }
  }

  private def varDeclareStmt(immutable: Boolean) = {
    val kw = if (immutable) VAL else VAR
    (keyword(kw) *> nameWithType ~ (op('=') *> expr).?).mapWithLoc {
      case ((name, typeAnno), valueExpr) =>
        VarDeclareStmt(name, typeAnno, valueExpr, immutable)
    }
  }

  protected[frontend] lazy val varDeclareStmt: P[VarDeclareStmt] =
    varDeclareStmt(immutable=false) | varDeclareStmt(immutable=true)

  protected[frontend] lazy val returnStmt: P[Statement] =
    (keyword(RETURN) *> expr.?).mapWithLoc(exp => ReturnStmt(exp.getOrElse(TupleExpr())))

  protected[frontend] lazy val exprStmt: P[Statement] =
    expr.mapWithLoc(ExprStmt)

  protected[frontend] lazy val stmt: P[Statement] =
     assignStmt | varDeclareStmt | ifElseStmt | returnStmt | exprStmt

  private val variable: P[Name] =
    (identifier.soft <* P.not(P.char('(')))

  private val call: P[(Name, Seq[Expression])] =
    (identifier.soft ~ inParentheses(seq0(P.defer(expr))))

  private val baseApplyMethod: P[(Name, Option[Seq[Expression]])] =
    (encloseBetween(identifier, scalaQuoteChar).soft ~ inParentheses(seq0(P.defer(expr))).?)

  protected[frontend] val variableReadExpr: P[VarReadExpr] =
    variable.mapWithLoc(VarReadExpr.apply)

  protected[frontend] val constructorExpr: P[ConstructorExpr] =
    (keyword(NEW) *> call).mapWithLoc { case (name, argList) => ConstructorExpr(ClassRef(name), argList) }

  protected[frontend] val superExpr: P[SuperExpr] =
    (op("this") *> inParentheses(seq0(P.defer(expr)))).mapWithLoc(SuperExpr)

  protected[frontend] lazy val typeCastExpr: P[TypeCastExpr] =
    (keyword(CAST) *> inParentheses((P.defer(expr) <* op(",")) ~ typeAnno)).mapWithLoc {
      case (recv, typeAnno) => TypeCastExpr(recv, typeAnno)
    }

  protected[frontend] lazy val instanceOfExpr: P[InstanceOfExpr] =
    (keyword(INSTANCEOF) *> inParentheses((P.defer(expr) <* op(",")) ~ typeAnno)).mapWithLoc {
      case (recv, typeAnno) => InstanceOfExpr(recv, typeAnno)
    }

  protected[frontend] lazy val nestedAccessExpr: P[Expression] = {
    // (someVar | someConstructor | `someBaseLit` | `someBaseApply`(...)).(attr | `baseApplyMethod`)
    // (someVar | someConstructor | `someBaseLit` | `someBaseApply`(...)).(someMethod(...) | baseApplyMethod`(...))
    ((constructorExpr | variableReadExpr | baseLitExpr | baseApplyExpr)
      ~ (op('.') *> (variable | call | baseApplyMethod)).rep0)
      .mapWithLoc { case (startExpr, pathIdentifiers) =>
        pathIdentifiers.foldLeft(startExpr) { case (prev, current) =>
          current match {
          case name: Name                                     => FieldReadExpr(prev, name)
          case (name: Name, argList: Seq[Expression])         => MethodCallExpr(prev, name, argList)
          case (name: Name, argList: Option[Seq[Expression]]) => BaseApplyMethodExpr(prev, name, argList)
          }
        }
    }
  }

  /** NullLiteral parser */
  protected[frontend] val nullExpr: P[NullExpr] =
    keyword(NULL).mapWithLoc(_ => NullExpr())

  protected[frontend] lazy val parensExpr: P[Expression] =
    inParentheses(P.defer(expr))

  protected[frontend] lazy val expr: P[Expression] =
    infixExpr

  /** base parser */
  protected[frontend] val scalaTerm: P[Scala[meta.Term]] =
    P.charsWhile(_ != scalaQuoteChar).flatMap { raw_code =>
      raw_code.parse[Term] match {
        case err: Parsed.Error    => fail(err.message)
        case Parsed.Success(code) => pass(Scala(code))
      }
    }

  /** NumericLiteral parser */
  protected[frontend] val numericLiteral: P[BaseLitExpr] = {
    (op('-').string.?.with1 ~ digit.rep.string ~
      ((P.string("L") | P.string("l")).string.map(_ => "long") |
        P.string("d").string.map(_ => "double") |
        (P.string(".").string *> digit.rep0.string <* P.string("d").?)
        ).?
    ).flatMapWithLoc { case ((sign, whole), suffix) =>
      val integral = sign.getOrElse("") + whole
      suffix match {
        case None => integral.toIntOption match {
          case Some(i) => pass(BaseLitExpr(Scala(meta.Lit.Int(i))))
          case None => fail()
        }
        case Some("long") => integral.toLongOption match {
          case Some(l) => pass(BaseLitExpr(Scala(meta.Lit.Long(l))))
          case None => fail()
        }
        case Some("double") => integral.toDoubleOption match {
          case Some(d) => pass(BaseLitExpr(Scala(meta.Lit.Double(d))))
          case None => fail()
        }
        case Some(fraction) =>
          s"$integral.$fraction".toDoubleOption match {
            case Some(d) => pass(BaseLitExpr(Scala(meta.Lit.Double(d))))
            case None => fail()
          }
      }
    }
  }

  /** StringLiteral parser */
  protected[frontend] val stringLiteral: P[BaseLitExpr] =
    (P.string("\"") *> P.charsWhile0(_ != '\"') <* P.string("\"")).mapWithLoc {
      s => BaseLitExpr(Scala(meta.Lit.String(s)))
    }

  /** BooleanLiteral parser */
  protected[frontend] val booleanLiteral: P[BaseLitExpr] =
    (P.string(TRUE.toString) | P.string(FALSE.toString)).string.mapWithLoc  {
      s => BaseLitExpr(Scala(meta.Lit.Boolean(s.toBoolean)))
    }

  protected[frontend] val baseLitExpr: P[BaseLitExpr] = {
    (encloseBetween(scalaTerm, scalaQuoteChar).soft <* P.not(P.char('('))).mapWithLoc(BaseLitExpr) |
      spaced(numericLiteral) |
      spaced(stringLiteral) |
      spaced(booleanLiteral)
  }

  protected[frontend] lazy val baseApplyExpr: P[BaseApplyExpr] =
    (encloseBetween(scalaTerm, scalaQuoteChar).soft ~ inParentheses(seq0(P.defer(expr)))).mapWithLoc {
      case (funTerm, args) => BaseApplyExpr(funTerm, args)
    }

  protected[frontend] val subinfixExpr: P[Expression] =
    nestedAccessExpr | parensExpr | baseApplyUnaryExpr | typeCastExpr | nullExpr | superExpr | instanceOfExpr

  protected[frontend] val infixExpr: P[Expression] =
    baseApplyInfixExpr | subinfixExpr

  protected[frontend] lazy val baseApplyInfixExpr: P[BaseApplyInfixExpr] =
    (subinfixExpr ~ spaced(P.charsWhile(isOpChar)) ~ P.defer(infixExpr)).backtrack.flatMapWithLoc {
      case ((_, "@"), _)    => fail("@ not allowed as infix opertor")
      case ((_, "=>"), _)   => fail("=> not allowed as infix baseApplyInfixExpropertor")
      case ((_, "|"), _)    => fail("| not allowed as infix opertor")
      case ((lhs, op), rhs) => pass(BaseApplyInfixExpr(lhs, Scala(meta.Term.Name(op)), rhs))
    }

  protected[frontend] lazy val baseApplyUnaryExpr: P[BaseApplyUnaryExpr] =
    (P.charsWhile(OpCharNotSlash) ~ P.defer(infixExpr)).flatMapWithLoc {
      //      case ("@", _) => fail("@ not allowed as infix opertor")
      case (op, rhs) => pass(BaseApplyUnaryExpr(Scala(Term.Name(op)), rhs))
    }

  protected[frontend] val defParams: P[Seq[Param]] =
    spaced(inParentheses(paramList))

  protected[frontend] lazy val paramList: P0[Seq[Param]] =
    seq0(param)

  protected[frontend] lazy val param: P[Param] =
    nameWithType.mapWithLoc {
      case (name, typeAnno) => Param(name, typeAnno)
    }

  protected[frontend] val methodDef: P[MethodDef] = {
    val functionHeader = (((((overrideAnnotation | mainAnnotation).? ~ visibility.?).with1
      <* keyword(DEF)).backtrack ~ identifier ~ defParams)
      ~ (op(':') *> typeAnno)
      ~ (op('=') *> inBraces(stmt.rep0)))
    functionHeader.mapWithLoc { case (((((overrideAnnotation, visibility), funcName), params), typeAnno), content) =>
      val anno = if (overrideAnnotation.isEmpty) Seq() else Seq(overrideAnnotation.get)
      MethodDef(anno, visibility, funcName, params, typeAnno, content)
    }
  }

  private def fieldDef(immutable: Boolean): P[FieldDef] = {
    val kw = if (immutable) VAL else VAR
    (((visibility.? <* keyword(kw)).with1 ~ nameWithType).backtrack ~ (op('=') *> expr).?).mapWithLoc {
      case ((visibility, (name, typeAnno)), valueExpr) =>
        FieldDef(Seq(), visibility, name, typeAnno, valueExpr, immutable)
    }
  }

  protected[frontend] val fieldDef: P[FieldDef] =
    fieldDef(false) | fieldDef(true)

  protected[frontend] val constructorDef: P[ConstructorDef] = {
    val functionHeader = ((((overrideAnnotation.? ~ visibility.?).with1
      <* (keyword(DEF) ~ op("this"))).backtrack ~ defParams ~ (op(':') *> typeAnno))
      ~ (op('=') *> inBraces(stmt.rep0)))
    functionHeader.mapWithLoc { case ((((overrideAnnotation, visibility), params), _), content) =>
      val anno = if (overrideAnnotation.isEmpty) Seq() else Seq(overrideAnnotation.get)
      ConstructorDef(anno, visibility, params, content)
    }
  }

  protected[frontend] val classContentDef: P[ClassContent] =
    constructorDef | methodDef | fieldDef

  protected[frontend] val classDef: P[ClassDef] = {
    val className = spaced(keyword(CLASS) *> identifier)
    val parentClassName = keyword(EXTENDS) *> classRef
    val header = visibility.?.with1 ~ className ~ parentClassName.map(Seq(_)).?
    val content = spaced(inBraces(classContentDef.rep0))

    (header ~ content).mapWithLoc { case (((visibility, name), parents), content)  =>
      ClassDef(Seq(), visibility, name, parents.getOrElse(Seq()), content)
    }
  }

  val moduleContent: P[ClassDef] =
    classDef

  val module: P[Module] = {
    (op("module") *> identifier ~ moduleContent.rep0(0)).mapWithLoc { case (name, content) =>
      // TODO: imports are empty for now
      Module(name, List(), content)
    }
  }

  implicit class Ploc[T](p: => P[T]) {
    def mapWithLoc[U <: SourceLocation](f: T => U): P[U] =
      ((P.index.with1 ~ p) ~ P.index).map {
        case ((start, t), end) =>
          val u = f(t)
          u.startIndex = start
          u.endIndex = end
          u
      }

    def flatMapWithLoc[U <: SourceLocation](f: T => P0[U]): P[U] =
      ((P.index.with1 ~ p) ~ P.index).flatMap {
        case ((start, t), end) =>
          val up = f(t)
          up.map { u =>
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