package inca.frontend.parser

import fastparse.ScalaWhitespace._
import fastparse._
import inca.compiler.SourceLocation
import inca.frontend.core._
import inca.util.Meta.Scala
import scalaparse.syntax.Identifiers.OpCharNotSlash

import scala.language.reflectiveCalls
import scala.meta.Term
import scala.meta.parsers.{Parsed, _}

/**
 * Parser for the IncA Core language.
 */
trait Parser {

  final lazy val allKeywords: Set[String] =
    Set("if", "let", "in", "match", "fail") ++
      Set("Option", "None", "Some", "Set")

  def identifier[_: P]: P[Name] =
    P((CharIn("a-z", "A-Z", "_") ~~ CharIn("a-z", "A-Z", "0-9", "_").repX).!).mapWithLoc { s =>
      if (allKeywords.contains(s)) return fastparse.Fail
      else Name(s)
    }

  /** Module parser */
  def module[_: P]: P[Module] =
    P("module " ~ identifier ~
      import_.rep ~
      moduleContent.rep ~
      End
    ).mapWithLoc { case (name, imports, contents) =>
      Module(name, imports, contents.flatten)
    }

  def import_[_: P]: P[Import] =
    P("import" ~ identifier).mapWithLoc(Import.apply)


  def moduleContent[_: P]: P[Seq[ModuleContent]] =
    P(functionDef.map(Seq(_)) | dataDef.map(Seq(_)))


  /** PatternFunction parser */
  protected[frontend] def functionDef[_: P]: P[ModuleContent] = {
    P(annotation.rep ~ visibility.? ~ "def" ~ identifier ~ "(" ~ paramList ~ ")" ~ ":" ~ typeAnno ~ "=" ~ exp).mapWithLoc{
      case (annos, vis, name, params, ty, exp) =>
        FunctionDef(annos, vis, name, params, ty, exp)
    }

  }

  protected[frontend]  def annotation[_: P]: P[Annotation] = mainFuncAnno
  protected[frontend]  def mainFuncAnno[_: P]: P[MainFunctionAnno.type] = P("@main").map(_ => MainFunctionAnno)

  protected[frontend] def exp[_: P]: P[Expression] = wideExp

  protected[frontend] def wideExp[_: P]: P[Expression] = P(ifExp | letExp | infixExp)
  protected[frontend] def infixExp[_: P]: P[Expression] = P(baseApplyInfixExp | matchExp | atomicExp)
  protected[frontend] def atomicExp[_: P]: P[Expression] = P(optionExp | tupleExp | callExp | baseLitExp| baseApplyExp | variable | parensExp)

  /** Let parser */
  final protected[frontend] def parensExp[_: P]: P[Expression] = P("(" ~ exp ~ ")")

  final protected[frontend] def letExp[_: P]: P[Let] =
    P(singleLetExp | multipleLetExp)

  final protected[frontend] def singleLetExp[_: P]: P[Let] =
    P("let " ~ identifier ~ (":" ~ typeAnno).? ~ "=" ~ exp ~ "in" ~ exp).mapWithLoc {
      case (name, typeAnno, bound, body) => Let(Seq(name), typeAnno, bound, body)
    }

  final protected[frontend] def multipleLetExp[_: P]: P[Let] =
    P("let " ~ "(" ~ identifier.rep(min = 2, sep = ",") ~ ")" ~ (":" ~ typeAnno).? ~ "=" ~ exp ~ "in" ~exp).mapWithLoc {
      case (names, typeAnno, bound, body) => Let(names, typeAnno, bound, body)
    }


  protected[frontend] def ifExp[_: P]: P[If] =
    P("if" ~ "(" ~ exp ~ ")" ~ exp ~ "else" ~ exp).mapWithLoc {
      case (cond, thn, els) => If(cond, thn, els)
    }

  protected[frontend] def callExp[_: P]: P[Call] =
    P(identifier ~ "(" ~ exp.rep(sep = ",") ~ ")").mapWithLoc {
      case (name, args) => Call(name, args)
    }

  protected[frontend] def tupleExp[_: P]: P[CoreExpression] =
    P("(" ~ exp.rep(2, sep = ",") ~ ")").mapWithLoc(Tuple)


  protected[frontend] def variable[_: P]: P[Var] =
    identifier.mapWithLoc(Var.apply)

  protected[frontend] def matchExp[_: P]: P[Match] =
    P(atomicExp ~ "match" ~ "{" ~ matchCase.rep() ~ "}").mapWithLoc {
      case (matchee, cases) => Match(matchee, cases)
    }

  protected[frontend] def matchCase[_: P]: P[(Pattern, Expression)] =
    P("case" ~ pattern ~ "=>" ~ exp).map {
      case (pat, body) => (pat, body)
    }

  protected[frontend] def optionExp[_: P]: P[Expression] = {
    P("None").mapWithLoc(_ => NoneExp()) |
    P("Some" ~ "(" ~ exp.rep(sep = ",") ~ ")").mapWithLoc {
      case Seq(arg) => SomeExp(arg)
      case args => Call(Name("Some"), args)
    }
  }

  protected[frontend] def pattern[_: P]: P[Pattern] =
    P(optionPattern | constructorPattern)

  protected[frontend] def constructorPattern[_: P]: P[ConstructorPattern] = {
    P(identifier ~ "(" ~ identifier.rep(sep = ",") ~")").mapWithLoc {
      case (name, args) => ConstructorPattern(name, args)
    }
  }

  protected[frontend] def optionPattern[_: P]: P[Pattern] =
    P("None").mapWithLoc(_ => NonePattern()) |
    P("Some" ~ "(" ~ identifier.rep(sep = ",") ~")").mapWithLoc {
      case Seq(arg) => SomePattern(arg)
      case args => ConstructorPattern(Name("Some"), args)
    }


  /** base parser */
  protected[frontend] def baseLitExp[_: P]: P[BaseLit] =
    P("`" ~~ scalaTerm ~~ "`").mapWithLoc(t => BaseLit(t)) |
      numericLiteral |
      stringLiteral |
      booleanLiteral

  protected[frontend] def numericLiteral[_: P]: P[BaseLit] =
    P("-".!.? ~~ ParserUtils.rawInteger ~~
      (("L" | "l").map(_=>"long") |
        "d".!.map(_=>"double") |
        "." ~~ (ParserUtils.rawInteger | "".!) ~~ "d".?
        ).?
    ).flatMapWithLoc { case (sign, whole, suffix) =>
      val integral = sign.getOrElse("") + whole
      suffix match {
        case None => integral.toIntOption match {
          case Some(i) => Pass(BaseLit(Scala(meta.Lit.Int(i))))
          case None => Fail
        }
        case Some("long") => integral.toLongOption match {
          case Some(l) => Pass(BaseLit(Scala(meta.Lit.Long(l))))
          case None => Fail
        }
        case Some("double") => integral.toDoubleOption match {
          case Some(d) => Pass(BaseLit(Scala(meta.Lit.Double(d))))
          case None => Fail
        }
        case Some(fraction) =>
          s"$integral.$fraction".toDoubleOption match {
            case Some(d) => Pass(BaseLit(Scala(meta.Lit.Double(d))))
            case None => Fail
          }
      }
    }

  /** StringLiteral parser */
  protected[frontend] def stringLiteral[_: P]: P[BaseLit] =
    P(ParserUtils.string).mapWithLoc(s => BaseLit(Scala(meta.Lit.String(s))))

  /** BooleanLiteral parser */
  protected[frontend] def booleanLiteral[_: P]: P[BaseLit] =
    P(("true" | "false").!).mapWithLoc(s => BaseLit(Scala(meta.Lit.Boolean(s.toBoolean))))


  protected[frontend] def baseApplyExp[_: P]: P[BaseApply] =
    P("`" ~~ scalaTerm ~~ "`" ~ "(" ~ exp.rep(sep = ",") ~ ")").mapWithLoc {
      case (funTerm, args) => BaseApply(funTerm, args)
    }

  protected[frontend] def baseApplyInfixExp[_: P]: P[BaseApplyInfix] =
    P(atomicExp ~ CharsWhile(OpCharNotSlash).! ~ atomicExp).mapWithLoc {
      case (lhs, op, rhs) => BaseApplyInfix(lhs, Scala(meta.Term.Name(op)), rhs)
    }


  protected[frontend] def scalaTerm[_: P]: P[Scala[meta.Term]] =
    P(CharsWhile(_ != '`').!).flatMap { raw_code =>
      raw_code.parse[Term] match {
        case err: Parsed.Error =>
          ParserUtils.fail(err.message)
        case Parsed.Success(code) =>
          fastparse.Pass(Scala(code))
      }
    }


  /** Visibility parser */
  protected[frontend] def visibility[_: P]: P[Visibility] = P(privateVisibility)

  protected[frontend] def privateVisibility[_: P]: P[Visibility] =
    P("private").mapWithLoc(_ => Private)

  protected[frontend] def paramList[_: P]: P[Seq[Param]] = P(param.rep(sep = ","))

  /** Param parser */
  protected[frontend] def param[_: P]: P[Param] =
    P(identifier ~ ":" ~ typeAnno).mapWithLoc {
      case (name, typeAnno) => Param(name, typeAnno)
    }

  protected[frontend] def typeAnno[_: P]: P[Type] =
    P(simpleType("Any", TAny) | simpleType("Nothing", TNothing) |
      tOption | tSet |
      simpleType("Unit", TTuple(Seq())) | tTuple | tData | scalaType)

  /** Helper for the Type like TAny. */
  protected[frontend] def simpleType[_: P, Ty <: Type](s: String, t: Ty): P[Ty] =
    P(s).map(_ => t)

  /** TTuple parser without Unit */
  protected[frontend] def tTuple[_: P]: P[Type] =
    P("(" ~ typeAnno ~ ")") | P("(" ~ typeAnno.rep(2, sep = ",") ~ ")").map(TTuple)

  // mapWithLoc is not typable
  protected[frontend] def tData[_: P]: P[TData] = P(identifier.!).map(s => TData(Name(s)))

  protected[frontend] def scalaType[_: P]: P[Type] =
    P("`" ~~ scalaTypeCore ~~ "`")

  protected[frontend] def tOption[_: P]: P[Type] =
    P("Option" ~ "[" ~ typeAnno ~ "]").mapWithLoc(TOption)

  protected[frontend] def tSet[_: P]: P[Type] =
    P("Set" ~ "[" ~ typeAnno ~ "]").mapWithLoc(TSet)

  protected[frontend] def scalaTypeCore[_: P]: P[Type] =
    P(CharsWhile(_ != '`').!).flatMap { raw_code =>
      raw_code.parse[meta.Type] match {
        case err: Parsed.Error =>
          ParserUtils.fail(err.message)
        case Parsed.Success(ty) =>
          fastparse.Pass(TScala(Scala(ty)))
      }
    }

  /** DataDef parser */
  protected[frontend] def dataDef[_: P]: P[ModuleContent] = {
    P(annotation.rep ~ visibility.? ~ "data" ~ identifier ~ "=" ~ dataConstructor.rep(min = 1, sep = "|")).mapWithLoc {
      case (annos, vis, name, dataConstructors) => DataDef(annos, vis, name, dataConstructors)
    }
  }

  protected[frontend] def dataConstructor[_: P]: P[DataConstructor] =
    P(identifier ~ "(" ~ typeAnno.rep(sep = ",") ~ ")").mapWithLoc {
      case (name, paramTypes) => DataConstructor(name, paramTypes)
    }

  implicit class Ploc[T](p: => P[T])(implicit ctx: P[_]) {
    def mapWithLoc[U <: SourceLocation](f: T => U): P[U] =
      (Index ~ p ~ Index).map {
        case (start, t, end) =>
          val u = f(t)
          u.startIndex = start
          u.endIndex = end
          u
      }

    def mapWithLocFun[U <: SourceLocation, V <: SourceLocation](f: T => (U => V)): P[U => V] =
      (Index ~ p ~ Index).map {
        case (start, t, end) =>
          val uv = f(t)
          u => {
            val v = uv(u)
            v.startIndex = u.startIndex
            v.endIndex = end
            v
          }
      }

    def flatMapWithLoc[U <: SourceLocation](f: T => P[U]): P[U] =
      (Index ~ p ~ Index).flatMap {
        case (start, t, end) =>
          val up = f(t)
          up.map { u =>
            u.startIndex = start
            u.endIndex = end
            u
          }
      }
  }

}
