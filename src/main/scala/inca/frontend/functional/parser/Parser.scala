package inca.frontend.functional.parser

import fastparse.ScalaWhitespace._
import fastparse._
import inca.compiler.SourceLocation
import inca.frontend.functional.core._
import inca.frontend.util.ParserUtils
import inca.util.Scala
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
      Set("Option", "None", "Some", "Set", "fold")

  def nochar[_: P]: P[Unit] =
    P(!CharIn("a-z", "A-Z", "0-9", "_"))

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
    P(annotation.rep ~ visibility.? ~ "def" ~ identifier ~ defParams ~ ":" ~ typeAnno ~ "=" ~ exp).mapWithLoc{
      case (annos, vis, name, params, ty, exp) =>
        FunctionDef(annos, vis, name, params, ty, exp)
    }
  }

  protected[frontend] def defParams[_: P]: P[Seq[Param]] =
    P("(" ~ paramList ~ ")") | P("").map(_ => Seq())

  protected[frontend]  def annotation[_: P]: P[Annotation] = mainFuncAnno
  protected[frontend]  def mainFuncAnno[_: P]: P[MainFunctionAnno.type] = P("@main").map(_ => MainFunctionAnno)

  protected[frontend] def exp[_: P]: P[Expression] = wideExp

  protected[frontend] def wideExp[_: P]: P[Expression] =
    P(ifExp | letExp | memberExp | infixExp)
  protected[frontend] def infixExp[_: P]: P[Expression] =
    P(typeCastExp | baseApplyMethodExp | baseApplyInfixExp | matchExp | subinfixExp)
  protected[frontend] def subinfixExp[_: P]: P[Expression] =
    P(callExp | lambdaExp | atomicExp)
  protected[frontend] def atomicExp[_: P]: P[Expression] =
    P(parensExp | optionExp | comprehensionExp | constSetExp |
      tupleExp | foldExp | baseApplyExp | baseLitExp | variable | baseApplyUnaryExp)

  /** Let parser */
  final protected[frontend] def parensExp[_: P]: P[Expression] = P("(" ~ exp ~ ")")

  final protected[frontend] def letExp[_: P]: P[Let] =
    P(singleLetExp | multipleLetExp)

  final protected[frontend] def singleLetExp[_: P]: P[Let] =
    P("let " ~ identifier ~ (":" ~ typeAnno).? ~ "=" ~ infixExp ~ "in" ~ exp).mapWithLoc {
      case (name, typeAnno, bound, body) => Let(Seq(name), typeAnno, bound, body)
    }

  final protected[frontend] def multipleLetExp[_: P]: P[Let] =
    P("let " ~ "(" ~ identifier.rep(min = 2, sep = ",") ~ ")" ~ (":" ~ typeAnno).? ~ "=" ~ infixExp ~ "in" ~ exp).mapWithLoc {
      case (names, typeAnno, bound, body) => Let(names, typeAnno, bound, body)
    }


  protected[frontend] def ifExp[_: P]: P[If] =
    P("if" ~ "(" ~ exp ~ ")" ~ exp ~ "else" ~ exp).mapWithLoc {
      case (cond, thn, els) => If(cond, thn, els)
    }

  protected[frontend] def typeCastExp[_: P]: P[TypeCast] =
    P(subinfixExp ~~ ".as[" ~ tData ~ "]") mapWithLoc {
      case (e, ty) => TypeCast(e, ty)
    }

  protected[frontend] def callExp[_: P]: P[Expression] =
    P(atomicExp ~ ("(" ~ exp.rep(sep = ",") ~ ")").rep(1)).mapWithLoc {
      case (fun, argLists) => argLists.foldLeft(fun)((exp, args) => Call(exp, args))
    }

  protected[frontend] def foldExp[_: P]: P[SetFold] =
    P("fold" ~ ("[" ~ typeAnno ~ "]").? ~ "(" ~ exp ~ "," ~ exp ~ "," ~ exp ~ ")").mapWithLoc { case (ty, init, op, set) =>
      SetFold(ty, init, op, set)
    }

  protected[frontend] def tupleExp[_: P]: P[Expression] =
    P("(" ~ exp.rep(2, sep = ",") ~ ")").mapWithLoc(Tuple.apply)


  protected[frontend] def variable[_: P]: P[Var] =
    identifier.mapWithLoc(Var.apply)

  protected[frontend] def matchExp[_: P]: P[Match] =
    P(subinfixExp ~ "match" ~ "{" ~ matchCase.rep() ~ "}").mapWithLoc {
      case (matchee, cases) => Match(matchee, cases)
    }

  protected[frontend] def matchCase[_: P]: P[(Pattern, Expression)] =
    P("case" ~ pattern ~ "=>" ~ exp).map {
      case (pat, body) => (pat, body)
    }

  protected[frontend] def lambdaExp[_: P]: P[Lambda] =
    P(lambdaVars ~ "=>" ~ exp).mapWithLoc(Lambda.tupled)

  protected[frontend] def lambdaVars[_: P]: P[Seq[(Name, Type)]] =
    P("(" ~ (identifier ~ ":" ~ typeAnno).rep(sep = ",") ~ ")")

  protected[frontend] def optionExp[_: P]: P[Expression] = {
    P("None" ~~ nochar).mapWithLoc(_ => NoneExp()) |
    P("Some" ~ "(" ~ exp.rep(sep = ",") ~ ")").mapWithLoc {
      case Seq(arg) => SomeExp(arg)
      case args => Call(Var(Name("Some")), args)
    }
  }

  protected[frontend] def constSetExp[_: P]: P[Expression] =
    P("{" ~ exp.rep(sep = ",") ~ "}").mapWithLoc(SetExp)

  protected[frontend] def comprehensionExp[_: P]: P[Expression] =
    P("{" ~ subinfixExp ~ "|" ~ exp.rep(sep = ",") ~ "}").mapWithLoc(SetComprehension.tupled)

  protected[frontend] def memberExp[_: P]: P[Expression] =
    P(atomicExp ~ "not".!.? ~ "in" ~ infixExp).mapWithLoc { case (tup, not, set) => SetMember(tup, set, not.isDefined) }

  protected[frontend] def pattern[_: P]: P[Pattern] =
    P(optionPattern | constructorPattern)

  protected[frontend] def constructorPattern[_: P]: P[ConstructorPattern] = {
    P(identifier ~ "(" ~ identifier.rep(sep = ",") ~")").mapWithLoc {
      case (name, args) => ConstructorPattern(name, args)
    }
  }

  protected[frontend] def optionPattern[_: P]: P[Pattern] =
    P("None" ~~ nochar).mapWithLoc(_ => NonePattern()) |
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

  protected[frontend] def baseApplyUnaryExp[_: P]: P[BaseApplyUnary] =
    P(CharsWhile(OpCharNotSlash).! ~ infixExp).flatMapWithLoc {
//      case ("@", _) => ParserUtils.fail("@ not allowed as infix opertor")
      case (op, rhs) => fastparse.Pass(BaseApplyUnary(op, rhs))
    }

  protected[frontend] def baseApplyMethodExp[_: P]: P[BaseApplyMethod] =
    P(subinfixExp ~~ ".`" ~~ identifier ~~ "`" ~ ("(" ~ infixExp.rep(sep = ",") ~ ")").?).mapWithLoc {
      case (recv, method, None) => BaseApplyMethod(recv, method, None)
      case (recv, method, Some(args)) => BaseApplyMethod(recv, method, Some(args))
    }

  protected[frontend] def baseApplyInfixExp[_: P]: P[BaseApplyInfix] =
    P(subinfixExp ~ CharsWhile(OpCharNotSlash).! ~ infixExp).flatMapWithLoc {
      case (_, "@", _) => ParserUtils.fail("@ not allowed as infix opertor")
      case (_, "=>", _) => ParserUtils.fail("=> not allowed as infix opertor")
      case (_, "|", _) => ParserUtils.fail("| not allowed as infix opertor")
      case (lhs, op, rhs) => fastparse.Pass(BaseApplyInfix(lhs, Scala(meta.Term.Name(op)), rhs))
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
    P(funType | atomicType)

  protected[frontend] def atomicType[_: P]: P[Type] =
    P(tTuple | simpleType("Any", TAny) | simpleType("Nothing", TNothing) | simpleType("Unit", TTuple(Seq())) |
      tOption | tSet | scalaType | tData)

  /** Helper for the Type like TAny. */
  protected[frontend] def simpleType[_: P, Ty <: Type](s: String, t: Ty): P[Ty] =
    P(s ~~ nochar).map(_ => t)

  protected[frontend] def funType[_: P]: P[Type] =
    P(atomicType ~  "=>" ~ typeAnno).mapWithLoc {
      case (TTuple(ts), to) => TFun(ts, to)
      case (from, to) => TFun(Seq(from), to)
    }

  /** TTuple parser without Unit */
  protected[frontend] def tTuple[_: P]: P[Type] =
    P("(" ~ typeAnno.rep(sep = ",") ~ ")").map(TTuple.from)

  // mapWithLoc is not typable
  protected[frontend] def tData[_: P]: P[TData] = P(identifier.!).map(s => TData(Name(s)))

  protected[frontend] def scalaType[_: P]: P[Type] =
    P("`" ~~ scalaTypeCore ~~ "`") |
      P("Int" ~~ nochar).mapWithLoc(_ => TScalaInt) |
      P("Long" ~~ nochar).mapWithLoc(_ => TScalaLong) |
      P("String" ~~ nochar).mapWithLoc(_ => TScalaString) |
      P("Boolean" ~~ nochar).mapWithLoc(_ => TScalaBoolean) |
      P("Double" ~~ nochar).mapWithLoc(_ => TScalaDouble)


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

object Parser {
  private lazy val parser: Parser = new Parser {}
  def parse(code: ParserInput): Module = {
    import fastparse.Parsed

    fastparse.parse(code, parser.module(_), verboseFailures = true) match {
      case Parsed.Success(value, _) => value
      case fail: Parsed.Failure =>
        throw new IllegalArgumentException(s"Parsing Error: ${fail.trace(true).longTerminalsMsg}")
    }
  }
}