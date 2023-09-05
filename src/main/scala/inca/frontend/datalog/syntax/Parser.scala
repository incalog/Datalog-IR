package inca.frontend.datalog.syntax

import fastparse._
import fastparse.ScalaWhitespace._
import inca.compiler.source.SourceLocation
import inca.frontend.util.ParserUtils
import inca.util.Scala
import scala.language.reflectiveCalls
import scala.meta.parsers._
import scala.meta.parsers.Parsed

/**
 * Parser for the IncA Core language.
 */
trait Parser {

  final lazy val allKeywords: Set[String] =
    Set("data", "relation", "not", "true", "false")

  def nochar[_: P]: P[Unit] =
    P(!CharIn("a-z", "A-Z", "0-9", "_"))

  def identifier[_: P]: P[Name] =
    P((CharIn("a-z", "A-Z", "_") ~~ CharIn("a-z", "A-Z", "0-9", "_").repX).!).mapWithLoc { s =>
      if (allKeywords.contains(s)) return fastparse.Fail
      else Name(s)
    }

  /** Module parser */
  def module[_: P]: P[Module] =
    P(
      "module " ~ identifier ~
        moduleContent.rep ~
        End).mapWithLoc { case (name, contents) =>
      Module(name, contents)
    }

  def moduleContent[_: P]: P[ModuleContent] =
    P(ruleSig | rule | dataDef)

  protected[frontend] def annotation[_: P]: P[Annotation] = extensionalAnno | mainAnno
  protected[frontend] def extensionalAnno[_: P]: P[ExtensionalAnno.type] =
    P("@extensional").map(_ => ExtensionalAnno)
  protected[frontend] def mainAnno[_: P]: P[MainAnno.type] = P("@main").map(_ => MainAnno)

  protected[frontend] def ruleSig[_: P]: P[RuleSig] =
    P(annotation.rep ~ "relation" ~ identifier ~ "(" ~ typeAnno.rep(sep = ",") ~ ")").mapWithLoc {
      case (annos, name, tys) =>
        RuleSig(annos, name, tys.map(Param))
    }

  protected[frontend] def rule[_: P]: P[Rule] =
    P(identifier ~ "(" ~ term.rep(sep = ",") ~ ")" ~ ".").mapWithLoc { case (name, headTerms) =>
      Rule(name, headTerms, Seq())
    } |
      P(
        identifier ~ "(" ~ term.rep(sep = ",") ~ ")" ~ ":-" ~ atom.rep(sep =
          ",") ~ ".").mapWithLoc { case (name, headTerms, atoms) =>
        Rule(name, headTerms, atoms)
      }

  protected[frontend] def term[_: P]: P[Term] =
    P(path | constant | "_".!.map(_ => Wildcard()) | variable)

  protected[frontend] def path[_: P]: P[Path] =
    P((constant | variable) ~~ ("." ~~ identifier).rep(1)).mapWithLoc { case (term, links) =>
      links.foldLeft(term)(Path.apply).asInstanceOf[Path]
    }

  protected[frontend] def atom[_: P]: P[Atom] =
    compare | call

  protected[frontend] def compare[_: P]: P[Compare] =
    P(term ~ ("==" | "!=").! ~ term).mapWithLoc {
      case (lhs, "==", rhs) => Compare(EqComparator, lhs, rhs)
      case (lhs, "!=", rhs) => Compare(NeqComparator, lhs, rhs)
      case _ => throw new IllegalArgumentException
    }

  protected[frontend] def call[_: P]: P[Call] =
    P("not ".!.? ~ identifier ~ "(" ~ term.rep(sep = ",") ~ ")").mapWithLoc {
      case (not, name, args) => Call(name, args, not.isDefined)
    }

  protected[frontend] def variable[_: P]: P[Var] =
    identifier.mapWithLoc(Var.apply)

  protected[frontend] def constant[_: P]: P[Constant] =
    (numericLiteral |
      stringLiteral |
      booleanLiteral).map(Constant)

  protected[frontend] def numericLiteral[_: P]: P[Literal] =
    P(
      "-".!.? ~~ ParserUtils.rawInteger ~~
        (("L" | "l").map(_ => "long") |
          "d".!.map(_ => "double") |
          "." ~~ (ParserUtils.rawInteger | "".!) ~~ "d".?).?).flatMapWithLoc {
      case (sign, whole, suffix) =>
        val integral = sign.getOrElse("") + whole
        suffix match {
          case None =>
            integral.toIntOption match {
              case Some(i) => Pass(IntLiteral(i))
              case None => Fail
            }
          case Some("long") =>
            integral.toLongOption match {
              case Some(l) => Pass(LongLiteral(l))
              case None => Fail
            }
          case Some("double") =>
            integral.toDoubleOption match {
              case Some(d) => Pass(DoubleLiteral(d))
              case None => Fail
            }
          case Some(fraction) =>
            s"$integral.$fraction".toDoubleOption match {
              case Some(d) => Pass(DoubleLiteral(d))
              case None => Fail
            }
        }
    }

  /** StringLiteral parser */
  protected[frontend] def stringLiteral[_: P]: P[Literal] =
    P(ParserUtils.string).mapWithLoc(s => StringLiteral(s))

  /** BooleanLiteral parser */
  protected[frontend] def booleanLiteral[_: P]: P[Literal] =
    P(("true" | "false").!).mapWithLoc(s => BooleanLiteral(s.toBoolean))

  protected[frontend] def dataConstrParamList[_: P]: P[Seq[DataConstrParam]] = P(
    dataConstrParam.rep(sep = ","))

  /** Param parser */
  protected[frontend] def dataConstrParam[_: P]: P[DataConstrParam] =
    P(identifier ~ ":" ~ typeAnno).mapWithLoc { case (name, typeAnno) =>
      DataConstrParam(name, typeAnno)
    }

  protected[frontend] def typeAnno[_: P]: P[Type] =
    atomicType

  protected[frontend] def atomicType[_: P]: P[Type] =
    P(
      simpleType("Any", TAny) | simpleType("Nothing", TNothing) |
        simpleType("Boolean", TLiteral.Bool) | simpleType("Long", TLiteral.Long) |
        simpleType("Int", TLiteral.Int) | simpleType("Double", TLiteral.Double) | simpleType(
          "String",
          TLiteral.String) |
        scalaType | tData)

  /** Helper for the Type like TAny. */
  protected[frontend] def simpleType[_: P, Ty <: Type](s: String, t: Ty): P[Ty] =
    P(s ~~ nochar).map(_ => t)

  // mapWithLoc is not typable
  protected[frontend] def tData[_: P]: P[TData] = P(identifier.!).map(s => TData(Name(s)))

  protected[frontend] def scalaType[_: P]: P[Type] =
    P("`" ~~ scalaTypeCore ~~ "`")

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
    P(
      annotation.rep ~ "data" ~ identifier ~ "=" ~ dataConstructor.rep(
        min = 1,
        sep = "|")).mapWithLoc { case (annos, name, dataConstructors) =>
      DataDef(annos, name, dataConstructors)
    }
  }

  protected[frontend] def dataConstructor[_: P]: P[DataConstructor] =
    P(identifier ~ "(" ~ dataConstrParamList ~ ")").mapWithLoc { case (name, params) =>
      DataConstructor(name, params)
    }

  implicit class Ploc[T](p: => P[T])(implicit ctx: P[_]) {
    def mapWithLoc[U <: SourceLocation](f: T => U): P[U] =
      (Index ~ p ~ Index).map { case (start, t, end) =>
        val u = f(t)
        u.startIndex = start
        u.endIndex = end
        u
      }

    def mapWithLocFun[U <: SourceLocation, V <: SourceLocation](f: T => (U => V)): P[U => V] =
      (Index ~ p ~ Index).map { case (start, t, end) =>
        val uv = f(t)
        u => {
          val v = uv(u)
          v.startIndex = u.startIndex
          v.endIndex = end
          v
        }
      }

    def flatMapWithLoc[U <: SourceLocation](f: T => P[U]): P[U] =
      (Index ~ p ~ Index).flatMap { case (start, t, end) =>
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
