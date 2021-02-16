package inca.frontend_old.parser

import fastparse.ScalaWhitespace._
import fastparse._
import inca.frontend_old.core.tree._
import inca.util.Meta.Scala

import scala.language.reflectiveCalls
import scala.meta.Term
import scala.meta.parsers.{Parsed, _}

/**
  * Parser for the IncA Core language.
  */
trait CoreParser {

  final lazy val allKeywords: Set[String] = this.keywords

  protected[frontend_old] def keywords: Set[String] =
    Set("module", "import","def", "undef", "true",
      "false", "aggregate", "count", "_", "unit", "yield",
      "union", "private", "assert", "fail", "continue")

  // Parser ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /** Parse a variable identifier.
    * The first character must be an alphabetical one. After that digits and underscores are also allowed
    */
  def identifier[_: P]: P[Name] =
    P((CharIn("a-z", "A-Z", "_") ~~ CharIn("a-z", "A-Z", "0-9", "_").repX).!).mapWithLoc { s =>
      if (allKeywords.contains(s)) return fastparse.Fail
      else Name(s)
    }

  /** A parser for fully qualified identifier. Allows '.' in the name */
  protected[frontend_old] def fullyQualifiedIdentifier[_: P]: P[Name] =
    P((CharIn("a-z", "A-Z", "_") ~~ CharIn("a-z", "A-Z", "0-9", "_", ".").repX).!).mapWithLoc { s =>
      if(allKeywords.contains(s)) return fastparse.Fail else Name(s)
  }

  /** TNode parser */
  protected[frontend_old] def tNode[_: P]: P[TNode] = P(fullyQualifiedIdentifier.!).mapWithLoc(TNode)

  /** Helper for the Type like TAny. */
  protected[frontend_old] def simpleType[_: P, Ty <: Type](s: String, t: Ty): P[Ty] =
    P(s).mapWithLoc(_ => t)

  protected[frontend_old] def tAnyLinked[_: P]: P[TLinked] = simpleType("AnyNode", TAnyLinked)

  /** TLinked parser */
  protected[frontend_old] def tLinked[_: P]: P[TLinked] = P(tAnyLinked | tNode | tList)

  /** Type parser */
  protected[frontend_old] def typeAnno[_: P]: P[Type] =
    P(tAnyLinked | simpleType("Any", TAny) | simpleType("Boolean", TLiteral.Bool) | simpleType("Long", TLiteral.Long) |
        simpleType("Int", TLiteral.Int) | simpleType("Double", TLiteral.Double) | simpleType("String", TLiteral.String) |
        simpleType("Unit", TTuple(Seq())) | tLinked | tIterable | tTuple | scalaType
    )

  protected[frontend_old] def bracketedType[_: P]: P[Type] =
    P("[" ~ typeAnno ~ "]")

  /** Visibility parser */
  protected[frontend_old] def visibility[_: P]: P[Visibility] = P(privateVisibility)

  protected[frontend_old] def privateVisibility[_: P]: P[Visibility] =
    P("private").mapWithLoc(_ => Private)

  /** TTuple parser without Unit */
  protected[frontend_old] def tTuple[_: P]: P[Type] = {
    P("(" ~ typeAnno ~ ")") |
    P("(" ~ typeAnno.rep(2, sep = ",") ~ ")").mapWithLoc(TTuple)
  }

  /** TList parser */
  protected[frontend_old] def tList[_: P]: P[TLinked with TIterable] =
    P("List[" ~ tLinked ~ "]").mapWithLoc(TList)

  /** TEnumeration parser */
  protected[frontend_old] def tEnumeration[_: P]: P[TIterable] =
    P("Enum[" ~ tLinked ~ "]").mapWithLoc(TEnumeration)

  /** TIterable parser */
  protected[frontend_old] def tIterable[_: P]: P[TIterable] = P(tList | tEnumeration)

  /** Literal parser */
  protected[frontend_old] def literal[_: P]: P[Literal] =
    P(stringLiteral | unitLiteral | numericLiteral | booleanLiteral)

  /** UnitLiteral parser */
  protected[frontend_old] def unitLiteral[_: P]: P[Literal] = P("unit").mapWithLoc(_ => UnitLiteral)

  /** Whole number parser */
  protected[frontend_old] def numericLiteral[_: P]: P[Literal] =
    P("-".!.? ~~ ParserUtils.rawInteger ~~
      (("L" | "l").map(_=>"long") |
        "d".!.map(_=>"double") |
        "." ~~ (ParserUtils.rawInteger | "".!) ~~ "d".?
      ).?
    ).flatMapWithLoc { case (sign, whole, suffix) =>
      val integral = sign.getOrElse("") + whole
      suffix match {
        case None => integral.toIntOption match {
          case Some(i) => Pass(IntLiteral(i))
          case None => Fail
        }
        case Some("long") => integral.toLongOption match {
          case Some(l) => Pass(LongLiteral(l))
          case None => Fail
        }
        case Some("double") => integral.toDoubleOption match {
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
  def stringLiteral[_: P]: P[Literal] = P(ParserUtils.string).mapWithLoc(StringLiteral)

  /** BooleanLiteral parser */
  protected[frontend_old] def booleanLiteral[_: P]: P[Literal] =
    P(("true" | "false").!).mapWithLoc(s => BooleanLiteral(s.toBoolean))

  /** Param parser */
  protected[frontend_old] def param[_: P]: P[Param] =
    P(identifier ~ ":" ~ typeAnno).mapWithLoc {
      case (name, typeAnno) => Param(name, typeAnno)
    }

  /** Link parser */
  protected[frontend_old] def link[_: P]: P[Link] = coreLink

  /** CoreLink parser */
  protected[frontend_old] def coreLink[_: P]: P[CoreLink] =
    P(parentLink | childrenLink | prevLink | sizeLink | nextLink | namedLink)

  /** ParentLink parser */
  protected[frontend_old] def parentLink[_: P]: P[CoreLink] =
    P("parent").mapWithLoc(_ => ParentLink)

  /** ChildrenLink parser */
  protected[frontend_old] def childrenLink[_: P]: P[CoreLink] =
    P("children").mapWithLoc(_ => ChildrenLink)

  /** NextLink parser */
  protected[frontend_old] def nextLink[_: P]: P[CoreLink] =
    P("next").mapWithLoc(_ => NextLink)

  /** PreviousLink parser */
  protected[frontend_old] def prevLink[_: P]: P[CoreLink] =
    P("prev").mapWithLoc(_ => PreviousLink)

  /** SizeLink parser */
  protected[frontend_old] def sizeLink[_: P]: P[CoreLink] =
    P("size").mapWithLoc(_ => SizeLink)

  /** NamedLink parser */
  protected[frontend_old] def namedLink[_: P]: P[CoreLink] =
    P(identifier).mapWithLoc(NamedLink.apply)


  /** Exp parser */
  protected[frontend_old] def exp[_: P]: P[Expression] = wideExp

  protected[frontend_old] def wideExp[_: P]: P[Expression] =
    P(("def " ~ exp).mapWithLoc(Def) |
      ("undef " ~ exp).mapWithLoc(Undef) |
      ("count " ~ callExp).mapWithLoc(Count) |
      infixExp)

  protected[frontend_old] def infixExp[_: P]: P[Expression] =
    P(
      (dotExp ~ "==" ~ dotExp).mapWithLoc{ case (l,r) => Eq(l,r) } |
      (dotExp ~ "!=" ~ dotExp).mapWithLoc{ case (l,r) => Neq(l,r) } |
      dotExp
    )

  protected[frontend_old] def dotExp[_: P]: P[Expression] = {
    (atomicExp ~ trailExp.rep).map {
      case (e, trails) =>
        trails.foldLeft(e)((left, trailer) => trailer(left))
    }
  }

  protected[frontend_old] def trailExp[_: P]: P[Expression => Expression] =
    P("." ~ "isInstanceOf" ~ bracketedType).mapWithLocFun[Expression, Expression](ty => InstanceOf(_, ty)) |
    P("." ~ "notInstanceOf" ~ bracketedType).mapWithLocFun[Expression, Expression](ty => NotInstanceOf(_, ty)) |
    P("." ~ link).mapWithLocFun[Expression, Expression](l => PathAccess(_, l)) |
    P(":" ~ typeAnno).mapWithLocFun[Expression, Expression](ty => Cast(_, ty))

  protected[frontend_old] def atomicExp[_: P]: P[Expression] =
    P(evalExp | callExp | wildcardExp | constantExp | varExp
     | tupleExp | aggregateExp | parensExp)


  protected[frontend_old] def Chain[_: P, A <: SourceLocation, B <: SourceLocation](p: => P[A], op: String, q: => P[B], opNode: (A, B) => A, min: Int = 0): P[A] =
    P( p ~ (op ~ q).rep(min) ).mapWithLoc {
      case (lhs, chunks) =>
        chunks.foldLeft(lhs){case (lhs, rhs) =>
          val node = opNode(lhs, rhs)
          node.startIndex = lhs.startIndex
          node.endIndex = rhs.endIndex
          node
        }
    }

  /** Eval parser */
  protected[frontend_old] def evalCore[_: P]: P[Eval] =
    P(CharsWhile(_ != '`').!).flatMap { raw_code =>
      raw_code.parse[Term] match {
        case err: Parsed.Error =>
          ParserUtils.fail(err.message)
        case Parsed.Success(code) =>
          val eval = Eval(Scala(code))
          fastparse.Pass(eval)
      }
    }

  protected[frontend_old] def evalExp[_: P]: P[CoreExpression] =
    P("`" ~~ evalCore ~~ "`").mapWithLoc(t => t)

  /** Call parser */
  protected[frontend_old] def callExp[_: P]: P[Call] =
    P(fullyQualifiedIdentifier ~ "+".?.! ~ ("(" ~ exp.rep(sep = ",") ~ ")")).mapWithLoc {
      case (name, transitive_str, exp) =>
        Call(name, exp, transitive_str == "+")
    }

  /** Tuple parser */
  protected[frontend_old] def tupleExp[_: P]: P[CoreExpression] =
    P("(" ~ exp.rep(2, sep = ",") ~ ")").mapWithLoc(Tuple)

  /** Parens parser */
  protected[frontend_old] def parensExp[_: P]: P[Expression] = P("(" ~ exp ~ ")")

  /** Var parser */
  protected[frontend_old] def varExp[_: P]: P[CoreExpression] = P(identifier).mapWithLoc(Var.apply)

  protected[frontend_old] def wildcardExp[_: P]: P[CoreExpression] = P("_").mapWithLoc(_ => Wildcard)

  /** Constant parser */
  protected[frontend_old] def constantExp[_: P]: P[CoreExpression] = P(literal).mapWithLoc(Constant)

  /** Statement parser */
  protected[frontend_old] def statement[_: P]: P[Statement] =
    coreStatement | terminatorStatement

  /** CoreStatement parser */
  final protected[frontend_old] def coreStatement[_: P]: P[CoreStatement] =
    P(valuesStatement | assignStatement | assertStatement)

  /** Values parser */
  final protected[frontend_old] def valuesStatement[_: P]: P[CoreStatement] =
    P("vals " ~ identifier ~ "<-" ~ typeAnno).mapWithLoc {
      case (name, typeAnno) => Values(name, typeAnno)
    }

  /** Assign parser */
  final protected[frontend_old] def assignStatement[_: P]: P[CoreStatement] =
    P(singleAssignStatement | multipleAssignStatement)

  final protected[frontend_old] def singleAssignStatement[_: P]: P[CoreStatement] =
    P("val " ~ identifier ~ "=" ~ exp).mapWithLoc {
      case (name, expr) => Assign(Seq(name), expr)
    }

  final protected[frontend_old] def multipleAssignStatement[_: P]: P[CoreStatement] =
    P("val " ~ "(" ~ identifier.rep(min = 2, sep = ",") ~ ")" ~ "=" ~ exp).mapWithLoc {
      case (names, expr) => Assign(names, expr)
    }

  /** Assert parser */
  final protected[frontend_old] def assertStatement[_: P]: P[CoreStatement] =
    P("assert " ~ exp).mapWithLoc(Assert)

  /** Body parser */
  protected[frontend_old] def body[_: P]: P[Body] =
    P("{" ~ statement.rep ~ "}").mapWithLoc(Body.apply) |
    P(statement).mapWithLoc(s => Body(Seq(s)))

  /** PatternFunction parser */
  protected[frontend_old] def patternFunction[_: P]: P[ModuleContent] = {
    P(visibility.? ~ "def" ~ identifier ~ "(" ~ paramList ~ ")" ~ ":" ~ typeAnno ~ "=" ~ bodyList).mapWithLoc{
      case (vis, name, params, ty, bodies) => PatternFunction(vis, name, params, ty, bodies)
    }
  }

  protected[frontend_old] def paramList[_: P]: P[Seq[Param]] = P(param.rep(sep = ","))
  protected[frontend_old] def bodyList[_: P]: P[Seq[Body]] = P(body.rep(min = 1, sep = "union"))

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
    P(patternFunction.map(Seq(_)) | valDef.map(Seq(_)) | scalaModuleContent)


  def scalaModuleContent[_: P]: P[Seq[ModuleContent]] =
    P("```" ~/ takeCharsUntil("```")).flatMap(s => nativeStatHelper(s, multiple = true)) |
    P("`" ~ takeCharsUntil("`")).flatMap(s => nativeStatHelper(s, multiple = false))

  def takeCharsUntil[_: P](p: => P[_]): P[String] =
    p.map(_ => "") | P(SingleChar ~~ takeCharsUntil(p)).map { case (c, str) => c +: str }

  private def nativeStatHelper[_: P](raw_code: String, multiple: Boolean): P[Seq[ModuleContent]] = {
    meta.dialects.Sbt1(raw_code).parse[meta.Source] match {
      case err: Parsed.Error =>
        ParserUtils.fail(err.message)
      case Parsed.Success(code) =>
        val stats = code.stats
        if (!multiple && stats.size != 1)
          fastparse.Fail(s"Required exactly one statement, but got ${stats.size} in ${code.syntax}")
        else
          fastparse.Pass(stats.map(stat => ScalaModuleContent(Scala(stat))))
    }
  }

  protected[frontend_old] def valDef[_: P]: P[ModuleContent] =
    P(visibility.? ~ "val" ~ identifier ~ (":" ~ typeAnno).? ~ "=" ~ exp).mapWithLoc {
      case (vis, name, ty, expression) => ValDef(vis, name, ty, expression)
    }


  /** Yield parser */
  protected[frontend_old] def yieldStatement[_: P]: P[Statement] =
    P("yield " ~ exp).mapWithLoc(Yield)

  /** Fail/Continue parser */
  protected[frontend_old] def failStatement[_: P]: P[Statement] =
    P("continue" | "fail").mapWithLoc(_ => FailStatement)

  /** Terminator parser. */
  protected[frontend_old] def terminatorStatement[_: P]: P[Statement] =
    P(yieldStatement | failStatement)

  protected[frontend_old] def scalaType[_: P]: P[Type] =
    P("`" ~~ scalaTypeCore ~~ "`")

  protected[frontend_old] def scalaTypeCore[_: P]: P[Type] =
    P(CharsWhile(_ != '`').!).flatMap { raw_code =>
      raw_code.parse[meta.Type] match {
        case err: Parsed.Error =>
          ParserUtils.fail(err.message)
        case Parsed.Success(ty) =>
          fastparse.Pass(TScala(Scala(ty)))
      }
    }

  /** Aggregate parser */
  protected[frontend_old] def aggregateExp[_: P]: P[Expression] =
    P("aggregate" ~ "(" ~ exp ~ ")" ~ bodyList).mapWithLoc { case (agg, bodies) =>
      Aggregate(agg, bodies)
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
