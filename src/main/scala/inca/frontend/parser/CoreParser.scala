package inca.frontend.parser

import fastparse.ScalaWhitespace._
import fastparse._
import inca.frontend.core.Core
import inca.frontend.core.Core.{Name, _}

import scala.meta._
import scala.meta.parsers.Parsed

/**
  * Parser for the IncA Core language.
  */
class CoreParser {
  final lazy val allKeywords: Set[String] = this.keywords

  protected[frontend] def keywords: Set[String] =
    Set("def", "undef", "true", "false", "eval", "aggregate", "count", "_", "unit", "isInstanceOf", "notInstanceOf")

  // Parser ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /** Parse a variable identifier.
    * The first character must be an alphabetical one. After that digits and underscores are also allowed
    */
  def identifier[_: P]: P[Name] =
    P((CharIn("a-z", "A-Z", "_") ~~ CharIn("a-z", "A-Z", "0-9", "_").repX).!).mapWithLoc { s =>
      if (allKeywords.contains(s)) return fastparse.Fail
      else Name(s)
    }

  /** TAnyLinked parser */
  protected[frontend] def tAnyLinked[_: P]: P[TLinked] =
    P(TAnyLinked.prettyprint).mapWithLoc(_ => TAnyLinked)

  /** A parser for fully qualified identifier. Allows '.' in the name */
  protected[frontend] def fullyQualifiedIdentifier[_: P]: P[Name] =
    P((CharIn("a-z", "A-Z", "_") ~~ CharIn("a-z", "A-Z", "0-9", "_", ".").repX).!).mapWithLoc { s =>
      if(allKeywords.contains(s)) return fastparse.Fail else Name(s)
  }

  /** TNode parser */
  protected[frontend] def tNode[_: P]: P[TNode] = P(fullyQualifiedIdentifier.!).mapWithLoc(TNode)

  /** Helper for the TypeAnno like TAny. */
  protected[frontend] def simpleTypeAnno[_: P](t: TypeAnno): P[TypeAnno] =
    P(t.prettyprint).mapWithLoc(_ => t)

  /** TLinked parser */
  protected[frontend] def tLinked[_: P]: P[TLinked] = P(tAnyLinked | tNode | tList)

  /** TypeAnno parser */
  protected[frontend] def typeAnno[_: P]: P[TypeAnno] =
    P(simpleTypeAnno(TAny) | simpleTypeAnno(TBool) | simpleTypeAnno(TLong) |
        simpleTypeAnno(TInt) | simpleTypeAnno(TDouble) | simpleTypeAnno(TString) |
        simpleTypeAnno(TUnit) | tLinked | tIterable | tTuple | dataType
    )

  protected[frontend] def bracketedTypeAnno[_: P]: P[TypeAnno] =
    P("[" ~ typeAnno ~ "]")

  /** Visibility parser */
  protected[frontend] def visibility[_: P]: P[Visibility] = P(privateVisibility | publicVisivility)

  protected[frontend] def privateVisibility[_: P]: P[Private.type] =
    P(Private.prettyprint("")).mapWithLoc(_ => Private)
  protected[frontend] def publicVisivility[_: P]: P[Public.type] =
    P(Public.prettyprint("")).mapWithLoc(_ => Public)

  /** TTuple parser without Unit */
  protected[frontend] def tTuple[_: P]: P[TypeAnno] = {
    P("(" ~ typeAnno ~ ")") |
    P("(" ~ typeAnno.rep(2, sep = ",") ~ ")").mapWithLoc(TTuple)
  }

  /** TList parser */
  protected[frontend] def tList[_: P]: P[TList] =
    P("List[" ~ tLinked ~ "]").mapWithLoc(TList)

  /** TEnumeration parser */
  protected[frontend] def tEnumeration[_: P]: P[TEnumeration] =
    P("Enum[" ~ tLinked ~ "]").mapWithLoc(TEnumeration)

  /** TIterable parser */
  protected[frontend] def tIterable[_: P]: P[TIterable] = P(tList | tEnumeration)

  /** Literal parser */
  protected[frontend] def literal[_: P]: P[Literal] =
    P(stringLiteral | unitLiteral /* tuple*/ | doubleLiteral | longLiteral |
        intLiteral | booleanLiteral)

  /** UnitLiteral parser */
  protected[frontend] def unitLiteral[_: P]: P[UnitLiteral.type] = P("unit").mapWithLoc(_ => UnitLiteral)

  /** IntLiteral parser */
  protected[frontend] def intLiteral[_: P]: P[IntLiteral] = P(ParserUtils.integer).mapWithLoc(IntLiteral)

  /** LongLiteral parser */
  def longLiteral[_: P]: P[LongLiteral] = P(ParserUtils.long ~ "L").mapWithLoc(LongLiteral)

  /** DoubleLiteral parser */
  protected[frontend] def doubleLiteral[_: P]: P[DoubleLiteral] = P(ParserUtils.double).mapWithLoc(DoubleLiteral)

  /** StringLiteral parser */
  def stringLiteral[_: P]: P[StringLiteral] = P(ParserUtils.string).mapWithLoc(StringLiteral)

  /** BooleanLiteral parser */
  protected[frontend] def booleanLiteral[_: P]: P[BooleanLiteral] =
    P(("true" | "false").!).mapWithLoc(s => BooleanLiteral(s.toBoolean))

  /** Param parser */
  protected[frontend] def param[_: P]: P[Param] =
    P(identifier ~ ":" ~ typeAnno).mapWithLoc {
      case (name, typeAnno) => Param(name, typeAnno)
    }

  /** AnnoParam parser */
  protected[frontend] def annoParam[_: P]: P[AnnoParam] =
    P("(" ~ param ~ ")" | typeAnno).mapWithLoc {
      case Param(name, typeAnno) => AnnoParam(Some(name), typeAnno)
      case typeAnno: TypeAnno    => AnnoParam(None, typeAnno)
    }

  /** Link parser */
  protected[frontend] def link[_: P]: P[Link] = coreLink

  /** CoreLink parser */
  protected[frontend] def coreLink[_: P]: P[CoreLink] =
    P(parentLink | childrenLink | prevLink | sizeLink | nextLink | namedLink)

  /** ParentLink parser */
  protected[frontend] def parentLink[_: P]: P[ParentLink.type] =
    P(ParentLink.prettyprint).mapWithLoc(_ => ParentLink)

  /** ChildrenLink parser */
  protected[frontend] def childrenLink[_: P]: P[ChildrenLink.type] =
    P(ChildrenLink.prettyprint).mapWithLoc(_ => ChildrenLink)

  /** NextLink parser */
  protected[frontend] def nextLink[_: P]: P[NextLink.type] =
    P(NextLink.prettyprint).mapWithLoc(_ => NextLink)

  /** PreviousLink parser */
  protected[frontend] def prevLink[_: P]: P[PreviousLink.type] =
    P(PreviousLink.prettyprint).mapWithLoc(_ => PreviousLink)

  /** SizeLink parser */
  protected[frontend] def sizeLink[_: P]: P[SizeLink.type] =
    P(SizeLink.prettyprint).mapWithLoc(_ => SizeLink)

  /** NamedLink parser */
  protected[frontend] def namedLink[_: P]: P[NamedLink] =
    P(identifier).mapWithLoc(NamedLink.apply)


  /** Exp parser */
  protected[frontend] def exp[_: P]: P[Exp] = wideExp

  protected[frontend] def wideExp[_: P]: P[Exp] =
    P(("def " ~ exp).mapWithLoc(Def) |
      ("undef " ~ exp).mapWithLoc(Undef) |
      ("count " ~ callExp).mapWithLoc(Count) |
      Chain(infixExp, "==", infixExp, Eq, min = 1) |
      Chain(infixExp, "!=", infixExp, Neq, min = 1) |
      infixExp)

  protected[frontend] def infixExp[_: P]: P[Exp] = dotExp

  protected[frontend] def dotExp[_: P]: P[Exp] = {
    (atomicExp ~ trailExp.rep).map {
      case (e, trails) =>
        trails.foldLeft(e)((left, trailer) => trailer(left))
    }
  }

  protected[frontend] def trailExp[_: P]: P[Exp => Exp] =
    P("." ~ link).mapWithLocFun[Exp, Exp](l => PathAccess(_, l)) |
    P("." ~ "isInstanceOf" ~ bracketedTypeAnno).mapWithLocFun[Exp, Exp](ty => InstanceOf(_, ty)) |
    P("." ~ "notInstanceOf" ~ bracketedTypeAnno).mapWithLocFun[Exp, Exp](ty => NotInstanceOf(_, ty)) |
    P(":" ~ typeAnno).mapWithLocFun[Exp, Exp](ty => Cast(_, ty))

  protected[frontend] def atomicExp[_: P]: P[Exp] =
    P(callExp | evalExp | wildcardExp | constantExp | varExp
     | tupleExp | aggregateExp | parensExp)


  protected[frontend] def Chain[_: P, A <: SourceLocation, B <: SourceLocation](p: => P[A], op: String, q: => P[B], opNode: (A, B) => A, min: Int = 0): P[A] =
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
  protected[frontend] def evalCore[_: P]: P[Eval] = P(scalaparse.Scala.Exprs.!).flatMap { raw_code =>
    raw_code.parse[Term] match {
      case Parsed.Error(_, _, _) =>
        fastparse.Fail
      case Parsed.Success(code) =>
        val params = EvalHelper.freeVars(code)
        val eval = Eval(params.toSeq, code)
        fastparse.Pass(eval)
    }
  }

  protected[frontend] def evalExp[_: P]: P[Eval] =
    P("eval" ~ (("(" ~ evalCore ~ ")") | ("{" ~ evalCore ~ "}"))).mapWithLoc(t => t)

  /** Call parser */
  protected[frontend] def callExp[_: P]: P[Call] =
    P(fullyQualifiedIdentifier ~ "+".?.! ~ ("(" ~ exp.rep(sep = ",") ~ ")")).mapWithLoc {
      case (name, transitive_str, exp) =>
        Call(name, exp, transitive_str == "+")
    }

  /** Tuple parser */
  protected[frontend] def tupleExp[_: P]: P[Tuple] =
    P("(" ~ exp.rep(2, sep = ",") ~ ")").mapWithLoc(Tuple)

  /** Parens parser */
  protected[frontend] def parensExp[_: P]: P[Exp] = P("(" ~ exp ~ ")")

  /** Var parser */
  protected[frontend] def varExp[_: P]: P[Var] = P(identifier).mapWithLoc(Var.apply)

  protected[frontend] def wildcardExp[_: P]: P[Wildcard.type] = P("_").mapWithLoc(_ => Wildcard)

  /** Constant parser */
  protected[frontend] def constantExp[_: P]: P[Constant] = P(literal).mapWithLoc(Constant)

  /** Statement parser */
  protected[frontend] def statement[_: P]: P[Statement] =
    coreStatement | terminatorStatement

  /** CoreStatement parser */
  final protected[frontend] def coreStatement[_: P]: P[CoreStatement] =
    P(valuesStatement | assignStatement | assertStatement)

  /** Values parser */
  final protected[frontend] def valuesStatement[_: P]: P[Values] =
    P("vals " ~ identifier ~ "<-" ~ typeAnno).mapWithLoc {
      case (name, typeAnno) => Values(name, typeAnno)
    }

  /** Assign parser */
  final protected[frontend] def assignStatement[_: P]: P[Assign] =
    P(singleAssignStatement | multipleAssignStatement)

  final protected[frontend] def singleAssignStatement[_: P]: P[Assign] =
    P("val " ~ identifier ~ "=" ~ exp).mapWithLoc {
      case (name, expr) => Assign(Seq(name), expr)
    }

  final protected[frontend] def multipleAssignStatement[_: P]: P[Assign] =
    P("val " ~ "(" ~ identifier.rep(min = 2, sep = ",") ~ ")" ~ "=" ~ exp).mapWithLoc {
      case (names, expr) => Assign(names, expr)
    }

  /** Assert parser */
  final protected[frontend] def assertStatement[_: P]: P[Assert] =
    P("assert " ~ exp).mapWithLoc(Assert)

  /** Body parser */
  protected[frontend] def body[_: P]: P[Body] =
    P("{" ~/ statement.rep ~ "}").mapWithLoc(Body.apply)

  /** Parses only the AnnoParam Unit. */
  protected[frontend] def annoParamUnit[_: P]: P[Seq[AnnoParam]] =
    P("Unit").map(_ => Seq.empty[AnnoParam])

  /** Parses an AnnoParam which has only one member. */
  protected[frontend] def annoParamSingle[_: P]: P[Seq[AnnoParam]] = P(annoParam).map(Seq(_))

  /** PatternFunction parser */
  protected[frontend] def patternFunction[_: P]: P[PatternFunction] = {
    P(visibility.? ~ "def" ~ identifier ~ "(" ~ paramList ~ ")" ~ ":" ~ outParamList ~/ "=" ~/ bodyList).mapWithLoc(Core.PatternFunction.tupled)
  }

  protected[frontend] def paramList[_: P]: P[Seq[Param]] = P(param.rep(sep = ","))
  protected[frontend] def outParamList[_: P]: P[Seq[AnnoParam]] =
    P(annoParamUnit | P("(" ~ annoParam.rep(sep = ",") ~ ")") | annoParamSingle)
  protected[frontend] def bodyList[_: P]: P[Seq[Body]] = P(body.rep(min = 1, sep = "union"))

  /** Module parser */
  def module[_: P]: P[Module] =
    P("module " ~/ identifier ~
      ("import" ~ identifier).rep ~
      moduleContent.rep ~
      End
    ).mapWithLoc { case (name, imports, contents) =>
      val funs = contents.collect { case Left(fun) => fun }
      val stats = contents.collect { case Right(stat) => stat }
      Module(name, imports, funs, stats)
    }

  def moduleContent[_: P]: P[Either[PatternFunction, meta.Stat]] =
    P(patternFunction.map(Left(_)) | nativeStat.map(Right(_)))


  def nativeStat[_: P]: P[meta.Stat] =
    P("scala " ~ nativeStatHelper(scalaparse.Scala.Import) |
      "scala " ~ nativeStatHelper(scalaparse.Scala.BlockDef)
    )

  private def nativeStatHelper[_: P](statParser: => P[_]): P[meta.Stat] =
    P(statParser.!).flatMap { raw_code =>
      raw_code.parse[Stat] match {
        case _: Parsed.Error => fastparse.Fail
        case Parsed.Success(code) =>
          fastparse.Pass(code)
      }
    }

//
//  {
//
//    P(scalaparse.Scala.TmplBody.!).flatMap { raw_code =>
//      raw_code.parse[Term] match {
//        case Parsed.Error(_, _, _) =>
//          fastparse.Fail
//        case Parsed.Success(code) =>
//          val params = EvalHelper.freeVars(code)
//          val eval = Eval(params.toSeq, code)
//          fastparse.Pass(eval)
//      }
//    }
//  }


  /** Yield parser */
  protected[frontend] def yieldStatement[_: P]: P[Yield] =
    P("yield " ~ exp).mapWithLoc(Yield)

  /** Fail/Continue parser */
  protected[frontend] def failStatement[_: P]: P[TerminatorStatement] =
    P("continue" /* assert false*/).mapWithLoc(_ => Core.Fail)

  /** Terminator parser. */
  protected[frontend] def terminatorStatement[_: P]: P[TerminatorStatement] =
    P(yieldStatement | failStatement)

  /** DataType parser */
  protected[frontend] def dataType[_: P]: P[TypeAnno] =
    P((identifier ~ ".").? ~ identifier).mapWithLoc(Core.DataType.tupled)

  /** DataOp parser */
  protected[frontend] def dataOp[_: P]: P[DataOp] =
    P((identifier ~ ".").? ~ identifier).mapWithLoc { case (qual, name) =>
      Core.DataOp(qual, name, isAssociative = false, isCommutative = false)
    }

  /** Aggregate parser */
  protected[frontend] def aggregateExp[_: P]: P[Exp] =
    P("aggregate" ~ "(" ~ dataOp ~ "," ~ dataOp ~ ")" ~ callExp).mapWithLoc { case (init, join, call) =>
      Aggregate(init, join, None, call)
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
