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

  final lazy val allKeywords: Set[Name] = this.keywords

  protected[frontend] def keywords: Set[Name] =
    Set("def", "undef", "true", "false", "eval", "aggregate", "count", "_", "unit", "isInstanceOf", "notInstanceOf")

  // Parser ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /** Parse a variable identifier.
    * The first character must be an alphabetical one. After that digits and underscores are also allowed
    */
  def identifier[_: P]: P[String] =
    P(CharIn("a-z", "A-Z", "_") ~~ CharIn("a-z", "A-Z", "0-9", "_").repX).!.map { s =>
      if (allKeywords.contains(s)) return fastparse.Fail
      else s
    }

  /** TAnyLinked parser */
  protected[frontend] def tAnyLinked[_: P]: P[TLinked] =
    P(TAnyLinked.prettyprint).map(_ => TAnyLinked)

  /** A parser for fully qualified identifier. Allows '.' in the name */
  protected[frontend] def fullyQualifiedIdentifier[_: P]: P[String] =
    P(CharIn("a-z", "A-Z", "_") ~~ CharIn("a-z", "A-Z", "0-9", "_", ".").repX).!.map { s =>
      if(allKeywords.contains(s)) return fastparse.Fail else s
  }

  /** TNode parser */
  protected[frontend] def tNode[_: P]: P[TNode] = P(fullyQualifiedIdentifier).!.map(TNode)

  /** Helper for the TypeAnno like TAny. */
  protected[frontend] def simpleTypeAnno[_: P](t: TypeAnno): P[TypeAnno] =
    P(t.prettyprint).map(_ => t)

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
    P(Private.prettyprint("")).map(_ => Private)
  protected[frontend] def publicVisivility[_: P]: P[Public.type] =
    P(Public.prettyprint("")).map(_ => Public)

  /** TTuple parser without Unit */
  protected[frontend] def tTuple[_: P]: P[TypeAnno] = {
    P("(" ~ typeAnno ~ ")") |
    P("(" ~ typeAnno.rep(2, sep = ",") ~ ")").map(TTuple)
  }

  /** TList parser */
  protected[frontend] def tList[_: P]: P[TList] =
    P("List[" ~ tLinked ~ "]").map(TList)

  /** TEnumeration parser */
  protected[frontend] def tEnumeration[_: P]: P[TEnumeration] =
    P("Enum[" ~ tLinked ~ "]").map(TEnumeration)

  /** TIterable parser */
  protected[frontend] def tIterable[_: P]: P[TIterable] = P(tList | tEnumeration)

  /** Literal parser */
  protected[frontend] def literal[_: P]: P[Literal] =
    P(stringLiteral | unitLiteral /* tuple*/ | doubleLiteral | longLiteral |
        intLiteral | booleanLiteral)

  /** UnitLiteral parser */
  protected[frontend] def unitLiteral[_: P]: P[UnitLiteral.type] = P("unit").map(_ => UnitLiteral)

  /** IntLiteral parser */
  protected[frontend] def intLiteral[_: P]: P[IntLiteral] = P(ParserUtils.integer).map(IntLiteral)

  /** LongLiteral parser */
  def longLiteral[_: P]: P[LongLiteral] = P(ParserUtils.long ~ "L").map(LongLiteral)

  /** DoubleLiteral parser */
  protected[frontend] def doubleLiteral[_: P]: P[DoubleLiteral] = P(ParserUtils.double).map(DoubleLiteral)

  /** StringLiteral parser */
  def stringLiteral[_: P]: P[StringLiteral] = P(ParserUtils.string).map(StringLiteral)

  /** BooleanLiteral parser */
  protected[frontend] def booleanLiteral[_: P]: P[BooleanLiteral] =
    P("true" | "false").!.map(s => BooleanLiteral(s.toBoolean))

  /** Param parser */
  protected[frontend] def param[_: P]: P[Param] =
    P(identifier ~ ":" ~ typeAnno).map {
      case (name, typeAnno) => Param(name, typeAnno)
    }

  /** AnnoParam parser */
  protected[frontend] def annoParam[_: P]: P[AnnoParam] =
    P("(" ~ param ~ ")" | typeAnno).map {
      case Param(name, typeAnno) => AnnoParam(Some(name), typeAnno)
      case typeAnno: TypeAnno    => AnnoParam(None, typeAnno)
    }

  /** Link parser */
  protected[frontend] def link[_: P](node: TNode): P[Link] = P(coreLink(node))

  /** CoreLink parser */
  protected[frontend] def coreLink[_: P](node: TNode): P[CoreLink] =
    P(parentLink | childrenLink | prevLink | sizeLink | nextLink | namedLink(node))

  /** ParentLink parser */
  protected[frontend] def parentLink[_: P]: P[ParentLink.type] =
    P(ParentLink.prettyprint).map(_ => ParentLink)

  /** ChildrenLink parser */
  protected[frontend] def childrenLink[_: P]: P[ChildrenLink.type] =
    P(ChildrenLink.prettyprint).map(_ => ChildrenLink)

  /** NextLink parser */
  protected[frontend] def nextLink[_: P]: P[NextLink.type] =
    P(NextLink.prettyprint).map(_ => NextLink)

  /** PreviousLink parser */
  protected[frontend] def prevLink[_: P]: P[PreviousLink.type] =
    P(PreviousLink.prettyprint).map(_ => PreviousLink)

  /** SizeLink parser */
  protected[frontend] def sizeLink[_: P]: P[SizeLink.type] = P(SizeLink.prettyprint).map(_ => SizeLink)

  /** NamedLink parser */
  protected[frontend] def namedLink[_: P](node: TNode): P[NamedLink] = P(identifier).map(NamedLink)


  /** Exp parser */
  protected[frontend] def exp[_: P]: P[Exp] = wideExp

  protected[frontend] def wideExp[_: P]: P[Exp] =
    P(("def " ~ exp).map(Def) |
      ("undef " ~ exp).map(Undef) |
      ("count " ~ callExp).map(Count) |
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
    P("." ~ link(TNode("dummy"))).map(l => PathAccess(_, l)) |
    P("." ~ "isInstanceOf" ~ bracketedTypeAnno).map(ty => InstanceOf(_, ty)) |
    P("." ~ "notInstanceOf" ~ bracketedTypeAnno).map(ty => NotInstanceOf(_, ty))

  protected[frontend] def atomicExp[_: P]: P[Exp] =
    P(callExp | evalExp | wildcardExp | constantExp | varExp
     | tupleExp | aggregateExp | parensExp)


  protected[frontend] def Chain[_: P, A, B](p: => P[A], op: String, q: => P[B], opNode: (A, B) => A, min: Int = 0): P[A] =
    P( p ~ (op ~ q).rep(min) ).map {
      case (lhs, chunks) =>
        chunks.foldLeft(lhs){case (lhs, rhs) =>
          opNode(lhs, rhs)
        }
    }

  /** Eval parser */
  protected[frontend] def evalCore[_: P]: P[Eval] = P(scalaparse.Scala.Exprs.!).flatMap {raw_code =>
    raw_code.parse[Term] match {
      case Parsed.Error(_, _, _) =>
        fastparse.Fail
      case Parsed.Success(code) =>
        val params = EvalHelper.freeVars(code)
        val eval = Eval(params.toSeq, code)
        fastparse.Pass(eval)
    }
  }

  protected[frontend] def evalExp[_: P]: P[Eval] = P("eval" ~ (("(" ~ evalCore ~ ")") | ("{" ~ evalCore ~ "}")))

  /** Call parser */
  protected[frontend] def callExp[_: P]: P[Call] =
    P(fullyQualifiedIdentifier ~ "+".?.! ~ P("(" ~ exp.rep(sep = ",") ~ ")")).map {
      case (name, transitive_str, exp) =>
        Call(name, exp, transitive_str == "+")
    }

  /** Tuple parser */
  protected[frontend] def tupleExp[_: P]: P[Tuple] =
    P("(" ~ exp.rep(2, sep = ",") ~ ")").map(Tuple)

  /** Parens parser */
  protected[frontend] def parensExp[_: P]: P[Exp] = P("(" ~ exp ~ ")")

  /** Var parser */
  protected[frontend] def varExp[_: P]: P[Var] = P(identifier).map(Var)

  protected[frontend] def wildcardExp[_: P]: P[Wildcard.type] = P("_").map(_ => Wildcard)

  /** Constant parser */
  protected[frontend] def constantExp[_: P]: P[Constant] = P(literal).map(Constant)

  /** Statement parser */
  protected[frontend] def statement[_: P]: P[Statement] =
    coreStatement | terminatorStatement

  /** CoreStatement parser */
  final protected[frontend] def coreStatement[_: P]: P[CoreStatement] =
    P(valuesStatement | assignStatement | assertStatement)

  /** Values parser */
  final protected[frontend] def valuesStatement[_: P]: P[Values] =
    P("vals " ~ identifier ~ "<-" ~ typeAnno).map {
      case (name, typeAnno) => Values(name, typeAnno)
    }

  /** Assign parser */
  final protected[frontend] def assignStatement[_: P]: P[Assign] =
    P(singleAssignStatement | multipleAssignStatement)

  final protected[frontend] def singleAssignStatement[_: P]: P[Assign] =
    P("val " ~ identifier ~ "=" ~ exp).map {
      case (name, expr) => Assign(Seq(name), expr)
    }

  final protected[frontend] def multipleAssignStatement[_: P]: P[Assign] =
    P("val " ~ "(" ~ identifier.rep(min = 2, sep = ",") ~ ")" ~ "=" ~ exp).map {
      case (names, expr) => Assign(names, expr)
    }

  /** Assert parser */
  final protected[frontend] def assertStatement[_: P]: P[Assert] = P("assert " ~ exp).map(Assert)

  /** Body parser */
  protected[frontend] def body[_: P]: P[Body] = P("{" ~/ statement.rep ~ "}").map({ Body(_) })

  /** Parses only the AnnoParam Unit. */
  protected[frontend] def annoParamUnit[_: P]: P[Seq[AnnoParam]] =
    P("Unit").map(_ => Seq.empty[AnnoParam])

  /** Parses an AnnoParam which has only one member. */
  protected[frontend] def annoParamSingle[_: P]: P[Seq[AnnoParam]] = P(annoParam).map(Seq(_))

  /** PatternFunction parser */
  protected[frontend] def patternFunction[_: P]: P[PatternFunction] = {
    P(visibility.? ~ "def" ~ identifier ~ "(" ~ paramList ~ ")" ~ ":" ~ outParamList ~/ "=" ~/ bodyList).map(Core.PatternFunction.tupled)
  }

  protected[frontend] def paramList[_: P]: P[Seq[Param]] = P(param.rep(sep = ","))
  protected[frontend] def outParamList[_: P]: P[Seq[AnnoParam]] =
    P(annoParamUnit | P("(" ~ annoParam.rep(sep = ",") ~ ")") | annoParamSingle)
  protected[frontend] def bodyList[_: P]: P[Seq[Body]] = P(body.rep(min = 1, sep = "union"))

  /** Module parser */
  def module[_: P]: P[Module] =
    P("module " ~/ identifier ~
      ("import" ~ identifier).rep ~
      patternFunction.rep ~ End
    ).map(Module.tupled)

  /** Yield parser */
  protected[frontend] def yieldStatement[_: P]: P[Yield] =
    P("yield " ~ exp).map(Yield)

  /** Fail/Continue parser */
  protected[frontend] def failStatement[_: P]: P[TerminatorStatement] =
    P("continue" /* assert false*/).map(_ => Core.Fail)

  /** Terminator parser. */
  protected[frontend] def terminatorStatement[_: P]: P[TerminatorStatement] =
    P(yieldStatement | failStatement)

  /** DataType parser */
  protected[frontend] def dataType[_: P]: P[TypeAnno] =
    P((identifier ~ ".").? ~ identifier).map(Core.DataType.tupled)

  /** DataOp parser */
  protected[frontend] def dataOp[_: P]: P[DataOp] =
    P((identifier ~ ".").? ~ identifier).map { case (qual, name) =>
      Core.DataOp(qual, name, isAssociative = false, isCommutative = false)
    }

  /** Aggregate parser */
  protected[frontend] def aggregateExp[_: P]: P[Exp] =
    P("aggregate" ~ "(" ~ dataOp ~ "," ~ dataOp ~ ")" ~ callExp).map { case (init, join, call) =>
      Aggregate(init, join, None, call)
    }
}
