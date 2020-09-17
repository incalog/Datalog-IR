package inca.frontend.parser

import fastparse.ScalaWhitespace._
import fastparse._
import inca.frontend.core.Core
import inca.frontend.core.Core._
import inca.frontend.parser.ParserUtils._
import inca.frontend.util.EvalHelper

import scala.meta._
import scala.util.control.Breaks._

/**
  * Parser for the IncA Core language.
  *
  * @todo    unfinished
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
case class CoreParser(extensions: Seq[ParserExtension] = Seq.empty) {

  // Data initialization ///////////////////////////////////////////////////////////////////////////////////////////////
  extensions.map(_.coreparser = this)

  val recursiveExpExtensions: Seq[RecursiveExpressionParser] =
    extensions.flatMap(_.recursiveExpression)

  val anchorExpExtensions: Seq[AnchorExpressionParser] =
    extensions.flatMap(_.anchorExpression)

  val statementExtensions: Seq[StatementParser] =
    extensions.flatMap(_.statement)

  val keywords =
    Set("def", "undef", "true", "false", "eval", "aggregate") ++ extensions.flatMap(
      _.keywords
    )

  // Parser ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /** Parse a variable identifier.
    * The first character must be an alphabetical one. After that digits and underscores are also allowed
    */
  def identifier[_: P]: P[String] =
    P(CharIn("a-z", "A-Z") ~~ CharIn("a-z", "A-Z", "0-9", "_").repX(0)).!.map { s =>
      if (keywords.contains(s)) return fastparse.Fail
      else s
    }

  /** TAnyLinked parser */
  def tAnyLinked[_: P]: P[TLinked] =
    P(P(TAnyLinked.prettyprint).map(_ => TAnyLinked))

  /** TNode parser */
  def tNode[_: P]: P[TNode] = P(P(identifier).!.map(TNode))

  /** Helper for the basic TypeAnno like TAny. */
  private def typeAnnoHelper[_: P](t: TypeAnno): P[TypeAnno] =
    P(P(t.prettyprint).map(_ => t))

  /** TLinked parser */
  def tLinked[_: P]: P[TLinked] = P(tAnyLinked | tNode | tList)

  /** TypeAnno parser */
  def typeAnno[_: P]: P[TypeAnno] =
    P(
      typeAnnoHelper(TAny)
        | typeAnnoHelper(TBool)
        | typeAnnoHelper(TLong)
        | typeAnnoHelper(TInt)
        | typeAnnoHelper(TDouble)
        | typeAnnoHelper(TString)
        | tLinked
        | tIterable
        | tTuple
        | dataType
    )

  /** Visibility parser */
  def visibility[_: P]: P[Visibility] =
    P(
      P(sp ~ P(Private.prettyprint(""))).map(_ => Private)
        | P(sp ~ P(Public.prettyprint(""))).map(_ => Public)
    )

  /** TTuple parser */
  def tTuple[_: P]: P[TTuple] =
    P(
      P("Unit").map(_ => TTuple(Seq.empty))
        | (sp ~ "(" ~ typeAnno.rep(1, sep = ",") ~ ")")
          .map(TTuple)
    )

  /** TList parser */
  def tList[_: P]: P[TList] =
    P("List[" ~ tLinked ~ "]").map(TList)

  /** TEnumeration parser */
  def tEnumeration[_: P]: P[TEnumeration] =
    P("Enum[" ~ tLinked ~ "]").map(TEnumeration)

  /** TIterable parser */
  def tIterable[_: P]: P[TIterable] = P(tList | tEnumeration)

  /** Literal parser */
  def literal[_: P]: P[Literal] =
    P(
      stringLiteral
        | unitLiteral // tuple
        | doubleLiteral
        | longLiteral
        | intLiteral
        | booleanLiteral
    )

  /** UnitLiteral parser */
  def unitLiteral[_: P]: P[UnitLiteral.type] = P("unit").map(_ => UnitLiteral)

  /** IntLiteral parser */
  def intLiteral[_: P]: P[IntLiteral] = P(ParserUtils.integer).map(IntLiteral)

  /** LongLiteral parser */
  def longLiteral[_: P]: P[LongLiteral] = P(ParserUtils.long ~ "L").map(LongLiteral)

  /** DoubleLiteral parser */
  def doubleLiteral[_: P]: P[DoubleLiteral] = P(ParserUtils.double).map(DoubleLiteral)

  /** StringLiteral parser */
  def stringLiteral[_: P]: P[StringLiteral] = P(ParserUtils.string).map(StringLiteral)

  /** BooleanLiteral parser */
  def booleanLiteral[_: P]: P[BooleanLiteral] =
    P("true" | "false").!.map(s => BooleanLiteral(s.toBoolean))

  /** Param parser */
  def param[_: P]: P[Param] =
    P(identifier ~ ":" ~ typeAnno).map {
      case (name, typeAnno) => Param(name, typeAnno)
    }

  /** AnnoParam parser */
  def annoParam[_: P]: P[AnnoParam] =
    P("(" ~ param ~ ")" | typeAnno).map {
      case Param(name, typeAnno) => AnnoParam(Some(name), typeAnno)
      case typeAnno: TypeAnno    => AnnoParam(None, typeAnno)
    }

  /** Link parser */
  def link[_: P](node: TNode): P[Link] = P(coreLink(node))

  /** CoreLink parser */
  def coreLink[_: P](node: TNode): P[CoreLink] =
    P(
      parentLink
        | childrenLink
        | prevLink
        | sizeLink
        | nextLink
        | namedLink(node)
    )

  /** ParentLink parser */
  def parentLink[_: P]: P[ParentLink.type] =
    P(ParentLink.prettyprint).map(_ => ParentLink)

  /** ChildrenLink parser */
  def childrenLink[_: P]: P[ChildrenLink.type] =
    P(ChildrenLink.prettyprint).map(_ => ChildrenLink)

  /** NextLink parser */
  def nextLink[_: P]: P[NextLink.type] = P(NextLink.prettyprint).map(_ => NextLink)

  /** PreviousLink parser */
  def prevLink[_: P]: P[PreviousLink.type] =
    P(PreviousLink.prettyprint).map(_ => PreviousLink)

  /** SizeLink parser */
  def sizeLink[_: P]: P[SizeLink.type] = P(SizeLink.prettyprint).map(_ => SizeLink)

  /** NamedLink parser */
  def namedLink[_: P](node: TNode): P[NamedLink] = P(identifier).map(NamedLink(node, _))

  /** Exp parser */
  def exp[_: P]: P[Exp] =
    P(
      recursionAnchorExp(anchorExpExtensions).flatMap { e =>
        P(recursionCallExp(recursiveExpExtensions, e))
      }
        | recursionAnchorExp(anchorExpExtensions)
    )

  /** Recalls the resulting expression on expression as left hand.
    * Ensures left to right binding.
    */
  private def decorateRecursionExp[_: P, T](p: => P[Exp]): P[Exp] =
    P(p.flatMap(t => recursionCallExp(recursiveExpExtensions, t)) | p)

  /** Higher order extension call combination parser that require a left side expression. */
  private def recursionCallExp[_: P, T](
      p: Seq[RecursiveExpressionParser],
      e: Exp
  ): P[Exp] =
    if (p.isEmpty) {
      decorateRecursionExp(
        P(
          eqExp(e)
            | neqExp(e)
            | notInstanceOfCoreExp(e)
            | instanceOfCoreExp(e)
            | pathAccessExp(e)
        )
      )
    }
    else
      P(decorateRecursionExp(p.head.parse(e)) | recursionCallExp(p.tail, e))

  /** Higher order extension call combination parser that function as recursion anchor. */
  private def recursionAnchorExp[_: P, T](p: Seq[AnchorExpressionParser]): P[Exp] =
    if (p.isEmpty) {
      decorateRecursionExp(
        P(
          callExp
            | countExp
            | defExp
            | undefExp
            | varExp
            | constantExp
            | tupleExp
            | aggregateExp
            | bracketExp
        )
      )
    } else P(decorateRecursionExp(p.head.parse) | recursionAnchorExp(p.tail))

  /** Eval parser */
  def evalExp[_: P]: P[Eval] = {
    var codeStr: String = ""
    var c: Int = 0
    var error = false
    var free = Set[String]()
    var closing = ')'
    var code: Term = Term.Name("unused")

    P(
      "eval" ~ ("(" | "{").! flatMapX
        (openStr =>
          P(
            AnyChar.repX.!.map(raw_str => {
              val stack = scala.collection.mutable.Stack[Char]()
              val opening = openStr(0)
              closing = if(opening == '(') ')' else '}'
              breakable {
                for (ch <- raw_str) {
                  if (stack.isEmpty && ch == closing)
                    break
                  else if (ch == opening)
                    stack.push(ch)
                  else if (ch == closing)
                    stack.pop()
                  codeStr += ch
                }
              }
              if (stack.nonEmpty) {
                error = true
                return fastparse.Fail
              }
              c = codeStr.length
              code = codeStr.parse[Term] match {
                case scala.meta.parsers.Parsed.Error(_, _, _) => {
                  error = true
                  return fastparse.Fail
                }
                case scala.meta.parsers.Parsed.Success(t) =>
                  free = EvalHelper.freeVars(t)
                  t
              }
            }) ~~
              fastparse.Fail
          ).? ~~
            (
              if (error)
                fastparse.Fail
              else
                AnyChar.repX(max = c)
              ) ~~
            s"$closing"
          )
    ).map(_ => Eval(free.toSeq, code))
  }


  /** PathAccess parser */
  // @todo Wait for fix commit in Core language
  def pathAccessExp[_: P](e: Exp): P[Exp] =
    P("." ~ link(TNode("dummy"))).map(PathAccess(e, _))

  /** Call parser */
  def callExp[_: P]: P[Call] =
    P(
      identifier ~ "+".?.! ~ P("(" ~ exp.rep(sep = ",") ~ ")")
    ).map {
      case (name, transitive_str, exp) =>
        Call(name, exp, transitive_str == "+")
    }

  /** Count parser */
  def countExp[_: P]: P[Count] =
    P("count " ~ callExp).map(Count)

  /** Tuple parser */
  def tupleExp[_: P]: P[Tuple] =
    P("(" ~ exp.rep(2, sep = ",") ~ ")").map(Tuple)

  /** Bracket parser */
  def bracketExp[_: P]: P[Exp] = P("(" ~ exp ~ ")")

  /** Var parser */
  def varExp[_: P]: P[Var] = P(identifier).map(Var)

  /** Constant parser */
  def constantExp[_: P]: P[Constant] = P(literal).map(Constant)

  /** Eq parser */
  def eqExp[_: P](e: Exp): P[Exp] =
    P("==" ~ exp).map(Eq(e, _))

  /** Neq parser */
  def neqExp[_: P](e: Exp): P[Exp] =
    P("!=" ~ exp).map(Neq(e, _))

  /** Def parser */
  def defExp[_: P]: P[Def] = P("def " ~ exp).map(Def)

  /** Undef parser */
  def undefExp[_: P]: P[Undef] = P("undef " ~ exp).map(Undef)

  /** InstanceOf parser */
  def instanceOfCoreExp[_: P](e: Exp): P[Exp] =
    P("instanceOf " ~ typeAnno).map(InstanceOf(e, _))

  /** NotInstanceOf parser */
  def notInstanceOfCoreExp[_: P](e: Exp): P[Exp] =
    P("notInstanceOf " ~ typeAnno).map(NotInstanceOf(e, _))

  /** Statement parser */
  def statement[_: P]: P[Statement] = statementRecursive(statementExtensions)

  private def statementRecursive[_: P](ss: Seq[StatementParser]): P[Statement] =
    if (ss.isEmpty)
      P(coreStatement | terminatorStatement)
    else
      P(ss.head.parse | statementRecursive(ss.tail))

  /** CoreStatement parser */
  def coreStatement[_: P]: P[CoreStatement] =
    P(
      sp ~ P(
        valuesStatement
          | assignStatement
          | assertStatement
      )
    )

  /** Values parser */
  def valuesStatement[_: P]: P[Values] =
    P("vals " ~ identifier ~ "<-" ~ typeAnno).map {
      case (name, typeAnno) => Values(name, typeAnno)
    }

  /** Assign parser */
  def assignStatement[_: P]: P[Assign] =
    P(
      ("val " ~ identifier ~ "=" ~ exp).map {
        case (name, expr) => Assign(Seq(name), expr)
      }
        | ("val " ~ "(" ~ identifier.rep(min = 2, sep = ",") ~ ")" ~ "=" ~ exp).map {
          case (names, expr) => Assign(names, expr)
        }
    )

  /** Assert parser */
  def assertStatement[_: P]: P[Assert] = P("assert " ~ exp).map(Assert)

  /** Body parser */
  def body[_: P]: P[Body] =
    P("{" ~ P(sp_nl ~~ statement ~~ sp).repX(sep = nl_!) ~ "}").map(Body(_))

  /** Parses only the AnnoParam Unit. */
  private def annoParamUnit[_: P]: P[Seq[AnnoParam]] =
    P("Unit").map(_ => Seq.empty[AnnoParam])

  /** Parses an AnnoParam which has only one member. */
  private def annoParamSingle[_: P]: P[Seq[AnnoParam]] = P(annoParam).map(Seq(_))

  /** Parses the beginning aka visibility of a PatternFunction */
  private def patternFunctionVisibility[_: P]: P[Visibility] =
    P("def " | P("private " ~ "def ")).!.map {
      case "def " => Public
      case _      => Private
    }

  /** PatternFunction parser */
  def patternFunction[_: P]: P[PatternFunction] =
    P(
      sp_nl ~ patternFunctionVisibility ~ identifier ~ "(" ~
        param.rep(0, sep = ",") ~ ")" ~ ":" ~ P(
        annoParamUnit | P("(" ~ annoParam.rep(sep = ",") ~ ")") | annoParamSingle
      )
        ~ "=" ~ body.rep(1, sep = "union")
    ).map {
      case (v, name, params, ret_params, bodies) =>
        PatternFunction(Option(v), name, params, ret_params, bodies)
    }

  /** Module parser */
  def module[_: P]: P[Module] =
    P(
      sp_nl ~ "module " ~ identifier ~ P("import".? ~ identifier).rep ~ patternFunction.rep
    ).map {
      case (name, imports, patternFunctions) =>
        Module(name, imports, patternFunctions)
    }

  /** Yield parser */
  def yieldStatement[_: P]: P[Yield] =
    P(
      sp ~ "yield " ~ exp
    ).map(Yield)

  /** Fail/Continue parser */
  def failStatement[_: P]: P[TerminatorStatement] =
    P(
      sp ~ "continue" // assert false
    ).map(_ => Core.Fail)

  /** Terminator parser. */
  def terminatorStatement[_: P]: P[TerminatorStatement] =
    P(yieldStatement | failStatement)

  /** DataType parser */
  def dataType[_: P]: P[TypeAnno] =
    P(
      P(
        identifier ~ "." ~ identifier
      ).map { case (qual, name) => Core.DataType(Option(qual), name) }
        | P(
          identifier
        ).map(s => Core.DataType(None, s))
    )

  /** DataOp parser */
  def dataOp[_: P]: P[DataOp] =
    P(
      P(
        identifier ~ "." ~ identifier
      ).map { case (qual, name) => Core.DataOp(Option(qual), name) }
        | P(
          identifier
        ).map(s => Core.DataOp(None, s))
    )

  /** Aggregate parser */
  def aggregateExp[_: P]: P[Exp] =
    P(
      "aggregate" ~ "(" ~ dataOp ~ "," ~ dataOp ~ ")" ~ callExp
    ).map {
      case (init, join, call) => Aggregate(init, join, None, call)
    }
}
