package inca.frontend.parser

import inca.frontend.core.Core._
import fastparse._
import NoWhitespace._
import ParserUtils._
import inca.frontend.core.Core
import scala.util.control.Breaks._
import scala.meta._
import inca.frontend.parser.extensions._

/**
  * Parser for the IncA Core language.
  *
  * @todo    unfinished
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
case class CoreParser(val extensions: Seq[ParserExtension] = Seq.empty) {

  // Data initialization ///////////////////////////////////////////////////////////////////////////////////////////////
  extensions.map(_.coreparser = this)

  val recursiveExpExtensions: Seq[RecursiveExpressionParser] =
    extensions.foldLeft(Seq.empty[RecursiveExpressionParser]) {
      case (s, ext) => s ++ ext.recursiveExpression
    }

  val anchorExpExtensions: Seq[AnchorExpressionParser] =
    extensions.foldLeft(Seq.empty[AnchorExpressionParser]) {
      case (s, ext) => s ++ ext.anchorExpression
    }

  val statementExtensions: Seq[StatementParser] =
    extensions.foldLeft(Seq.empty[StatementParser]) {
      case (s, ext) => s ++ ext.statement
    }

  val keywords =
    extensions.foldLeft(Set("def", "undef", "true", "false", "eval", "aggregate")) {
      case (s, ext) => s ++ ext.keywords
    }

  // Parser ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /** Parse a variable identifier.
    * The first character must be an alphabetical one. After that digits and underscores are also allowed
    */
  def identifier[_: P]: P[String] =
    P(CharIn("a-z", "A-Z") ~ CharIn("a-z", "A-Z", "0-9", "_").rep(0)).!.map { s =>
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
      P(" ".rep() ~ P(Private.prettyprint(""))).map(_ => Private) |
        P(" ".rep() ~ P(Public.prettyprint(""))).map(_ => Public)
    )

  /** TTuple parser */
  def tTuple[_: P]: P[TTuple] =
    P(
      "Unit".!.map(_ => TTuple(Seq.empty))
        | (s_i ~ "(" ~ s_i ~ P(typeAnno ~ s_i ~ ",".? ~ s_i).rep(1) ~ ")" ~ s_i)
          .map(TTuple)
      // | typeAnno.map(typ => TTuple(Seq(typ))) // @todo uniqueness TBool == TTuple(TBool)
    )

  /** TList parser */
  def tList[_: P]: P[TList] =
    P(
      P("List[" ~ tLinked ~ "]").map(TList)
    )

  /** TEnumeration parser */
  def tEnumeration[_: P]: P[TEnumeration] =
    P(
      P("Enum[" ~ tLinked ~ "]").map(TEnumeration)
    )

  /** TIterable parser */
  def tIterable[_: P]: P[TIterable] = P(tList | tEnumeration)

  /** Literal parser */
  def literal[_: P]: P[Literal] =
    P(
      stringLiteral
        | unitLiteral
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
    P(identifier ~ s_i ~ ":" ~ s_i ~ typeAnno).map {
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
      recursionAnchorExp(anchorExpExtensions).flatMap { e: Exp =>
        { P(recursionCallExp(recursiveExpExtensions, e)) }
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
    } else P(decorateRecursionExp(p.head.parse(e)) | recursionCallExp(p.tail, e))

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
  def evalExp[_: P]: P[Any] = {
    var code: String = ""
    var c: Int = 0

    P(
      "eval" ~ s_i ~ "(" ~
        P(
          AnyChar.rep.!.map(raw_str => {
            val stack = scala.collection.mutable.Stack[Char]()

            breakable {
              for (ch <- raw_str) {
                if (stack.isEmpty && ch == ')')
                  break
                else if (ch == '(')
                  stack.push(ch)
                else if (ch == ')')
                  stack.pop()
                code += ch
              }
            }
            c = code.length

            try {
              val res_tree = code.parse[Term].get
              // @todo Scala type checker and unbound variables checker
            } catch {
              case e: Exception =>
                println(e)
                return fastparse.Fail
            }
          }) ~
            fastparse.Fail
        ).? ~
        AnyChar.rep(max = c) ~ ")"
    ).map(_ => "NOT IMPLEMENTED YET")
  }
  //def eval_test[_: P]: P[Any] = P(eval ~ AnyChar.rep.!)

  /** PathAccess parser */
  def pathAccessExp[_: P](e: Exp): P[Exp] =
    P("." ~ link(TNode("dummy")))
      .map(PathAccess(e, _)) // @todo Wait for fix commit in Core language

  /** Call parser */
  def callExp[_: P]: P[Call] =
    P(
      identifier ~ s_i ~ "+".?.! ~ s_i ~ P(
        P("()").map(_ => Seq.empty[Exp])
          | "(" ~ P(s_i ~ exp ~ s_i).rep(1, sep = ",") ~ ")"
      )
    ).map {
      case (name, transitive_str, exp) =>
        Call(name, exp, if (transitive_str == "+") true else false)
    }

  /** Count parser */
  def countExp[_: P]: P[Count] =
    P(
      "count " ~ s_i ~ callExp
    ).map(Count)

  /** Tuple parser */
  def tupleExp[_: P]: P[Tuple] =
    P(
      "(" ~ P(s_i ~ exp ~ s_i).rep(2, sep = ",") ~ ")"
    ).map(Tuple)

  /** Bracket parser */
  def bracketExp[_: P]: P[Exp] = P("(" ~ s_i ~ exp ~ s_i ~ ")")

  /** Var parser */
  def varExp[_: P]: P[Var] = P(identifier).map(Var)

  /** Constant parser */
  def constantExp[_: P]: P[Constant] = P(literal).map(Constant)

  /** Eq parser */
  def eqExp[_: P](e: Exp): P[Exp] =
    P(s_i ~ "==" ~ s_i ~ exp).map(Eq(e, _))

  /** Neq parser */
  def neqExp[_: P](e: Exp): P[Exp] =
    P(s_i ~ "!=" ~ s_i ~ exp).map(Neq(e, _))

  /** Def parser */
  def defExp[_: P]: P[Def] = P("def " ~ exp).map(Def)

  /** Undef parser */
  def undefExp[_: P]: P[Undef] = P("undef " ~ exp).map(Undef)

  /** InstanceOf parser */
  def instanceOfCoreExp[_: P](e: Exp): P[Exp] =
    P(" " ~ s_i ~ "instanceOf " ~ s_i ~ typeAnno).map(InstanceOf(e, _))

  /** NotInstanceOf parser */
  def notInstanceOfCoreExp[_: P](e: Exp): P[Exp] =
    P(" " ~ s_i ~ "notInstanceOf " ~ s_i ~ typeAnno).map(NotInstanceOf(e, _))

  /** Statement parser */
  def statement[_: P]: P[Statement] = statementRecursive(statementExtensions)

  private def statementRecursive[_: P](ss: Seq[StatementParser]): P[Statement] =
    if (ss.isEmpty) P(coreStatement | terminatorStatement)
    else P(ss.head.parse | statementRecursive(ss.tail))

  /** CoreStatement parser */
  def coreStatement[_: P]: P[CoreStatement] =
    P(
      s_i ~ P(
        valuesStatement
          | assignStatement
          | assertStatement
      )
    )

  /** Values parser */
  def valuesStatement[_: P]: P[Values] =
    P("vals " ~ s_i ~ identifier ~ s_i ~ "<-" ~ s_i ~ typeAnno).map {
      case (name, typeAnno) => Values(name, typeAnno)
    }

  /** Assign parser */
  def assignStatement[_: P]: P[Assign] =
    P(
      ("val " ~ s_i ~ identifier ~ s_i ~ "=" ~ s_i ~ exp).map {
        case (name, expr) => Assign(Seq(name), expr)
      }
        | ("val " ~ s_i ~ "(" ~ (s_i ~ identifier ~ s_i)
          .rep(min = 2, sep = ",") ~ s_i ~ ")" ~ s_i ~ "=" ~ s_i ~ exp).map {
          case (names, expr) => Assign(names, expr)
        }
    )

  /** Assert parser */
  def assertStatement[_: P]: P[Assert] = P("assert " ~ s_i ~ exp).map(Assert)

  /** Body parser */
  def body[_: P]: P[Body] =
    P(
      sn_i ~ "{" ~  P(sn_i ~ statement ~ s_i).rep(sep=n_) ~ sn_i ~ "}"
    ).map(Body(_))

  /** Parses only the AnnoParam unit or Unit. */
  private def annoParamUnit[_: P]: P[Seq[AnnoParam]] =
    P("unit" | "Unit").map(_ => Seq.empty[AnnoParam])

  /** Parses an AnnoParam which has only one member. */
  private def annoParamSingle[_: P]: P[Seq[AnnoParam]] = P(annoParam).map(Seq(_))

  /** Parses the beginning aka visibility of a PatternFunction */
  private def patternFunctionVisibility[_: P]: P[Visibility] =
    P("def " | P("private " ~ s_i ~ "def ")).!.map {
      case "def " => Public
      case _      => Private
    }

  /** PatternFunction parser */
  def patternFunction[_: P]: P[PatternFunction] =
    P(
      sn_i ~
        patternFunctionVisibility ~ s_i ~ identifier ~ s_i ~
        "(" ~ s_i ~ P(s_i ~ param ~ s_i).rep(0, sep = ",") ~ s_i ~ ")" ~ s_i ~ ":" ~ s_i ~
        P(
          annoParamUnit | P(
            "(" ~ s_i ~ P(s_i ~ annoParam ~ s_i).rep(sep = ",") ~ s_i ~ ")"
          ) | annoParamSingle
        )
        ~ s_i ~ "=" ~ sn_i ~ P(sn_i ~ body ~ sn_i).rep(1, sep = "union")
    ).map {
      case (v, name, params, ret_params, bodies) =>
        PatternFunction(Option(v), name, params, ret_params, bodies)
    }

  /** Module parser */
  def module[_: P]: P[Module] =
    P(
      sn_i ~ "module " ~ identifier ~ s_i ~ n_ ~ sn_i ~
        P(P("import" ~ s_i).? ~ identifier ~ s_i ~ sn_i).rep ~
        P(patternFunction ~ sn_i).rep ~ End
    ).map {
      case (name, imports, patternFunctions) =>
        Module(name, imports, patternFunctions)
    }

  /** Yield parser */
  def yieldStatement[_: P]: P[Yield] =
    P(
      s_i ~ "yield " ~ exp
    ).map(Yield)

  /** Fail/Continue parser */
  def failStatement[_: P]: P[TerminatorStatement] =
    P(
      s_i ~ "continue" ~ s_i
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
      "aggregate" ~ s_i ~ "(" ~ dataOp ~ s_i ~ "," ~ s_i ~ dataOp ~ s_i ~ ")" ~ s_i ~ callExp
    ).map {
      case (init, join, call) => Aggregate(init, join, None, call)
    }
}
