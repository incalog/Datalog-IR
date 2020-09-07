package inca.frontend.parser

import inca.frontend.core.Core._
import fastparse._
import NoWhitespace._
import ParserUtils._
import inca.frontend.core.Core
import fastparse.Parsed.Success
import inca.frontend.parser.CoreParser.link

import scala.util.control.Breaks._
import scala.meta._

/**
  * Parser for the IncA Core language.
  *
  * @todo    unfinished
  * @todo    whitspacing
  * @version 0.0.1
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object CoreParser {

  def tAnyLinked[_: P]: P[TLinked] =
    P(P(TAnyLinked.prettyprint).map(_ => TAnyLinked))

  def tNode[_: P]: P[TNode] = P(P(ParserUtils.identifier).!.map(TNode))

  /** Helper for the basic TypeAnno like TAny. */
  private def typeAnnoHelper[_: P](t: TypeAnno): P[TypeAnno] =
    P(P(t.prettyprint).map(_ => t))

  def tLinked[_: P]: P[TLinked] = P(tAnyLinked | tNode | tList)

  /** TypeAnno parser */
  def typeAnno[_: P]: P[TypeAnno] =
    P(
      typeAnnoHelper(TAny) | typeAnnoHelper(TBool) | typeAnnoHelper(TLong) |
        typeAnnoHelper(TInt) | typeAnnoHelper(TDouble) | typeAnnoHelper(TString) |
        tLinked | tIterable | tTuple
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
    P(ParserUtils.identifier ~ s_i ~ ":" ~ s_i ~ typeAnno).map {
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
  def exp[_: P]: P[Exp] = P(coreExp)

  /** CoreExp parser
    *
    * The recursion is ordered in a loop sequence such that every testet subexpression is
    * continuing the next subexpression thus avoiding endless loop aka stackoverflow.
    * As fasparse has no better option to handle this lefthand recursion there is a parser
    * for continuing the loop sequence for each individual subexpression needed.
    * In the sequence expressions with preset keywords such as 'def' are placed in the beginning (these do not
    * require a seperate terminate function as they don't do left recusion.)
    * reducing the recusion depth; expressions with infix operators in the center avoiding an early stop
    * from the constant expression which do no recursive call at all at the end.
    *
    * This might make it impossible to do language extentions in a seperate class or object.
    */
  def coreExp[_: P]: P[Exp] =
    P(
      callCoreExp
        | countCoreExp
        | defCoreExp
        | undefCoreExp

        | eqCoreExp //
        | neqCoreExp

      // | pathAccessCoreExp
        | instanceOfCoreExp
        | notInstanceOfCoreExp

        | varCoreExp
        | constantCoreExp
        | tupleCoreExp
        | bracketExp
    )

  /** See CoreExp parser @see coreExp */
  def terminateEq[_: P]: P[Exp] =
    P(
      neqCoreExp
      // | pathAccessCoreExp
        | instanceOfCoreExp
        | notInstanceOfCoreExp
        | varCoreExp
        | constantCoreExp
        | tupleCoreExp
        | bracketExp
        | exp
    )

  /** See CoreExp parser @see coreExp */
  def terminateNeq[_: P]: P[Exp] =
    P(
      // pathAccessCoreExp
      // |
      instanceOfCoreExp
        | notInstanceOfCoreExp
        | varCoreExp
        | constantCoreExp
        | tupleCoreExp
        | bracketExp
        | exp
    )

  def terminatePathAccess[_: P]: P[Exp] =
    P(
      instanceOfCoreExp
        | notInstanceOfCoreExp
        | varCoreExp
        | constantCoreExp
        | tupleCoreExp
        | bracketExp
        | exp
    )

  /** See CoreExp parser @see coreExp */
  def terminateInstanceOf[_: P]: P[Exp] =
    P(
      notInstanceOfCoreExp
        | varCoreExp
        | constantCoreExp
        | tupleCoreExp
        | bracketExp
        | exp
    )

  /** See CoreExp parser @see coreExp */
  def terminateNotInstanceOf[_: P]: P[Exp] =
    P(
      varCoreExp
        | constantCoreExp
        | tupleCoreExp
        | bracketExp
        | exp
    )

  def eval[_: P]: P[Any] = {
    var code: String = ""
    var c: Int = 0

    P(
      "eval" ~ s_i ~ "(" ~
        P(
          AnyChar.rep.!.map(raw_str => {
            var stack = scala.collection.mutable.Stack[Char]()

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
            c = code.size

            try {
              val res_tree = code.parse[Term].get
              println(res_tree.structure)

              // val res_type = code.parse[Type].get.stats
              // println(res_type)

              // res_tree match {
              //   case _: Term =>
              //   case
              // }

              println(code)
            } catch {
              case e: Exception => {
                println(e)
                return fastparse.Fail
              }
            }
          }) ~
            fastparse.Fail
        ).? ~
        AnyChar.rep(max = c) ~ ")"
    ).map(_ => "NOT IMPLEMENTED YET")
  }
  //def eval_test[_: P]: P[Any] = P(eval ~ AnyChar.rep.!)

  /** See CoreExp parser @see coreExp */
  def pathAccessCoreExp[_: P]: P[PathAccess] =
    P(
      terminatePathAccess ~ "." ~ link(TNode("dummy"))
    ).map { case (exp, lnk) => PathAccess(exp, lnk) }

  /** See CoreExp parser @see coreExp */
  def callCoreExp[_: P]: P[Call] =
    P(
      identifier ~ s_i ~ "+".?.! ~ s_i ~ P(
        P("()")
          .map(_ => Seq.empty[Exp]) | "(" ~ P(s_i ~ exp ~ s_i).rep(1, sep = ",") ~ ")"
      )
    ).map {
      case (name, transitive_str, exp) =>
        Call(name, exp, if (transitive_str == "+") true else false)
    }

  /** See CoreExp parser @see coreExp */
  def countCoreExp[_: P]: P[Count] =
    P(
      "count " ~ s_i ~ callCoreExp
    ).map(Count)

  /** See CoreExp parser @see coreExp */
  def tupleCoreExp[_: P]: P[Tuple] =
    P(
      "(" ~ P(s_i ~ exp ~ s_i).rep(2, sep = ",") ~ ")"
    ).map(Tuple)

  /** Bracket parser */
  def bracketExp[_: P]: P[Exp] = P("(" ~ s_i ~ exp ~ s_i ~ ")")

  /** Var parser */
  def varCoreExp[_: P]: P[Var] = P(identifier).map(Var)

  /** Constant parser */
  def constantCoreExp[_: P]: P[Constant] = P(literal).map(Constant)

  /** Eq parser */
  def eqCoreExp[_: P]: P[Eq] =
    P(terminateEq ~ s_i ~ "==" ~ s_i ~ exp).map { case (l, r) => Eq(l, r) }

  /** Neq parser */
  def neqCoreExp[_: P]: P[Neq] =
    P(terminateNeq ~ s_i ~ "!=" ~ s_i ~ exp).map { case (e1, e2) => Neq(e1, e2) }

  /** Def parser */
  def defCoreExp[_: P]: P[CoreExp] = P("def " ~ exp).map(Def)

  /** Undef parser */
  def undefCoreExp[_: P]: P[Undef] = P("undef " ~ exp).map(Undef)

  /** InstanceOf parser */
  def instanceOfCoreExp[_: P]: P[InstanceOf] =
    P(terminateInstanceOf ~ " " ~ w_i ~ "instanceOf " ~ w_i ~ typeAnno).map {
      case (e, typ) => InstanceOf(e, typ)
    }

  /** NotInstanceOf parser */
  def notInstanceOfCoreExp[_: P]: P[NotInstanceOf] =
    P(terminateNotInstanceOf ~ " " ~ w_i ~ "notInstanceOf " ~ w_i ~ typeAnno).map {
      case (e, typ) => NotInstanceOf(e, typ)
    }

  /** Statement parser */
  def statement[_: P]: P[Statement] = P(coreStatement | terminatorStatement)

  /** CoreStatement parser */
  def coreStatement[_: P]: P[CoreStatement] =
    P(
      s_i ~ P(
        valuesStatement
          | assignStatement
          | assertStatement
      )
    )

  def valuesStatement[_: P]: P[Values] =
    P("vals " ~ s_i ~ identifier ~ s_i ~ "<-" ~ s_i ~ typeAnno).map {
      case (name, typeAnno) => Values(name, typeAnno)
    }

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

  def assertStatement[_: P]: P[Assert] = P("assert " ~ s_i ~ exp).map(Assert)

  def body[_: P]: P[Body] =
    P(
      sn_i ~ "{" ~ s_i ~ P(("\n" | "\r\n").rep(1) ~ sn_i ~ statement ~ s_i)
        .rep() ~ sn_i ~ "}"
    ).map(Body(_))

  def annoParamUnit[_: P]: P[Seq[AnnoParam]] =
    P("unit" | "Unit").map(_ => Seq.empty[AnnoParam])

  def annoParamSingle[_: P]: P[Seq[AnnoParam]] = P(annoParam).map(Seq(_))

  def patternFunctionVisibility[_: P]: P[Visibility] =
    P("def " | P("private " ~ s_i ~ "def ")).!.map {
      _ match {
        case "def " => Public
        case _      => Private
      }
    }

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
      case (visib, name, params, ret_params, bodies) =>
        PatternFunction(Option(visib), name, params.toSeq, ret_params.toSeq, bodies.toSeq)
    }

  def module[_: P]: P[Module] =
    P(
      sn_i ~ "module " ~ identifier ~ s_i ~ n_ ~ sn_i ~
        P(P("import" ~ s_i).? ~ identifier ~ s_i ~ sn_i).rep ~
        P(patternFunction ~ sn_i).rep ~ End
    ).map {
      case (name, imports, patternfunctions) =>
        Module(name, imports.toSeq, patternfunctions.toSeq)
    }

  def yieldStatement[_: P]: P[Yield] =
    P(
      s_i ~ "yield " ~ exp
    ).map(Yield)

  def failStatement[_: P]: P[TerminatorStatement] =
    P(
      s_i ~ "continue" ~ s_i
    ).map(_ => Core.Fail)

  def terminatorStatement[_: P]: P[TerminatorStatement] =
    P(yieldStatement | failStatement)
}
