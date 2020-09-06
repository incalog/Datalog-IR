package inca.frontend.parser

import inca.frontend.core.Core._
import fastparse._
import NoWhitespace._
import ParserUtils._
import inca.frontend.core.Core

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
    P(ParserUtils.identifier ~ ":" ~ typeAnno).map {
      case (name, typeAnno) => Param(name, typeAnno)
    }

  /** AnnoParam parser */
  def annoParam[_: P]: P[AnnoParam] =
    P("(" ~ param ~ ")" | typeAnno).map {
      case Param(name, typeAnno) => AnnoParam(Some(name), typeAnno)
      case typeAnno: TypeAnno    => AnnoParam(None, typeAnno)
    }

  /** Link parser */
  def link[_: P]: P[Link] = P(tNode ~ ".").flatMap(coreLink)

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
    * In the sequence expressions with preset keywords such as 'def' are placed in the beginning
    * reducing the recusion depth; expressions with infix operators in the center avoiding an early stop
    * from the constant expression which do no recursive call at all at the end.
    *
    * This might make it impossible to do language extentions in a seperate class or object.
    */
  def coreExp[_: P]: P[Exp] =
    P(
      defCoreExp
        | undefCoreExp
        | eqCoreExp
        | neqCoreExp
        | instanceOfCoreExp
        | notInstanceOfCoreExp
        | varCoreExp
        | constantCoreExp
        | bracketExp
    )

  /** See CoreExp parser @see coreExp */
  def terminateDef[_: P]: P[Exp] =
    P(
      undefCoreExp
        | eqCoreExp
        | neqCoreExp
        | instanceOfCoreExp
        | notInstanceOfCoreExp
        | varCoreExp
        | constantCoreExp
        | bracketExp
        | exp
    )

  /** See CoreExp parser @see coreExp */
  def terminateUndef[_: P]: P[Exp] =
    P(
      eqCoreExp
        | neqCoreExp
        | instanceOfCoreExp
        | notInstanceOfCoreExp
        | varCoreExp
        | constantCoreExp
        | bracketExp
        | exp
    )

  /** See CoreExp parser @see coreExp */
  def terminateEq[_: P]: P[Exp] =
    P(
      neqCoreExp
        | instanceOfCoreExp
        | notInstanceOfCoreExp
        | varCoreExp
        | constantCoreExp
        | bracketExp
        | exp
    )

  /** See CoreExp parser @see coreExp */
  def terminateNeq[_: P]: P[Exp] =
    P(
      instanceOfCoreExp
        | notInstanceOfCoreExp
        | varCoreExp
        | constantCoreExp
        | bracketExp
        | exp
    )

  /** See CoreExp parser @see coreExp */
  def terminateInstanceOf[_: P]: P[Exp] =
    P(
      notInstanceOfCoreExp
        | varCoreExp
        | constantCoreExp
        | bracketExp
        | exp
    )

  /** See CoreExp parser @see coreExp */
  def terminateNotInstanceOf[_: P]: P[Exp] =
    P(
      varCoreExp
        | constantCoreExp
        | bracketExp
        | exp
    )

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
  def defCoreExp[_: P]: P[CoreExp] = P("def " ~ terminateDef).map(Def)

  /** Undef parser */
  def undefCoreExp[_: P]: P[Undef] = P("undef " ~ terminateUndef).map(Undef)

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


  def statement[_: P]: P[Statement] = P(coreStatement)

  def coreStatement[_: P]: P[CoreStatement] =
    P(
      s_i ~ (
        valuesStatement
        | assignStatement
        | assertStatement
        )
    )

  def valuesStatement[_: P]: P[Values] = P("vals " ~ s_i ~ identifier ~ s_i ~ "<-" ~ s_i ~ typeAnno).map {
    case (name, typeAnno) => Values(name, typeAnno)
  }

  def assignStatement[_: P]: P[Assign] =
    P(
      ("val " ~ s_i ~ identifier ~ s_i ~ "=" ~ s_i ~ exp).map {
        case (name, expr) => Assign(Seq(name), expr)
      }
      | ("val " ~ s_i ~ "(" ~ (s_i ~ identifier ~ s_i).rep(min = 2, sep = ",") ~ s_i ~ ")" ~ s_i ~ "=" ~ s_i ~ exp).map {
        case (names, expr) => Assign(names, expr)
      }
    )

  def assertStatement[_: P]: P[Assert] = P("assert " ~ s_i ~ exp).map(Assert)


}
