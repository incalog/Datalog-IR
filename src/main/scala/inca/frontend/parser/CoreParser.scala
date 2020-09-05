package inca.frontend.parser

import inca.frontend.core.Core._
import fastparse._
import NoWhitespace._
import ParserUtils._

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
        tLinked | tIterable
    )

  /** Visibility parser */
  def visibility[_: P]: P[Visibility] =
    P(
      P(" ".rep() ~ P(Private.prettyprint(""))).map(_ => Private) |
        P(" ".rep() ~ P(Public.prettyprint(""))).map(_ => Public)
    )

  /**
    * TTuple parser
    * @todo  test missing
    */
  def tTuple[_: P]: P[TTuple] =
    P(
      "Unit".!.map(_ => TTuple(Seq.empty))
        | typeAnno.map(typ => TTuple(Seq(typ)))
        | ("(" ~ typeAnno.rep(min = 1, sep = ",") ~ ")").map(TTuple)
    )

  /** TList parser */
  def tList[_:P]:P[TList] = P(
    P("List[" ~ tLinked ~ "]").map(TList)
  )

  /** TEnumeration parser */
  def tEnumeration[_:P]:P[TEnumeration] = P(
    P("Enum[" ~ tLinked ~ "]").map(TEnumeration)
  )

  /** TIterable parser */
  def tIterable[_:P]:P[TIterable] = P(tList | tEnumeration)


  /** Literal parser */
  def literal[_: P]: P[Literal] = P(unitLiteral | longLiteral | intLiteral | booleanLiteral)

  /** UnitLiteral parser */
  def unitLiteral[_: P]: P[UnitLiteral.type] = P("unit").map(_ => UnitLiteral)

  /** IntLiteral parser */
  def intLiteral[_: P]: P[IntLiteral] = P(ParserUtils.integer).map(IntLiteral)

  /** LongLiteral parser */
  def longLiteral[_: P]: P[LongLiteral] = P(ParserUtils.long ~ "L").map(LongLiteral)

  /** BooleanLiteral parser */
  def booleanLiteral[_: P]: P[BooleanLiteral] = P("true" | "false").!.map(s => BooleanLiteral(s.toBoolean))

  /** Param parser */
  def param[_: P]: P[Param] = P(ParserUtils.identifier ~ ":" ~ typeAnno).map {
    case (name, typeAnno) => Param(name, typeAnno)
  }

  /** AnnoParam parser */
  def annoParam[_: P]: P[AnnoParam] = P("(" ~ param ~ ")" | typeAnno).map {
    case Param(name, typeAnno) => AnnoParam(Some(name), typeAnno)
    case typeAnno: TypeAnno => AnnoParam(None, typeAnno)
  }

  /** Link parser */
  def link[_: P]: P[Link] = P(tNode ~ ".").flatMap(coreLink)

  /** CoreLink parser */
  def coreLink[_: P](node: TNode): P[CoreLink] = P(
    parentLink
      | childrenLink
      | prevLink
      | sizeLink
      | nextLink
      | namedLink(node)
  )

  /** ParentLink parser */
  def parentLink[_: P]: P[ParentLink.type] = P(ParentLink.prettyprint).map(_ => ParentLink)

  /** ChildrenLink parser */
  def childrenLink[_: P]: P[ChildrenLink.type] = P(ChildrenLink.prettyprint).map(_ => ChildrenLink)

  /** NextLink parser */
  def nextLink[_: P]: P[NextLink.type] = P(NextLink.prettyprint).map(_ => NextLink)

  /** PreviousLink parser */
  def prevLink[_: P]: P[PreviousLink.type] = P(PreviousLink.prettyprint).map(_ => PreviousLink)

  /** SizeLink parser */
  def sizeLink[_: P]: P[SizeLink.type] = P(SizeLink.prettyprint).map(_ => SizeLink)

  /** NamedLink parser */
  def namedLink[_: P](node: TNode): P[NamedLink] = P(identifier).map(NamedLink(node, _))


  /** Exp parser */
  def exp[_: P]: P[Exp] = P(coreExp)

  /**
   *CoreExp parser
   * @todo fix stack overflow on Def and Undef parsing
   */
  def coreExp[_: P]: P[CoreExp] = P(
    constantCoreExp
    | varCoreExp
    | defCoreExp
    | undefCoreExp
    | eqCoreExp
    | neqCoreExp
    | instanceOfCoreExp
    | notInstanceOfCoreExp
  )

  /** Var parser  */
  def varCoreExp[_: P]: P[Var] = P(identifier).map(Var)

  /** Constant parser */
  def constantCoreExp[_: P]: P[Constant] = P(literal).map(Constant)

  /** Eq parser */
  def eqCoreExp[_: P]: P[Eq] = P(exp ~ w_i ~ "==" ~ w_i ~ exp).map {
    case (e1, e2) => Eq(e1, e2)
  }

  /** Neq parser */
  def neqCoreExp[_: P]: P[Neq] = P(exp ~ w_i ~ "!=" ~ w_i ~ exp).map {
    case (e1, e2) => Neq(e1, e2)
  }

  /** Def parser */
  def defCoreExp[_: P]: P[Def] = P("def " ~ exp).map(Def)

  /** Undef parser */
  def undefCoreExp[_: P]: P[Undef] = P("undef " ~ exp).map(Undef)

  /** InstanceOf parser */
  def instanceOfCoreExp[_: P]: P[InstanceOf] = P(exp ~ " instanceOf " ~ typeAnno).map {
    case (e, typ) => InstanceOf(e, typ)
  }

  /** NotInstanceOf parser */
  def notInstanceOfCoreExp[_: P]: P[NotInstanceOf] = P(exp ~ " notInstanceOf " ~ typeAnno).map {
    case (e, typ) => NotInstanceOf(e, typ)
  }

  
}
