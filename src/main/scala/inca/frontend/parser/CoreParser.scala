package inca.frontend.parser

import inca.frontend.core.Core._
import fastparse._
import NoWhitespace._

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

  def tanylinked[_: P]: P[TLinked] =
    P(P(TAnyLinked.prettyprint).map(_ => TAnyLinked))

  def tnode[_: P]: P[TLinked] = P(P(ParserUtils.identifier).!.map(TNode(_)))

  /** Helper for the basic TypeAnno like TAny. */
  private def typeanno_helper[_: P](t: TypeAnno): P[TypeAnno] = 
    P(P(t.prettyprint).map(_ => t))

  def tlinked[_: P]: P[TLinked] = P(tanylinked | tnode | tlist)

  /** TypeAnno parser */
  def typeanno[_: P]: P[TypeAnno] =
    P(
      typeanno_helper(TAny) | typeanno_helper(TBool) | typeanno_helper(TLong) |
        typeanno_helper(TInt) | typeanno_helper(TDouble) | typeanno_helper(TString) |
        tlinked
    )

  /** Visibility parser */
  def visibility[_: P]: P[Visibility] =
    P(
      P((P(" ").rep() ~ P(Private.prettyprint(""))).map(_ => Private)) |
        P((P(" ").rep() ~ P(Public.prettyprint(""))).map(_ => Public))
    )

  /**
    * TTuple parser
    * @todo  should single element tuples with parens be allowed?
    * @todo  test missing
    */
  def ttuple[_: P]: P[TTuple] =
    P(
      "Unit".!.map(_ => TTuple(Seq.empty))
        | typeanno.map(typ => TTuple(Seq(typ)))
        | ("(" ~ typeanno.rep(min = 2, sep = ",") ~ ")").map(TTuple)
    )

  /** TList parser */
  def tlist[_:P]:P[TList] = P(
    P("List[" ~ tlinked ~ "]").map(TList(_))
  )

  /** TEnumeration parser */
  def tenumeration[_:P]:P[TEnumeration] = P(
    P("Enum[" ~ tlinked ~ "]").map(TEnumeration(_))
  )

  /** TIterable parser */
  def titerable[_:P]:P[TIterable] = P(tlist | tenumeration)
}
