package inca.frontend.parser

import inca.frontend.core.Core._
import fastparse._
import NoWhitespace._

/**
  * Parser for the IncA Core language.
  *
  * @todo    unfinished
  * @version 0.0.1
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object CoreParser {
  
  /** Parse a variable identifier */
  def identifier[_: P]: P[String] =
    P(P(CharIn("a-z", "A-Z")) ~ P(CharIn("a-z", "A-Z", "0-9", "_").rep(0))).!

  def tanylinked_typeanno[_: P]: P[TLinked] =
    P(P(TAnyLinked.prettyprint).map(_ => TAnyLinked))

  def tnode_typeanno[_: P]: P[TLinked] = P(P(identifier).!.map(TNode(_)))

  /** Helper for the basic TypeAnno like TAny. */
  private def typeanno_helper[_: P](t: TypeAnno): P[TypeAnno] =
    P(P(t.prettyprint).map(_ => t))

  def tlinked_typeanno[_:P]: P[TLinked] = P(tanylinked_typeanno | tnode_typeanno)

  /** TypeAnno base parser */
  def typeanno[_: P]: P[TypeAnno] =
    P(
      typeanno_helper(TAny) | typeanno_helper(TBool) | typeanno_helper(TLong) |
        typeanno_helper(TInt) | typeanno_helper(TDouble) | typeanno_helper(TString) |
        tlinked_typeanno
    )

  /** Visibility base parser */
  def visibility[_: P]: P[Visibility] =
    P(
      P((P(" ").rep() ~ P(Private.prettyprint(""))).map(_ => Private)) |
        P((P(" ").rep() ~ P(Public.prettyprint(""))).map(_ => Public))
    )
}
