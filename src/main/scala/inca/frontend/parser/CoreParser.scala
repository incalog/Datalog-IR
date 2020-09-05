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

  def tanylinked[_: P]: P[TLinked] =
    P(P(TAnyLinked.prettyprint).map(_ => TAnyLinked))

  def tnode[_: P]: P[TLinked] = P(P(ParserUtils.identifier).!.map(TNode))

  /** Helper for the basic TypeAnno like TAny. */
  private def typeanno_helper[_: P](t: TypeAnno): P[TypeAnno] =
    P(P(t.prettyprint).map(_ => t))

  def tlinked[_: P]: P[TLinked] = P(tanylinked | tnode | tlist)

  /** TypeAnno parser */
  def typeanno[_: P]: P[TypeAnno] =
    P(
      typeanno_helper(TAny) | typeanno_helper(TBool) | typeanno_helper(TLong) |
        typeanno_helper(TInt) | typeanno_helper(TDouble) | typeanno_helper(TString) |
        tlinked | titerable
    )

  /** Visibility parser */
  def visibility[_: P]: P[Visibility] =
    P(
      P((P(" ").rep() ~ P(Private.prettyprint(""))).map(_ => Private)) |
        P((P(" ").rep() ~ P(Public.prettyprint(""))).map(_ => Public))
    )

  /**
    * TTuple parser
    * @todo  test missing
    */
  def ttuple[_: P]: P[TTuple] =
    P(
      "Unit".!.map(_ => TTuple(Seq.empty))
        | typeanno.map(typ => TTuple(Seq(typ)))
        | ("(" ~ typeanno.rep(min = 1, sep = ",") ~ ")").map(TTuple)
    )

  /** TList parser */
  def tlist[_:P]:P[TList] = P(
    P("List[" ~ tlinked ~ "]").map(TList)
  )

  /** TEnumeration parser */
  def tenumeration[_:P]:P[TEnumeration] = P(
    P("Enum[" ~ tlinked ~ "]").map(TEnumeration)
  )

  /** TIterable parser */
  def titerable[_:P]:P[TIterable] = P(tlist | tenumeration)

  /** IntLiteral parser */
  def intliteral[_: P]: P[IntLiteral] = P(ParserUtils.integer).map(IntLiteral)

  /** LongLiteral parser */
  def longliteral[_: P]: P[LongLiteral] = P(ParserUtils.integer ~ "l").map(i => LongLiteral(i))

  /** BooleanLiteral parser */
  def booleanliteral[_: P]: P[BooleanLiteral] = P("true".! | "false".!).map(s => BooleanLiteral(s.toBoolean))

  /** Param parser */
  def param[_: P]: P[Param] = P(ParserUtils.identifier ~ ":" ~ typeanno).map {
    case (name, typeAnno) => Param(name, typeAnno)
  }

  /** AnnoParam parser */
  def annoparam[_: P]: P[AnnoParam] = P("(" ~ param ~ ")" | typeanno).map {
    case Param(name, typeAnno) => AnnoParam(Some(name), typeAnno)
    case typeAnno: TypeAnno => AnnoParam(None, typeAnno)
  }

  /** Typeable parser */
  def typeable[_:P]:P[Typeable] = P(typeable)
  
  /** Exp parser */
  def exp[_:P] :P[Exp] = P(coreexp)
  
  /** CoreExp parser */
  def coreexp[_:P] :P[CoreExp] = P(eq | neq | instanceof | notinstanceof)

  /** Eq parser */
  def eq[_:P] :P[Eq] = P(exp ~ w_i ~ "==" ~ w_i ~ exp).map({case (l, r) => Eq(l, r)})
  
  /** Neq parser */
  def neq[_:P] :P[Neq] = P(exp ~ w_i ~ "!=" ~ w_i ~ exp).map({case (l, r) => Neq(l, r)})

  def instanceof[_:P] : P[InstanceOf] = P(exp ~ " instanceOf " ~ typeanno).map({case (l, r) => InstanceOf(l, r)})

  def notinstanceof[_:P] :P[NotInstanceOf] = P(exp ~ " notInstanceOf " ~ exp).map({case(l, r) => NotInstanceOf(l, r)})

  def define[_:P] :P[Def] = P("def " ~ exp).map(Def(_))

  def undef[_:P]:P[Undef] = P("undef " ~ exp).map(Undef(_))
}
