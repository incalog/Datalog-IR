package inca.frontend.util

import inca.frontend.core.Core.{TAny, TBool, TDouble, TInt, TLinked, TList, TLong, TNode, TString, TTuple, TUnit, TypeAnno}
import inca.frontend.parser.CoreParser
import inca.frontend.parser.ParserUtils.sp
import inca.frontend.typechecker.TypeContext

object TypeHelper {

  import fastparse._
  import ScalaWhitespace._

  // TODO avoid code duplication

  private def identifier[_: P]: P[String] =
    P(CharIn("a-z", "A-Z") ~~ CharIn("a-z", "A-Z", "0-9", "_", ".").repX(0)).!

  private def tNode[_: P]: P[TNode] = P(identifier ~~ ( " with " ~~ identifier).repX).!.map(TNode)

  private def typeAnnoHelper[_: P](t: TypeAnno): P[TypeAnno] =
    P(t.prettyprint).map(_ => t)

  private def tLinked[_: P](implicit ctx: TypeContext): P[TLinked] = P(CoreParser().tAnyLinked | tNode)

  private def typeAnno[_: P](implicit ctx: TypeContext): P[TypeAnno] =
    P(
      typeAnnoHelper(TAny)
        | typeAnnoHelper(TBool)
        | typeAnnoHelper(TLong)
        | typeAnnoHelper(TInt)
        | typeAnnoHelper(TDouble)
        | typeAnnoHelper(TString)
        | typeAnnoHelper(TUnit)
        | tList
        | tLinked
        | tTuple
    )

  private def tTuple[_: P](implicit ctx: TypeContext): P[TTuple] =
    P(
      (sp ~ "(" ~ typeAnno.rep(1, sep = ",") ~ ")").map(TTuple)
    )

  private def tList[_: P](implicit ctx: TypeContext): P[TList] =
    P("List[" ~ tLinked ~ "]").map(TList)


  def decode(typName: String)(implicit ctx: TypeContext): TypeAnno = {
    // in case the result type of an expression is a string literal scala.reflect actually places this literal in the type
    // this means "hello world" results in the type String("hello world")
    // to get around this we have to remove all such occurrences
    val name = typName.trim.replaceAll("""\(".*"\)""", "")
    val rawAnno = fastparse.parse(name, typeAnno(_, ctx)).get.value
    refineTypeAnno(rawAnno)
  }

  private def refineTypeAnno(raw: TypeAnno): TypeAnno = raw match {
    case TNode("Char") | TNode("Short") | TNode("Byte") => TInt
    case TNode("Float") => TDouble
    case TTuple(ts) => TTuple(ts.map(refineTypeAnno))
    case anno => anno
  }

}
