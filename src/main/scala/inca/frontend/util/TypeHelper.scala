package inca.frontend.util

import inca.frontend.Frontend
import inca.frontend.core.Core._
import inca.runtime.context.LanguageMetaInfo

object TypeHelper {

  import fastparse._
  import ScalaWhitespace._

  private val cp = Frontend.Inca(new LanguageMetaInfo())

  private def tNode[_: P]: P[TNode] = P(cp.fullyQualifiedIdentifier.! ~~ ( " with " ~~ cp.fullyQualifiedIdentifier).repX).map {
    case (name, _) => TNode(name)
  }

  private def typeAnnoHelper[_: P](t: TypeAnno): P[TypeAnno] =
    P(t.prettyprint).map(_ => t)

  private def tLinked[_: P]: P[TLinked] = P(cp.tAnyLinked | tNode)

  private def typeAnno[_: P]: P[TypeAnno] =
    Start ~ P(
      typeAnnoHelper(TAny)
        | typeAnnoHelper(TBool)
        | typeAnnoHelper(TLong)
        | typeAnnoHelper(TInt)
        | typeAnnoHelper(TDouble)
        | typeAnnoHelper(TString)
        | typeAnnoHelper(TUnit)
        | tList
        | tLinked
        | cp.tTuple
    ) ~ End

  private def tList[_: P]: P[TList] =
    P("List[" ~ tLinked ~ "]").flatMap {
      case inner@TNode(name) => name match {
        case "Int"
             | "Double"
             | "Boolean"
             | "Any"
             | "Long"
             | "String" => fastparse.Fail
        case name if name.startsWith("(") => fastparse.Fail
        case _ => fastparse.Pass(TList(inner))
      }
      case inner => fastparse.Pass(TList(inner))
    }

  def decode(typName: String): Option[TypeAnno] = {
    // in case the result type of an expression is a string literal scala.reflect actually places this literal in the type
    // this means "hello world" results in the type String("hello world")
    // to get around this we have to remove all such occurrences
    val name = typName.trim.replaceAll("""\(".*"\)""", "")
    val rawAnno = fastparse.parse(name, typeAnno(_)) match {
      case Parsed.Failure(_, _, _) => None
      case Parsed.Success(anno, _) => Some(anno)
    }
    rawAnno.map(refineTypeAnno)
  }

  private def refineTypeAnno(raw: TypeAnno): TypeAnno = raw match {
    case TNode("Char") | TNode("Short") | TNode("Byte") => TInt
    case TNode("Float") => TDouble
    case TTuple(ts) => TTuple(ts.map(refineTypeAnno))
    case anno => anno
  }

}
