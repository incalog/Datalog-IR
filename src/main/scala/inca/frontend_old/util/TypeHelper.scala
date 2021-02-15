package inca.frontend_old.util

import inca.compiler.CompilerFrontend
import inca.frontend_old.core.tree._
import inca.runtime.context.LanguageMetaInfo
import inca.util.Meta.Scala

object TypeHelper {

  import fastparse._
  import ScalaWhitespace._

  private val cp = CompilerFrontend.Inca(new LanguageMetaInfo())

  private def tNode[_: P]: P[TNode] = P(cp.fullyQualifiedIdentifier.! ~~ ( " with " ~~ cp.fullyQualifiedIdentifier).repX).map {
    case (name, _) => TNode(name)
  }

  private def typeAnnoHelper[_: P](t: Type): P[Type] =
    P(t.prettyprint).map(_ => t)

  private def tLinked[_: P]: P[TLinked] = P(cp.tLinked | tNode)

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

  def decode(typStr: String): Either[String, Type] = {
//    // in case the result type of an expression is a string literal scala.reflect actually places this literal in the type
//    // this means "hello world" results in the type String("hello world")
//    // to get around this we have to remove all such occurrences
    val typStrCleaned = typStr.trim
      .replaceAll("""String\(".*"\)""", "String")
      .replaceAll("""Int\(\d*\)""", "Int")
//    val rawAnno = fastparse.parse(name, typeAnno(_)) match {
//      case Parsed.Failure(_, _, _) => None
//      case Parsed.Success(anno, _) => Some(anno)
//    }
//    rawAnno.map(refineType)
    import meta.parsers._
    val metaTyp = typStrCleaned.parse[meta.Type].get
    decode(metaTyp)
  }

  def decode(typ: meta.Type): Either[String, Type] = typ match {
    case meta.Type.Function(_, _) => Left("Inca does not support higher-order functions")
    case meta.Type.Name("Unit") => Right(TUnit)
    case meta.Type.Tuple(ts) =>
      val types = ts.map(decode).foldRight[Either[String, Seq[Type]]](Right(Seq())) {
        case (Right(ty), Right(seq)) => Right(ty +: seq)
        case (_, err@Left(_)) => err
        case (r@Left(msg), _) => Left(msg)
      }
      types.map(TTuple)
    case _ => Right(TScala(Scala(typ)))
  }

}
