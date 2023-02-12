package inca.frontend.constraint.typechecker

import inca.frontend.constraint.core._
import inca.util.Scala

object TypeHelper {

  def decode(typStr: String): Either[String, Type] = {
//    // in case the result type of an expression is a string literal scala.reflect actually places this literal in the type
//    // this means "hello world" results in the type String("hello world")
//    // to get around this we have to remove all such occurrences
    val typStrCleaned = typStr.trim
      .replaceAll("""String\(".*"\)""", "String")
      .replaceAll("""Int\(\d*\)""", "Int")
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
