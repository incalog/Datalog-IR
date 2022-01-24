package inca.backend.hints

import inca.backend.hints.Hint.Key

import scala.reflect.ClassTag

object DebugHints {
  object SourceConstruct {
    val key: Key = "SourceConstruct"
    def from[C](constr: C): Hint = constr match {
      case hinted: Hints => SourceConstruct(hinted.hints.getOrElse(key, constr))
      case _ => SourceConstruct(constr)
    }
    def from[C](constr: C, default: C): Hint = constr match {
      case hinted: Hints => SourceConstruct(hinted.hints.getOrElse(key, constr))
      case _ => SourceConstruct(default)
    }
    def get(hints: Hints): Option[Any] =
      hints.getHint(key) match {
        case None => None
        case Some(SourceConstruct(constr)) => Some(constr)
      }
  }
  case class SourceConstruct[C](constr: C) extends Hint {
    override val key: Key = SourceConstruct.key
  }
}
