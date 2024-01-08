package inca.runtime.data

import inca.runtime.db.{Database, DatabaseInspector}

class MockURI(val repr: String) extends truechange.URI {
  override def equals(obj: Any): Boolean = obj match {
    case other: MockURI => this.repr == other.repr
    case _ => false
  }

  override def hashCode(): Int = repr.hashCode

  override def toString: String = repr
}

trait Value {
  def deepPrettyPrint(db: DatabaseInspector): String
}
case class ConstructorValue(name: String, args: Seq[Value]) extends Value {
  override def deepPrettyPrint(db: DatabaseInspector): String = s"$name(${args.map(_.deepPrettyPrint(db)).mkString(", ")})"
}
case class ScalaValue(v: Any) extends Value {
  override def deepPrettyPrint(db: DatabaseInspector): String = v.toString
}
case class URIValue(id: String) extends Value {
  override def deepPrettyPrint(db: DatabaseInspector): String = db.prettyPrint(this)
}

object MockURI {
  val DEBUG_PRINT = true

  def apply(constr: String, args: Any*): MockURI = {
    val strArgs = args.map {
      case data: MockURI => data.repr
      case arg => arg.toString
    }
    new MockURI(constr + strArgs.mkString("(", ", ", ")"))
  }

  def convertToValue(uri: MockURI): Value = {
    def deconstruct(str: String): Value = {
      val openIdx = str.indexOf("(")
      val closingIdx = str.lastIndexOf(")")
      if (openIdx != -1 && closingIdx != -1) {
        val name = str.substring(0, openIdx)
        val args = str.substring(openIdx + 1, closingIdx).split(", ").toSeq
        ConstructorValue(name , args.map(deconstruct))
      } else {
        str.toBooleanOption.map(ScalaValue.apply).getOrElse(
          str.toIntOption.map(ScalaValue.apply).getOrElse(
            str.toLongOption.map(ScalaValue.apply).getOrElse(
              str.toDoubleOption.map(ScalaValue.apply).getOrElse(
                URIValue(str)))))
      }
    }

    deconstruct(uri.repr)
  }
}

