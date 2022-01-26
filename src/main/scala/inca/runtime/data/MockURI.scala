package inca.runtime.data

import inca.runtime.db.{Database, DatabaseInspector}

case class MockURI(constr: String, args: Seq[Any]) extends truechange.URI {
  override def toString: String = s"$constr(${args.mkString(", ")})"

//  def convertToValue: Value = {
//    def deconstruct(str: String): Value = {
//      val openIdx = str.indexOf("(")
//      val closingIdx = str.lastIndexOf(")")
//      if (openIdx != -1 && closingIdx != -1) {
//        val name = str.substring(0, openIdx)
//        val args = str.substring(openIdx + 1, closingIdx).split(", ")
//        ConstructorValue(name , args.map(deconstruct))
//      } else {
//        str.toBooleanOption.map(ScalaValue.apply).getOrElse(
//          str.toIntOption.map(ScalaValue.apply).getOrElse(
//            str.toLongOption.map(ScalaValue.apply).getOrElse(
//              str.toDoubleOption.map(ScalaValue.apply).getOrElse(
//                URIValue(str)))))
//      }
//    }
//    deconstruct(repr)
//  }
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
}

