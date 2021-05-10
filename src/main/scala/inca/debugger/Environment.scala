package inca.debugger

import inca.backend.ir.Datalog.Name
import truechange.URI

case class Environment(columns: Map[Name, Int], tuples: Set[Tuple]) {
  def columnNames: Seq[Name] = columns.toList.sortBy(_._2).map(_._1)
  override def toString: String = columnNames + " -> " + tuples.mkString("{", ",", "}")
}

case class Tuple(private[debugger] val ar: Array[Value]) {
  override def toString: Name = ar.mkString("(", ", ", ")")
}

sealed trait Value {
  def prettyPrint: String
}
case class URIValue(uri: URI) extends Value {
  override def toString: String = prettyPrint
  override def prettyPrint: String = uri.toString
}
case class ScalaValue(v: Any) extends Value {
  override def toString: String = prettyPrint
  override def prettyPrint: String = v.toString
}
