package inca.foreign.scala.data

case class StructuralUID(constr: String, args: Any*):
  override def toString: String =
    s"""${constr}_uid(${args.mkString(", ")})"""
