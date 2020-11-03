package inca.frontend.core

import inca.frontend.parser.SourceLocation

case class PatternFunction(vis: Option[Visibility], name: Name, params: Seq[Param], outParams: Seq[AnnoParam], bodies: Seq[Body])
  extends SourceLocation with Call.Target {
  def boundNames: Seq[Name] = params.map(_.name) ++ outParams.flatMap(_.name)

  def freeVars: Map[Name, Option[Type]] = allVars -- boundNames

  def allVars: Map[Name, Option[Type]] = bodies.flatMap(_.allVars).toMap ++ params.flatMap(_.freeVars) ++ outParams.flatMap(_.freeVars)

  lazy val outType: Type =
    if (outParams.isEmpty)
      TUnit
    else if (outParams.size == 1)
      outParams.head.typ
    else
      TTuple(outParams.map(_.typ))

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val paramsS = params.map(_.prettyprint).mkString(", ")
    val outS = outType.prettyprint
    val bodiesS = if (bodies.isEmpty) "{ }" else
      bodies.map(_.prettyprint(indent)).mkString(" union ")
    s"$indent${visS}def $name($paramsS): $outS = $bodiesS"
  }
}

case class Param(name: Name, typ: Type) extends SourceLocation with Var.Target {
  def freeVars: Map[Name, Option[Type]] = Map(name -> Some(typ))

  def prettyprint: String = s"$name: ${typ.prettyprint}"
}

case class AnnoParam(name: Option[Name], typ: Type) extends SourceLocation {
  def freeVars: Map[Name, Option[Type]] = name.map(_ -> Some(typ)).toMap

  def prettyprint: String = name match {
    case Some(nam) => s"($nam: ${typ.prettyprint})"
    case None => typ.prettyprint
  }

}