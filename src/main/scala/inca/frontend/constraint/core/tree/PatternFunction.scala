package inca.frontend.constraint.core.tree

import inca.compiler.SourceLocation

case class PatternFunction(vis: Option[Visibility], name: Name, params: Seq[Param], outType: Type, bodies: Seq[Body])
  extends ModuleContent with Call.Target {
  def boundNames: Seq[Name] = params.map(_.name)

  def freeVars: Map[Name, Option[Type]] = allVars -- boundNames

  def allVars: Map[Name, Option[Type]] = bodies.flatMap(_.allVars).toMap ++ params.flatMap(_.freeVars)

  def outParams: Seq[Type] = outType match {
    case TUnit => Seq()
    case TTuple(ts) => ts
    case ty => Seq(ty)
  }

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
