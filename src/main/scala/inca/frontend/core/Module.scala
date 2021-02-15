package inca.frontend.core

import inca.frontend.parser.SourceLocation
import inca.frontend.typechecker.Resolvable

case class Module(name: Name, imports: Seq[Import], content: Seq[ModuleContent])
  extends SourceLocation with Import.Target {

  def usedModuleNames: Seq[Name] = name +: imports.map(_.name)

  def usedDefNames: Seq[Name] = content.flatMap {
    case fun: FunctionDef => Some(fun.name)
    case valDef: ValDef => Some(valDef.name)
  }

  def prettyprint(implicit indent: String): String = {
    val importsS = if (imports.isEmpty) "" else
      "\n" + imports.map(_.prettyprint).mkString("\n")
    val contentS = if (content.isEmpty) "" else
      "\n" + content.map(_.prettyprint).mkString("\n")
    s"${indent}module $name$importsS$contentS".stripMargin
  }

  override def toString: String = prettyprint("")
}

case class Import(name: Name) extends SourceLocation with Resolvable[Import.Target] {
  def prettyprint(implicit indent: String): String = s"${indent}import $name"
}
object Import {
  trait Target
}




trait ModuleContent extends SourceLocation {
  def vis: Option[Visibility]
  def prettyprint(implicit indent: String): String
}

case class ValDef(vis: Option[Visibility], name: Name, typ: Option[Type], exp: Expression) extends ModuleContent with Var.Target {
  override def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val typS = typ match {
      case Some(ty) => s": ${ty.prettyprint}"
      case None => ""
    }
    s"$indent${visS}val $name$typS = ${exp.prettyprint}"
  }

  def getType: Option[Type] = typ.orElse(exp.typ)
}

case class FunctionDef(vis: Option[Visibility], name: Name, params: Seq[Param], outType: Type, body: Expression)
  extends ModuleContent with Call.Target {
  def boundNames: Seq[Name] = params.map(_.name)

  def vars: Map[Name, Option[Type]] = body.vars ++ params.flatMap(_.vars)

  def outParams: Seq[Type] = outType match {
    case TUnit => Seq()
    case TTuple(ts) => ts
    case ty => Seq(ty)
  }

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val paramsS = params.map(_.prettyprint).mkString(", ")
    val outS = outType.prettyprint
    s"""$indent${visS}def $name($paramsS): $outS =
       |$indent  ${body.prettyprint(indent + "  ")}""".stripMargin
  }
}

case class Param(name: Name, typ: Type) extends SourceLocation with Var.Target {
  def vars: Map[Name, Option[Type]] = Map(name -> Some(typ))

  def prettyprint: String = s"$name: ${typ.prettyprint}"
}