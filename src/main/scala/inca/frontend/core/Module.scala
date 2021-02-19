package inca.frontend.core

import inca.compiler.SourceLocation
import inca.frontend.typechecker.Resolvable

case class Module(name: Name, imports: Seq[Import], content: Seq[ModuleContent])
  extends SourceLocation with Import.Target {

  def usedModuleNames: Seq[Name] = name +: imports.map(_.name)

  def usedDefNames: Seq[Name] = content.flatMap {
    case fun: FunctionDef => Seq(fun.name)
    case data: DataDef => data.name +: data.constrs.map(_.name)
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




trait ModuleContent extends SourceLocation with Annotations {
  def vis: Option[Visibility]
  def prettyprint(implicit indent: String): String
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

case class DataDef(vis: Option[Visibility], name: Name, constrs: Seq[DataConstructor])
  extends ModuleContent with TData.Target {

  override def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    if (constrs.isEmpty)
      s"$indent${visS}data $name"
    else {
      val constrS = constrs.map(_.prettyprint(indent + "  "))
      s"""$indent${visS}data $name =
         |${constrS.mkString(" |\n")}
         |""".stripMargin
    }
  }
}

case class DataConstructor(name: Name, paramTypes: Seq[Type]) extends DataConstructor.Target {

  def selectorName: String = "un$_" + name.name

  def prettyprint(implicit indent: String): String = {
    val paramTypesS = paramTypes.map(_.prettyprint).mkString(", ")
    s"$indent$name($paramTypesS)"
  }
}
object DataConstructor {
  trait Target extends Call.Target
}