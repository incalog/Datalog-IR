package inca.frontend.functional.core

import inca.compiler.source.SourceLocation
import inca.frontend.util.Resolvable

case class Module(name: Name, imports: Seq[Import], content: Seq[ModuleContent])
    extends SourceLocation
    with Import.Target {

  def usedModuleNames: Seq[Name] = name +: imports.map(_.name)

  def usedDefNames: Seq[Name] = content.flatMap {
    case fun: FunctionDef => Seq(fun.name)
    case data: DataDef => data.name +: data.constrs.map(_.name)
  }

  def prettyprint(implicit indent: String): String = {
    val importsS =
      if (imports.isEmpty) ""
      else
        "\n" + imports.map(_.prettyprint).mkString("\n")
    val contentS =
      if (content.isEmpty) ""
      else
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
  def calls: Set[Call]
}

case class FunctionDef(
    annos: Seq[Annotation],
    vis: Option[Visibility],
    name: Name,
    params: Seq[Param],
    outType: Type,
    body: Expression)
    extends ModuleContent
    with Var.Target {

  lazy val boundNames: Seq[Name] = params.map(_.name)

  val funType: TFun = TFun(params.map(_.typ), outType)

  lazy val vars: Map[Name, Option[Type]] = body.vars ++ params.flatMap(_.vars)

  def freevars: Seq[Var] =
    body.freevars.filter(v => !v.target.contains(this) && !boundNames.contains(v.name))
  def freeTvars: Seq[TData] = body.freeTvars ++ params.flatMap(_.typ.freeTvars) ++ outType.freeTvars

  lazy val calls: Set[Call] = body.calls

  def isRelation: Boolean = outType.isSet

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val paramsS = params.map(_.prettyprint).mkString(", ")
    val outS = outType.prettyprint
    s"""$annoPrefix$indent${visS}def $name($paramsS): $outS =
      |$indent  ${body.prettyprint(indent + "  ")}""".stripMargin
  }
}

case class Param(name: Name, typ: Type) extends SourceLocation with Var.Target {
  def vars: Map[Name, Option[Type]] = Map(name -> Some(typ))

  def prettyprint: String = s"$name: ${typ.prettyprint}"
}

case class DataDef(
    annos: Seq[Annotation],
    vis: Option[Visibility],
    name: Name,
    constrs: Seq[DataConstructor])
    extends ModuleContent
    with TData.Target {

  def freeTvars: Set[TData] = constrs.flatMap(_.freeTvars).toSet

  def parentName: String = "parent$_" + name.name

  override def calls: Set[Call] = Set()

  override def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    if (constrs.isEmpty)
      s"$annoPrefix$indent${visS}data $name"
    else {
      val constrS = constrs.map(_.prettyprint(indent + "  "))
      s"""$annoPrefix$indent${visS}data $name =
        |${constrS.mkString(" |\n")}
        |""".stripMargin
    }
  }
}

case class DataConstructor(name: Name, paramTypes: Seq[Type])
    extends SourceLocation
    with DataConstructor.Target
    with Var.Target {

  def constructorType(data: DataDef): TFun =
    TFun(paramTypes, TData(data.name))
  def constructorType(data: Name): TFun =
    TFun(paramTypes, TData(data))

  def selectorName: String = "un$_" + name.name

  def freeTvars: Set[TData] = paramTypes.flatMap(_.freeTvars).toSet

  def prettyprint(implicit indent: String): String = {
    val paramTypesS = paramTypes.map(_.prettyprint).mkString(", ")
    s"$indent$name($paramTypesS)"
  }
}
object DataConstructor {
  trait Target
}
