package inca.frontend.objectoriented.core

import inca.compiler.SourceLocation

case class Module(name: Name, imports: Seq[Import], classes: Seq[ClassDef])
  extends SourceLocation with Import.Target {

  def usedModuleNames: Seq[Name] = name +: imports.map(_.name)

  def prettyprint(implicit indent: String): String = {
    val importsS = if (imports.isEmpty) "" else
      "\n" + imports.map(_.prettyprint).mkString("\n")
    val contentS = if (classes.isEmpty) "" else
      "\n" + classes.map(_.prettyprint("\t")).mkString("\n")
    s"${indent}module $name$importsS$contentS".stripMargin
  }

  override def toString: String = prettyprint("")
}

case class Import(name: Name) extends SourceLocation {
  def prettyprint(implicit indent: String): String = s"${indent}import $name"
}
object Import {
  trait Target
}

trait ClassContent extends SourceLocation with Annotations {
  def vis: Option[Visibility]
  def prettyprint(implicit indent: String): String
}

case class ClassDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, parentClassNames: Seq[Name], content: Seq[ClassContent])
  extends SourceLocation with Annotations {
  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val contentS = if (content.isEmpty) "" else
      "\n" + content.map(_.prettyprint(indent+"\t")).mkString("\n\n")
    s"""$annoPrefix$indent${visS}class $name(${parentClassNames.mkString(", ")}) {$contentS\n$indent}""".stripMargin
  }
}

case class FieldDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, typ: Type, body: Option[Expression])
  extends ClassContent {
  def prettyprint(implicit indent: String): String =
    s"${indent}var $name: ${typ.prettyprint}"
}

case class MethodDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, params: Seq[Param], outType: Type, body: Seq[Statement])
  extends ClassContent {

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val paramsS = params.map(_.prettyprint).mkString(", ")
    val bodyS = body.map(stm => s"$indent\t${stm.prettyprint(indent + "\t")}").mkString("\n")
    val outS = outType.prettyprint
    s"""$annoPrefix$indent${visS}def $name($paramsS): $outS {
       |$bodyS
       |$indent}""".stripMargin
  }
}

case class ConstructorDef(annos: Seq[Annotation], vis: Option[Visibility], params: Seq[Param], body: Seq[Statement])
  extends ClassContent {

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val paramsS = params.map(_.prettyprint).mkString(", ")
    val bodyS = body.map(stm => s"$indent\t${stm.prettyprint(indent + "\t")}").mkString("\n")
    s"""$annoPrefix$indent${visS}init($paramsS) {
       |$bodyS
       |$indent}""".stripMargin
  }
}

case class Param(name: Name, typ: Type) extends SourceLocation {
  def prettyprint: String = s"$name: ${typ.prettyprint}"
}