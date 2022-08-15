package inca.frontend.objectoriented.core

import inca.compiler.SourceLocation

import java.util.UUID

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

  lazy val nodeId: Int = UUID.randomUUID().hashCode()
  lazy val nodeName: String = this.getClass.getSimpleName

  def dotString(): String =
    s"""digraph G {\n$nodeId [label="$nodeName", shape=plaintext];\n""" + classes.map { c =>
      s"$nodeId -> ${c.nodeId};\n${c.dotString()}"
    }.mkString("") + "}"

  override def toString: String = prettyprint("")
}

case class Import(name: Name) extends SourceLocation {
  def prettyprint(implicit indent: String): String = s"${indent}import $name"
}
object Import {
  trait Target
}

trait ClassContentDef extends SourceLocation with Annotations {
  def vis: Option[Visibility]
  def prettyprint(implicit indent: String): String
  def dotString(): String

  lazy val nodeId: Int = UUID.randomUUID().hashCode()
  lazy val nodeName: String = this.getClass.getSimpleName
}

case class ClassDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, parentClassNames: Seq[Name], content: Seq[ClassContentDef])
  extends SourceLocation with Annotations {
  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val contentS = if (content.isEmpty) "" else
      "\n" + content.map(_.prettyprint(indent+"\t")).mkString("\n\n")
    s"""$annoPrefix$indent${visS}class $name(${parentClassNames.mkString(", ")}) {$contentS\n$indent}""".stripMargin
  }

  lazy val nodeId: Int = UUID.randomUUID().hashCode()
  lazy val nodeName: String = this.getClass.getSimpleName

  def dotString(): String =
    s"""$nodeId [label="$nodeName", shape=box];\n""" + content.map { c =>
      s"$nodeId -> ${c.nodeId};\n${c.dotString()}"
    }.mkString("")
}

case class FieldDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, typ: Type, body: Option[Expression])
  extends ClassContentDef {
  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val expr = if (body.isEmpty) "" else s" = ${body.get}"
    s"${indent}${visS}var $name: ${typ.prettyprint}$expr"
  }

  def dotString(): String =
    s"""$nodeId [label="$nodeName", shape=diamond];\n"""
}

case class MethodDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, params: Seq[Param], outType: Type, body: Seq[Statement])
  extends ClassContentDef {

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val paramsS = params.map(_.prettyprint).mkString(", ")
    val bodyS = body.map(_.prettyprint(indent + "\t")).mkString("\n")
    val outS = outType.prettyprint
    s"""$annoPrefix$indent${visS}def $name($paramsS): $outS {
       |$bodyS
       |$indent}""".stripMargin
  }

  def dotString(): String =
    s"""$nodeId [label="$nodeName", shape=octagon];\n""" + body.zipWithIndex.map { case (stmt, i) =>
      s"""$nodeId -> ${stmt.nodeId} [label="body[$i]"];\n${stmt.dotString()}"""
    }.mkString("") + params.zipWithIndex.map { case (param, i) =>
      s"""$nodeId -> ${param.nodeId} [label="param[$i]"];\n${param.dotString()}"""
    }.mkString("")
}

case class ConstructorDef(annos: Seq[Annotation], vis: Option[Visibility], params: Seq[Param], body: Seq[Statement])
  extends ClassContentDef {

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val paramsS = params.map(_.prettyprint).mkString(", ")
    val bodyS = body.map(_.prettyprint(indent + "\t")).mkString("\n")
    s"""$annoPrefix$indent${visS}init($paramsS) {
       |$bodyS
       |$indent}""".stripMargin
  }

  def dotString(): String =
    s"""$nodeId [label="$nodeName", shape=octagon];\n""" + body.zipWithIndex.map { case (stmt, i) =>
      s"""$nodeId -> ${stmt.nodeId} [label="body[$i]"];\n${stmt.dotString()}"""
    }.mkString("") + params.zipWithIndex.map { case (param, i) =>
      s"""$nodeId -> ${param.nodeId} [label="param[$i]"];\n${param.dotString()}"""
    }.mkString("")
}

case class Param(name: Name, typ: Type) extends SourceLocation {
  def prettyprint: String = s"$name: ${typ.prettyprint}"

  lazy val nodeId: Int = UUID.randomUUID().hashCode()
  lazy val nodeName: String = this.getClass.getSimpleName

  def dotString(): String =
    s"""$nodeId [label="$nodeName", shape=polygon];\n"""
}