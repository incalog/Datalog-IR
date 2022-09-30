package inca.frontend.objectoriented.core

import inca.compiler.SourceLocation
import inca.frontend.util.Resolvable

import scala.language.postfixOps

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

  override def dotString: String =
    "digraph G {\n" + super.dotString + classes.map { c =>
      s"$nodeId -> ${c.nodeId};\n${c.dotString}"
    }.mkString("") + "}"

  override def toString: String = prettyprint("")
}

case class Import(name: Name) extends SourceLocation with Resolvable[Import.Target] {
  def prettyprint(implicit indent: String): String = s"${indent}import $name"
}
object Import {
  trait Target
}

trait ClassContent extends SourceLocation with Annotations {
  def vis: Option[Visibility]
  def prettyprint(implicit indent: String): String
}

case class ClassDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, parentClassRefs: Seq[ClassRef], content: Seq[ClassContent])
  extends SourceLocation with Annotations with VarReadExpr.Target {

  val contentMap: Map[Name, Seq[ClassContent]] = content.groupBy {
    case field: FieldDef => field.name
    case method: MethodDef => method.name
    case _: ConstructorDef => name
  }

  def fields: Seq[FieldDef] = content.collect { case f: FieldDef => f }
  def methods: Seq[MethodDef] = content.collect { case f: MethodDef => f }
  def constructors: Seq[ConstructorDef] = content.collect { case f: ConstructorDef => f }

  def typ: TClass = {
    val ref = ClassRef(name)
    ref.target = Some(this)
    TClass(ref)
  }

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val contentS = if (content.isEmpty) "" else
      "\n" + content.map(_.prettyprint(indent+"\t")).mkString("\n\n")
    s"""$annoPrefix$indent${visS}class $name(${parentClassRefs.mkString(", ")}) {$contentS\n$indent}""".stripMargin
  }

  override def nodeShape: String = "box"

  override def dotString: String =
    super.dotString + content.map { c =>
      s"$nodeId -> ${c.nodeId};\n${c.dotString}"
    }.mkString("")
}

case class ClassRef(name: Name) extends SourceLocation with Resolvable[ClassDef] {
  override def toString: String = name.toString
}

case class FieldDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, typ: Type, body: Option[Expression],
                    immutable: Boolean)
  extends ClassContent {
  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val expr = if (body.isEmpty) "" else s" = ${body.get}"
    val prefix = if (immutable) "val " else "var "
    s"$indent$visS$prefix$name: ${typ.prettyprint}$expr"
  }

  override def nodeShape: String = "diamond"

  override def dotString: String = {
    if (body.isDefined) {
      super.dotString + s"""$nodeId -> ${body.get.nodeId} [label="body"];\n${body.get.dotString}"""
    } else {
      super.dotString
    }
  }
}

case class MethodDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, params: Seq[Param], outType: Type, body: Seq[Statement])
  extends ClassContent {

  lazy val vars: Map[Name, Option[Type]] = (body.flatMap(_.vars) ++ params.flatMap(_.vars)).toMap

  def returnsUnit: Boolean = outType == TUnit
  def isMain: Boolean = annos.contains(MainAnnotation)

  def paramSignature: Int = (params.map(_.typ) :+ outType).hashCode()

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val paramsS = params.map(_.prettyprint).mkString(", ")
    val bodyS = body.map(_.prettyprint(indent + "\t")).mkString("\n")
    val outS = outType.prettyprint
    s"""$annoPrefix$indent${visS}def $name($paramsS): $outS {
       |$bodyS
       |$indent}""".stripMargin
  }

  override def nodeShape: String = "octagon"

  override def dotString: String =
    super.dotString + body.zipWithIndex.map { case (stmt, i) =>
      s"""$nodeId -> ${stmt.nodeId} [label="body[$i]"];\n${stmt.dotString}"""
    }.mkString("") + params.zipWithIndex.map { case (param, i) =>
      s"""$nodeId -> ${param.nodeId} [label="param[$i]"];\n${param.dotString}"""
    }.mkString("")
}

case class ConstructorDef(annos: Seq[Annotation], vis: Option[Visibility], params: Seq[Param], body: Seq[Statement])
  extends ClassContent {

  lazy val vars: Map[Name, Option[Type]] = (body.flatMap(_.vars) ++ params.flatMap(_.vars)).toMap

  def paramSignature: Int = params.map(_.typ).hashCode()

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val paramsS = params.map(_.prettyprint).mkString(", ")
    val bodyS = body.map(_.prettyprint(indent + "\t")).mkString("\n")
    s"""$annoPrefix$indent${visS}init($paramsS) {
       |$bodyS
       |$indent}""".stripMargin
  }

  override def nodeShape: String = "doubleoctagon"

  override def dotString: String =
    super.dotString + body.zipWithIndex.map { case (stmt, i) =>
      s"""$nodeId -> ${stmt.nodeId} [label="body[$i]"];\n${stmt.dotString}"""
    }.mkString("") + params.zipWithIndex.map { case (param, i) =>
      s"""$nodeId -> ${param.nodeId} [label="param[$i]"];\n${param.dotString}"""
    }.mkString("")
}

case class Param(name: Name, typ: Type) extends SourceLocation with VarReadExpr.Target {
  def vars: Map[Name, Option[Type]] = Map(name -> Some(typ))

  def prettyprint: String = s"$name: ${typ.prettyprint}"

  override def nodeShape: String = "polygon"
}