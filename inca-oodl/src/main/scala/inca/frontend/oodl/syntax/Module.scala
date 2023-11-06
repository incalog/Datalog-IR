package inca.frontend.oodl.syntax

import inca.ir.Name
import inca.ir.typing.Resolvable
import inca.ir.util.SourceLocation

case class Import(name: Name) extends SourceLocation with Resolvable[Import.Target]:
  def prettyprint(implicit indent: String): String = s"${indent}import $name"

object Import:
  trait Target


sealed trait Visibility extends SourceLocation:
  def prettyprint(implicit indent: String): String

case class Private() extends Visibility:
  def prettyprint(implicit indent: String): String = "private"

case class Module(name: Name, imports: Seq[Import], content: Seq[ModuleContent]) extends SourceLocation with Import.Target:

  def usedModuleNames: Seq[Name] = name +: imports.map(_.name)

  def classes: Seq[ClassDef] = content.flatMap {
    case c: ClassDef => Some(c)
    case _ => None
  }
  
  def functions: Seq[FunctionDef] = content.flatMap {
    case f: FunctionDef => Some(f)
    case _ => None
  }

  def prettyprint(implicit indent: String): String = {
    val importsS = if (imports.isEmpty) "" else
      "\n" + imports.map(_.prettyprint).mkString("\n")
    val contentS = if (content.isEmpty) "" else
      "\n" + content.map(_.prettyprint).mkString("\n")
    s"${indent}module $name$importsS$contentS".stripMargin
  }

  override def toString: String = prettyprint("")

trait ModuleContent extends SourceLocation with Annotations:
  def vis: Option[Visibility]
  def prettyprint(implicit indent: String): String
  override def toString: String = prettyprint("")

case class Param(name: Name, typ: Type) extends SourceLocation with Var.Target:
  def vars: Map[Name, Option[Type]] = Map(name -> Some(typ))
  def prettyprint(implicit indent: String): String = s"$name: ${typ.prettyprint}"
  override def toString: String = prettyprint("")

case class ParametricType(name: Name) extends SourceLocation with TName.Target:
  def prettyprint(implicit indent: String): String = name.name
  override def toString: String = prettyprint("")

case class FunctionDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, tyVars: Seq[ParametricType], params: Seq[Param], outType: Type, body: Seq[Statement]) extends ModuleContent:
  override def prettyprint(implicit indent: String): String = {
    val tyS = if (tyVars.isEmpty) "" else tyVars.mkString("[", ", ", "]")
    val visS = if (vis.contains(Private)) "private " else ""
    val paramsS = params.map(_.prettyprint).mkString("(", ", ", ")")
    val bodyS = body.map(_.prettyprint(indent + "\t")).mkString("\n")
    val outS = outType.prettyprint
    s"""$annoPrefix$indent${visS}def $name$tyS$paramsS: $outS = {
       |$bodyS
       |$indent}""".stripMargin
  }

case class ClassDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, tyVars: Seq[ParametricType], parentCls: Seq[Type], content: Seq[ClassContent]) extends ModuleContent with TName.Target:
  def isCaseClass: Boolean = annos.exists(_.isInstanceOf[CaseClassAnno])

  val contentMap: Map[Name, Seq[ClassContent]] = content.groupBy {
    case field: FieldDef => field.name
    case method: MethodDef => method.name
    case _: ConstructorDef => name
  }

  def fields: Seq[FieldDef] = content.collect { case f: FieldDef => f }
  def methods: Seq[MethodDef] = content.collect { case f: MethodDef => f }
  def constructors: Seq[ConstructorDef] = content.collect { case f: ConstructorDef => f }

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val contentS = if (content.isEmpty) "" else
      "\n" + content.map(_.prettyprint(indent + "\t")).mkString("\n\n")
    val parentClassesS = if (parentCls.nonEmpty) {
      val tailS = if (parentCls.tail.nonEmpty) "with " + parentCls.tail.mkString("with ") else ""
      s"""extends ${parentCls.head} $tailS"""
    } else
      s""
    s"""$annoPrefix$indent${visS}class $name $parentClassesS {$contentS\n$indent}""".stripMargin
  }


trait ClassContent extends SourceLocation with Annotations:
  def vis: Option[Visibility]
  def prettyprint(implicit indent: String): String
  override def toString: String = prettyprint("")

case class FieldDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, typ: Type, body: Option[Expression], immutable: Boolean) extends ClassContent with Resolvable[ClassDef]:
  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val expr = if (body.isEmpty) "" else s" = ${body.get}"
    val prefix = if (immutable) "val " else "var "
    s"$indent$visS$prefix$name: ${typ.prettyprint}$expr"
  }

case class MethodDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, tyVars: Seq[ParametricType], params: Seq[Param], outType: Type, body: Seq[Statement]) extends ClassContent with Resolvable[ClassDef]:
  def signature: Seq[Type] = params.map(_.typ) :+ outType

  override def prettyprint(implicit indent: String): String = {
    val tyS = if (tyVars.isEmpty) "" else tyVars.mkString("[", ", ", "]")
    val visS = if (vis.contains(Private)) "private " else ""
    val paramsS = params.map(_.prettyprint).mkString("(" , ", ", ")")
    val bodyS = body.map(_.prettyprint(indent + "\t")).mkString("\n")
    val outS = outType.prettyprint
    s"""$annoPrefix$indent${visS}def $name$tyS$paramsS: $outS = {
       |$bodyS
       |$indent}""".stripMargin
  }

case class ConstructorDef(annos: Seq[Annotation], vis: Option[Visibility], params: Seq[Param], body: Seq[Statement]) extends ClassContent with Resolvable[ClassDef]:
  override def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val paramsS = params.map(_.prettyprint).mkString(", ")
    val bodyS = body.map(_.prettyprint(indent + "\t")).mkString("\n")
    s"""$annoPrefix$indent${visS}this($paramsS) = {
       |$bodyS
       |$indent}""".stripMargin
  }