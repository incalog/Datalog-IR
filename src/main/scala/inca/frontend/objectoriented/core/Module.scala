package inca.frontend.objectoriented.core

import inca.compiler.SourceLocation
import inca.frontend.util.Resolvable

case class Module(name: Name, imports: Seq[Import], content: Seq[Content])
  extends SourceLocation with Import.Target {

  def usedModuleNames: Seq[Name] = name +: imports.map(_.name)

  def usedDefNames: Seq[Name] = content.flatMap {
    case fun: FunctionDef => Seq(fun.name) // this is only relevant if we allow functions inside the module e.g the main function
    case clazz: ClassDef => Seq(clazz.name)
    // case var: VariableDef => // We do not support module level variables
  }

  def prettyprint(implicit indent: String): String = {
    val importsS = if (imports.isEmpty) "" else
      "\n" + imports.map(_.prettyprint).mkString("\n")
    val contentS = if (content.isEmpty) "" else
      "\n" + content.map(_.prettyprint("\t")).mkString("\n")
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




trait Content extends SourceLocation with Annotations {
  def vis: Option[Visibility]
  def prettyprint(implicit indent: String): String
  def calls: Set[Call]
}

// TODO: Add attributes and methods to ClassDef
case class ClassDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, parentClassNames: Seq[Name], content: Seq[Content]) extends Content {
  override def calls: Set[Call] = Set() // TODO: All calls of every expression inside a function ?

  def usedDefNames: Seq[Name] = content.flatMap {
    case fun: FunctionDef => Seq(fun.name) // this is only relevant if we allow functions inside the module e.g the main function
    case attr: VariableDef => Seq(attr.name)
    //case clazz: ClassDef => // We do not allow inner classes for now
  }

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val contentS = if (content.isEmpty) "" else
      "\n" + content.map(_.prettyprint(indent+"\t")).mkString("\n\n")
    s"""$annoPrefix$indent${visS}class $name(${parentClassNames.mkString(", ")}) {$contentS\n$indent}""".stripMargin
  }
}

case class VariableDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, typ: Type, body: Option[Expression]) extends  Content with Var.Target {
  def vars: Map[Name, Option[Type]] = Map(name -> Some(typ))

  def prettyprint(implicit indent: String): String =
    s"${indent}var $name: ${typ.prettyprint}"

  override def calls: Set[Call] = Set()
}

case class FunctionDef(annos: Seq[Annotation], vis: Option[Visibility], name: Name, params: Seq[Param], outType: Type, body: Expression)
  extends Content with Var.Target {

  lazy val boundNames: Seq[Name] = params.map(_.name)

  val funType: TFun = TFun(params.map(_.typ), outType)

  lazy val vars: Map[Name, Option[Type]] = body.vars ++ params.flatMap(_.vars)

  def freevars: Seq[Var] = body.freevars.filter(v => !v.target.contains(this) && !boundNames.contains(v.name))

  lazy val calls: Set[Call] = body.calls

  def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val paramsS = params.map(_.prettyprint).mkString(", ")
    val outS = outType.prettyprint
    s"""$annoPrefix$indent${visS}def $name($paramsS): $outS {
       |$indent\t${body.prettyprint(indent + "\t")}
       |$indent}""".stripMargin
  }
}

case class Param(name: Name, typ: Type) extends SourceLocation with Var.Target {
  def vars: Map[Name, Option[Type]] = Map(name -> Some(typ))

  def prettyprint: String = s"$name: ${typ.prettyprint}"
}