package inca.frontend.core

import inca.frontend.parser.SourceLocation
import inca.frontend.typechecker.Resolvable
import inca.util.Meta.Scala

case class Module(name: Name, imports: Seq[Import], content: Seq[ModuleContent])
  extends SourceLocation with Import.Target {

  def allVars: Map[Name, Option[Type]] = content.flatMap {
    case fun: PatternFunction => fun.allVars
    case _: ScalaStatement => Map()
  }.toMap

  def usedModuleNames: Seq[Name] = name +: imports.map(_.name)

  def usedFunNames: Seq[Name] = content.flatMap {
    case fun: PatternFunction => Some(fun.name)
    case _: ScalaStatement => None
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


trait ModuleContent {
  def vis: Option[Visibility]
  def prettyprint(implicit indent: String): String
}

class ScalaStatement(stat: meta.Stat) extends Scala[meta.Stat](stat) with ModuleContent {
  override def vis: Option[Visibility] = None // todo: analyze scala code to retrieve its visibility
  override def prettyprint(implicit indent: String): String = indent + this.toString
}
object ScalaStatement {
  def apply(stat: meta.Stat): ScalaStatement = new ScalaStatement(stat)
}