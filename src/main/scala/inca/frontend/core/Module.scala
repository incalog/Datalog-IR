package inca.frontend.core

import inca.frontend.parser.SourceLocation
import inca.frontend.typechecker.Resolvable
import inca.util.Meta.Scala

case class Module(name: Name, imports: Seq[Import], funs: Seq[PatternFunction], stats: Seq[Scala[meta.Stat]])
  extends SourceLocation with Import.Target {

  def allVars: Map[Name, Option[Type]] = funs.flatMap(_.allVars).toMap

  def usedModuleNames: Seq[Name] = name +: imports.map(_.name)

  def usedFunNames: Seq[Name] = funs.map(_.name)

  def prettyprint(implicit indent: String): String = {
    val importsS = if (imports.isEmpty) "" else
      "\n" + imports.map(_.prettyprint).mkString("\n")
    val funsS = if (funs.isEmpty) "" else
      "\n" + funs.map(_.prettyprint).mkString("\n")
    val statsS = if (stats.isEmpty) "" else
      "\n" + indent + stats.map("scala " + _.tree.syntax).mkString("\n" + indent)
    s"${indent}module $name$importsS$funsS$statsS".stripMargin
  }

  override def toString: String = prettyprint("")
}

case class Import(name: Name) extends SourceLocation with Resolvable[Import.Target] {
  def prettyprint(implicit indent: String): String = s"${indent}import $name"
}
object Import {
  trait Target
}