package inca.frontend.core

import inca.frontend.parser.SourceLocation
import inca.util.Meta.Scala

case class Module(name: Name, imports: Seq[Name], funs: Seq[PatternFunction], stats: Seq[Scala[meta.Stat]]) extends SourceLocation {
  def allVars: Map[Name, Option[Type]] = funs.flatMap(_.allVars).toMap

  def usedModuleNames: Seq[Name] = name +: imports

  def usedFunNames: Seq[Name] = funs.map(_.name)

  def prettyprint(implicit indent: String): String = {
    val importsS = if (imports.isEmpty) "" else
      "\n" + indent + imports.map("import " + _).mkString("\n" + indent)
    val funsS = if (funs.isEmpty) "" else
      "\n" + funs.map(_.prettyprint).mkString("\n")
    val statsS = if (stats.isEmpty) "" else
      "\n" + indent + stats.map("scala " + _.tree.syntax).mkString("\n" + indent)
    s"${indent}module $name$importsS$funsS$statsS".stripMargin
  }

  override def toString: String = prettyprint("")
}