package inca.frontend_old.core.tree

import inca.frontend_old.parser.SourceLocation
import inca.frontend_old.typechecker.Resolvable
import inca.util.Meta.Scala

case class Module(name: Name, imports: Seq[Import], content: Seq[ModuleContent])
  extends SourceLocation with Import.Target {

  def allVars: Map[Name, Option[Type]] = content.flatMap {
    case fun: PatternFunction => fun.allVars
    case v: ValDef => v.exp.freeVars + (v.name -> v.getType)
    case _: ScalaModuleContent[_] => Map()
  }.toMap

  def usedModuleNames: Seq[Name] = name +: imports.map(_.name)

  def usedDefNames: Seq[Name] = content.flatMap {
    case fun: PatternFunction => Some(fun.name)
    case valDef: ValDef => Some(valDef.name)
    case _: ScalaModuleContent[_] => None
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


trait ModuleContent extends SourceLocation {
  def vis: Option[Visibility]
  def prettyprint(implicit indent: String): String
}

case class ValDef(vis: Option[Visibility], name: Name, typ: Option[Type], exp: Expression) extends ModuleContent with Var.Target {
  override def prettyprint(implicit indent: String): String = {
    val visS = if (vis.contains(Private)) "private " else ""
    val typS = typ match {
      case Some(ty) => s": ${ty.prettyprint}"
      case None => ""
    }
    s"$indent${visS}val $name$typS = ${exp.prettyprint}"
  }

  def getType: Option[Type] = typ.orElse(exp.typ)
}

case class ScalaModuleContent[T <: meta.Stat](t: Scala[T]) extends ModuleContent {
  def vis: Option[Visibility] = {
    def detVis(mods: List[meta.Mod]): Option[Visibility] = {
      if (mods.contains(meta.Mod.Protected))
        throw new IllegalArgumentException("Scala block definition cannot have protected visibility")

      if (mods.contains(meta.Mod.Private)) Some(Private)
      else None
    }

    t match {
      case valu: meta.Decl.Val => detVis(valu.mods)
      case vari: meta.Decl.Var => detVis(vari.mods)
      case defn: meta.Decl.Def => detVis(defn.mods)
      case typ: meta.Decl.Type => detVis(typ.mods)
      case tr: meta.Defn.Trait => detVis(tr.mods)
      case obj: meta.Defn.Object => detVis(obj.mods)
      case clazz: meta.Defn.Class => detVis(clazz.mods)
      case _ => None
    }
  }

  override def prettyprint(implicit indent: String): String = s"$indent`${t.syntax}`"
}
