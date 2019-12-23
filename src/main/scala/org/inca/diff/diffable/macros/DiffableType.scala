package org.inca.diff.diffable.macros

import org.inca.diff.diffable.{Change, ChangeHole, DiffData, Diffable, MetaVar, MetaVarHole}

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

@compileTimeOnly("Scala 2.13 and compiler flag -Ymacro-annotations required")
class diffableType extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DiffableTypeImpl.impl
}
object DiffableTypeImpl {
  def impl(c: whitebox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    val tDiffable = symbolOf[Diffable[_]]
    val tMetaVar = symbolOf[MetaVar[_]]
    val tMetaVarHole = symbolOf[MetaVarHole[_]]
    val tChangeHole = symbolOf[ChangeHole[_]]
    val tContext = symbolOf[DiffData.Context[_]]
    val tPatch = symbolOf[DiffData.Patch[_]]
    val tChange = symbolOf[Change[_]]

    val q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = annottees.head

    val tp = tq"$tpname[..$tparams]"
    val obj = TermName(tpname.toString)

    val diffableTrait =
      q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents with $tDiffable[$tpname] { $self => ..$stats }"

    val varHole =
      q"""
        case class VarHole[..$tparams](mv: $tMetaVar[$tp]) extends $tp with $tMetaVarHole[$tp] {
          override def lifted: $tContext[$tp] = this
          override def mkChangeHole: $tChange[$tp] => $tPatch[$tp] = $obj.ChangeHole.apply
        }
       """
    val changeHole =
      q"""
        case class ChangeHole[..$tparams](change: $tChange[$tp]) extends $tp with $tChangeHole[$tp] {
          override def lifted: $tp = this
        }
       """
    val companion =
      q"""
        object $obj {
          $varHole
          $changeHole
        }
       """

    q"{$diffableTrait; $companion}"
  }
}
