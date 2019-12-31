package org.inca.diff.macros

import org.inca.diff.{Change, ChangeHole, DiffData, Diffable, MetaVar, MetaVarHole}

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.whitebox

//@compileTimeOnly("Scala 2.13 and compiler flag -Ymacro-annotations required")
class diffableType extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DiffableTypeImpl.impl
}
object DiffableTypeImpl {
  def impl(c: whitebox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    val tDiffable = symbolOf[Diffable[_]]

    annottees.head match {
      case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>

        val tp = tq"$tpname[..$tparams]"
        val obj = TermName(tpname.toString)
        val (varHole, changeHole) = makeHoles(c)(tparams, stats, tp, obj)

        val diffableTrait =
          q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents with $tDiffable[$tpname] { $self => ..$stats }"


        val companion: c.Tree =
          if (annottees.tail.isEmpty) {
            q"""
          object $obj {
            $varHole
            $changeHole
          }
        """
          } else {
            val q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" = annottees.tail.head
            val diffableBody = body.map {
              case q"$mods class $subname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
                val Modifiers(flags, privs, annos) = mods
                val newmods = Modifiers(flags, privs, annos :+ q"new _root_.org.inca.diff.macros.diffableConstr()")
                if (parents.exists(_.toString == tpname.toString))
                  q"$newmods class $subname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }"
                else
                  q"$mods class $subname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }"

              case q"$mods object $subname extends { ..$earlydefns } with ..$parents { $self => ..$body }" =>
                val Modifiers(flags, privs, annos) = mods
                val newmods = Modifiers(flags, privs, annos :+ q"new _root_.org.inca.diff.macros.diffableConstr()")
                if (parents.exists(_.toString == tpname.toString))
                  q"$newmods object $subname extends { ..$earlydefns } with ..$parents { $self => ..$body }"
                else
                  q"$mods object $subname extends { ..$earlydefns } with ..$parents { $self => ..$body }"
            }
            q"""$mods object $tname extends { ..$earlydefns } with ..$parents { $self =>
              ..$diffableBody
              $varHole
              $changeHole
            }
         """
          }

//        println(q"{$diffableTrait; $companion}")

        q"{$diffableTrait; $companion}"


      case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        val Modifiers(flags, privs, annos) = mods
        val newmods = Modifiers(flags, privs, annos :+ q"new _root_.org.inca.diff.macros.diffableConstr()")
        val diffableClass =
          q"$newmods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }"

        if (annottees.tail.isEmpty)
          diffableClass
        else
          q"{$diffableClass; ..${annottees.tail}}"
    }


  }

  private def makeHoles(c: whitebox.Context)(tparams: Seq[c.Tree], stats: Seq[c.Tree], tp: c.Tree, obj: c.TermName) = {
    import c.universe._
    val tMetaVar = symbolOf[MetaVar[_]]
    val tMetaVarHole = symbolOf[MetaVarHole[_]]
    val tChangeHole = symbolOf[ChangeHole[_]]
    val tContext = symbolOf[DiffData.Context[_]]
    val tPatch = symbolOf[DiffData.Patch[_]]
    val tChange = symbolOf[Change[_]]
    val tUnsupportedOperationException = symbolOf[UnsupportedOperationException]

    val abstractMethodImpls = stats.flatMap {
      case q"${Modifiers(flags, privs, annos)} def $tname[..$tparams](...$paramss): $tpt = $expr" if expr.isEmpty =>
        Some(q"override def $tname[..$tparams](...$paramss): $tpt = throw new $tUnsupportedOperationException()")
      case _ => None
    }

    val abstractValImpls = stats.flatMap {
      case q"$mods val $tname: $tpt = $expr" if expr.isEmpty =>
        Some(q"$mods val $tname: $tpt = null.asInstanceOf[$tpt]")
      case q"$mods var $tname: $tpt = $expr" if expr.isEmpty =>
        Some(q"$mods var $tname: $tpt = null.asInstanceOf[$tpt]")
      case _ => None
    }

    val varHole =
      q"""
        class VarHole[..$tparams](val mv: $tMetaVar[$tp]) extends $tp with $tMetaVarHole[$tp] {
          override def lifted: $tContext[$tp] = this
          override def mkChangeHole: $tChange[$tp] => $tPatch[$tp] = x=> new $obj.ChangeHole(x)
          ..$abstractValImpls
          ..$abstractMethodImpls
        }
       """
    val changeHole =
      q"""
        class ChangeHole[..$tparams](val change: $tChange[$tp]) extends $tp with $tChangeHole[$tp] {
          override def lifted: $tp = this
          ..$abstractValImpls
          ..$abstractMethodImpls
        }
       """
    (varHole, changeHole)
  }
}
