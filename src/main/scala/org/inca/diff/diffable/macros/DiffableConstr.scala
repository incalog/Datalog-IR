package org.inca.diff.diffable.macros

import org.inca.diff.diffable.Diffable.{ApplyDiffFailed, GreatestCommonPrefixFailed}
import org.inca.diff.diffable.{Change, ChangeHole, DiffData, Diffable, DiffableForeach, DiffableOracle, MetaVar, MetaVarHole}

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.whitebox

@compileTimeOnly("Scala 2.13 and compiler flag -Ymacro-annotations required")
class diffableConstr extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DiffableConstrImpl.impl
}
object DiffableConstrImpl {
  def impl(c: whitebox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    val tDiffable = symbolOf[Diffable[_]]
    val tyDiffable = typeOf[Diffable[_]]
    val tMetaVar = symbolOf[MetaVar[_]]
    val tMetaVarHole = symbolOf[MetaVarHole[_]]
    val tChangeHole = symbolOf[ChangeHole[_]]
    val oChangeHole = symbolOf[ChangeHole.type].asClass.module
    val tContext = symbolOf[DiffData.Context[_]]
    val tPatch = symbolOf[DiffData.Patch[_]]
    val tChange = symbolOf[Change[_]]
    val tSet = symbolOf[Set[_]]
    val oSet = symbolOf[Set.type].asClass.module
    val tArray = symbolOf[Array[_]]
    val tByte = symbolOf[Byte]
    val tDiffableOracle = symbolOf[DiffableOracle]
    val oApplyDiffFailed = symbolOf[ApplyDiffFailed.type].asClass.module
    val oGreatestCommonPrefixFailed = symbolOf[GreatestCommonPrefixFailed.type].asClass.module
    val tDiffableForeach = symbolOf[DiffableForeach]

    val q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = annottees.head

    val tThis = tq"$tpname[..$tparams]"
    val oThis = TermName(tpname.toString)
    val tParent = parents.head
    val oParent = TermName(tParent.toString)

    val params = paramss.flatMap(params => params.asInstanceOf[Seq[Tree]].map{
      case q"$_ val $name: $tp = $_" => (name, treeType(c)(tp))
    })

    def mapParams(diffable: TermName => Tree, nonDiffable: TermName => Tree): Seq[Tree] =
      for ((p, tp) <- params)
        yield if (tp <:< tyDiffable) diffable(p) else nonDiffable(p)

    def mapDiffableParams(diffable: TermName => Tree): Seq[Tree] =
      for ((p, tp) <- params if tp <:< tyDiffable)
        yield diffable(p)

    def mapNonDiffableParams(nonDiffable: TermName => Tree): Seq[Tree] =
      for ((p, tp) <- params if !(tp <:< tyDiffable))
        yield nonDiffable(p)

    def nondiffableCond(other: Tree) = {
      reduce(mapNonDiffableParams(p => q"this.$p == $other.$p"), "$amp$amp", q"")
    }

    def reduce(seq: Seq[Tree], op: String, nil: Tree) =
      if (seq.isEmpty)
        nil
      else
        seq.reduce((l,r) => q"$l ${TermName(op)} $r")

    val res = q"""
      $mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self =>
        ..$stats

        override lazy val $$hash: $tArray[$tByte] = {
          val digest = mkDigest
          digest.update(this.getClass.getCanonicalName.getBytes)
            ..${mapParams(
              p => q"digest.update(this.$p.$$hash)",
              p => q"hashNonDiffable(this.$p, digest)"
            )}
          digest.digest()
        }

        override lazy val freevars: $tSet[$tMetaVar[_]] =
          ${reduce(mapDiffableParams(p => q"this.$p.freevars"), "$plus$plus", q"$oSet()")}

        override def extract(oracle: $tDiffableOracle): $tContext[$tParent] = oracle.predict[$tParent](this) match {
          case Some(i) => $oParent.VarHole(i)
          case _ =>
            $oThis(..${mapParams(
              p => q"this.$p.extract(oracle)",
              p => q"this.$p"
            )})
        }

        override def foreach(f: $tDiffableForeach): Unit = {
          f(this)
          ..${mapDiffableParams(p => q"this.$p.foreach(f)")}
        }

        override def retainMetaVars(vs: Set[MetaVar[_]], other: $tContext[$tParent]): $tContext[$tParent] = other match {
          case other: $tpname if ${nondiffableCond(q"other")} =>
            $oThis(..${mapParams(
              p => q"this.$p.retainMetaVars(vs, other.$p)",
              p => q"this.$p"
            )})
        }

        override def greatestCommonClosedPrefix(other: $tContext[$tParent]): $tPatch[$tParent] = other match {
          case other: $tpname if ${nondiffableCond(q"other")} =>
            try {
              $oThis(..${mapParams(
                p => q"this.$p.greatestCommonClosedPrefix(other.$p)",
                p => q"this.$p"
              )})
            } catch {
              case $oGreatestCommonPrefixFailed() => $oChangeHole.mkClosedChangeHole(this, other, $oParent.ChangeHole.apply)
            }
          case _ => $oChangeHole.mkClosedChangeHole(this, other, $oParent.ChangeHole.apply)
        }

        override def applyPatchTo(other: $tParent): $tParent = other match {
          case other: $tpname if ${nondiffableCond(q"other")} =>
            $oThis(..${mapParams(
              p => q"this.$p.applyPatchTo(other.$p)",
              p => q"this.$p"
            )})
          case _ => throw $oApplyDiffFailed()
        }

        override def matchTree(other: $tParent): Unit = other match {
          case other: $tpname if ${nondiffableCond(q"other")} =>
            ..${mapDiffableParams(p => q"this.$p.matchTree(other.$p)")}
          case _ => throw $oApplyDiffFailed()
        }

        override def buildTree(): $tParent =
          $oThis(..${mapParams(
            p => q"this.$p.buildTree()",
            p => q"this.$p"
          )})

      }
     """

//    println(res)

    res
  }

  def treeType(c: whitebox.Context)(tp: Any) = {
    import c.universe._
    val t = q"{type T = ${tp.asInstanceOf[c.Tree]}; ()}"
    val tt = c.typecheck(t)
    val q"{type T = $ttp; ()}" = tt
    ttp.tpe
  }
}
