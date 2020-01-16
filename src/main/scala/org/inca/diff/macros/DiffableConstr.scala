package org.inca.diff.macros

import org.inca.diff.{ApplyDiffFailed, Change, ChangeHole, DiffData, Diffable, DiffableForeach, DiffableOracle, GreatestCommonPrefixFailed, HasCryptoHash, MetaVar, MetaVarHole}

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import Util._
import org.inca.diff.changeset.ChangesetApi
import org.inca.diff.changeset.ChangesetApi._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

//@compileTimeOnly("Scala 2.13 and compiler flag -Ymacro-annotations required")
class diffableConstr extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DiffableConstrImpl.impl
}
object DiffableConstrImpl {
  def impl(c: whitebox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    val tDiffable = symbolOf[Diffable[_]]
    val oDiffable = tDiffable.companion
    val tyDiffable = typeOf[Diffable[_]]
    val tHasCryptoHash = symbolOf[HasCryptoHash]
    val tyHasCryptoHash = typeOf[HasCryptoHash]
    val tMetaVar = symbolOf[MetaVar[_]]
    val tMetaVarHole = symbolOf[MetaVarHole[_]]
    val tChange = symbolOf[Change[_]]
    val oChangeHole = symbolOf[ChangeHole.type].asClass.module
    val tContext = symbolOf[DiffData.Context[_]]
    val tPatch = symbolOf[DiffData.Patch[_]]
    val tSet = symbolOf[Set[_]]
    val oSet = symbolOf[Set.type].asClass.module
    val tArray = symbolOf[Array[_]]
    val tByte = symbolOf[Byte]
    val oBigInt = symbolOf[BigInt.type].asClass.module
    val tDiffableOracle = symbolOf[DiffableOracle]
    val oApplyDiffFailed = symbolOf[ApplyDiffFailed.type].asClass.module
    val oGreatestCommonPrefixFailed = symbolOf[GreatestCommonPrefixFailed.type].asClass.module
    val tGreatestCommonPrefixFailed = symbolOf[GreatestCommonPrefixFailed]
    val tDiffableForeach = symbolOf[DiffableForeach]
    val tInt = symbolOf[Int]
    val tArrayBuffer = symbolOf[ArrayBuffer[_]]
    val tBoolean = symbolOf[Boolean]
    val oSeq = symbolOf[Seq.type].asClass.module


    val tChangesetBuffer = symbolOf[ChangesetBuffer]
    val tNodeRef = symbolOf[NodeRef]
    val oLiteral = symbolOf[ChangesetApi.Literal[_]].companion
    val oNoneNode = symbolOf[NoneNode.type].asClass.module
    val oSomeNode = symbolOf[SomeNode].companion
    val oListNode = symbolOf[ListNode].companion

    val tLink = symbolOf[Link]
    val oNamedLink = symbolOf[NamedLink].companion
    val tListIndexLink = symbolOf[ListIndexLink]

    val oLoadNode = symbolOf[LoadNode].companion
    val oUnloadNode = symbolOf[UnloadNode].companion
    val oAttachNode = symbolOf[AttachNode].companion
    val oDettachNode = symbolOf[DetachNode].companion


    annottees.head match {
      case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        val oThis = TermName(tpname.toString)
        val tParent = parents.find(isDiffableSubtype(c)(_))
        val oParent = tParent.map(p => TermName(p.toString))
        val hasParent = if (oParent.isDefined) q"true" else q"false"
        val diffType = tParent.getOrElse(tq"$tpname")
        val newparents = if (oParent.isDefined) parents else parents :+ tq"$tDiffable[$tpname]"
        def mkChangeHole(ex: Tree) = if (oParent.isDefined) q"$oChangeHole.mkClosedChangeHole(this, other, x=>new ${oParent.get}.ChangeHole(x), $ex)" else q"throw $ex"

        def mapDiffableParams(diffable: TermName => Tree, option: TermName => Tree, seq: TermName => Tree): Seq[Tree] =
          mapParams(c)(paramss, tyDiffable, p => Some(diffable(p)), _ => None, p => Some(option(p)), p => Some(seq(p))).flatten

        def mapNonDiffableParams(nonDiffable: TermName => Tree): Seq[Tree] =
          mapParams(c)(paramss, tyDiffable, _ => None, p => Some(nonDiffable(p)), _ => None, _ => None).flatten

        def nondiffableCond(other: Tree) = {
          reduce(mapNonDiffableParams(p => q"this.$p == $other.$p"), "$amp$amp", q"")
        }

        def reduce(seq: Seq[Tree], op: String, nil: Tree) =
          if (seq.isEmpty)
            nil
          else
            seq.reduce((l, r) => q"$l ${TermName(op)} $r")

        val res =
          q"""
            $mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$newparents { $self =>
              ..$stats

              override lazy val $$hash: $tArray[$tByte] = {
                val digest = mkDigest
                digest.update(this.getClass.getCanonicalName.getBytes)
                  ..${
                mapParams(c)(paramss, tyHasCryptoHash,
                  p => q"digest.update(this.$p.$$hash)",
                  p => q"hashNonDiffable(this.$p, digest)",
                  p => q"{if ($p.isEmpty) digest.update(0:$tByte) else {digest.update(1:$tByte); digest.update($p.get.$$hash)}}",
                  p => q"{digest.update($oBigInt($p.size).toByteArray); $p.foreach((x: $tHasCryptoHash) => digest.update(x.$$hash))}"
                )
              }
                digest.digest()
              }

              override lazy val freevars: $tSet[$tMetaVar[_]] =
                ${
                reduce(
                  mapDiffableParams(
                    p => q"this.$p.freevars",
                    p => q"this.$p.map(_.freevars).getOrElse($oSet())",
                    p => q"this.$p.foldLeft($oSet[$tMetaVar[_]]())((vs, s) => vs union (s.freevars))"
                  ),
                  "$plus$plus",
                  q"$oSet()")
                }

              override def extract(oracle: $tDiffableOracle): $tContext[$diffType] = oracle.predict[$diffType](this) match {
                case Some(i) if $hasParent => ${if (oParent.isDefined) q"new ${oParent.get}.VarHole(i)" else q"null"}
                case _ =>
                  $oThis(..${
                mapParams(c)(paramss, tyDiffable,
                  p => q"this.$p.extract(oracle)",
                  p => q"this.$p",
                  p => q"this.$p.map(_.extract(oracle))",
                  p => q"this.$p.map(_.extract(oracle))"
                )
              })
              }

              override def foreach(f: $tDiffableForeach): Unit = {
                f(this)
                ..${
                mapDiffableParams(
                  p => q"this.$p.foreach(f)",
                  p => q"this.$p.foreach(_.foreach(f))",
                  p => q"this.$p.foreach(_.foreach(f))")
              }
              }

              override def retainMetaVars(vs: $tSet[$tMetaVar[_]], other: $tContext[$diffType]): $tContext[$diffType] = (other: @_root_.scala.unchecked) match {
                case other: $tpname if ${nondiffableCond(q"other")} =>
                  $oThis(..${
                mapParams(c)(paramss, tyDiffable,
                  p => q"this.$p.retainMetaVars(vs, other.$p)",
                  p => q"this.$p",
                  p => q"this.$p.map(_.retainMetaVars(vs, other.$p.get))",
                  p => q"this.$p.zip(other.$p).map(pp => pp._1.retainMetaVars(vs, pp._2))",
                )
              })
              }

              override def greatestCommonClosedPrefix(other: $tContext[$diffType]): $tPatch[$diffType] = other match {
                case other: $tpname if ${nondiffableCond(q"other")} =>
                  try {
                    $oThis(..${
                mapParams(c)(paramss, tyDiffable,
                  p => q"this.$p.greatestCommonClosedPrefix(other.$p)",
                  p => q"this.$p",
                  p => q"$oDiffable.greatestCommonClosedOptionPrefix(this.$p, other.$p)",
                  p => q"if (this.$p.size != other.$p.size) throw $oGreatestCommonPrefixFailed() else this.$p.zip(other.$p).map(pp => pp._1.greatestCommonClosedPrefix(pp._2))",
                )
              })
                  } catch {
                    case ex: $tGreatestCommonPrefixFailed => ${mkChangeHole(q"ex")}
                  }
                case _ => ${mkChangeHole(q"$oGreatestCommonPrefixFailed()")}
              }

              override def findMinimalClosedChanges(other: $tContext[$diffType], changes: $tArrayBuffer[$tChange[_]]): Unit = other match {
                case other: $tpname if ${nondiffableCond(q"other")} =>
                  val changesBefore = changes.size
                  try {
                    ..${
                mapParams(c)(paramss, tyDiffable,
                  p => q"this.$p.findMinimalClosedChanges(other.$p, changes)",
                  p => q"{}",
                  p => q"this.$p.foreach(_.findMinimalClosedChanges(other.$p.get, changes))",
                  p => q"this.$p.zip(other.$p).foreach(pp => pp._1.findMinimalClosedChanges(pp._2, changes))",
                )
              }
                  } catch {
                    case ex: $tGreatestCommonPrefixFailed =>
                      changes.remove(changesBefore, changes.size - changesBefore)
                      $oChangeHole.addClosedChange(this, other, changes, ex)
                  }
                case _ => $oChangeHole.addClosedChange(this, other, changes)
              }

              override def applyPatchTo(other: $diffType): $diffType = other match {
                case other: $tpname if ${nondiffableCond(q"other")} =>
                  $oThis(..${
                mapParams(c)(paramss, tyDiffable,
                  p => q"this.$p.applyPatchTo(other.$p)",
                  p => q"this.$p",
                  p => q"this.$p.map(_.applyPatchTo(other.$p.get))",
                  p => q"this.$p.zip(other.$p).map(pp => pp._1.applyPatchTo(pp._2))",
                )
              })
                case _ => throw $oApplyDiffFailed()
              }

              override def matchTree(other: $diffType): Unit = other match {
                case other: $tpname if ${nondiffableCond(q"other")} =>
                  ..${
                mapDiffableParams(
                  p => q"this.$p.matchTree(other.$p)",
                  p => q"this.$p.map(_.matchTree(other.$p.get))",
                  p => q"this.$p.zip(other.$p).map(pp => pp._1.matchTree(pp._2))",
                )
              }
                case _ => throw $oApplyDiffFailed()
              }

              override def buildTree(): $diffType =
                $oThis(..${
                mapParams(c)(paramss, tyDiffable,
                  p => q"this.$p.buildTree()",
                  p => q"this.$p",
                  p => q"this.$p.map(_.buildTree())",
                  p => q"this.$p.map(_.buildTree())",
                )
              })

              override def load(changes: $tChangesetBuffer, forceClone: $tBoolean): $tNodeRef = {
                val v = changes.freshVar()
                changes += $oLoadNode(v, this.getClass, $oSeq(
                  ..${mapParams(c)(paramss, tyDiffable,
                    p => q"$oNamedLink(${p.toString}) -> this.$p.load(changes, forceClone)",
                    p => q"$oNamedLink(${p.toString}) -> $oLiteral(this.$p)",
                    p => q"$oNamedLink(${p.toString}) -> (if (this.$p.isEmpty) $oNoneNode else $oSomeNode(this.$p.get.load(changes, forceClone)))",
                    p => q"$oNamedLink(${p.toString}) -> $oListNode(this.$p.map(_.load(changes, forceClone)))"
                  )}
                ))
                v
              }

              override def unload(changes: $tChangesetBuffer): Unit = {
                ..${mapParams(c)(paramss, tyDiffable,
                  p => q"this.$p.unload(changes)",
                  p => q"{}",
                  p => q"(if (this.$p.nonEmpty) this.$p.get.unload(changes))",
                  p => q"this.$p.foreach(_.unload(changes))"
                )}
                changes += $oUnloadNode(this.ref)
              }

              override def computeChangeset(parent: $tNodeRef, link: $tLink, other: $tContext[$diffType], changes: $tChangesetBuffer): Unit = other match {
                case other: $tpname if ${nondiffableCond(q"other")} =>
                  ..${mapParams(c)(paramss, tyDiffable,
                    p => q"this.$p.computeChangeset(this.ref, $oNamedLink(${p.toString}), other.$p, changes)",
                    p => q"{}",
                    p => q"$oDiffable.computeOptionChangeset(this.ref, $oNamedLink(${p.toString}), this.$p, other.$p, changes)",
                    p => q"$oDiffable.computeListChangeset(this.ref, $oNamedLink(${p.toString}), this.$p, other.$p, changes)",
                  )}
                case _ =>
                  this.unload(changes)
                  val newnode = other.load(changes, false)
                  changes += $oAttachNode(parent, link, newnode)
              }

              override def size: $tInt =
                1 + ${
                reduce(
                  mapDiffableParams(
                    p => q"this.$p.size",
                    p => q"this.$p.map(_.size).getOrElse(0)",
                    p => q"this.$p.foldLeft(0)((sum, s) => sum + s.size)"
                  ),
                  "$plus",
                  q"0")
              }

            }

          """

//        if (tpname.toString().contains("Num"))
//          println(res)

        if (annottees.tail.isEmpty)
          res
        else
          q"{$res; ..${annottees.tail}}"




      case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" =>
        val tParent = parents.find(isDiffableSubtype(c)(_))
        val oParent = tParent.map(p => TermName(p.toString))
        val hasParent = if (oParent.isDefined) q"true" else q"false"
        val newparents = if (oParent.isDefined) parents else parents :+ tq"$tDiffable[$tname.type]"
        val diffType = tParent.getOrElse(tq"$tname.type")
        val mkChangeHole = if (oParent.isDefined) q"$oChangeHole.mkClosedChangeHole(this, other, x=>new ${oParent.get}.ChangeHole(x))" else q"throw $oGreatestCommonPrefixFailed()"

        val res =
          q"""
            $mods object $tname extends { ..$earlydefns } with ..$newparents { $self =>
              ..$body

              override lazy val $$hash: $tArray[$tByte] = {
                val digest = mkDigest
                digest.update(this.getClass.getCanonicalName.getBytes)
                digest.digest()
              }

              override lazy val freevars: $tSet[$tMetaVar[_]] = $oSet()

              override def extract(oracle: $tDiffableOracle): $tContext[$diffType] = oracle.predict[$diffType](this) match {
                case Some(i) if $hasParent => ${if (oParent.isDefined) q"new ${oParent.get}.VarHole(i)" else q"null"}
                case _ => this
              }

              override def foreach(f: $tDiffableForeach): Unit = {
                f(this)
              }

              override def retainMetaVars(vs: $tSet[$tMetaVar[_]], other: $tContext[$diffType]): $tContext[$diffType] = (other: @_root_.scala.unchecked) match {
                case other: $tname.type => this
              }

              override def greatestCommonClosedPrefix(other: $tContext[$diffType]): $tPatch[$diffType] = other match {
                case other: $tname.type => this
                case _ => $mkChangeHole
              }

              override def findMinimalClosedChanges(other: $tContext[$diffType], changes: $tArrayBuffer[$tChange[_]]): Unit = other match {
                case other: $tname.type =>
                case _ => $oChangeHole.addClosedChange(this, other, changes)
              }

              override def applyPatchTo(other: $diffType): $diffType = other match {
                case other: $tname.type => this
                case _ => throw $oApplyDiffFailed()
              }

              override def matchTree(other: $diffType): Unit = other match {
                case other: $tname.type =>
                case _ => throw $oApplyDiffFailed()
              }

              override def buildTree(): $diffType =
                this

              override def load(changes: $tChangesetBuffer, forceClone: $tBoolean): $tNodeRef = {
                val v = changes.freshVar()
                changes += $oLoadNode(v, this.getClass, $oSeq())
                v
              }

              override def unload(changes: $tChangesetBuffer): Unit = {
                changes += $oUnloadNode(this.ref)
              }

              override def computeChangeset(parent: $tNodeRef, link: $tLink, other: $tContext[$diffType], changes: $tChangesetBuffer): Unit = other match {
                case other: $tname.type =>
                case _ =>
                  this.unload(changes)
                  val newnode = other.load(changes, false)
                  changes += $oAttachNode(parent, link, newnode)
              }

              override def size: $tInt = 1

            }
          """

//        println(res)

        if (annottees.tail.isEmpty)
          res
        else
          q"{$res; ..${annottees.tail}}"

    }
  }

}
