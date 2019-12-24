package org.inca.diff.diffable.macros

import org.inca.diff.HasCryptoHash

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.whitebox

@compileTimeOnly("Scala 2.13 and compiler flag -Ymacro-annotations required")
class cryptoHash extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro CryptoHashImpl.impl
}

object CryptoHashImpl {
  def impl(c: whitebox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    val tHasCryptoHash = symbolOf[HasCryptoHash]
    val tyHasCryptoHash = typeOf[HasCryptoHash]
    val tArray = symbolOf[Array[_]]
    val tByte = symbolOf[Byte]
    val oBigInt = symbolOf[BigInt.type].asClass.module

    annottees.head match {
      case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents with $tHasCryptoHash { $self => ..$stats }"

      case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        q"""
          $mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents with $tHasCryptoHash { $self =>
            ..$stats

            override lazy val $$hash: $tArray[$tByte] = {
              val digest = mkDigest
              digest.update(this.getClass.getCanonicalName.getBytes)
              ..${Util.mapParams(c)(paramss, tyHasCryptoHash,
                p => q"digest.update(this.$p.$$hash)",
                p => q"hashNonDiffable(this.$p, digest)",
                p => q"{if ($p.isEmpty) digest.update(0:$tByte) else {digest.update(1:$tByte); digest.update($p.get)}}",
                p => q"{digest.update($oBigInt($p.size).toByteArray); $p.foreach((x: $tHasCryptoHash) => digest.update(x.$$hash))}"
              )}
              digest.digest()
            }
          }
         """

      case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" =>
        q"""
          $mods object $tname extends { ..$earlydefns } with ..$parents with $tHasCryptoHash  { $self =>
            ..$body

            override lazy val $$hash: $tArray[$tByte] = {
              val digest = mkDigest
              digest.update(this.getClass.getCanonicalName.getBytes)
              digest.digest()
            }
          }
         """
    }
  }
}