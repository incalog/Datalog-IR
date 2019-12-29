package org.inca.diff.macros

import org.inca.diff.Diffable

import scala.reflect.macros.whitebox

object Util {
  def treeType(c: whitebox.Context)(tp: Any) = {
    import c.universe._
    val t = q"{type T = ${tp.asInstanceOf[c.Tree]}; ()}"
    val tt = c.typecheck(t)
    val q"{type T = $ttp; ()}" = tt
    ttp.tpe
  }

  def mapParams[A](c: whitebox.Context)(
    paramss: Seq[Seq[c.Tree]],
    splitType: c.Type,
    sub: c.TermName => A,
    notSub: c.TermName => A,
    option: c.TermName => A,
    seq: c.TermName => A
  ): Seq[A] = {
    import c.universe._
    for (ps <- paramss;
         q"$_ val $p: $tp = $_" <- ps;
         ty = Util.treeType(c)(tp))
      yield
        if (ty <:< splitType)
          sub(p)
        else if (ty <:< appliedType(typeOf[Option[_]].typeConstructor, splitType))
          option(p)
        else if (ty <:< appliedType(typeOf[Seq[_]].typeConstructor, splitType))
          seq(p)
        else
          notSub(p)
  }

  def isDiffableSubtype(c: whitebox.Context)(tp: c.Tree): Boolean = {
    import c.universe._
    tp match {
      case q"${tq"$name[..$targs]"}(...$_)" => treeType(c)(tq"$name[..$targs]") <:< typeOf[Diffable[_]]
      case tq"$name[..$targs]" => treeType(c)(tq"$name[..$targs]") <:< typeOf[Diffable[_]]
      case _ => false
    }
  }
}
