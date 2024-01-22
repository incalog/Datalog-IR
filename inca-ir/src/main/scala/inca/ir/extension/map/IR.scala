package inca.ir.extension.map

import inca.ir.{Atom, BaseIR, Language, Name, Param, Term, Type, Var}

trait IR extends BaseIR:
  override val name: String = "Map"
  override def language: Language = super.language + IR
  override def requires: Language = Language()

object IR extends IR {}

case class TMap(k: Type, v: Type) extends Type:
  override def toString: String = s"Map[$k, $v]"

case class MapLit(ts: Seq[(Term, Term)]) extends Term:
  override def toString: String = s"Map(${ts.map[String]{(t1, t2) => s"$t1 -> $t2"}.mkString(", ")})"
  override def vars: Seq[Var] = ts.flatMap[Var]((t1, t2) => t1.vars ++ t2.vars)
object MapLit:
  def from(ts: (Term, Term)*): MapLit = new MapLit(ts)
  def empty: MapLit = new MapLit(Seq())

case class MapFrom(name: Name) extends Term:
  override def toString: String = s"Map.from($name)"
  override def vars: Seq[Var] = Seq()

case class MapFun(params: Seq[Param], valTerm: Term) extends Term:
  lazy val names: Set[Name] = params.map(_._1).toSet
  override def vars: Seq[Var] = valTerm.vars.filterNot(v => names.contains(v.name))
  override def toString: String =
    s"MapFun(${params.mkString(", ")} => $valTerm)"

case class MapPlus(map: Term, key: Term, value: Term) extends Term:
  override def toString: String = s"$map += $key -> $value"
  override def vars: Seq[Var] = map.vars ++ key.vars ++ value.vars

case class MapUnion(t1: Term, t2: Term) extends Term:
  override def toString: String = s"$t1 ∪ $t2"
  override def vars: Seq[Var] = t1.vars ++ t2.vars

case class MapConcat(t1: Term, t2: Term) extends Term:
  override def toString: String = s"$t1 ++ $t2"
  override def vars: Seq[Var] = t1.vars ++ t2.vars

case class MapComprehension(key: Term, value: Term, atoms: Seq[Atom]) extends Term:
  override def toString: String = s"{ $key -> $value | ${atoms.mkString(",")} }"
  override def vars: Seq[Var] = key.vars ++ value.vars ++ atoms.flatMap(atom => atom.vars)

case class MapLookUp(map: Term, key: Term) extends Term:
  override def toString: String = s"$map($key)"
  override def vars: Seq[Var] = map.vars ++ key.vars
  
case class MapContains(map: Term, key: Term) extends Atom:
  override def toString: String = s"$key in $map"
  override def vars: Seq[Var] = map.vars ++ key.vars