package inca.embedded3

/*

Boring state of the art: unparametric (monomorphic?) lanuages
PSystemScala
DatalogScala
SouffleCpp
FunScala
OOScala


Interesting new: parametric languages, their embeddings, and relations
PSystemScala
SouffleCpp
Datalog[L]
Fun[L]
OO[L]
Constr[L]

given
- Datalog[Scala]=>PsystemScala
- Fun[Scala] => Datalog[Scala]
- SouffleCpp => Datalog[Scala]
- Datalog[Scala] => (Relation,PartialMatch) => Table

new
- unparametric languages: only chaining of semantics, no nesting of languages
- mixing compilation and interpretation: PL[L]
    - interp PL, interp L   => full interpretation
    - interp PL, compile L  => pre-compilation, then interpretation (staging?)
    - compile PL, interp L  => compilation with delayed interpretation (when compile time of L is a problem)
    - compile PL, compile L => full compilation
- composition operators
    - not fmap: PL[L1] => PL[L2]
- how to interpret a parametric language?
- how to compile a parametric language?
  - where to put top-level

- language container, compilation container that stores the result of compilation, syntax tree only references things from he container
  - Scala: toplevel defs,
 */

trait Embedding[L] {
  type Ref
}
trait ScalaEmbedding {
  val toplevelDefs: Seq[meta.Stat]
  trait Ref
  case class Literal(v: meta.Lit)
}

trait Lang
object Scala extends Lang
object Cpp extends Lang
object Souffle extends Lang

trait ParametricLang extends Lang {
  type L <: Lang
  val l: L
  val embedding: Embedding[L]
}

trait Datalog extends ParametricLang {
  trait Rule
  trait Term
  trait Atom
  case class Var(name: String) extends Atom
}

trait DatalogSimple extends Datalog {
  case class Constant(i: Int) extends Atom
}

trait DatalogEmbeddingInterface extends Lang {
  type Literal // <: embedding.Ref
}
trait RealDatalog[L <: DatalogEmbeddingInterface] extends Datalog {
  type L <: DatalogEmbeddingInterface
  case class Constant(lit: l.Literal) extends Atom

  def fmap[L2](f: Any => Any)(a: Atom) = a match {
    case Constant(lit) => Constant(f(lit))
    case Var(name) => ???
    case _ => ???
  }
}

/*
  - meta language, in which interpreters and compilers are written (may call other languages under hood)
     <= always Scala
  - object language, which we design, model, interpret, and compile
     <= function, oo, Datalog, Souffle, cpp, scala

  - an object language can be a host language, in which another object language is embedded

 */

// Datalog[Scala]
// Datalog[Functional[Scala]] => Datalog[Datalog[Scala]]

// Scala => ScalaEv
// Datalog[Scala] => PSystem[ScalaC]
// Datalog[Scala] => Datalog[ScalaEv] => PSystem[ScalaEv]

// metaAsObjectLang: (Interp[L], prog: L.Prog) => Scala

// CppInterp: Cpp => CppEv

// PSystemInterp: Psystem[Scala] => IncDB

// OO[OO[Scala] => OO[Scala]

//trait Compiler[S <: Lang, T <: Lang] {
//  val s: S
//  val t: T
//  def compile(src: s.Prog): t.Prog
//}
//
//object compose {
//  // HL[S] => HL[T]
//  def fmap[S <: Lang, T <: Lang, HL[_] <: HostLang[_]](c: Compiler[S, T]): Compiler[HL[S], HL[T]] =
//    ???
//
//}
//
//trait HostLang[FL <: Lang] extends Lang {
//  type FL <: Lang
//}
//
//trait Interp[L <: Lang] {
//  type Value
//  val l: L
//  def interp(prog: l.Prog): Value
//}

//trait HostLangInterp[L <: HostLang] extends Interp[L] {
//  val flInterp: Interp[l.FL]
//}
//
//object ScalaL extends Lang {
//  override type Prog = Any
//}
//object ScalaInterp extends Interp[ScalaL.type] {
//  type Value = Any
//  val l = ScalaL
//  override def interp(prog: l.Prog): Int = ???
//}
//
//trait DatalogEmbedded {
//  type Toplevel
//  type Type
//  type Literal
//  type Callable
//}
//
//trait Datalog extends HostLang {
//  val embedded: DatalogEmbedded
//  override type Prog = Exp
//  trait Exp
//  case class EmbeddedLit(v: embedded.Literal)
//  case class SingleSet(i: Int)
//}
//
//object ScalaInDatalog extends DatalogEmbedded {
//  override type Toplevel = meta.Stat
//  override type Type = meta.Type
//  override type Literal = meta.Lit
//  override type Callable = meta.Term.Name
//}
//
//object DatalogWithScala extends Datalog {
//  override type FL = ScalaL.type
//  override val embedded: DatalogEmbedded = ScalaInDatalog
//}
//
//trait DatalogInterp extends HostLangInterp[Datalog] {
//  trait Value
//  case class EmbeddedValue(v: flInterp.Value) extends Value
//  case class SetValue(s: Set[Int]) extends Value
//
//  def interp(prog: l.Prog): Value = prog match {
//    case l.EmbeddedLit(v) => EmbeddedValue(flInterp.interp(v))
//    case l.SingleSet(i) => SetValue(Set(i))
//  }
//}
//
//object DatalogWithScalaInterp extends DatalogInterp {
//  override val l: Datalog = DatalogWithScala
//  val ev: ScalaL.type =:= l.FL = ???
//  override val flInterp: Interp[l.FL] = ScalaInterp
//}
