package inca.ir.extensions
import scala.language.implicitConversions
import inca.ir.{Atom, BaseIR, Language, ModuleEntry, Name, Term, Type, Var}

// TODO: We need a typechecker for this
// TODO Discuss: Do we need records ? We currently only support sum types

case class TData(name: Name) extends Type:
  override def toString: String = s"$name"

case class Case(name: Name, args: Seq[Type]):
  override def toString: String = s"""$name ${args.mkString(" ")}"""

case class DataDefinition(name: Name, cases: Seq[Case]) extends ModuleEntry:
  //private lazy val caseMap: Map[Name, Case] = cases.map(c => c.name -> c).toMap
  //def getCaseByName(name: Name): Option[Case] = caseMap.get(name)
  override def toString: String = s"""data $name = ${cases.mkString("|")}"""

case class Construct(name: Name, caseName: Name, data: Seq[Term]) extends Term
case class Match(name: Term, caseName: Name, caseVars: Seq[Var]) extends Atom


/* We could write code such as:
data LinkedList = Empty | Cons Int LinkedList

R(x: TInt, y: TInt, z: TVariant("LinkedList")) :-
  a = Construct("LinkedList", "Empty", Seq()) // -> We might compile this to a scala object
  z = Construct("LinkedList", "Cons", Seq(4, a))

  Match(z, "Cons", Seq(n, t))

  // 1. Use typechecker to find out which case we are matching
  // 2. Scala could look like this:
  // ((z: Any) => z.asInstanceOf["LinkedList"].caseName == "Cons)(z)
  // n = ((z: Any) => z.asInstanceOf["LinkedList"].data(0))(z)
  // t = ((z: Any) => z.asInstanceOf["LinkedList"].data(1))(z)
*/


trait DataIR extends BaseIR:
  override val name: String = "Data"
  override def language: Language = super.language + new DataIR {}
  override def requires: Language = Language()
