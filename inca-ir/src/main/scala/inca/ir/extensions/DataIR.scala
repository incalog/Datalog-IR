package inca.ir.extensions
import scala.language.implicitConversions
import inca.ir.{Atom, BaseIR, Body, Language, ModuleEntry, Name, Term, Type, Var}

case class TData(name: Name) extends Type:
  override def toString: String = s"$name"

case class CaseDefinition(name: Name, args: Seq[Type]):
  override def toString: String = s"""$name ${args.mkString(" ")}"""

case class DataDefinition(name: Name, cases: Seq[CaseDefinition]) extends ModuleEntry:
  //private lazy val caseMap: Map[Name, Case] = cases.map(c => c.name -> c).toMap
  //def getCaseByName(name: Name): Option[Case] = caseMap.get(name)
  override def toString: String = s"""data $name = ${cases.mkString("|")}"""

case class Construct(name: Name, data: Seq[Term]) extends Term

case class Binding(name: Name, vars: Seq[Var])
case class Case(binding: Binding, body: Body)
case class Match(data: Term, caseName: Name, caseVars: Seq[Var]) extends Atom


/* We could write code such as:
Outdated:

data LinkedList = Empty | Cons Int LinkedList

R(x: TInt, y: TInt, z: TData("LinkedList")) :-
  a = Construct("Empty", Seq()) // -> We might compile this to a scala object
  z = Construct("Cons", Seq(4, a))

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
