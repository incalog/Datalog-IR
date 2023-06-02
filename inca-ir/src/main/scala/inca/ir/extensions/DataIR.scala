package inca.ir.extensions
import scala.language.implicitConversions
import inca.ir.{Atom, BaseIR, Language, ModuleEntry, Name, Term, Type, Var}

// TODO: We need a typechecker for this
// TODO Discuss: Do we need records (after all we have a tuple IR) ?
//  Something like this:
/*case class TVariant(name: Name) extends Type:

case class Case(name: Name, args: Seq[Type]):
case class VariantDefinition(name: Name, cases: Seq[Case]) extends ModuleEntry:

case class Variant(name: Name, caseName: Name, data: Seq[Term]) extends Term
case class Match(variant: Term, caseName: Name, caseVars: Seq[Var]) extends Atom



case class TRecord(name: Name) extends Type

case class Field(name: Name, args: Type)
case class RecordDefinition(name: Name, cases: Seq[Field]) extends ModuleEntry

case class Record(name: Name, data: Seq[(Name, Term)]) extends Term
case class Read(record: Term, fieldName: Name) extends Term */


// TODO Discuss: If we just support Constructors, then do not get correct type information, since we have no notion
//  of inheritance. E.g if a relation can either return an empty linked list or a Cons, how can we define the return
//  type in this case, since empty and cons are not related.

case class TData(name: Name) extends Type:
  override def toString: String = s"$name"

case class Case(name: Name, args: Seq[Type]):
  override def toString: String = s"""$name ${args.mkString(" ")}"""

case class DataDefinition(name: Name, cases: Seq[Case]) extends ModuleEntry:
  //private lazy val caseMap: Map[Name, Case] = cases.map(c => c.name -> c).toMap
  //def getCaseByName(name: Name): Option[Case] = caseMap.get(name)
  override def toString: String = s"""data $name = ${cases.mkString("|")}"""

case class Construct(name: Name, caseName: Name, data: Seq[Term]) extends Term
case class Match(data: Term, caseName: Name, caseVars: Seq[Var]) extends Atom


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
