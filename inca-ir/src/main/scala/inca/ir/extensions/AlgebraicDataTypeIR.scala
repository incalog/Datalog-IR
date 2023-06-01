package inca.ir.extensions
import scala.language.implicitConversions
import inca.ir.{Atom, BaseIR, Language, ModuleEntry, Name, Term, Type, Var}

// TODO: Algebraic DataType definition should probably be a Module entry
// Then the type can just be a name, since the definition is extern
// Maybe something such as:
/*case class TData(name: Name) extends Type

case class Case(name: Name, args: Seq[Type])
case class DataDefinition(name: Name, cases: Seq[Case]) extends ModuleEntry:
  private lazy val caseMap: Map[Name, Case] = cases.map(c => c.name -> c).toMap
  def getCaseByName(name: Name): Option[Case] = caseMap.get(name)

case class Data(name: Name, caseName: Name, data: Seq[Term]) extends Term
case class Match(name: Term, caseName: Name, caseVars: Seq[Var]) extends Atom*/

trait NamedType(name: Name) extends Type:
  override def toString: String = s"$name"

type Case = (Name, Seq[Type])
implicit def arg2func(arg: => Case): (() => Case) = () => arg

case class TVariant(name: Name, _cases: (() => Case)*) extends NamedType(name):
  lazy val cases: Map[Name, Seq[Type]] = this._cases.map(_()).toMap

  override def toString: String = s"$name"

  override def equals(x: Any): Boolean = x match {
    case x: TVariant => this.name == x.name
    case _ => false
  }

/** We can now define recursive types... This might be a bad idea since we now need to catch infinite recursion in the
 * type checker and all other places
val v: TVariant = TVariant(Name("LinkedList"),
  Name("Nil") -> Seq(),
  Name("Cons") -> Seq(TInt, v)
)
 */

case class Variant(name: Name, caseName: Name, data: Seq[Term]) extends Term:
  override def toString: String = s"""$caseName${data.mkString(" ", " ", "")}"""

// Bind variables on success, fail otherwise ?
case class Match(variantTerm: Term, caseName: Name, caseVars: Seq[Var]) extends Atom:
  override def toString: String =  s"""$variantTerm = $caseName${caseVars.mkString(" ", " ", "")}"""

/* We could write code such as:
R(x: TInt, y: TInt, z: TVariant("LinkedList")) :-
  a = Variant("LinkedList", "Empty", Seq()) // -> We might compile this to a scala object that looks exactly like this
  z = Variant("LinkedList", "Cons", Seq(4, a))

  Match(z, "Cons", Seq(n, t))

  // 1. Use typechecker to find out which variant we are matching
  // 2. Scala could look like this:
  // ((z: Any) => z.asInstanceOf["LinkedList"].caseName == "Cons)(z)
  // n = ((z: Any) => z.asInstanceOf["LinkedList"].data(0))(z)
  // t = ((z: Any) => z.asInstanceOf["LinkedList"].data(1))(z)
*/


trait AlgebraicDataTypeIR extends BaseIR:
  override val name: String = "ADT"
  override def language: Language = super.language + new AlgebraicDataTypeIR {}
  override def requires: Language = Language()
