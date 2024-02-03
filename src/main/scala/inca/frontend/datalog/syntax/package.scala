package inca.frontend.datalog

import inca.compiler.SourceLocation
import inca.frontend.util.{Resolvable, Typeable}

package object syntax {
  case class Name(name: String) extends SourceLocation {
    override def toString: String = name
  }

  case class Module(name: Name, content: Seq[ModuleContent]) extends SourceLocation {
    override def toString: String =
      s"""module $name
         |${content.map(_.prettyprint("")).mkString("\n")}
         |""".stripMargin
  }

  trait ModuleContent extends SourceLocation with Annotations {
    def prettyprint(implicit indent: String): String
  }

  case class DataDef(annos: Seq[Annotation], name: Name, constrs: Seq[DataConstructor])
    extends ModuleContent with TData.Target with Call.Target {

    override def prettyprint(implicit indent: String): String = {
      if (constrs.isEmpty)
        s"\n$annoPrefix${indent}data $name"
      else {
        val constrS = constrs.map(_.prettyprint(indent + "  "))
        s"""\n$annoPrefix${indent}data $name =
           |${constrS.mkString(" |\n")}
           |""".stripMargin
      }
    }
  }


  case class DataConstructor(name: Name, params: Seq[DataConstrParam]) extends SourceLocation with Call.Target with TData.Target {
    def prettyprint(implicit indent: String): String = {
      val paramTypesS = params.map(_.prettyprint).mkString(", ")
      s"$indent$name{$paramTypesS}"
    }
    def paramTypes: Seq[Type] = params.map(_.typ)
    def selectorName: String = "un$_" + name.name
  }
  case class DataConstrParam(name: Name, typ: Type) extends Path.Target with SourceLocation {
    def prettyprint(implicit indent: String): String = s"$name: ${typ.prettyprint}"
  }

  case class RuleSig(annos: Seq[Annotation], name: Name, params: Seq[Param]) extends ModuleContent with Call.Target {
    override def prettyprint(implicit indent: String): String =
      s"\n$annoPrefix${indent}relation $name(${params.map(_.prettyprint).mkString(", ")})."
  }
  case class Rule(name: Name, headTerms: Seq[Term], body: Seq[Atom]) extends ModuleContent with Resolvable[RuleSig] {
    override val annos: Seq[Annotation] = Seq()
    override def prettyprint(implicit indent: String): String = {
      val bodyS =
        if (body.isEmpty) "."
        else " :-\n" + body.map(_.prettyprint(indent + "  ")).mkString("\n") + "."
      s"$annoPrefix$indent$name(${headTerms.mkString(", ")})$bodyS"
    }
  }

  case class Param(typ: Type) {
    def prettyprint(implicit indent: String): String = typ.prettyprint
  }

  sealed trait Atom extends SourceLocation {
    def prettyprint(implicit indent: String): String
  }
  case class Call(name: Name, args: Seq[Term], not: Boolean) extends Atom with Resolvable[Call.Target] {
    override def prettyprint(implicit indent: String): String = {
      val notS = if (not) "not " else ""
      s"$indent$notS$name(${args.map(_.prettyprint).mkString(", ")})"
    }
  }
  object Call { trait Target }
  case class Compare(comp: Comparator, lhs: Term, rhs: Term) extends Atom {
    override def prettyprint(implicit indent: String): String =
      s"$indent${lhs.prettyprint} $comp ${rhs.prettyprint}"
  }
  def Eq(lhs: Term, rhs: Term): Compare = Compare(EqComparator, lhs, rhs)
  def Neq(lhs: Term, rhs: Term): Compare = Compare(NeqComparator, lhs, rhs)

  sealed trait Comparator
  case object EqComparator extends Comparator {
    override def toString: String = "=="
  }
  case object NeqComparator extends Comparator {
    override def toString: String = "!="
  }

  sealed trait Term extends Typeable[Type] with SourceLocation {
    def prettyprint: String = toString
  }
  case class Wildcard() extends Term {
    override def toString: String = "_"
  }
  case class Var(name: Name) extends Term {
    override def toString: String = name.name
  }
  case class Constant(lit: Literal) extends Term {
    override def toString: String = lit.toString
  }
  case class Path(src: Term, link: Name) extends Term with Resolvable[Path.Target] {
    override def toString: String = s"${src.prettyprint}.$link"
  }
  object Path {
    trait Target
  }

  sealed trait Literal extends SourceLocation {
    def typ: Type
  }
  object Literal {
    def fromScalaMeta(t: meta.Lit): Option[Literal] = t match {
      case meta.Lit.Int(i) => Some(IntLiteral(i))
      case meta.Lit.Long(l) => Some(LongLiteral(l))
      case d: meta.Lit.Double => Some(DoubleLiteral(d.value.asInstanceOf[Double]))
      case meta.Lit.Boolean(b) => Some(BooleanLiteral(b))
      case meta.Lit.String(s) => Some(StringLiteral(s))
      case _ => None
    }
  }
  case class IntLiteral(v: Int) extends Literal {
    override def typ: Type = TScalaInt
    override def toString: String = v.toString
  }
  case class LongLiteral(v: Long) extends Literal {
    override def typ: Type = TScalaLong
    override def toString: String = v.toString
  }
  case class DoubleLiteral(v: Double) extends Literal {
    override def typ: Type = TScalaDouble
    override def toString: String = v.toString
  }
  case class StringLiteral(v: String) extends Literal {
    override def typ: Type = TScalaString
    override def toString: String = "\"" + v + "\""
  }
  case class BooleanLiteral(v: Boolean) extends Literal {
    override def typ: Type = TScalaBoolean
    override def toString: String = v.toString
  }
  def True: Constant = Constant(BooleanLiteral(true))
  def False: Constant = Constant(BooleanLiteral(false))
}
