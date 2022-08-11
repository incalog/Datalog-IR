package inca.frontend.objectoriented.core

import inca.compiler.SourceLocation
import inca.frontend.util.{Resolvable, Typeable}

trait Expression extends Typeable[Type] with SourceLocation {
  def vars: Map[Name, Option[Type]]
  def freevars: Seq[Var]
  def calls: Set[Call]

  def prettyprint(infixParens: Boolean)(implicit indent: String): String
  def prettyprint(implicit indent: String): String = prettyprint(infixParens = false)(indent)
  override def toString: String = prettyprint("")

  def infix(infixParens: Boolean)(f: => String): String =
    if (infixParens)
      s"($f)"
    else
      f
}

case class Var(name: Name) extends Expression with Resolvable[Var.Target] {
  override def vars: Map[Name, Option[Type]] = Map(name -> typ)
  override def freevars: Seq[Var] = this.target match {
    case Some(_: FunctionDef) => Seq()
    case _ => Seq(this)
  }

  override def calls: Set[Call] = Set()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = name.name
}
object Var {
  def apply(name: String): Var = new Var(Name(name))
  trait Target extends SourceLocation
}

case class Get(name: Name) extends Expression with Resolvable[Get.Target] {
  override def vars: Map[Name, Option[Type]] = Map(name -> typ)
  override def freevars: Seq[Var] = Seq() // TODO: Think about this

  override def calls: Set[Call] = Set()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = name.name
}
object Get {
  def apply(name: String): Get = new Get(Name(name))
  trait Target extends SourceLocation
}

// TODO: This signature is wrong. Is this only required to assign to an attribute ? Do we even need this ?
case class Assign(name: Name) extends Expression with Resolvable[Assign.Target] {
  override def vars: Map[Name, Option[Type]] = Map(name -> typ)
  override def freevars: Seq[Var] = Seq() // TODO: Think about this

  override def calls: Set[Call] = Set()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = name.name
}
object Assign {
  def apply(name: String): Assign = new Assign(Name(name))
  trait Target extends SourceLocation
}

case class If(cnd: Expression, thn: Expression, els: Expression) extends Expression {
  override def vars: Map[Name, Option[Type]] = cnd.vars ++ thn.vars ++ els.vars
  override def freevars: Seq[Var] = cnd.freevars ++ thn.freevars ++ els.freevars
  override def calls: Set[Call] = cnd.calls ++ thn.calls ++ els.calls
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = infix(infixParens) {
    s"""if (${cnd.prettyprint})
       |${indent}  ${thn.prettyprint(indent + "  ")}
       |${indent}else
       |${indent}  ${els.prettyprint(indent + "  ")}""".stripMargin
  }
}

// TODO: Do add a target to assign the call to a class ?? Or add a new Clazz.target or something ? Is this required ?
case class Call(fun: Expression, args: Seq[Expression], transitive: Boolean = false) extends Expression {
  override def vars: Map[Name, Option[Type]] = args.flatMap(_.vars).toMap
  override def freevars: Seq[Var] = fun.freevars ++ args.flatMap(_.freevars)
  override def calls: Set[Call] = Set(this)
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val argsS = args.map(_.prettyprint).mkString(", ")
    val transS = if (transitive) "+" else ""
    s"${fun.prettyprint(infixParens = true)}$transS($argsS)"
  }
}