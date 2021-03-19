package inca.frontend.core

import inca.compiler.SourceLocation
import inca.frontend.typechecker.{Resolvable, Typeable}
import inca.util.Meta.Scala

trait Expression extends Typeable with SourceLocation {
  def vars: Map[Name, Option[Type]]
  def calls: Set[Call]

  def prettyprint(infixParens: Boolean)(implicit indent: String): String
  def prettyprint(implicit indent: String): String = prettyprint(infixParens = false)(indent)
  override def toString: String = prettyprint("")

  def ensureCore: CoreExpression = this match {
    case self: CoreExpression => self
    case _ => throw new IllegalArgumentException(s"Core expression required but got $this")
  }

  def infix(infixParens: Boolean)(f: => String): String =
    if (infixParens)
      s"($f)"
    else
      f
}

sealed trait CoreExpression extends Expression

case class Let(names: Seq[Name], anno: Option[Type], bound: Expression, body: Expression) extends CoreExpression with Var.Target {
  override def vars: Map[Name, Option[Type]] = bound.vars ++ body.vars ++ (bound.typ match {
    case Some(ty) if names.size == 1 => Map(names.head -> Some(ty))
    case Some(TTuple(ts)) if names.size == ts.size => (names zip ts.map(Some(_))).toMap
    case _ => names.map(_ -> None).toMap
  })

  override def calls: Set[Call] = bound.calls ++ body.calls

  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = infix(infixParens) {
    val namesS = names match {
      case Nil => "()"
      case Seq(name) => name.name
      case names => names.mkString("(",", ",")")
    }
    val annoS = anno match {
      case Some(ty) => s": ${ty.prettyprint}"
      case None => ""
    }
    s"""let $namesS$annoS = ${bound.prettyprint} in
       |${indent}  ${body.prettyprint(indent + "  ")}""".stripMargin
  }
}

//case class BlockExp(stmts: Seq[Statement]) extends CoreExpression {
//  override def vars: Map[Name, Option[Type]] = stmts.flatMap(_.vars).toMap
//
//  override def prettyprint(implicit indent: String): String =
//    if (stmts.isEmpty)
//      s"{\n$indent}"
//    else {
//      val stmtsS = stmts.map(_.prettyprint(indent + "  ")).mkString("\n")
//      s"{\n$stmtsS\n$indent}"
//    }
//}


case class Var(name: Name) extends CoreExpression with Resolvable[Var.Target] {
  override def vars: Map[Name, Option[Type]] = Map(name -> typ)
  override def calls: Set[Call] = Set()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = name.name
}
object Var {
  def apply(name: String): Var = new Var(Name(name))
  trait Target extends SourceLocation
}

case class If(cnd: Expression, thn: Expression, els: Expression) extends CoreExpression {
  override def vars: Map[Name, Option[Type]] = cnd.vars ++ thn.vars ++ els.vars
  override def calls: Set[Call] = cnd.calls ++ thn.calls ++ els.calls
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = infix(infixParens) {
    s"""if (${cnd.prettyprint})
       |${indent}  ${thn.prettyprint(indent + "  ")}
       |${indent}else
       |${indent}  ${els.prettyprint(indent + "  ")}""".stripMargin
  }
}


case class Call(name: Name, args: Seq[Expression], transitive: Boolean = false)
  extends CoreExpression with Resolvable[Call.Target] {
  override def vars: Map[Name, Option[Type]] = args.flatMap(_.vars).toMap
  override def calls: Set[Call] = Set(this)
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val argsS = args.map(_.prettyprint).mkString(", ")
    val transS = if (transitive) "+" else ""
    s"$name$transS($argsS)"
  }
}
object Call {
  trait Target
}

case class Tuple(exps: Seq[Expression]) extends CoreExpression {
  override def vars: Map[Name, Option[Type]] = exps.flatMap(_.vars).toMap
  override def calls: Set[Call] = exps.flatMap(_.calls).toSet
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    exps.map(_.prettyprint).mkString("(", ", ", ")")
}
object Tuple {
  def from(exps: Seq[Expression]): Expression = exps match {
    case Nil => Tuple(Seq())
    case e :: Nil => e
    case es => Tuple(es)
  }
}

case class Match(matchee: Expression, cases: Seq[(Pattern, Expression)]) extends CoreExpression {
  override def vars: Map[Name, Option[Type]] = matchee.vars ++ cases.flatMap(pe => pe._1.vars ++ pe._2.vars)
  override def calls: Set[Call] = matchee.calls ++ cases.flatMap(_._2.calls)
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = infix(infixParens) {
    val casesS = cases.map { case (pat, exp) =>
      s"${indent}  case ${pat.prettyprint} => ${exp.prettyprint(indent + "  ")}"
    }.mkString("\n")
    s"${matchee.prettyprint} match {\n$casesS\n$indent}"
  }
}
trait Pattern extends SourceLocation {
  def vars: Map[Name, Option[Type]]
  def prettyprint: String
}
case class ConstructorPattern(constr: Name, args: Seq[Name]) extends Pattern with Resolvable[DataConstructor.Target] with Var.Target {
  override def vars: Map[Name, Option[Type]] = args.map(_ -> None).toMap
  override def prettyprint: String = s"$constr(${args.mkString(", ")})"
}

case class NonePattern() extends Pattern {
  override def vars: Map[Name, Option[Type]] = Map()
  override def prettyprint: String = "None"
}
case class SomePattern(arg: Name) extends Pattern with Var.Target {
  override def vars: Map[Name, Option[Type]] = Map(arg -> None)
  override def prettyprint: String = s"Some($arg)"
}


case class BaseLit(code: Scala[meta.Term]) extends CoreExpression {
  override def vars: Map[Name, Option[Type]] = Map()
  override def calls: Set[Call] = Set()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = code.tree match {
    case meta.Lit.Int(i) => i.toString
    case meta.Lit.Long(l) => l.toString
    case meta.Lit.Float(f) => f
    case meta.Lit.Double(d) => d
    case meta.Lit.String(s) => s
    case meta.Lit.Boolean(b) => b.toString
    case meta.Lit.Char(c) => c.toString
    case t => s"`${t.syntax}`"
  }
}
object BaseLit {
  def apply(code: meta.Term, typ: Type): BaseLit =
    new BaseLit(Scala(code)).typed(typ)
}

case class BaseApply(fun: Scala[meta.Term], args: Seq[Expression]) extends CoreExpression {
  override def vars: Map[Name, Option[Type]] = args.flatMap(_.vars).toMap
  override def calls: Set[Call] = args.flatMap(_.calls).toSet
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val argsS = args.map(_.prettyprint).mkString(", ")
    s"`$fun`($argsS)"
  }
}
object BaseApply {
  def apply(fun: meta.Term, args: Seq[Expression], typ: Type): BaseApply =
    new BaseApply(Scala(fun), args).typed(typ)
}

case class BaseApplyInfix(left: Expression, op: Scala[meta.Term.Name], right: Expression) extends CoreExpression {
  override def vars: Map[Name, Option[Type]] = left.vars ++ right.vars
  override def calls: Set[Call] = left.calls ++ right.calls
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = infix(infixParens) {
    s"${left.prettyprint(infixParens = true)} $op ${right.prettyprint(infixParens = true)}"
  }
}
object BaseApplyInfix {
  def apply(left: Expression, op: String, right: Expression): BaseApplyInfix =
    new BaseApplyInfix(left, Scala(meta.Term.Name(op)), right)
}

case class NoneExp() extends CoreExpression {
  override def vars: Map[Name, Option[Type]] = Map()
  override def calls: Set[Call] = Set()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = "None"
}
case class SomeExp(e: Expression) extends CoreExpression {
  override def vars: Map[Name, Option[Type]] = e.vars
  override def calls: Set[Call] = e.calls
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = s"Some(${e.prettyprint})"
}

case class SetExp(es: Seq[Expression]) extends CoreExpression {
  override def vars: Map[Name, Option[Type]] = es.flatMap(_.vars).toMap
  override def calls: Set[Call] = es.flatMap(_.calls).toSet
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"{${es.map(_.prettyprint).mkString(", ")}}"
}

case class SetComprehension(build: Expression, predicates: Seq[Expression]) extends CoreExpression {
  override def vars: Map[Name, Option[Type]] = build.vars ++ predicates.flatMap(_.vars)
  override def calls: Set[Call] = build.calls ++ predicates.flatMap(_.calls)
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"{${build.prettyprint(infixParens = true)} | ${predicates.map(_.prettyprint).mkString(", ")}}"
}

case class SetMember(tup: Expression, set: Expression, neg: Boolean) extends CoreExpression with Var.Target {
  var isTypeMember: Boolean = false
  override def vars: Map[Name, Option[Type]] = set.vars ++ tup.vars
  override def calls: Set[Call] = set.calls ++ tup.calls
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val negS = if (neg) "not " else ""
    s"${tup.prettyprint(infixParens = true)} ${negS}in ${set.prettyprint(infixParens = true)}"
  }
}

case class FoldOp(name: Name) extends Resolvable[Call.Target] with SourceLocation {
  override def toString: String = name.name
}
case class SetFold(anno: Option[Type], init: Expression, op: FoldOp, set: Expression) extends CoreExpression {
  override def vars: Map[Name, Option[Type]] = init.vars ++ set.vars
  override def calls: Set[Call] = init.calls ++ set.calls
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val annoS = anno match {
      case Some(ty) => s"[${ty.prettyprint}]"
      case None => ""
    }
    s"""fold$annoS(
       |$indent  ${init.prettyprint(indent + "  ")},
       |$indent  $op,
       |$indent  ${set.prettyprint(indent + "  ")})
       |""".stripMargin
  }
}