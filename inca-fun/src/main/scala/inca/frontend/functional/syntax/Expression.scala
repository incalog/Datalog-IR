package inca.frontend.functional.syntax

import inca.ir.typing.{Resolvable, Typeable}
import inca.ir.Name
import inca.ir.util.SourceLocation

trait Expression extends Typeable[Type] with SourceLocation {
  def vars: Map[Name, Option[Type]]
  def freevars: Seq[Var]
  def freeTvars: Seq[TName] = typ.toSeq.flatMap(_.freeTvars)
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

case class Let(names: Seq[Name], anno: Option[Type], bound: Expression, body: Expression) extends Expression with Var.Target {
  override def vars: Map[Name, Option[Type]] = bound.vars ++ body.vars ++ (bound.typ match {
    case Some(ty) if names.size == 1 => Map(names.head -> Some(ty))
    case Some(TTuple(ts)) if names.size == ts.size => (names zip ts.map(Some(_))).toMap
    case _ => names.map(_ -> None).toMap
  })

  override def freevars: Seq[Var] = bound.freevars ++ body.freevars.filter(!_.target.contains(this))
  override def freeTvars: Seq[TName] = super.freeTvars ++ bound.freeTvars ++ body.freeTvars ++ anno.toSeq.flatMap(_.freeTvars)

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

case class Var(name: Name) extends Expression with Resolvable[Var.Target] {
  override def vars: Map[Name, Option[Type]] = Map(name -> typ)
  override def freevars: Seq[Var] = this.target match {
    case Some(_: FunctionDef) => Seq()
    case Some(_: DataConstructor) => Seq()
    case _ => Seq(this)
  }

  override def freeTvars: Seq[TName] = super.freeTvars
  override def calls: Set[Call] = Set()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = name.name
}
object Var {
  def apply(name: String): Var = new Var(Name(name))
  trait Target extends SourceLocation
}

case class If(cnd: Expression, thn: Expression, els: Expression) extends Expression {
  override def vars: Map[Name, Option[Type]] = cnd.vars ++ thn.vars ++ els.vars
  override def freevars: Seq[Var] = cnd.freevars ++ thn.freevars ++ els.freevars
  override def freeTvars: Seq[TName] = super.freeTvars ++ cnd.freeTvars ++ thn.freeTvars ++ els.freeTvars
  override def calls: Set[Call] = cnd.calls ++ thn.calls ++ els.calls
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = infix(infixParens) {
    s"""if (${cnd.prettyprint})
       |${indent}  ${thn.prettyprint(indent + "  ")}
       |${indent}else
       |${indent}  ${els.prettyprint(indent + "  ")}""".stripMargin
  }
}


case class Call(fun: Expression, tyArgs: Seq[Type], args: Seq[Expression]) extends Expression {
  override def vars: Map[Name, Option[Type]] = args.flatMap(_.vars).toMap
  override def freevars: Seq[Var] = fun.freevars ++ args.flatMap(_.freevars)
  override def freeTvars: Seq[TName] = super.freeTvars ++ fun.freeTvars ++ args.flatMap(_.freeTvars)
  override def calls: Set[Call] = Set(this)
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val argsS = args.map(_.prettyprint).mkString(", ")
    val tyS = if (tyArgs.isEmpty) "" else tyArgs.mkString("[", ",", "]")
    s"${fun.prettyprint(infixParens = true)}$tyS($argsS)"
  }
}

case class Lambda(vs: Seq[(Name, Type)], body: Expression) extends Expression with Var.Target {
  override def vars: Map[Name, Option[Type]] = body.vars ++ vs.map(kv => kv._1 -> Some(kv._2)).toMap
  override def freevars: Seq[Var] = body.freevars.filter(!_.target.contains(this))
  override def freeTvars: Seq[TName] = super.freeTvars ++ body.freeTvars ++ vs.flatMap(_._2.freeTvars)
  override def calls: Set[Call] = body.calls
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = infix(infixParens) {
    val vsS = vs.map(v => s"${v._1}: ${v._2.prettyprint}").mkString(", ")
    s"($vsS) => ${body.prettyprint}"
  }
}

case class Tuple(exps: Seq[Expression]) extends Expression {
  override def vars: Map[Name, Option[Type]] = exps.flatMap(_.vars).toMap
  override def freevars: Seq[Var] = exps.flatMap(_.freevars)
  override def freeTvars: Seq[TName] = super.freeTvars ++ exps.flatMap(_.freeTvars)
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

case class Match(matchee: Expression, cases: Seq[(Pattern, Expression)]) extends Expression {
  override def vars: Map[Name, Option[Type]] = matchee.vars ++ cases.flatMap(pe => pe._1.vars ++ pe._2.vars)
  override def freevars: Seq[Var] = matchee.freevars ++ cases.flatMap {
    case (pat, cas) => cas.freevars.filter(!_.target.contains(pat))
  }
  override def freeTvars: Seq[TName] = super.freeTvars ++ matchee.freeTvars ++ cases.flatMap(_._2.freeTvars)
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
case class ConstructorPattern(constr: Name, args: Seq[PatternVariable]) extends Pattern with Resolvable[DataConstructor.Target] with Var.Target {
  override def vars: Map[Name, Option[Type]] = args.flatMap(_.vars).toMap
  override def prettyprint: String = s"$constr(${args.map(_.prettyprint).mkString(", ")})"
}
case class PatternVariable(name: Name) extends Resolvable[Type] {
  def vars: Map[Name, Option[Type]] = Map(name -> target)

  def prettyprint: String = name.name
}

case class BoolLit(b: Boolean) extends Expression {
  override def vars: Map[Name, Option[Type]] = Map()
  override def freevars: Seq[Var] = Seq()
  override def calls: Set[Call] = Set()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = b.toString
}

case class IntLit(i: Int) extends Expression {
  override def vars: Map[Name, Option[Type]] = Map()
  override def freevars: Seq[Var] = Seq()
  override def calls: Set[Call] = Set()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = i.toString
}

case class DoubleLit(d: Double) extends Expression {
  override def vars: Map[Name, Option[Type]] = Map()
  override def freevars: Seq[Var] = Seq()
  override def calls: Set[Call] = Set()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = d.toString
}

case class StringLit(s: String) extends Expression {
  override def vars: Map[Name, Option[Type]] = Map()
  override def freevars: Seq[Var] = Seq()
  override def calls: Set[Call] = Set()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = '"' + s + '"'
}

case class BinOp(e1: Expression, op: String, e2: Expression) extends Expression {
  override def vars: Map[Name, Option[Type]] = e1.vars ++ e2.vars
  override def freevars: Seq[Var] = e1.freevars ++ e2.freevars
  override def calls: Set[Call] = e1.calls ++ e2.calls
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = infix(infixParens) {
      s"${e1.prettyprint(infixParens = true)} $op ${e2.prettyprint(infixParens = true)}"
    }
}

case class UnOp(op: String, e: Expression) extends Expression {
  override def vars: Map[Name, Option[Type]] = e.vars
  override def freevars: Seq[Var] = e.freevars
  override def calls: Set[Call] = e.calls
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = infix(infixParens) {
    s"$op${e.prettyprint(infixParens = true)}"
  }
}

case class SetExp(es: Seq[Expression]) extends Expression {
  override def vars: Map[Name, Option[Type]] = es.flatMap(_.vars).toMap
  override def freevars: Seq[Var] = es.flatMap(_.freevars)
  override def freeTvars: Seq[TName] = super.freeTvars ++ es.flatMap(_.freeTvars)
  override def calls: Set[Call] = es.flatMap(_.calls).toSet
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"{${es.map(_.prettyprint).mkString(", ")}}"
}

case class SetComprehension(build: Expression, predicates: Seq[Expression]) extends Expression {
  override def vars: Map[Name, Option[Type]] = build.vars ++ predicates.flatMap(_.vars)
  override def freevars: Seq[Var] = {
    var free: Seq[Var] = Seq()
    var bindings: Seq[Var] = Seq()
    predicates.foreach {
      case mem: SetMember =>
        free ++= mem.freevars diff bindings
        bindings ++= mem.bindings
      case pred =>
        free ++= pred.freevars diff bindings
    }
    free ++ (build.freevars diff bindings)
  }
  override def freeTvars: Seq[TName] = super.freeTvars ++ build.freeTvars ++ predicates.flatMap(_.freeTvars)

  override def calls: Set[Call] = build.calls ++ predicates.flatMap(_.calls)
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"{${build.prettyprint(infixParens = true)} | ${predicates.map(_.prettyprint).mkString(", ")}}"
}

case class SetMember(tup: Expression, set: Expression, neg: Boolean) extends Expression with Var.Target {
  var isTypeMember: Boolean = false
  override def vars: Map[Name, Option[Type]] =
    if (isTypeMember)
      tup.vars
    else
      set.vars ++ tup.vars

  override def freevars: Seq[Var] =
    (if (isTypeMember) Seq() else set.freevars) ++
    (tup match {
    case v: Var if v.target.isEmpty => Seq()
    case Tuple(es) => es.flatMap {
      case v: Var if v.target.isEmpty => Seq()
      case e => e.freevars
    }
    case _ => tup.freevars
  })
  override def freeTvars: Seq[TName] = super.freeTvars ++ tup.freeTvars ++ set.freeTvars

  def bindings: Seq[Var] = tup match {
    case v: Var if v.target.isEmpty => Seq(v)
    case Tuple(es) => es.flatMap {
      case v: Var if v.target.isEmpty => Seq(v)
      case e => Seq()
    }
    case _ => Seq()
  }
  override def calls: Set[Call] = set.calls ++ tup.calls
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val negS = if (neg) "not " else ""
    s"${tup.prettyprint(infixParens = true)} ${negS}in ${set.prettyprint(infixParens = true)}"
  }
}

case class SetFold(anno: Option[Type], init: Expression, op: Expression, set: Expression) extends Expression {
  override def vars: Map[Name, Option[Type]] = init.vars ++ set.vars
  override def freevars: Seq[Var] = init.freevars ++ op.freevars ++ set.freevars
  override def freeTvars: Seq[TName] = super.freeTvars ++ init.freeTvars ++ op.freeTvars ++ set.freeTvars
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