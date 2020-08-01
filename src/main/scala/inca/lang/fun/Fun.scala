package inca.lang.fun

import inca.util.Meta.TAB

object Fun {
  trait TypeAnno {
    def prettyprint: String
  }
  case object TBool extends TypeAnno {
    override def prettyprint: String = "bool"
  }
  case object TInt extends TypeAnno {
    override def prettyprint: String = "int"
  }
  case object TLong extends TypeAnno {
    override def prettyprint: String = "long"
  }
  case object TDouble extends TypeAnno {
    override def prettyprint: String = "double"
  }
  case object TString extends TypeAnno {
    override def prettyprint: String = "string"
  }

  trait TLinked extends TypeAnno
  case object TAnyLinked extends TLinked {
    override def prettyprint: String = "node"
  }
  case class TNode(name: String) extends TLinked {
    override def prettyprint: String = name
    def apply(field: String): NamedLink = NamedLink(this, field)
  }
  case class TList(contained: TLinked) extends TLinked {
    override def prettyprint: String = s"List[${contained.prettyprint}]"
  }

  type Name = String

  sealed trait Visibility {
    def prettyprint(implicit indent: String): String
  }
  case object Private extends Visibility {
    def prettyprint(implicit indent: String): String = "private"
  }
  case object Public extends Visibility {
    def prettyprint(implicit indent: String): String = "public"
  }

  case class Module(name: Name, imports: Seq[Name], funs: Seq[PatternFunction]) {
    def usedvars: Set[Name] = Set(name) ++ imports

    override def toString: Name = prettyprint("")

    def prettyprint(implicit indent: String): String = {
      val importsS = if (imports.isEmpty) "" else
        "\n" + indent + imports.mkString("\n"+indent)
      val funsS = if (funs.isEmpty) "" else
        "\n" + funs.map(_.prettyprint).mkString("\n")
      s"${indent}module $name$importsS$funsS".stripMargin
    }
  }
  case class PatternFunction(vis: Option[Visibility], name: Name, params: Seq[Param], outParams: Seq[AnnoParam], bodies: Seq[Body]) {
    def usedvars: Set[Name] = Set(name) ++ params.map(_.name) ++ outParams.flatMap(_.name) ++ bodies.flatMap(_.usedvars)

    def prettyprint(implicit indent: String): String = {
      val visS = if (vis.contains(Private)) "private " else ""
      val paramsS = params.map(_.prettyprint).mkString(", ")
      val outS = if (outParams.isEmpty) "Unit" else
        outParams.map(_.prettyprint)
      val bodiesS = if (bodies.isEmpty) "{ }" else
        bodies.map(_.prettyprint(indent)).mkString(" union ")
      s"$indent${visS}def $name($paramsS): $outS = $bodiesS"
    }
  }

  case class Param(name: Name, typ: Option[TypeAnno]) {
    def prettyprint: String = typ match {
      case Some(ty) => s"$name: ${ty.prettyprint}"
      case None => name
    }
  }
  case class AnnoParam(name: Option[Name], typ: TypeAnno) {
    def prettyprint: String = name match {
      case Some(nam) => s"($nam: ${typ.prettyprint})"
      case None => typ.prettyprint
    }

  }

  case class Body(stmts: Seq[Statement]) {
    def usedvars: Set[Name] = stmts.flatMap(_.usedvars).toSet
    def prettyprint(implicit indent: String): String = {
      val stmtsS = if (stmts.isEmpty) " " else
        "\n" + stmts.map(_.prettyprint(indent+TAB)).mkString("\n")
      s"""{$stmtsS
         |${indent}}""".stripMargin
    }
  }

  trait Statement {
    def usedvars: Set[Name]
    def prettyprint(implicit indent: String): String
    def ensureCore: CoreStatement = this match {
      case self: CoreStatement => self
      case _ => throw new IllegalArgumentException(s"Core statement required but got $this")
    }
  }
  sealed trait CoreStatement extends Statement
  case class Assign(names: Seq[Name], exp: Exp) extends CoreStatement {
    override def usedvars: Set[Name] = names.toSet ++ exp.usedvars

    override def prettyprint(implicit indent: String): String = {
      val namesS = if (names.size == 1) names.head else names.mkString("(", ", ", ")")
      s"${indent}let $namesS = ${exp.prettyprint}"
    }
  }
  case class Assert(cond: Cond) extends CoreStatement {
    override def usedvars: Set[Name] = cond.usedvars
    override def prettyprint(implicit indent: String): String =
      s"${indent}assert ${cond.prettyprint}"
  }
  case class Yield(exp: Exp) extends CoreStatement {
    override def usedvars: Set[Name] = exp.usedvars
    override def prettyprint(implicit indent: String): String =
      s"${indent}yield ${exp.prettyprint}"
  }

  trait Cond {
    def usedvars: Set[Name]
    def prettyprint(implicit indent: String): String
    def ensureCore: CoreCond = this match {
      case self: CoreCond => self
      case _ => throw new IllegalArgumentException(s"Core statement required but got $this")
    }
  }
  sealed trait CoreCond extends Cond
  case class Eq(lhs: Exp, rhs: Exp) extends CoreCond {
    def usedvars: Set[Name] = lhs.usedvars ++ rhs.usedvars
    override def prettyprint(implicit indent: String): String =
      s"${lhs.prettyprint} == ${rhs.prettyprint}"
  }
  case class Neq(lhs: Exp, rhs: Exp) extends CoreCond {
    override def usedvars: Set[Name] = lhs.usedvars ++ rhs.usedvars
    override def prettyprint(implicit indent: String): String =
      s"${lhs.prettyprint} != ${rhs.prettyprint}"
  }
  case class InstanceOf(exp: Exp, typ: TypeAnno) extends CoreCond {
    override def usedvars: Set[Name] = exp.usedvars
    override def prettyprint(implicit indent: String): String =
      s"${exp.prettyprint} instanceOf ${typ.prettyprint}"
  }
  case class NotInstanceOf(exp: Exp, typ: TypeAnno) extends CoreCond {
    override def usedvars: Set[Name] = exp.usedvars
    override def prettyprint(implicit indent: String): String =
      s"${exp.prettyprint} notInstanceOf ${typ.prettyprint}"
  }
  case class Def(exp: Exp) extends CoreCond {
    override def usedvars: Set[Name] = exp.usedvars
    override def prettyprint(implicit indent: String): String =
      s"def ${exp.prettyprint}"
  }
  case class Undef(exp: Exp) extends CoreCond {
    override def usedvars: Set[Name] = exp.usedvars
    override def prettyprint(implicit indent: String): String =
      s"undef ${exp.prettyprint}"
  }
  case class BooleanCond(v: Boolean) extends CoreCond {
    override def usedvars: Set[Name] = Set()
    override def prettyprint(implicit indent: String): String =
      v.toString
  }

  trait Typeable {
    var typ: Option[TypeAnno] = None
    def typed(ty: TypeAnno): this.type = {
      this.typ = Some(ty)
      this
    }
  }

  trait Exp extends Typeable {
    def usedvars: Set[Name]
    def prettyprint(implicit indent: String): String
    def ensureCore: CoreExp = this match {
      case self: CoreExp => self
      case _ => throw new IllegalArgumentException(s"Core statement required but got $this")
    }
  }
  sealed trait CoreExp extends Exp
  case class Var(name: Name) extends CoreExp {
    override def usedvars: Set[Name] = Set(name)
    override def prettyprint(implicit indent: String): String = name
  }
  case class Constant(lit: Literal) extends CoreExp {
    override def usedvars: Set[Name] = Set()
    override def prettyprint(implicit indent: String): String = lit.prettyprint
  }
  case class PathAccess(receiver: Exp, link: Link) extends CoreExp {
    override def usedvars: Set[Name] = receiver.usedvars
    override def prettyprint(implicit indent: String): String =
      s"${receiver.prettyprint}.${link.prettyprint}"
  }
  case class Call(name: Name, args: Seq[Exp], transitive: Boolean, count: Boolean) extends CoreExp {
    override def usedvars: Set[Name] = Set(name) ++ args.flatMap(_.usedvars)
    override def prettyprint(implicit indent: String): String = {
      val argsS = args.map(_.prettyprint).mkString(", ")
      val transS = if (transitive) "+" else ""
      val countS = if (count) "count " else ""
      s"$countS$name$transS($argsS)"
    }
  }
  case class Tuple(exps: Seq[Exp]) extends CoreExp {
    override def usedvars: Set[Name] = exps.flatMap(_.usedvars).toSet
    override def prettyprint(implicit indent: String): String =
      exps.map(_.prettyprint).mkString("(", ", ", ")")
  }

  sealed trait Link {
    def prettyprint: String
  }
  sealed trait CoreLink extends Link
  case object ParentLink extends CoreLink {
    override def prettyprint: String = "parent"
  }
  case object ChildrenLink extends CoreLink {
    override def prettyprint: String = "children"
  }
  case object NextLink extends CoreLink {
    override def prettyprint: String = "next"
  }
  case object PreviousLink extends CoreLink {
    override def prettyprint: String = "prev"
  }
  case class NamedLink(node: TNode, field: Name) extends CoreLink {
    override def prettyprint: String = field
  }

  sealed trait Literal {
    def prettyprint: String
  }
  case class BooleanLiteral(v: Boolean) extends Literal {
    override def prettyprint: String = v.toString
  }
  case class IntLiteral(v: Int) extends Literal {
    override def prettyprint: String = v.toString
  }
  case class LongLiteral(v: Long) extends Literal {
    override def prettyprint: String = v.toString
  }
  case class DoubleLiteral(v: Double) extends Literal {
    override def prettyprint: String = v.toString
  }
  case class StringLiteral(v: String) extends Literal {
    override def prettyprint: String = '\"' + v + '\"'
  }
}
