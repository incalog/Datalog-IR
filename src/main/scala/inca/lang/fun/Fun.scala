package inca.lang.fun

import inca.util.Meta.TAB

import scala.language.reflectiveCalls

object Fun {
  sealed trait TypeAnno {
    def prettyprint: String
    def javastring: String
  }
  case object TBool extends TypeAnno {
    override def prettyprint: String = "bool"
    override def javastring: String = "bool"
  }
  case object TInt extends TypeAnno {
    override def prettyprint: String = "int"
    override def javastring: String = "int"
  }
  case object TLong extends TypeAnno {
    override def prettyprint: String = "long"
    override def javastring: String = "long"
  }
  case object TDouble extends TypeAnno {
    override def prettyprint: String = "double"
    override def javastring: String = "double"
  }
  case object TString extends TypeAnno {
    override def prettyprint: String = "string"
    override def javastring: String = "string"
  }

  trait TLinked extends TypeAnno
  case object TAnyLinked extends TLinked {
    override def prettyprint: String = "node"
    override def javastring: String = "node"
  }
  case class TNode(name: String) extends TLinked {
    override def prettyprint: String = name
    override def javastring: String = name.replace('.','_')
    def apply(field: String): NamedLink = NamedLink(this, field)
  }

  trait TIterable extends TypeAnno {
    val contained: TLinked
  }
  case class TList(contained: TLinked) extends TLinked with TIterable {
    override def prettyprint: String = s"List[${contained.prettyprint}]"
    override def javastring: String = s"List_${contained.javastring}"
  }
  case class TEnumeration(contained: TLinked) extends TIterable {
    override def prettyprint: String = s"Enum[${contained.prettyprint}]"
    override def javastring: String = s"Enum_${contained.javastring}"
  }

  case class TTuple(ts: Seq[TypeAnno]) extends TypeAnno {
    override def prettyprint: String = ts.size match {
      case 0 => "Unit"
      case 1 => ts.head.prettyprint
      case _ => ts.map(_.prettyprint).mkString("(", ", ", ")")
    }

    override def javastring: String = "Tuple_" + ts.map(_.javastring).mkString("_")
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
    def usedvars: Map[Name, Option[TypeAnno]] = collectUsedvars(funs)
    def usedModuleNames: Seq[Name] = name +: imports
    def usedFunNames: Seq[Name] = funs.map(_.name)

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
    def usedvars: Map[Name, Option[TypeAnno]] = collectUsedvars(params) ++ collectUsedvars(outParams) ++ collectUsedvars(bodies)

    def prettyprint(implicit indent: String): String = {
      val visS = if (vis.contains(Private)) "private " else ""
      val paramsS = params.map(_.prettyprint).mkString(", ")
      val outS = if (outParams.isEmpty) "Unit"
        else if (outParams.size == 1)
          outParams.head.prettyprint
        else
          outParams.map(_.prettyprint).mkString("(", ", ", ")")
      val bodiesS = if (bodies.isEmpty) "{ }" else
        bodies.map(_.prettyprint(indent)).mkString(" union ")
      s"$indent${visS}def $name($paramsS): $outS = $bodiesS"
    }
  }

  case class Param(name: Name, typ: Option[TypeAnno]) {
    def usedvars: Map[Name, Option[TypeAnno]] = Map(name -> typ)
    def prettyprint: String = typ match {
      case Some(ty) => s"$name: ${ty.prettyprint}"
      case None => name
    }
  }
  case class AnnoParam(name: Option[Name], typ: TypeAnno) {
    def usedvars: Map[Name, Option[TypeAnno]] = name.map(_ -> Some(typ)).toMap
    def prettyprint: String = name match {
      case Some(nam) => s"($nam: ${typ.prettyprint})"
      case None => typ.prettyprint
    }

  }

  case class Body(stmts: Seq[Statement]) {
    def usedvars: Map[Name, Option[TypeAnno]] = collectUsedvars(stmts)
    def prettyprint(implicit indent: String): String = {
      val stmtsS = if (stmts.isEmpty) " " else
        "\n" + stmts.map(_.prettyprint(indent+TAB)).mkString("\n")
      s"""{$stmtsS
         |${indent}}""".stripMargin
    }
  }

  trait Statement {
    def usedvars: Map[Name, Option[TypeAnno]]
    def prettyprint(implicit indent: String): String
    def ensureCore: CoreStatement = this match {
      case self: CoreStatement => self
      case _ => throw new IllegalArgumentException(s"Core statement required but got $this")
    }
  }
  sealed trait CoreStatement extends Statement
  case class Assign(names: Seq[Name], exp: Exp) extends CoreStatement {
    override def usedvars: Map[Name, Option[TypeAnno]] = exp.typ match {
      case Some(ty) if names.size == 1 => Map(names.head -> Some(ty)) ++ exp.usedvars
      case Some(TTuple(ts)) if names.size == ts.size => (names zip ts.map(Some(_))).toMap ++ exp.usedvars
      case _ => names.map(_ -> None).toMap ++ exp.usedvars
    }

    override def prettyprint(implicit indent: String): String = {
      val namesS = if (names.size == 1) names.head else names.mkString("(", ", ", ")")
      s"${indent}let $namesS = ${exp.prettyprint}"
    }
  }
  case class Assert(cond: Cond) extends CoreStatement {
    override def usedvars: Map[Name, Option[TypeAnno]] = cond.usedvars
    override def prettyprint(implicit indent: String): String =
      s"${indent}assert ${cond.prettyprint}"
  }

  trait TerminatorStatement extends Statement
  case class Yield(exp: Exp) extends CoreStatement with TerminatorStatement {
    override def usedvars: Map[Name, Option[TypeAnno]] = exp.usedvars
    override def prettyprint(implicit indent: String): String =
      s"${indent}yield ${exp.prettyprint}"
  }
  case object Fail extends CoreStatement with TerminatorStatement {
    override def usedvars: Map[Name, Option[TypeAnno]] = Map()
    override def prettyprint(implicit indent: String): String =
      s"${indent}continue"
  }
  val Continue: CoreStatement = Fail

  trait Cond {
    def usedvars: Map[Name, Option[TypeAnno]]
    def prettyprint(implicit indent: String): String
    def ensureCore: CoreCond = this match {
      case self: CoreCond => self
      case _ => throw new IllegalArgumentException(s"Core statement required but got $this")
    }
  }
  sealed trait CoreCond extends Cond
  case class Eq(lhs: Exp, rhs: Exp) extends CoreCond {
    def usedvars: Map[Name, Option[TypeAnno]] = lhs.usedvars ++ rhs.usedvars
    override def prettyprint(implicit indent: String): String =
      s"${lhs.prettyprint} == ${rhs.prettyprint}"
  }
  case class Neq(lhs: Exp, rhs: Exp) extends CoreCond {
    override def usedvars: Map[Name, Option[TypeAnno]] = lhs.usedvars ++ rhs.usedvars
    override def prettyprint(implicit indent: String): String =
      s"${lhs.prettyprint} != ${rhs.prettyprint}"
  }
  case class InstanceOf(exp: Exp, typ: TypeAnno) extends CoreCond {
    override def usedvars: Map[Name, Option[TypeAnno]] = exp.usedvars
    override def prettyprint(implicit indent: String): String =
      s"${exp.prettyprint} instanceOf ${typ.prettyprint}"
  }
  case class NotInstanceOf(exp: Exp, typ: TypeAnno) extends CoreCond {
    override def usedvars: Map[Name, Option[TypeAnno]] = exp.usedvars
    override def prettyprint(implicit indent: String): String =
      s"${exp.prettyprint} notInstanceOf ${typ.prettyprint}"
  }
  case class Def(exp: Exp) extends CoreCond {
    override def usedvars: Map[Name, Option[TypeAnno]] = exp.usedvars
    override def prettyprint(implicit indent: String): String =
      s"def ${exp.prettyprint}"
  }
  case class Undef(exp: Exp) extends CoreCond {
    override def usedvars: Map[Name, Option[TypeAnno]] = exp.usedvars
    override def prettyprint(implicit indent: String): String =
      s"undef ${exp.prettyprint}"
  }
  case class BooleanCond(v: Boolean) extends CoreCond {
    override def usedvars: Map[Name, Option[TypeAnno]] = Map()
    override def prettyprint(implicit indent: String): String =
      v.toString
  }

  trait Typeable {
    var typ: Option[TypeAnno] = None
    def typed(ty: TypeAnno): this.type = {
      this.typ = Some(ty)
      this
    }
    def mtyped(ty: Option[TypeAnno]): this.type = {
      this.typ = ty
      this
    }
  }

  trait Exp extends Typeable {
    def usedvars: Map[Name, Option[TypeAnno]]
    def prettyprint(implicit indent: String): String
    def ensureCore: CoreExp = this match {
      case self: CoreExp => self
      case _ => throw new IllegalArgumentException(s"Core statement required but got $this")
    }
  }
  sealed trait CoreExp extends Exp
  case class Var(name: Name) extends CoreExp {
    override def usedvars: Map[Name, Option[TypeAnno]] = Map(name -> typ)
    override def prettyprint(implicit indent: String): String = name
  }
  case class Constant(lit: Literal) extends CoreExp {
    override def usedvars: Map[Name, Option[TypeAnno]] = Map()
    override def prettyprint(implicit indent: String): String = lit.prettyprint
  }
  case class PathAccess(receiver: Exp, link: Link) extends CoreExp {
    override def usedvars: Map[Name, Option[TypeAnno]] = receiver.usedvars
    override def prettyprint(implicit indent: String): String =
      s"${receiver.prettyprint}.${link.prettyprint}"
  }
  case class Call(name: Name, args: Seq[Exp], transitive: Boolean, count: Boolean) extends CoreExp {
    override def usedvars: Map[Name, Option[TypeAnno]] = collectUsedvars(args)
    override def prettyprint(implicit indent: String): String = {
      val argsS = args.map(_.prettyprint).mkString(", ")
      val transS = if (transitive) "+" else ""
      val countS = if (count) "count " else ""
      s"$countS$name$transS($argsS)"
    }
  }
  case class Tuple(exps: Seq[Exp]) extends CoreExp {
    override def usedvars: Map[Name, Option[TypeAnno]] = collectUsedvars(exps)
    override def prettyprint(implicit indent: String): String =
      exps.map(_.prettyprint).mkString("(", ", ", ")")
  }

  sealed trait Link {
    def prettyprint: String
  }
  sealed trait CoreLink extends Link
  case class NamedLink(node: TNode, field: Name) extends CoreLink {
    override def prettyprint: String = field
  }
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
  case object SizeLink extends CoreLink {
    override def prettyprint: String = "size"
  }

  sealed trait Literal {
    def prettyprint: String
  }
  case object UnitLiteral extends Literal {
    override def prettyprint: String = "unit"
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

  def collectUsedvars(it: Iterable[{def usedvars: Map[Name, Option[TypeAnno]]}]): Map[Name, Option[TypeAnno]] =
    it.foldLeft(Map[Name, Option[TypeAnno]]())((m,i) => m ++ i.usedvars)
}
