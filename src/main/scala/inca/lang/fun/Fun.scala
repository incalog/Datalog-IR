package inca.lang.fun

object Fun {
  trait TypeAnno
  case object TBool extends TypeAnno
  case object TInt extends TypeAnno
  case object TLong extends TypeAnno
  case object TDouble extends TypeAnno
  case object TString extends TypeAnno

  trait TLinked extends TypeAnno
  case object TAnyLinked extends TLinked
  case class TNode(name: String) extends TLinked {
    def apply(field: String): NamedLink = NamedLink(this, field)
  }
  case class TList(contained: TLinked) extends TLinked

  type Name = String

  sealed trait Visibility
  case object Private extends Visibility
  case object Public extends Visibility

  case class Module(name: Name, imports: Seq[Name], funs: Seq[PatternFunction])
  case class PatternFunction(vis: Option[Visibility], name: Name, params: Seq[Param], outParams: Seq[AnnoParam], bodies: Seq[Body])

  case class Param(name: Name, typ: Option[TypeAnno])
  case class AnnoParam(name: Option[Name], typ: TypeAnno)

  case class Body(stmts: Seq[Statement])

  trait Statement
  case class Assignment(names: Seq[Name], exp: Exp) extends Statement
  case class Assert(cond: Cond) extends Statement
  case class Return(exp: Exp) extends Statement

  trait Cond
  case class Eq(lhs: Exp, rhs: Exp) extends Cond
  case class Neq(lhs: Exp, rhs: Exp) extends Cond
  case class InstanceOf(exp: Exp, typ: TypeAnno) extends Cond
  case class NotInstanceOf(exp: Exp, typ: TypeAnno) extends Cond
  case class Def(exp: Exp) extends Cond
  case class Undef(exp: Exp) extends Cond

  trait Exp {
    var typ: TypeAnno = _
    def typed(ty: TypeAnno): this.type = {
      this.typ = ty
      this
    }
  }
  case class Var(name: Name) extends Exp
  case class Constant(lit: Literal) extends Exp
  case class PathAccess(receiver: Exp, link: Link) extends Exp
  case class Call(name: Name, args: Seq[Exp], transitive: Boolean, count: Boolean) extends Exp
  case class Tuple(exps: Seq[Exp]) extends Exp
  object PathAccess {
    def apply(receiver: Exp, links: Seq[Link]): Exp =
      links.foldRight(receiver)((link,exp) => PathAccess(exp, link))
  }

  sealed trait Link
  case object ParentLink extends Link
  case object ChildrenLink extends Link
  case object NextLink extends Link
  case object PreviousLink extends Link
  case class NamedLink(node: TNode, field: Name) extends Link

  sealed trait Literal
  case class BooleanLiteral(v: Boolean) extends Literal
  case class IntLiteral(v: Int) extends Literal
  case class LongLiteral(v: Long) extends Literal
  case class DoubleLiteral(v: Double) extends Literal
  case class StringLiteral(v: String) extends Literal
}
