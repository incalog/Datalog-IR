package inca.frontend.core

import inca.frontend.parser.SourceLocation
import inca.util.Meta.Scala
import truechange.{JavaLitType, LitType}

trait Syntax {
  type Name <: SourceLocation

  type Module <: SourceLocation
  type Import <: SourceLocation
  type ModuleContent <: SourceLocation
  type Visibility <: SourceLocation
  type Param <: SourceLocation
  type AnnoParam <: SourceLocation

  type Expression <: SourceLocation
  type CoreExpression <: Expression
  type Call <: CoreExpression
  type Eval <: CoreExpression
  type Literal <: SourceLocation

  type Body <: SourceLocation
  type Statement <: SourceLocation
  type CoreStatement <: Statement

  type Link <: SourceLocation
  type CoreLink <: Link

  type Type <: SourceLocation
  type TLinked <: Type
  type TNode <: TLinked
  type TIterable <: Type

  def Name(name: String): Name

  def Module(name: Name, imports: Seq[Import], content: Seq[ModuleContent]): Module
  def Import(name: Name): Import

  def Private: Visibility

  def ScalaModuleContent[T <: meta.Stat](imp: Scala[T]): ModuleContent

  def ValDef(vis: Option[Visibility], name: Name, typ: Option[Type], exp: Expression): ModuleContent
  def PatternFunction(vis: Option[Visibility], name: Name, params: Seq[Param], outType: Type, bodies: Seq[Body]): ModuleContent

  def Param(name: Name, typ: Type): Param

  /* TYPES */
  def TAny: Type
  def TNothing: Type
  def TLiteral(litType: LitType): Type
  object TLiteral {
    val Bool: Type = TLiteral(JavaLitType(classOf[java.lang.Boolean]))
    val Int: Type = TLiteral(JavaLitType(classOf[java.lang.Integer]))
    val Long: Type = TLiteral(JavaLitType(classOf[java.lang.Long]))
    val Double: Type = TLiteral(JavaLitType(classOf[java.lang.Double]))
    val String: Type = TLiteral(JavaLitType(classOf[java.lang.String]))
  }

  def TAnyLinked: TLinked
  def TNode(name: String): TNode
  def TList(contained: TLinked): TLinked with TIterable
  def TEnumeration(contained: Type): TIterable
  def TTuple(ts: Seq[Type]): Type
  def TScala(ty: Scala[meta.Type]): Type
  object TScala {
    def apply(typeString: String): Type = {
      import meta.parsers._
      TScala(Scala(typeString.parse[meta.Type].get))
    }
  }
  import scala.meta.quasiquotes._
  def TScalaBoolean: Type = TScala(Scala(t"Boolean"))
  def TScalaInt: Type = TScala(Scala(t"Int"))
  def TScalaLong: Type = TScala(Scala(t"Long"))
  def TScalaDouble: Type = TScala(Scala(t"Double"))
  def TScalaString: Type = TScala(Scala(t"String"))
  def TScalaAny: Type = TScala(Scala(t"Any"))


  /* EXPRESSIONS */
  def Var(name: Name): CoreExpression
  def Eq(lhs: Expression, rhs: Expression): CoreExpression
  def Neq(lhs: Expression, rhs: Expression): CoreExpression
  def InstanceOf(exp: Expression, ty: Type): CoreExpression
  def NotInstanceOf(exp: Expression, ty: Type): CoreExpression
  def Cast(src: Expression, targetTyp: Type): CoreExpression
  def Def(exp: Expression): CoreExpression
  def Undef(exp: Expression): CoreExpression
  def Wildcard: CoreExpression
  def Constant(lit: Literal): CoreExpression
  def PathAccess(receiver: Expression, link: Link): CoreExpression
  def Call(name: Name, args: Seq[Expression], transitive: Boolean = false): Call
  def Count(call: Call): CoreExpression
  def Tuple(exps: Seq[Expression]): CoreExpression
  def Eval(code: Scala[meta.Term]): Eval
  def Aggregate(agg: Expression, bodies: Seq[Body]): CoreExpression
  object Aggregate {
    def apply(agg: Expression, call: Call): CoreExpression = Aggregate(agg, Seq(Body(Seq(Yield(call)))))
  }

  def UnitLiteral: Literal
  def BooleanLiteral(v: Boolean): Literal
  def IntLiteral(v: Int): Literal
  def LongLiteral(v: Long): Literal
  def DoubleLiteral(v: Double): Literal
  def StringLiteral(v: String): Literal

  /* STATEMENTS */
  def Body(ss: Seq[Statement]): Body
  def Values(name: Name, typ: Type): CoreStatement
  def Assign(names: Seq[Name], exp: Expression): CoreStatement
  def Assert(cond: Expression): CoreStatement
  def Yield(exp: Expression): CoreStatement
  def FailStatement: CoreStatement

  /* LINKS */
  def NamedLink(field: Name): CoreLink
  def ParentLink: CoreLink
  def ChildrenLink: CoreLink
  def NextLink: CoreLink
  def PreviousLink: CoreLink
  def SizeLink: CoreLink
}
