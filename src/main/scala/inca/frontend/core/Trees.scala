package inca.frontend.core

import inca.util.Meta
import inca.util.Meta.Scala
import truechange.LitType

import scala.meta.Term

trait Trees extends Syntax {
  override type Name = tree.Name

  override type Module = tree.Module
  override type Import = tree.Import
  override type ModuleContent = tree.ModuleContent
  override type Visibility = tree.Visibility
  override type Param = tree.Param

  override type Expression = tree.Expression
  override type CoreExpression = tree.CoreExpression
  override type Call = tree.Call
  override type Eval = tree.Eval
  override type Literal = tree.Literal

  override type Body = tree.Body
  override type Statement = tree.Statement
  override type CoreStatement = tree.CoreStatement

  override type Link = tree.Link
  override type CoreLink = tree.CoreLink

  override type Type = tree.Type
  override type TNode = tree.TNode
  override type TLinked = tree.TLinked
  override type TIterable = tree.TIterable


  override def Name(name: String): Name = tree.Name(name)

  override def Module(name: Name, imports: Seq[Import], content: Seq[ModuleContent]): Module = tree.Module(name, imports, content)

  override def Import(name: Name): Import = tree.Import(name)

  override def ScalaModuleContent[T <: meta.Stat](t: Scala[T]): ModuleContent = tree.ScalaModuleContent(t)

  override def ValDef(vis: Option[Visibility], name: Name, typ: Option[Type], exp: Expression): ModuleContent =
    tree.ValDef(vis, name, typ, exp)

  override def PatternFunction(vis: Option[Visibility], name: Name, params: Seq[Param], outType: Type, bodies: Seq[Body]): ModuleContent =
    tree.PatternFunction(vis, name, params, outType, bodies)

  override def Param(name: Name, typ: Type): Param = tree.Param(name, typ)

  override def TAny: Type = tree.TAny

  override def TNothing: Type = tree.TNothing

  override def TLiteral(litType: LitType): Type = tree.TLiteral(litType)

  override def TAnyLinked: TLinked = tree.TAnyLinked

  override def TNode(name: String): TNode = tree.TNode(name)

  override def TList(contained: TLinked): TLinked with TIterable = tree.TList(contained)

  override def TEnumeration(contained: Type): TIterable = tree.TEnumeration(contained)

  override def TTuple(ts: Seq[Type]): Type = tree.TTuple(ts)

  override def TScala(ty: Meta.Scala[meta.Type]): Type = tree.TScala(ty)

  override def Private: tree.Visibility = tree.Private

  override def Var(name: tree.Name): CoreExpression = tree.Var(name)

  override def Eq(lhs: Expression, rhs: Expression): CoreExpression = tree.Eq(lhs, rhs)

  override def Neq(lhs: Expression, rhs: Expression): CoreExpression = tree.Neq(lhs, rhs)

  override def InstanceOf(exp: Expression, ty: Type): CoreExpression = tree.InstanceOf(exp, ty)

  override def NotInstanceOf(exp: Expression, ty: Type): CoreExpression = tree.NotInstanceOf(exp, ty)

  override def Cast(src: Expression, targetTyp: Type): CoreExpression = tree.Cast(src, targetTyp)

  override def Def(exp: Expression): CoreExpression = tree.Def(exp)

  override def Undef(exp: Expression): CoreExpression = tree.Undef(exp)

  override def Wildcard: CoreExpression = tree.Wildcard

  override def Constant(lit: Literal): CoreExpression = tree.Constant(lit)

  override def PathAccess(receiver: Expression, link: Link): CoreExpression = tree.PathAccess(receiver, link)

  override def Call(name: tree.Name, args: Seq[Expression], transitive: Boolean): Call = tree.Call(name, args, transitive)

  override def Count(call: Call): CoreExpression = tree.Count(call)

  override def Tuple(exps: Seq[Expression]): CoreExpression = tree.Tuple(exps)

  override def Eval(code: Meta.Scala[Term]): Eval = tree.Eval(code)

  override def Aggregate(agg: Expression, bodies: Seq[Body]): CoreExpression = tree.Aggregate(agg, bodies)

  override def UnitLiteral: Literal = tree.UnitLiteral

  override def BooleanLiteral(v: Boolean): Literal = tree.BooleanLiteral(v)

  override def IntLiteral(v: Int): Literal = tree.IntLiteral(v)

  override def LongLiteral(v: Long): Literal = tree.LongLiteral(v)

  override def DoubleLiteral(v: Double): Literal = tree.DoubleLiteral(v)

  override def StringLiteral(v: String): Literal = tree.StringLiteral(v)

  override def Body(ss: Seq[Statement]): Body = tree.Body(ss)

  override def Values(name: tree.Name, typ: Type): CoreStatement = tree.Values(name, typ)

  override def Assign(names: Seq[tree.Name], exp: Expression): CoreStatement = tree.Assign(names, exp)

  override def Assert(cond: Expression): CoreStatement = tree.Assert(cond)

  override def Yield(exp: Expression): CoreStatement = tree.Yield(exp)

  override def FailStatement: CoreStatement = tree.FailStatement

  override def NamedLink(field: tree.Name): CoreLink = tree.NamedLink(field)

  override def ParentLink: CoreLink = tree.ParentLink

  override def ChildrenLink: CoreLink = tree.ChildrenLink

  override def NextLink: CoreLink = tree.NextLink

  override def PreviousLink: CoreLink = tree.PreviousLink

  override def SizeLink: CoreLink = tree.SizeLink
}
