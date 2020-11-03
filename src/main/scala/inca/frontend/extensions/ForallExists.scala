package inca.frontend.extensions

import inca.frontend.Frontend
import inca.frontend.core._
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.frontend.typechecker.{NoTerminator, StmType}
import inca.util.Gensym
import inca.util.Meta.Scala

import scala.collection.mutable.ListBuffer
import scala.meta.{Term, XtensionQuasiquoteTerm}

case class Forall(name: Name, exp: Expression, body: Body) extends Statement {
  val elemTyp: Option[Type] = exp.typ.flatMap {
    case ty: TIterable => Some(ty.contained)
    case _ => None
  }

  override def boundVars: Set[Name] = Set(name) ++ body.boundVars
  override def allVars: Map[Name, Option[Type]] = Map(name -> elemTyp) ++ exp.freeVars ++ body.allVars

  override def prettyprint(implicit indent: String): String =
    s"${indent}forall $name in ${exp.prettyprint} ${body.prettyprint}"
}

case class Exists(name: Name, exp: Expression, body: Body) extends Statement {
  val elemTyp: Option[Type] = exp.typ.flatMap {
    case ty: TIterable => Some(ty.contained)
    case _ => None
  }

  override def boundVars: Set[Name] = Set(name) ++ body.boundVars
  override def allVars: Map[Name, Option[Type]] = Map(name -> elemTyp) ++ exp.freeVars ++ body.allVars

  override def prettyprint(implicit indent: String): String =
    s"${indent}exists $name in ${exp.prettyprint} ${body.prettyprint}"
}


/**
 * Extension adding "forallexists" statements to @see Parser.
 */
trait ForallExistsFrontend extends Frontend {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected def desugarables: Seq[Desugarable] = ForallExists +: super.desugarables

  override protected[frontend] def statement[_: P]: P[Statement] =
    P("forall " ~ identifier ~~ " " ~ "in " ~ exp ~ body).mapWithLoc(Forall.tupled) |
      P("exists " ~ identifier ~~ " " ~ "in " ~ exp ~ body).mapWithLoc(Exists.tupled) |
      super.statement

  override protected[frontend] def keywords: Set[String] = super.keywords ++ Seq("forall", "exists", "in")

  override protected def typecheckInternal(stm: Statement, mustTerminate: Boolean): StmType = stm match {
    case Forall(name, exp, body) =>
      val ety = typecheck(exp)
      val elemType = ety match {
        case it: TIterable => it.contained
        case _ =>
          error(s"Found $ety, but expected iterable type", exp)
          TAny
      }
      scopedTypeContext {
        bindVar(name, elemType)
        typecheck(body, mustTerminate = false)
      }
      NoTerminator

    case Exists(name, exp, body) =>
      val ety = typecheck(exp)
      val elemType = ety match {
        case it: TIterable => it.contained
        case _ =>
          error(s"Found $ety, but expected iterable type", exp)
          TAny
      }
      scopedTypeContext {
        bindVar(name, elemType)
        typecheck(body, mustTerminate = false)
      }
      NoTerminator

    case _ => super.typecheckInternal(stm, mustTerminate)
  }
}


object ForallExists extends Desugarable {

  override val desugarsTo: Seq[Desugarable] = Seq(Foreach)

  override def trans(): DesugarTrans = new DesugarTrans {
    val forallExistsFuns: ListBuffer[PatternFunction] = ListBuffer()

    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = stm match {
      case Forall(name, exp, body) => exp.typ.getOrElse(throw new IllegalArgumentException(s"Cannot support forall over untyped expression $exp")) match {
        case TList(ty) =>
          val funsym = Name(gensym.fresh("forallCond"))
          val sizeSym = Name(gensym.fresh("listSize"))
          val successSym = Name(gensym.fresh("successSize"))

          val vars = makeCondFun(name, exp, body, ty, funsym)
          val args = vars.map(v => Var(v._1))

          changed(Seq(
            Assign(Seq(sizeSym), PathAccess(exp, SizeLink).typed(TInt)),
            Assign(Seq(successSym), Count(Call(funsym, args).typed(ty)).typed(TInt)),
            Assert(Eq(Var(sizeSym), Var(successSym)))
          ))
        case ty => throw new IllegalArgumentException(s"Forall loop expression $exp must have iterable type, but was type $ty")
      }

      case Exists(name, exp, body) => exp.typ.getOrElse(throw new IllegalArgumentException(s"Cannot support exists over untyped expression $exp")) match {
        case TList(ty) =>
          val funsym = Name(gensym.fresh("existsCond"))
          val successSym = Name(gensym.fresh("successSize"))

          val vars = makeCondFun(name, exp, body, ty, funsym)
          val args = vars.map(v => Var(v._1))

          changed(Seq(
            Assign(Seq(successSym), Count(Call(funsym, args).typed(ty)).typed(TInt)),
            Assert(Eval(Seq(successSym), Scala(q"${Term.Name(successSym.name)} >= 1")).typed(TBool))
          ))
        case ty => throw new IllegalArgumentException(s"Forall loop expression $exp must have iterable type, but was type $ty")
      }

      case _ => super.desugarStm(stm)
    }

    private def makeCondFun(name: Name, exp: Expression, body: Body, ty: TLinked, funsym: Name)(implicit gensym: Gensym): Seq[(Name, Option[Type])] = {
      val newbody = Body(Seq(
        Foreach(name, exp,
          Body(body.stmts.flatMap(desugarStm) :+ Yield(Var(name)))
        )
      ))
      val vars = newbody.freeVars.toSeq
      val params = vars.map(v => Param(v._1, v._2.getOrElse(TAny)))
      val outParam = Seq(AnnoParam(None, ty))
      forallExistsFuns += PatternFunction(None, funsym, params, outParam, Seq(newbody))
      vars
    }

    override def desugarFun(fun: PatternFunction)(implicit gensym: Gensym): Seq[PatternFunction] = {
      val desugared = super.desugarFun(fun)
      if (forallExistsFuns.isEmpty)
        desugared
      else {
        val prepend = forallExistsFuns.toSeq
        forallExistsFuns.clear()
        prepend ++ desugared
      }
    }
  }
}

