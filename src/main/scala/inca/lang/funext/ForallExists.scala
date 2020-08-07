package inca.lang.funext

import inca.lang.fun.Fun._
import inca.lang.funext.desugar.{DesugarTrans, Desugarable}
import inca.util.Gensym

import scala.collection.mutable.ListBuffer

case class Forall(name: Name, exp: Exp, body: Body) extends Statement {
  val elemTyp: Option[TLinked] = exp.typ.flatMap {
    case ty: TIterable => Some(ty.contained)
    case _ => None
  }

  override def boundVars: Set[Name] = Set(name) ++ body.boundVars
  override def allVars: Map[Name, Option[TypeAnno]] = Map(name -> elemTyp) ++ exp.freeVars ++ body.allVars

  override def prettyprint(implicit indent: String): String =
    s"${indent}forall $name in ${exp.prettyprint} ${body.prettyprint}"
}

case class Exists(name: Name, exp: Exp, body: Body) extends Statement {
  val elemTyp: Option[TLinked] = exp.typ.flatMap {
    case ty: TIterable => Some(ty.contained)
    case _ => None
  }

  override def boundVars: Set[Name] = Set(name) ++ body.boundVars
  override def allVars: Map[Name, Option[TypeAnno]] = Map(name -> elemTyp) ++ exp.freeVars ++ body.allVars

  override def prettyprint(implicit indent: String): String =
    s"${indent}exists $name in ${exp.prettyprint} ${body.prettyprint}"
}


object ForallExists extends Desugarable {

  override val desugarsTo: Seq[Desugarable] = Seq(Foreach)

  override def trans(): DesugarTrans = new DesugarTrans {
    val forallExistsFuns: ListBuffer[PatternFunction] = ListBuffer()

    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = stm match {
      case Forall(name, exp, body) => exp.typ.getOrElse(throw new IllegalArgumentException(s"Cannot support forall over untyped expression $exp")) match {
        case TList(ty) =>
          val funsym = gensym.fresh("forallCond")
          val sizeSym = gensym.fresh("listSize")
          val successSym = gensym.fresh("successSize")

          val vars = makeCondFun(name, exp, body, ty, funsym)
          val args = vars.map(v => Var(v._1))

          changed(Seq(
            Assign(Seq(sizeSym), PathAccess(exp, SizeLink).typed(TInt)),
            Assign(Seq(successSym), Call(funsym, args, transitive = false, count = true).typed(TInt)),
            Assert(Eq(Var(sizeSym), Var(successSym)))
          ))
        case ty => throw new IllegalArgumentException(s"Forall loop expression $exp must have iterable type, but was type $ty")
      }

      case Exists(name, exp, body) => exp.typ.getOrElse(throw new IllegalArgumentException(s"Cannot support exists over untyped expression $exp")) match {
        case TList(ty) =>
          val funsym = gensym.fresh("existsCond")
          val successSym = gensym.fresh("successSize")

          val vars = makeCondFun(name, exp, body, ty, funsym)
          val args = vars.map(v => Var(v._1))

          changed(Seq(
            Assign(Seq(successSym), Call(funsym, args, transitive = false, count = true).typed(TInt)),
            Assert(Eval(Map(successSym -> Some(TInt)), TBool, s"""env.getValue("$successSym").asInstanceOf[Int] >= 1"""))
          ))
        case ty => throw new IllegalArgumentException(s"Forall loop expression $exp must have iterable type, but was type $ty")
      }

      case _ => super.desugarStm(stm)
    }

    private def makeCondFun(name: Name, exp: Exp, body: Body, ty: TLinked, funsym: String)(implicit gensym: Gensym): Seq[(Name, Option[TypeAnno])] = {
      val newbody = Body(Seq(
        Foreach(name, exp,
          Body(body.stmts.flatMap(desugarStm) :+ Yield(Var(name)))
        )
      ))
      val vars = newbody.freeVars.toSeq
      val params = vars.map(v => Param(v._1, v._2))
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

