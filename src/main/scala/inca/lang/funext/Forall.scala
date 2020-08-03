package inca.lang.funext

import inca.lang.fun.Fun._
import inca.lang.funext.desugar.{DesugarTrans, Desugarable}
import inca.util.Gensym
import inca.util.Meta.TAB

import scala.collection.mutable.ListBuffer

case class Forall(name: Name, exp: Exp, body: Seq[Statement]) extends Statement {
  val elemTyp: Option[TLinked] = exp.typ.flatMap {
    case ty: TIterable => Some(ty.contained)
    case _ => None
  }
  override def usedvars: Map[Name, Option[TypeAnno]] = Map(name -> elemTyp) ++ exp.usedvars ++ collectUsedvars(body)

  override def prettyprint(implicit indent: String): String = {
    val bodyS = if (body.isEmpty) "" else
      "\n" + body.map(_.prettyprint(indent+TAB)).mkString("\n")
    s"""${indent}forall $name in ${exp.prettyprint} {$bodyS
       |${indent}}""".stripMargin
  }
}

object Forall extends Desugarable {

  override val desugarsTo: Seq[Desugarable] = Seq(Foreach)

  override def trans(): DesugarTrans = new DesugarTrans {
    var forallFuns: ListBuffer[PatternFunction] = ListBuffer()

    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = stm match {
      case Forall(name, exp, body) => exp.typ.getOrElse(throw new IllegalArgumentException(s"Cannot support forall loop over untyped expression $exp")) match {
        case TList(ty) =>
          val funsym = gensym.fresh("forallFun")
          val sizeSym = gensym.fresh("listSize")
          val successSym = gensym.fresh("successSize")

          val vars = exp.usedvars.toSeq ++ (collectUsedvars(body) - name)
          val conditions = body.flatMap(desugarStm)
          forallFuns += PatternFunction(None, funsym, vars.map(v => Param(v._1, v._2)), Seq(AnnoParam(None, ty)), Seq(
            Body(Seq(
              Foreach(name, exp,
                conditions :+ Yield(Var(name))
              )
            ))
          ))

          changed(Seq(
            Assign(Seq(sizeSym), PathAccess(exp, SizeLink).typed(TInt)),
            Assign(Seq(successSym), Call(funsym, vars.map(v => Var(v._1)), transitive = false, count = true)),
            Assert(Eq(Var(sizeSym), Var(successSym)))
          ))
        case ty => throw new IllegalArgumentException(s"Forall loop expression $exp must have iterable type, but was type $ty")
      }

      case _ => super.desugarStm(stm)
    }

    override def desugarFun(fun: PatternFunction)(implicit gensym: Gensym): Seq[PatternFunction] = {
      val desugared = super.desugarFun(fun)
      if (forallFuns.isEmpty)
        desugared
      else {
        val prepend = forallFuns.toSeq
        forallFuns = ListBuffer()
        prepend ++ desugared
      }
    }
  }
}

