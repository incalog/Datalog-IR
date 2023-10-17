package inca.frontend.functional.compile

import inca.frontend.functional.syntax.*
import inca.ir.Name
import inca.util.Gensym

import scala.collection.mutable.ListBuffer

/** Rewrites the program so that for each fold(init, op, set) the following holds
 *  1. set == Call(Var(setName), setArgs) for some setName and setArgs
 *  2. the fold construct occurs in its own function as to avoid duplicate aggregation
 */
class NormalizeFold extends Visitor:
  private val newFuns: ListBuffer[FunctionDef] = ListBuffer.empty
  private val gensym: Gensym = new Gensym(Iterable.empty)

  var funs: Map[Name, FunctionDef] = _

  override def visitModule(m: Module): Module =
    gensym.register(m.usedModuleNames.map(_.name))
    gensym.register(m.usedDefNames.map(_.name))
    m.content.foreach {
      case fun: FunctionDef => gensym.register(fun.vars.keys.map(_.name))
      case _ => // skip
    }
    funs = m.content.collect { case f: FunctionDef => f }.map(f => f.name -> f).toMap

    val mm = super.visitModule(m)
    mm.copy(content = mm.content ++ newFuns)

  override def visitExp(e: Expression): Expression = e match
    case SetFold(anno, init, op, set) =>
      val collectCall = generateFunctionCall(set, "FoldCollectSet")
      val newFold = SetFold(anno, init, op, collectCall).typed(e.typ.get)
      generateFunctionCall(newFold, "FoldAggregate")
    case _ => super.visitExp(e)


  def generateFunctionCall(exp: Expression, basename: String): Call = exp match
    case c: Call => c
    case _ =>
      val name = Name(gensym.freshGlobal(basename))
      val vars = exp.freevars.distinct
      val params = vars.map(v => Param(v.name, v.typ.getOrElse(typeFail(exp))))
      val outType = exp.typ.getOrElse(typeFail(exp))
      val f = FunctionDef(Seq(), None, name, Seq(), params, outType, exp)
      newFuns += f
      val fref = Var(name).typed(f.funType)
      fref.target = Some(f)
      Call(fref, Seq(), params.map(p => Var(p.name).typed(p.typ))).typed(outType)

  def typeFail(exp: Expression): Nothing =
    throw new IllegalArgumentException(s"Cannot compile untyped expression $exp")