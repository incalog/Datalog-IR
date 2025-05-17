package inca.ir.extension.aggregategeneric

import inca.ir
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.*
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.arithmetic.TInt
import inca.ir.extension.demand.TDemand
import inca.ir.extension.set.{SetMember, TSet}
import inca.ir.lowering.BaseLowering
import inca.ir.typing.Mode
import inca.util.Gensym

import scala.collection.mutable.ListBuffer

trait Lowering extends BaseLowering:
  override val name: String = "AggregateSet"
  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set(aggregate.IR, block.IR, arithmetic.IR)

  var newrels: ListBuffer[Relation] = ListBuffer.empty

  private var replaceWildcards: Boolean = false
  def replaceWildcards(ats: Seq[Atom]): Seq[Atom] = {
    val oldReplaceWildcards = replaceWildcards
    replaceWildcards = true
    val res = ats.flatMap(visitAtom)
    replaceWildcards = oldReplaceWildcards
    res
  }

  override def visitArg(arg: Arg): Seq[Arg] =
    if (replaceWildcards)
      arg match
        case w@WildcardArg() =>
          val t = Var(Name(gensym.fresh("_")))
          t.typed(w.typ.get.ty.binding)
          Seq(t.arg)
        case _ => Seq(arg)
    else
      super.visitArg(arg)

  var relGensym = Gensym()

  override def visitModule(module: Module): Module = preserveHints(module) {
    val m = gensym.scoped {
      gensym.register(module.relations.keys)
      super.visitModule(module)
    }
    m.copy(contents = m.contents ++ newrels)
  }

  override def visitTerm(term: Term): Seq[Term] = term match
    case AggregateGeneric(Some(agg), ats, op) =>
      val allVars = ats.flatMap(_.vars)
      val boundVars = allVars.filter(_.typ.exists(_.mode.isBound))
      val bindingVars = allVars.filter(_.typ.exists(_.mode.isBinding))
      val boundBefore = boundVars.diff(bindingVars).distinct

      val outName = Name(gensym.fresh("out"))
      val outParam = Param(outName, agg.typ.get.ty)

      val relName = Name(relGensym.fresh("genericAggregate"))
      val relParams = boundBefore.map(v => Param(v.name, v.typ.get.ty)) :+ outParam
      val relAts = ats.flatMap(visitAtom) ++ visitTerm(agg).map(Eq(Var(outName), _))
      newrels += Relation(relName, relParams, Seq(Body(relAts)))

      val aggOut = Name(gensym.fresh("out"))
      val args = boundBefore.map(v => Var(v.name).arg) :+ aggregate.AggregateColumnArg(Var(aggOut))
      Seq(block.Block(
        aggregate.Aggregate(relName, args, visitAggregationOperator(op)),
        Var(aggOut)
      ))
    case AggregateGeneric(None, ats, op@arithmetic.ArithmeticAggregationOperator.Count) =>
      val visitedAts = replaceWildcards(ats)
      val allVars = visitedAts.flatMap(_.vars).distinct

      val outName = Name(gensym.fresh("out"))
      val outParam = Param(outName, TInt)

      val relName = Name(relGensym.fresh("genericAggregate"))
      val relParams = allVars.map(v => Param(v.name, v.typ.get.ty)) :+ outParam
      val relAts = visitedAts :+ Eq(Var(outName), arithmetic.IntNum(1))
      newrels += Relation(relName, relParams, Seq(Body(relAts)))

      val aggOut = Name(gensym.fresh("out"))
      val args = allVars.map {
        case v if v.typ.get.mode.isBinding => WildcardArg()
        case v => Var(v.name).arg
      } :+ aggregate.AggregateColumnArg(Var(aggOut))
      Seq(block.Block(
        aggregate.Aggregate(relName, args, visitAggregationOperator(op)),
        Var(aggOut)
      ))
    case AggregateGeneric(_, _, op) =>
      throw IllegalStateException(s"Can not lower aggregation $op")
    case _ => super.visitTerm(term)
