package inca.ir.analysis

import inca.ir.Name
import inca.ir
import inca.ir.analysis.base.effect
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, CSupplementaryTable, FixIn, FixOut}
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.*
import inca.ir.extension.arithmetic.analysis as arith
import sturdy.data.MayJoin.NoJoin
import sturdy.data.{NoJoin, finiteUnit}
import sturdy.effect.except.JoinedExcept
import sturdy.effect.failure.{CollectedFailures, Failure}
import sturdy.effect.store.{CStore, Store}
import sturdy.effect.{EffectStack, TrySturdy}
import sturdy.fix
import sturdy.fix.StackConfig.StackedStates
import sturdy.fix.{context, *}
import sturdy.fix.context.{ParameterSensitivity, Sensitivity, full, CallString as ContextCallString}
import sturdy.values.MaybeChanged.Unchanged
import sturdy.values.booleans.{BooleanOps, ConcreteBooleanOps}
import sturdy.values.exceptions.Exceptional
import sturdy.values.ordering.EqOps
import sturdy.values.references.{AllocationSiteAddr, given_Finite_AllocationSiteAddr}
import sturdy.values.*
import sturdy.fix.context.FiniteCallString

// Implicits
import sturdy.data.given
import sturdy.values.given
import inca.ir.analysis.base.effect.IRFailure
import inca.ir.analysis.base.interpreter.{FiniteFixIn, FiniteFixOut}
import inca.ir.analysis.base.values.FiniteCRelationValue
import sturdy.data.MakeJoined


class IRConcreteInterpreter extends BaseGenericInterpreter[Value, Boolean, CRelationValue[Value], NoJoin]
  with arith.interpreter.ConcreteInterpreter:

  type RV = CRelationValue[Value]

  given CCombineFixOut: Combine[FixOut[Value, RV], Widening.No] with
    override def apply(out1: FixOut[Value, RV], out2: FixOut[Value, RV]): MaybeChanged[FixOut[Value, RV]] = (out1, out2) match
      case (FixOut.Term(vs1), FixOut.Term(vs2)) =>
        val combined = (vs1 ++ vs2).distinct
        if (vs1 != combined)
          Changed(FixOut.Term(combined))
        else
          Unchanged(FixOut.Term(combined))
      case (FixOut.Atom(), FixOut.Atom()) => Unchanged(FixOut.Atom())
      case (FixOut.ExitCall(rv1), FixOut.ExitCall(rv2)) =>
        val combined = relationOps.union(rv1, rv2)
        if (combined != rv1)
          Changed(FixOut.ExitCall(combined))
        else
          Unchanged(FixOut.ExitCall(combined))
      case (FixOut.Body(rv1), FixOut.Body(rv2)) =>
        val combined = relationOps.union(rv1, rv2)
        if (combined != rv1)
          Changed(FixOut.Body(combined))
        else
          Unchanged(FixOut.Body(combined))
      case (FixOut.Relation(rv1), FixOut.Relation(rv2)) =>
        val combined = relationOps.union(rv1, rv2)
        if (combined != rv1)
          Changed(FixOut.Relation(combined))
        else
          Unchanged(FixOut.Relation(combined))
      case (FixOut.ExtensionalRelation(rv1), FixOut.ExtensionalRelation(rv2)) =>
        val combined = relationOps.union(rv1, rv2)
        if (combined != rv1)
          Changed(FixOut.Relation(combined))
        else
          Unchanged(FixOut.Relation(combined))
      case (FixOut.Module(idb1), FixOut.Module(idb2)) =>
        val allKeys = idb1.keys ++ idb2.keys
        val res = for (k <- allKeys) yield
          (idb1.get(k), idb2.get(k)) match
            case (Some(rv1), Some(rv2)) => k -> relationOps.union(rv1, rv2)
            case (Some(rv1), _) => k -> rv1
            case (_, Some(rv2)) => k -> rv2
            case _ => throw IllegalStateException(s"IDB key not found: $k")
        val idb = res.toMap
        if (idb != idb1) {
          Changed(FixOut.Module(idb))
        } else {
          Unchanged(FixOut.Module(idb))
        }

      case _ => throw new IllegalArgumentException(s"Cannot combine outputs of different kind, $out1 and $out2")

  override lazy val failure: CollectedFailures[effect.BaseIRFailure] = new CollectedFailures

  given Failure = failure

  override val boolOps: BooleanOps[Boolean] = implicitly
  //override val boolTop: Boolean = true

  given BooleanOps[Boolean] = boolOps

  override val eqOps: EqOps[Value, Boolean] = new EqOps[Value, Boolean] {
    def equ(v1: Value, v2: Value): Boolean = v1 == v2
    def neq(v1: Value, v2: Value): Boolean = v1 != v2
  }

  override val joinV: NoJoin[Value] = implicitly
  override val joinRV: NoJoin[RV] = implicitly
  override val joinUnit: NoJoin[Unit] = implicitly

  override lazy val supplementaryTable: CSupplementaryTable = new CSupplementaryTable
  override lazy val IDB: Store[AllocationSiteAddr, RV, NoJoin] = CStore[AllocationSiteAddr, RV](Map())
  // lazy is important because of cyclic implicits
  //override lazy val effects: EffectStack = EffectStack(supplementaryTable, failure, IDB)

  //override val top: Value = Top

  given EqOps[Value, Boolean] = eqOps

  override val relationOps: RelationOps[Value, Boolean, RV] = new CRelationValueOps[Value, Boolean] {}


  class PrintLogger extends Logger[FixIn, FixOut[Value, RV]]:
    var indent: Int = -1

    def printlnWithIndent(msg: String): Unit =
      val indentS = "    ".repeat(indent)
      println(s"$indentS$msg")

    override def enter(dom: FixIn): Unit =
      dom match
        case FixIn.Term(term) =>
        case FixIn.Atom(atom) => printlnWithIndent(s"Enter atom: $atom")
        case FixIn.EnterCall(_, _, _, _) => printlnWithIndent(s"Enter call: $dom")
        case FixIn.Body(body) => printlnWithIndent(s"Enter body: ${body.hashCode()}")
        case FixIn.Relation(rel) => printlnWithIndent(s"Enter relation: ${rel.name}")
        case FixIn.ExtensionalRelation(rel) =>
        case FixIn.Module(mod) =>
      indent += 1

    override def exit(dom: FixIn, codom: TrySturdy[FixOut[Value, RV]]): Unit =
      indent -= 1
      (dom, codom) match // getOrThrow
        //case (_, FixOut.Term(_)) =>

        case (FixIn.Atom(atom), _) => printlnWithIndent(s"Exit atom: $atom\tResult: $codom")
        case (FixIn.EnterCall(_, _, _, _), _) =>
          printlnWithIndent(s"Exit call: $dom\tResult: $codom")
        case (FixIn.Body(body), _) => printlnWithIndent(s"Exit body: ${body.hashCode()}\tResult: $codom")
        case (FixIn.Relation(rel), _) => printlnWithIndent(s"Exit relation: ${rel.name}\tResult: $codom")
        case (FixIn.Module(mod), _) =>
          println(s"Module: ${codom.getOrThrow}")
        //println(s"Exit:\n$dom\nResult: $codom")
        case _ => // nothing

  override val fixpoint: EffectStack ?=> Fixpoint[FixIn, FixOut[Value, RV]] =
    type CallString = ContextCallString[(ir.Name, Seq[ir.Arg], Boolean)]

    val fixpt = new ContextualFixpoint[FixIn, FixOut[Value, RV]] {
      override type Ctx = CallString

      // TODO: How can we make this 1-context-sensitive? Or is it?
      override protected def context: Sensitivity[FixIn, Ctx] = new Sensitivity[FixIn, Ctx] {
        def emptyContext: Ctx = null.asInstanceOf[Ctx]
        def switchCall(dom: FixIn): Boolean = true
        override def apply(dom: FixIn): Ctx = dom match
          case FixIn.EnterCall(r, params, args, neg) =>
            ContextCallString(Seq((r.name, args, neg)))
          case _ => ContextCallString(Seq())
      }

      // TODO: Not sure what this does
      override protected def contextFree: Combinator[FixIn, FixOut[Value, RV]] => Combinator[FixIn, FixOut[Value, RV]] = f => f

      override protected def contextSensitive: Contextual[Ctx, FixIn, FixOut[Value, RV]] ?=> Combinator[FixIn, FixOut[Value, RV]] =
        fix.iter.innermost(StackedStates())
    }

    fixpt.addContextSensitiveLogger(new PrintLogger())
    fixpt

