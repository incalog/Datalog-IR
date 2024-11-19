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
import sturdy.effect.store.{AStoreThreaded, CStore, Store}
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
//import sturdy.fix.context.FiniteCallString

// Implicits
import sturdy.data.given
import sturdy.values.given
import inca.ir.analysis.base.effect.IRFailure
import inca.ir.analysis.base.interpreter.FiniteFixIn
import inca.ir.analysis.base.values.JoinCRV
import sturdy.data.MakeJoined


type CallString = ContextCallString[(ir.Name, Seq[ir.Arg], Boolean)]

/*class PrintLogger(contextual: Contextual[CallString, FixIn, FixOut[Value, CRV]])
  extends Logger[FixIn, FixOut[Value, CRV]]:

  var indent: Int = -1

  def printlnWithIndent(msg: String): Unit =
    val callS = contextual.getCurrentContext
    val indentS = "    ".repeat(indent)
    println(s"$indentS$callS -> $msg")

  override def enter(dom: FixIn): Unit =
    dom match
      case FixIn.Term(term) =>
      case FixIn.Atom(atom) => printlnWithIndent(s"Enter atom: $atom")
      case FixIn.EnterCall(_, _, _, _) => printlnWithIndent(s"Enter call: $dom")
      case FixIn.Body(body) => printlnWithIndent(s"Enter body: ${body.hashCode()}")
      case FixIn.Relation(rel) => printlnWithIndent(s"Enter relation: ${rel.name}")
      case _ =>

    indent += 1

  override def exit(dom: FixIn, codom: TrySturdy[FixOut[Value, CRV]]): Unit =
    indent -= 1
    (dom, codom) match // getOrThrow
      case (FixIn.Atom(atom), _) => printlnWithIndent(s"Exit atom: $atom\tResult: $codom")
      case (FixIn.EnterCall(_, _, _, _), _) =>
        printlnWithIndent(s"Exit call: $dom\tResult: $codom")
      case (FixIn.Body(body), _) => printlnWithIndent(s"Exit body: ${body.hashCode()}\tResult: $codom")
      case (FixIn.Relation(rel), _) => printlnWithIndent(s"Exit relation: ${rel.name}\tResult: $codom")
      case _ => // nothing*/

given CCombineFixOut[W <: Widening]: Combine[FixOut[Value, CRelationValue[Value]], W] with
  override def apply(out1: FixOut[Value, CRelationValue[Value]], out2: FixOut[Value, CRelationValue[Value]]): MaybeChanged[FixOut[Value, CRelationValue[Value]]] = 
    (out1, out2) match
      case (FixOut.Term(rv1), FixOut.Term(rv2)) => MaybeChanged(FixOut.Term(rv1.union(rv2)), out1)
      case (FixOut.Atom(), FixOut.Atom()) => Unchanged(FixOut.Atom())
      case (FixOut.ExitCall(rv1), FixOut.ExitCall(rv2)) => MaybeChanged(FixOut.ExitCall(rv1.union(rv2)), out1)
      case (FixOut.Body(rv1), FixOut.Body(rv2)) => MaybeChanged(FixOut.Body(rv1.union(rv2)), out1)
      case (FixOut.Relation(rv1), FixOut.Relation(rv2)) => MaybeChanged(FixOut.Relation(rv1.union(rv2)), out1)
      case (FixOut.ExtensionalRelation(rv1), FixOut.ExtensionalRelation(rv2)) => MaybeChanged(FixOut.ExtensionalRelation(rv1.union(rv2)), out1)
      case (FixOut.Module(), FixOut.Module()) => Unchanged(FixOut.Module())
      case _ => throw new IllegalArgumentException(s"Cannot combine outputs of different kind, $out1 and $out2")


class IRConcreteInterpreter extends BaseGenericInterpreter[Value, Boolean, CRelationValue[Value], NoJoin]
  with arith.interpreter.ConcreteInterpreter:

  type CRV = CRelationValue[Value]

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
  override val joinRV: Join[CRV] = implicitly
  
  // Only correct for concrete Datalog interpreter
  given Widen[CRV] with {
    override def apply(v1: CRV, v2: CRV): MaybeChanged[CRV] = joinRV(v1, v2)
  }
  
  override val joinUnit: NoJoin[Unit] = implicitly

  override lazy val supplementaryTable: CSupplementaryTable = new CSupplementaryTable
  override lazy val idb: AStoreThreaded[AllocationSiteAddr, AllocationSiteAddr, CRV] = AStoreThreaded[AllocationSiteAddr, AllocationSiteAddr, CRV](Map())

  //override val top: Value = Top

  given EqOps[Value, Boolean] = eqOps

  override val relationOps: RelationOps[Value, Boolean, CRV] = new CRelationValueOps[Value]


  Fixpoint.DEBUG = true

  override val fixpoint: EffectStack ?=> Fixpoint[FixIn, FixOut[Value, CRV]] =
    fix.notContextSensitive[FixIn, FixOut[Value, CRV], fix.Combinator[FixIn, FixOut[Value, CRV]]](
      fix.iter.innermost[FixIn, FixOut[Value, CRV], Unit](StackedStates())
    ).fixpoint

    /*new ContextInsensitiveFixpoint[FixIn, FixOut[Value, CRV]] {
      override protected def contextInsensitive: Contextual[Unit, FixIn, FixOut[Value, CRV]] ?=> Combinator[FixIn, FixOut[Value, CRV]] =
        fix.iter.innermost(StackedStates())
    }*/


    /*val fixpt = new ContextualFixpoint[FixIn, FixOut[Value, CRV]] {
      override type Ctx = CallString

      // 1-context-sensitive should be enough.
      override protected def context: Sensitivity[FixIn, Ctx] = new Sensitivity[FixIn, Ctx] {
        def emptyContext: Ctx = null.asInstanceOf[Ctx]
        def switchCall(dom: FixIn): Boolean = dom match
          case FixIn.EnterCall(r, params, args, neg) => true
          case _ => false
        override def apply(dom: FixIn): Ctx = dom match
          case FixIn.EnterCall(r, params, args, neg) =>
            ContextCallString(Seq((r.name, args, neg)))
          case _ => ContextCallString(Seq())
      }

      // TODO: Not sure what this does
      override protected def contextFree: Combinator[FixIn, FixOut[Value, CRV]] => Combinator[FixIn, FixOut[Value, CRV]] = f => f

      override protected def contextSensitive: Contextual[Ctx, FixIn, FixOut[Value, CRV]] ?=> Combinator[FixIn, FixOut[Value, CRV]] =
        fix.iter.innermost(StackedStates())
    }

    fixpt.addContextSensitiveLogger(contextual ?=> new PrintLogger(contextual))
    fixpt*/



