package inca.ir.extension.data.analysis.interpreter

import inca.ir.analysis.base.effect.{AtomFailed, BaseIRException, BaseIRFailure}
import inca.ir.analysis.base.values.{ARelationValue, BaseJoinV, CRelationValue, Top, VBool, Value}
import sturdy.effect.{Effect, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.values.{Powerset, Topped}
import sturdy.data.MayJoin.{NoJoin, WithJoin}
import sturdy.effect.except.Except
import sturdy.values.ordering.EqOps

case object InvalidDeconstruct extends BaseIRFailure

case class CDataV(dataName: String, caseName: String, args: Seq[Value]) extends Value:
  override def toString: String = s"$caseName${args.mkString("(", ", ", ")")}"

private class CDataVOps(using failure: Failure, except: Except[BaseIRException, Powerset[BaseIRException], WithJoin], eqOps: EqOps[Value, Boolean]) extends DataOps[Value, Powerset[BaseIRException]]:
  override def construct(dataName: String, caseName: String, args: Seq[Value]): Value = CDataV(dataName, caseName, args)

  override def deconstruct(v: Value, dataName: String, caseName: String, args: Seq[Option[Value]]): Seq[Value] = v match
    case CDataV(`dataName`, `caseName`, cArgs) =>
      val argsMatch = cArgs.zip(args).forall {
        case (v1, None) => true // arg will be bound
        case (v1, Some(v2)) =>
          println(s"$v1 == $v2 :: ${eqOps.equ(v1, v2)}")
          eqOps.equ(v1, v2)
      }

      if (!argsMatch)
        except.throws(AtomFailed(s"Deconstruct argument mismatch: $v = ?$caseName${args.mkString("(", ", ", ")") }"))

      // provide values for all argument positions
      cArgs
    case CDataV(dName, cName, cArgs) =>
      except.throws(AtomFailed(s"Deconstruct case mismatch: $v = ?$caseName${args.mkString("(", ", ", ")") }"))


trait ConcreteInterpreter extends GenericInterpreter[Value, Boolean, CRelationValue[Value], Powerset[BaseIRException], NoJoin]:
  val dataOps: DataOps[Value, Powerset[BaseIRException]] = CDataVOps(using failure, except, eqOps)
