package inca.ir.extension.data.analysis.interpreter

import inca.ir
import inca.ir.Atom
import inca.ir.analysis.base.effect.{AtomFailed, BaseIRException, InvalidBindings}
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import inca.ir.extension.data.{Construct, Deconstruct}
import sturdy.data.MayJoin
import sturdy.data.MayJoin.WithJoin
import sturdy.effect.except.Except
import sturdy.effect.failure.Failure
import sturdy.data.MakeJoined
import sturdy.data.CombineUnit

trait DataOps[V, ExcV](using failure: Failure, except: Except[BaseIRException, ExcV, WithJoin]):
  def construct(dataName: String, caseName: String, args: Seq[V]): V
  // Throw an exception if the deconstruct fails. Otherwise, provide values for each argument, even if it was already
  // bound before. `args` contains optionals, where `None` at position `i` indicates that the argument at position `i`
  // is unbound. For a positive call that means we should provide a value for it. For a negative call all arguments
  // are collapsing, and we should return an empty sequence.
  def deconstruct(v: V, dataName: String, caseName: String, args: Seq[Option[V]]): Seq[V]

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val dataOps: DataOps[V, ExcV]

  override def evalAtomOpen(at: Atom)(using rec: Fixed): Unit = at match
    case Deconstruct(t, caseRef, args, neg) =>
      // TODO: How can we clean this up?
      val caseDef = caseRef.target.get
      val dataName = caseDef.data.ref.name

      val tSup = evalTerm(t)
      val argsSup = args.map(evalArg)
      val destructColNames = argsSup.zip(args).map {
        case (Some(subCol), _) => subCol
        case (_, arg) => extractVarName(arg).get.name
      }

      supplementaryTable.update { sup =>
        val tix = relationOps.columnIndex(sup, tSup)
        val aix = argsSup.map(_.map(relationOps.columnIndex(sup, _)))

        var bindings: Option[RV] = None

        val filteredSup = relationOps.filter(sup) { row =>
          val termV = row(tix)
          val argsV = aix.map(_.map(row))

          var success = boolFalse

          except.tryCatch {
            println(row)
            println(s"$termV :: $argsV")
            val deconstrRes = dataOps.deconstruct(termV, dataName.name, caseDef.name.name, argsV)
            println(s"Res: $deconstrRes")

            if (!neg)
              if (deconstrRes.size != args.size)
                throw IllegalArgumentException(s"Deconstruct must provide a value for each argument")

              // we found new valid binding
              val newBinding = relationOps.make(
                destructColNames,
                Seq(deconstrRes)
              )
              bindings = bindings match
                case Some(bd) => Some(relationOps.union(bd, newBinding))
                case _ => Some(newBinding)

            success = boolOps.boolLit(!neg)
          } { exec =>
            println(s"Exec: $exec")
            success = boolOps.boolLit(neg)
          }

          success
        }

        // TODO: Is this correct? This should do whatever a call does to bind parameters.
        if (!neg && bindings.isDefined)
          relationOps.naturalJoin(filteredSup, bindings.get)
        else
          filteredSup
      }

      branchOps.boolBranch(relationOps.isEmpty(supplementaryTable.getTable)) {
        except.throws(AtomFailed("Destruct failed"))
      } { /* nothing */ }
    case _ => super.evalAtomOpen(at)

  override def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case Construct(caseRef, args) =>
      val caseDef = caseRef.target.get
      val dataName = caseDef.data.ref.name
      naryOp(args.map(evalTerm))(dataOps.construct(dataName.name, caseDef.name.name, _))
    case _ => super.evalTermOpen(term)
