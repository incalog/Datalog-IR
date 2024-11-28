package inca.ir.extension.data.analysis.interpreter

import inca.ir
import inca.ir.Atom
import inca.ir.analysis.base.effect.{AtomFailed, BaseIRException, InvalidBindings}
import inca.ir.analysis.base.interpreter.BaseGenericInterpreter
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
  def deconstruct(v: V, dataName: String, caseName: String, args: Seq[Option[V]], neg: Boolean): Seq[V]

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val dataOps: DataOps[V, ExcV]

  override def evalAtomOpen(at: Atom)(using rec: Fixed): Unit = at match
    case Deconstruct(t, caseRef, args, neg) =>
      val caseDef = caseRef.target.get
      val dataName = caseDef.data.ref.name
      val tRV = evalTerm(t)

      var boundIndices: Seq[Int] = Seq()
      val argRV = args.zipWithIndex.map { case (a, idx) =>
        val evalRes = evalArg(a)
        if (relationOps.hasColumn(evalRes, RESULT_COLUMN) == boolTrue)
          boundIndices :+= idx
          relationOps.projectAndRename(evalRes, Map(RESULT_COLUMN -> s"$idx"))
        else
          evalRes
      }
      val combinations = (tRV +: argRV).foldLeft(relationOps.unit) { case (acc, tv) => relationOps.cartesian(acc, tv) }

      val bindingVarsOption = args.map(extractVarName)
      var newBindings: Option[RV] = None

      relationOps.foreach(combinations) { case termV :: asV =>
        val boundArgs = boundIndices.zip(asV).toMap
        val deconstrArgs = args.indices.map(boundArgs.get)

        except.tryCatch {
          val deconstrRes = dataOps.deconstruct(termV, dataName.name, caseDef.name.name, deconstrArgs, neg)

          // Make sure we get a value for each argument
          if (!neg && (deconstrRes.size != args.size))
            failure(InvalidBindings, s"Deconstruct must provide a value for each argument")

          // All updated binding information
          if (!neg)
            val bind = relationOps.make(
              bindingVarsOption.flatMap(_.map(_.name)),
              Seq(bindingVarsOption.zip(deconstrRes).filter(_._1.isDefined).map(_._2))
            )
            newBindings = newBindings match
              case Some(bd) => Some(relationOps.union(bd, bind))
              case _ => Some(bind)

        } { exc =>
          // This particular deconstruct failed. Remove the bindings if necessary (aka the term t is a variable)
          extractVarName(t) match
            case Some(varName) =>
              val dropEntries = relationOps.make(Seq(varName.name), Seq(Seq(termV)))
              mergeIntoEnv(dropEntries, true)
            case _ => // nothing
        }
      }

      if (!neg && newBindings.isDefined)
        mergeIntoEnv(newBindings.get, false)
    case _ => super.evalAtomOpen(at)

  override def evalTermOpen(term: ir.Term)(using Fixed): RV = term match
    case Construct(caseRef, args) =>
      val caseDef = caseRef.target.get
      val dataName = caseDef.data.ref.name
      naryOp(args.map(evalTerm))(dataOps.construct(dataName.name, caseDef.name.name, _))
    case _ => super.evalTermOpen(term)
