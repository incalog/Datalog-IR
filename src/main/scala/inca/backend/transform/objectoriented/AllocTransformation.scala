package inca.backend.transform.objectoriented

import inca.backend.hints.{Hints, MagicSetHints}
import inca.backend.hints.MagicSetHints.FixedAdornment
import inca.backend.hints.ObjectHints.{AllocationKey, AllocationRootKey}
import inca.backend.ir.Datalog._
import inca.backend.ir.util.CollectVars
import inca.backend.transform.{Transformation, Transformer}
import inca.runtime.context.DataModel
import inca.runtime.data.ObjectID
import inca.util.Scala.symbolOf
import inca.util.{Gensym, Scala}

import scala.+:
import scala.meta.XtensionQuasiquoteTerm

object AllocTransformation extends Transformation {
  override def transformer(dataModel: DataModel): Transformer = new Transformer {

    val gensym = new Gensym(Seq())

    override def transformModule(mod: Module): Module = {
      val transformedPattern = insertAllocationCount(mod.pats)
      Module(mod.name, mod.imports, transformedPattern.toSeq, mod.scalaContent)
    }

    // FIXME: Is there a nicer way to solve this ?
    private def hintWithAdjustedFixedAdornment(hints: Hints, allocIn: Boolean = true, allocOut: Boolean = false): Hints = {
      val fixedAdornment = hints.hints.remove(MagicSetHints.FixedAdornmentKey)
      if (fixedAdornment.isDefined) {
        val adorn = fixedAdornment.get.asInstanceOf[FixedAdornment].adorn
        hints.addHint(MagicSetHints.FixedAdornment(adorn :+ allocIn :+ allocOut))
      }
      hints
    }

    private def isReadonlyCall(call: Call): Boolean =
      call.hasHint(MagicSetHints.IgnoreCallKey)

    private def findCallSides(callName: Name, pattern: Set[Pattern]): Set[Pattern] = {
      pattern.filter { pat =>
        pat.bodies.exists { body =>
          body.atoms.exists {
            case call@Call(name, _, _, _) if name == callName => true
            case _ => false
          }
        }
      }
    }

    private def containsOnlyReadCalls(callName: Name, pattern: Pattern): Boolean = {
      !pattern.bodies.exists { body =>
        body.atoms.exists {
          case call@Call(name, _, _, _) if name == callName && !isReadonlyCall(call) => true
          case _ => false
        }
      }
    }

    private def transformAllocationPattern(alloc: Pattern): Pattern = gensym.scoped  {
      gensym.register(CollectVars.transPattern(alloc))

      val allocInName = gensym.fresh("allocIn")
      val allocOutName = gensym.fresh("allocOut")

      // wrap a Computed(Var, Evaluation) inside a lambda, that uses the dummy variable from the input call
      def transComputedEvaluation(lhs: Term, eval: Evaluation): Computed = {
        val orgFun = eval.code.tree
        val q"(..$params) => $f(..$args)" = orgFun
        val allocInArg = scala.meta.Term.Name(allocInName)
        val allocInParam = scala.meta.Term.Param(Nil, allocInArg, Some(TScalaInt.asScala), None)
        val newParams = params :+ allocInParam
        val newArgs = args :+ allocInArg
        val fun = q"(..$newParams) => $f(..$newArgs)"
        Computed(lhs, Evaluation(eval.evalArgs :+ Var(allocInName) -> TScalaInt, eval.resultType, Scala(fun)))
      }

      val bodies = alloc.bodies.map { body =>
        Body(body.atoms.map {
          case Computed(lhs, eval : Evaluation) =>
            transComputedEvaluation(lhs, eval)
          case a => a
        } :+ Computed(
          Var(allocOutName), Evaluation(
            Seq(Var(allocInName) -> TScalaInt),
            TScalaInt,
            Scala(q"""(${scala.meta.Term.Name(allocInName)}: ${TScalaInt.asScala}) => ${scala.meta.Term.Name(allocInName)} + 1""")
          )
        ))
      }
      val params = alloc.params :+ Param(allocInName, TScalaInt) :+ Param(allocOutName, TScalaInt)
      Pattern(alloc.vis, alloc.name, params, bodies).withHints(alloc)
    }

    private def transformAllocationRootPattern(allocRoot: Pattern, affectedPattern: Set[Pattern]): Pattern = gensym.scoped {
      gensym.register(CollectVars.transPattern(allocRoot))

      // name of all calls that end up calling a constructor
      val affectedCallNames = affectedPattern.map(_.name)

      val bodies = allocRoot.bodies.map { body =>
        gensym.scoped {
          var allocVar = Var(gensym.fresh("alloc"))
          val allocInit = Eq(allocVar, Constant(IntLiteral(0)))

          Body(allocInit +: body.atoms.map {
            case call@Call(name, args, trans, neg) if affectedCallNames.contains(name) =>
              val hint = hintWithAdjustedFixedAdornment(call, allocIn = true, allocOut = false)
              if (isReadonlyCall(call)) {
                Call(name, args :+ Var(gensym.fresh("_")) :+ Var(gensym.fresh("_")), trans, neg)
                  .withHints(hint)
              } else {
                val allocInVar = allocVar
                allocVar = Var(gensym.fresh("alloc"))
                Call(name, args :+ allocInVar :+ allocVar, trans, neg)
                  .withHints(hint)
              }
            case a => a
          })
        }
      }
      Pattern(allocRoot.vis, allocRoot.name, allocRoot.params, bodies).withHints(allocRoot)
    }

    private def transformAffectedPattern(pattern: Pattern, affectedPattern: Set[Pattern]): Pattern = gensym.scoped {
      gensym.register(CollectVars.transPattern(pattern))

      val allocInVar = Var(gensym.fresh("allocIn"))
      var allocOutVar = Var(gensym.fresh("allocOut"))

      val allocInParam = Param(allocInVar.name, TScalaInt)
      val allocOutParam = Param(allocOutVar.name, TScalaInt)

      // name of all calls that end up calling a constructor
      val affectedCallNames = affectedPattern.map(_.name)

      val bodies = pattern.bodies.map { body =>
        gensym.scoped {
          // reset the allocOutVar for each body
          allocOutVar = allocInVar

          Body(body.atoms.map {
            case call@Call(name, args, trans, neg) if affectedCallNames.contains(name) =>
              val hint = hintWithAdjustedFixedAdornment(call, allocIn = true, allocOut = false)
              if (isReadonlyCall(call)) {
                Call(name, args :+ Var(gensym.fresh("_")) :+ Var(gensym.fresh("_")), trans, neg)
                  .withHints(hint)
              } else {
                val allocInVar = allocOutVar
                allocOutVar = Var(gensym.fresh("allocOut"))
                Call(name, args :+ allocInVar :+ allocOutVar, trans, neg)
                  .withHints(hint)
              }
            case a => a
          } :+ Eq(Var(allocOutParam.name), allocOutVar))
        }
      }
      val params = pattern.params :+ allocInParam :+ allocOutParam
      Pattern(pattern.vis, pattern.name, params, bodies).withHints(pattern)
    }

    private def insertAllocationCount(pattern: Seq[Pattern]): Seq[Pattern] = {
      val allocPats = pattern.filter(_.hasHint(AllocationKey)).toSet
      val allocRootPats = pattern.filter(_.hasHint(AllocationRootKey)).toSet

      if (allocPats.nonEmpty && allocRootPats.isEmpty)
        throw new IllegalArgumentException("Missing allocation root!")
      else if (allocRootPats.size > 1)
        throw new IllegalArgumentException("Ambiguous allocation root!")

      if (allocPats.isEmpty)
        return pattern

      // exclude Allocation and AllocationRoot pattern from affected pattern
      val searchPattern = pattern.toSet.diff(allocPats).diff(allocRootPats)
      val affectedPattern = allocPats.flatMap(findAffectedPattern(_, searchPattern))
      val allAffectedPattern = affectedPattern.union(allocPats).union(allocRootPats)
      val unchangedPattern = searchPattern.diff(affectedPattern)

      val transAllocRootPats = allocRootPats.map(transformAllocationRootPattern(_, allAffectedPattern))
      val transAllocPats = allocPats.map(transformAllocationPattern)
      val transAffectedPats = affectedPattern.map(transformAffectedPattern(_, allAffectedPattern))

      transAllocRootPats.toSeq ++ transAllocPats ++ transAffectedPats ++ unchangedPattern
    }

    /**
     * Recursively find the pattern that either call `pat` directly or indirectly.
     * @param pat The pattern to find all callers for.
     * @param remainingPattern The search space.
     * @return Set with all pattern that directy or indirectly call `pat`.
     */
    private def findAffectedPattern(pat: Pattern, remainingPattern: Set[Pattern]): Set[Pattern] = {
        val callSides = findCallSides(pat.name, remainingPattern)
        callSides.union(callSides.flatMap { p =>
          // TODO: In theory this should work, in practise this breaks the demand transformation for recursive functions
          //  There must be some logical error in this if statement. The idea to filter these patterns out is correct-
          // reading a constructor does not required propagating alloc_in / alloc_out
          /*if (containsOnlyReadCalls(pat.name, p))
            None
          else*/
            findAffectedPattern(p, remainingPattern.diff(callSides))
        })
    }
  }
}
