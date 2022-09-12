package inca.backend.transform.objectoriented

// TODO: What happens on recursive call ? Consider multiple constructor. Each constructor needs to influence the
// TODO: passed parameters, that means we can not just skip everything
// Idea: it might be the easiest solution to just pass parameters per constructor. This does still not solve the
// recursion problem

import inca.backend.hints.{Hints, MagicSetHints}
import inca.backend.hints.MagicSetHints.FixedAdornment
import inca.backend.hints.ObjectHints.{AllocationKey, AllocationRootKey}
import inca.backend.ir.Datalog._
import inca.backend.ir.util.CollectVars
import inca.backend.transform.{Transformation, Transformer}
import inca.runtime.context.DataModel
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

    private def findCallSide(callName: Name, pattern: Set[Pattern]): Set[Pattern] = {
      pattern.filter { pat =>
        pat.bodies.exists { body =>
          body.atoms.exists {
            case Call(name, _, _, _) if name == callName => true
            case _ => false
          }
        }
      }
    }

    private def transformAllocationPattern(alloc: Pattern): Pattern = gensym.scoped  {
      gensym.register(CollectVars.transPattern(alloc))

      val allocInName = gensym.fresh("alloc_in")
      val allocOutName = gensym.fresh("alloc_out")

      // wrap a Computed(Var, Evaluation) inside a lambda, that uses the dummy variable from the input call
      def wrapComputedEvaluation(lhs: Term, eval: Evaluation): Computed = {
        val orgFun = eval.code.tree
        val fun = scala.meta.Term.Function(
          orgFun.params :+ scala.meta.Term.Param(Nil, scala.meta.Term.Name(allocInName), Some(TScalaInt.asScala), None),
          q"""${orgFun}(..${orgFun.params.map(p => scala.meta.Term.Name(p.name.toString))})"""
        )
        Computed(lhs, Evaluation(eval.evalArgs :+ Var(allocInName) -> TScalaInt, eval.resultType, Scala(fun)))
      }

      val bodies = alloc.bodies.map { body =>
        Body(body.atoms.map {
          case Computed(lhs, eval : Evaluation) =>
            wrapComputedEvaluation(lhs, eval)
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
      // TODO: It is probably enough to do this per body
      gensym.register(CollectVars.transPattern(allocRoot))

      def newAllocVar() = Var(gensym.fresh("alloc"))
      def newDummyVar() = Var(gensym.fresh("_"))

      // name of all calls that end up calling a constructor
      val affectedCallNames = affectedPattern.map(_.name)

      val bodies = allocRoot.bodies.map { body =>
        var allocVar = newAllocVar()
        val allocInit = Eq(allocVar, Constant(IntLiteral(0)))

        Body(allocInit +: body.atoms.map {
          case call@Call(name, args, trans, neg)  if affectedCallNames.contains(name) =>
            val hint = hintWithAdjustedFixedAdornment(call, allocIn = true, allocOut = false)
            if (call.hasHint(MagicSetHints.IgnoreCallKey)) {
              Call(name, args :+ newDummyVar() :+ newDummyVar(), trans, neg).withHints(hint)
            } else {
              val allocInVar = allocVar
              allocVar = newAllocVar()
              Call(name, args :+ allocInVar :+ allocVar, trans, neg).withHints(hint)
            }
          case a => a
        })
      }
      Pattern(allocRoot.vis, allocRoot.name, allocRoot.params, bodies).withHints(allocRoot)
    }

    private def transformCallSidePattern(pattern: Pattern, affectedPattern: Set[Pattern]): Pattern = gensym.scoped {
      // TODO: It is probably enough to do this per body
      gensym.register(CollectVars.transPattern(pattern))

      val allocInVar = Var(gensym.fresh("alloc_in"))
      var allocOutVar = allocInVar

      def newAllocOutVar() = Var(gensym.fresh("alloc_out"))
      def newDummyVar() = Var(gensym.fresh("_"))

      // name of all calls that end up calling a constructor
      val affectedCallNames = affectedPattern.map(_.name)
      println("Affected calls: ", affectedCallNames)

      val bodies = pattern.bodies.map { body =>
        // reset the allocOutVar for each body
        allocOutVar = allocInVar

        Body(body.atoms.map {
          case call@Call(name, args, trans, neg) if affectedCallNames.contains(name) =>
            val hint = hintWithAdjustedFixedAdornment(call, allocIn = true, allocOut = false)
            if (call.hasHint(MagicSetHints.IgnoreCallKey)) {
              Call(name, args :+ newDummyVar() :+ newDummyVar(), trans, neg).withHints(hint)
            } else {
              val allocInVar = allocOutVar
              allocOutVar = newAllocOutVar()
              Call(name, args :+ allocInVar :+ allocOutVar, trans, neg).withHints(hint)
            }
          case a => a
        })
      }
      val params = pattern.params :+ Param(allocInVar.name, TScalaInt) :+ Param(allocOutVar.name, TScalaInt)
      Pattern(pattern.vis, pattern.name, params, bodies).withHints(pattern)
    }

    private def insertAllocationCount(pattern: Seq[Pattern]): Set[Pattern] = {
      // Start from the allocation and move up the tree to the allocation root
      val allocationPattern = pattern.filter(_.hasHint(AllocationKey)).toSet
      if (allocationPattern.isEmpty)
        pattern.toSet
      else
        recInsertAllocationCount(allocationPattern, pattern.toSet, Set())
    }

    private def recInsertAllocationCount(pattern: Set[Pattern], remainingPattern: Set[Pattern], callSides: Set[Pattern]): Set[Pattern] = {
      pattern.flatMap {
        case pat: Pattern if pat.hasHint(AllocationKey) =>
          val newCallSides = findCallSide(pat.name, remainingPattern)
          val affectedPattern = newCallSides + pat
          val transPattern = transformAllocationPattern(pat)
          recInsertAllocationCount(newCallSides, remainingPattern.diff(affectedPattern), affectedPattern) + transPattern
        case pat: Pattern if pat.hasHint(AllocationRootKey) =>
          remainingPattern + transformAllocationRootPattern(pat, callSides)
        case pat: Pattern =>
          val transPattern = transformCallSidePattern(pat, callSides)
          val newCallSides = findCallSide(pat.name, remainingPattern)
          val affectedPattern = newCallSides + pat
          recInsertAllocationCount(newCallSides, remainingPattern.diff(affectedPattern), affectedPattern) + transPattern
      }
    }
  }
}
