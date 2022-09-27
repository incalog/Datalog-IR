package inca.backend.transform.objectoriented

import inca.backend.hints.MagicSetHints.FixedAdornment
import inca.backend.hints.{Hints, MagicSetHints}
import inca.backend.hints.ObjectHints.{TimestampKey, TimestampRootKey}
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog._
import inca.backend.ir.util.CollectVars
import inca.backend.transform.{Transformation, Transformer}
import inca.runtime.context.DataModel
import inca.util.Gensym

object TimestampTransformation extends Transformation {
  override def transformer(dataModel: DataModel): Transformer = new Transformer {

    /*val gensym = new Gensym(Seq())

    lazy val additionalParams: Seq[Param] = Seq(
      Param("tsIn", TScalaInt),
      Param("tsOut", TScalaInt)
    )

    lazy val additionalArgs: Seq[Var] = additionalParams.map(p => Var(p.name))

    override def transformModule(mod: Module): Module = {
      val transformedPattern = insertTimestampCount(mod.pats)
      Module(mod.name, mod.imports, transformedPattern, mod.scalaContent)
    }

    private def transformTimestampRootPattern(tsRoot: Pattern, affectedPattern: Set[Pattern]): Pattern = {
      gensym.register(CollectVars.transPattern(tsRoot))

      // name of all calls that end up calling a constructor
      val affectedCallNames = affectedPattern.map(_.name)

      val bodies = tsRoot.bodies.map { body =>
        gensym.scoped {
          var tsVar = Var(gensym.fresh("ts"))
          val tsInit = Eq(tsVar, Constant(IntLiteral(0)))

          Body(tsInit +: body.atoms.map {
            case call@Call(name, args, trans, neg) if affectedCallNames.contains(name) =>
              println(name + " is affected... " + affectedCallNames)
              val hint = hintWithAdjustedFixedAdornment(call, tsIn = true, tsOut = false)
              if (isReadonlyCall(call)) {
                Call(name, args :+ Var(gensym.fresh("_")) :+ Var(gensym.fresh("_")), trans, neg)
                  .withHints(hint)
              } else {
                val allocInVar = tsVar
                tsVar = Var(gensym.fresh("ts"))
                Call(name, args :+ allocInVar :+ tsVar, trans, neg)
                  .withHints(hint)
              }
            case a => a
          })
        }
      }
      Pattern(tsRoot.vis, tsRoot.name, tsRoot.params, bodies).withHints(tsRoot)
    }

    private def transformAffectedPattern(pat: Pattern, affectedPattern: Set[Pattern]): Pattern = {
      val Pattern(vis, name, params, bodies) = pat
      val newBodies = bodies.map { b =>
        Body(b.atoms.map {
          case Call(name, args, transitive, neg) => //if affectedPattern.contains(name) =>
            Call(name, args ++ additionalArgs, transitive, neg)
          case a => a
        })
      }
      Pattern(vis, name, params ++ additionalParams, newBodies).withHints(pat)
    }

    private def transformTimestampPattern(pat: Pattern) = {
      transformAffectedPattern(pat)
    }

    private def insertTimestampCount(pattern: Seq[Pattern]): Seq[Pattern] = {
      val tsPats = pattern.filter(_.hasHint(TimestampKey)).toSet
      val tsRootPats = pattern.filter(_.hasHint(TimestampRootKey)).toSet

      if (tsPats.nonEmpty && tsRootPats.isEmpty)
        throw new IllegalArgumentException("Missing timestamp root!")
      else if (tsRootPats.size > 1)
        throw new IllegalArgumentException("Ambiguous timestamp root!")

      if (tsPats.isEmpty)
        return pattern

      // exclude Allocation and AllocationRoot pattern from affected pattern
      val searchPattern = pattern.toSet.diff(tsPats).diff(tsRootPats)
      val affectedPattern = tsPats.flatMap(findAffectedPattern(_, searchPattern))
      val allAffectedPattern = affectedPattern.union(tsPats).union(tsRootPats)
      val unchangedPattern = searchPattern.diff(affectedPattern)

      println("AffectedPattern: ", searchPattern.map(_.name), affectedPattern.map(_.name), allAffectedPattern.map(_.name))

      val transTsRootPats = tsRootPats.map(transformTimestampRootPattern(_, allAffectedPattern))
      val transTsPats = tsPats//.map(transformTimestampPattern)
      val transAffectedPats = affectedPattern.map(transformAffectedPattern)

      transTsRootPats.toSeq ++ transTsPats ++ transAffectedPats ++ unchangedPattern
    }

    // FIXME: Is there a nicer way to solve this ?
    private def hintWithAdjustedFixedAdornment(hints: Hints, tsIn: Boolean = true, tsOut: Boolean = false): Hints = {
      val fixedAdornment = hints.hints.remove(MagicSetHints.FixedAdornmentKey)
      if (fixedAdornment.isDefined) {
        val adorn = fixedAdornment.get.asInstanceOf[FixedAdornment].adorn
        hints.addHint(MagicSetHints.FixedAdornment(adorn :+ tsIn :+ tsOut))
      }
      hints
    }

    private def isReadonlyCall(call: Call): Boolean =
      call.hasHint(MagicSetHints.IgnoreCallKey)

    private def findCallSides(callName: Name, pattern: Set[Pattern]): Set[Pattern] = {
      pattern.filter { pat =>
        pat.bodies.exists { body =>
          body.atoms.exists {
            case Call(name, _, _, _) if name == callName => true
            case _ => false
          }
        }
      }
    }

    /**
     * Recursively find the pattern that either call `pat` directly or indirectly.
     *
     * @param pat              The pattern to find all callers for.
     * @param remainingPattern The search space.
     * @return Set with all pattern that directy or indirectly call `pat`.
     */
    private def findAffectedPattern(pat: Pattern, remainingPattern: Set[Pattern]): Set[Pattern] = {
      val callSides = findCallSides(pat.name, remainingPattern)
      callSides.union(callSides.flatMap { p =>
        findAffectedPattern(p, remainingPattern.diff(callSides))
      })
    }*/
  }
}
