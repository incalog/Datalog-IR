package inca.backend.transform.objectoriented

import inca.backend.hints.{Hint, Hints, MagicSetHints}
import inca.backend.ir.Datalog._
import inca.backend.ir.util.CollectVars
import inca.backend.transform.{Transformer}
import inca.util.{Gensym}


abstract class CountTransformer(val rootPatternHint: Hint,
                                val leafPatternHint: Hint,
                                val rootParamName: String,
                                val inParamName: String,
                                val outParamName: String) extends Transformer {
    val gensym = new Gensym(Seq())

  /**
   * Transform a leaf pattern to respect the count arguments. A subclass must override this method.
   * @param leafPat The leaf pattern to change.
   * @return The modified leaf pattern.
   */
    def transformLeafPattern(leafPat: Pattern): Pattern

  /**
   * Transform a call to respect the count arguments. A subclass must override this methods. It is possible to return
   * a sequence of atoms to replace the single call with this sequence.
   * @param call The call atom to transform.
   * @param counterInVar The input counter variable.
   * @return The output count variable used as the next input and a sequence of atoms to replace the call with.
   */
    def transformCall(call: Call, counterInVar: Var): (Var, Seq[Atom])

  /**
   * Override this method in a subclass. Generate any additional pattern that are required in this method.
   * @param leafPattern Sequence with all leaf pattern.
   * @param rootPattern Sequence with all root pattern.
   * @param affectedPattern Sequence with all affected pattern.
   * @param unchangedPattern Sequence with all unchanged pattern.
   * @return Sequence with all additional pattern to add.
   */
    def generateAdditionalPattern(leafPattern: Seq[Pattern],
                                  rootPattern: Set[Pattern],
                                  affectedPattern: Set[Pattern],
                                  unchangedPattern: Set[Pattern]): Seq[Pattern] = Seq()

    override def transformModule(mod: Module): Module = {
      val transformedPattern = insertCounter(mod.pats)
      Module(mod.name, mod.imports, transformedPattern, mod.scalaContent)
    }

  /**
   * FIXME: Is there a nicer way to solve this ?
   * Append additional adornment information to an existing FixedAdornment hint if it exists.
   * @param hints The existing hint.
   * @param additionalAdornment The additional adornment information.
   * @return The modified hint.
   */
    private[objectoriented] def hintWithAdjustedFixedAdornment(hints: Hints, additionalAdornment: Seq[Boolean]): Hints = {
      val fixedAdornment = hints.hints.remove(MagicSetHints.FixedAdornmentKey)
      if (fixedAdornment.isDefined) {
        val adorn = fixedAdornment.get.asInstanceOf[MagicSetHints.FixedAdornment].adorn
        hints.addHint(MagicSetHints.FixedAdornment(adorn ++ additionalAdornment))
      }
      hints
    }

    private[objectoriented] def isIgnoreCall(call: Call): Boolean =
      call.hasHint(MagicSetHints.IgnoreCallKey)

    private[objectoriented] def createScalaTermAndParam(name: String, typ: Type): (scala.meta.Term.Name, scala.meta.Term.Param) = {
      val term = scala.meta.Term.Name(name)
      val param = scala.meta.Term.Param(Nil, term, Some(typ.asScala), None)
      (term, param)
    }

  /**
   * Find all pattern that call a pattern with the name `callName`
   * @param callName The name of the called pattern to find.
   * @param pattern A set with all pattern to search.
   * @return A set with all pattern that call the pattern with name `callName`.
   */
    private[objectoriented] def findCallSides(callName: Name, pattern: Set[Pattern]): Set[Pattern] = {
      pattern.filter { pat =>
        pat.bodies.exists { body =>
          body.atoms.exists {
            case Call(name, _, _, _) =>  name == callName
            case _ => false
          }
        }
      }
    }

  /**
   * Transform a root pattern by initializing a counter. The counter is passed as input value to the first call that
   * requires it. The output counter is received from the call and passed on to the next call.
   * @param rootPat The root pattern.
   * @param affectedPattern All pattern that require the counter.
   * @return The modified root pattern.
   */
    private[objectoriented] def transformRootPattern(rootPat: Pattern, affectedPattern: Set[Pattern]): Pattern = gensym.scoped {
      gensym.register(CollectVars.transPattern(rootPat))

      // name of all calls that end up calling the leaf pattern
      val affectedPatternNames = affectedPattern.map(_.name)

      val bodies = rootPat.bodies.map { body =>
        gensym.scoped {
          var countVar = Var(gensym.fresh(rootParamName))
          val countInit = Eq(countVar, Constant(IntLiteral(0)))

          Body(countInit +: body.atoms.flatMap {
            case c: Call if affectedPatternNames.contains(c.name) =>
              val (tsOutVar, transAtom) = transformCall(c, countVar)
              countVar = tsOutVar
              transAtom
            case a => Seq(a)
          })
        }
      }
      Pattern(rootPat.vis, rootPat.name, rootPat.params, bodies).withHints(rootPat)
    }

  /**
   * Transform an affected pattern to receive an input counter and return an output counter. All calls are transformed
   * to respect the counter variable as well.
   * @param pattern The affected pattern to modify.
   * @param affectedPattern All pattern that require the counter.
   * @return The modified affected pattern.
   */
    private[objectoriented] def transformAffectedPattern(pattern: Pattern, affectedPattern: Set[Pattern]): Pattern = gensym.scoped {
      gensym.register(CollectVars.transPattern(pattern))

      val countParams = Seq(
        Param(gensym.fresh(inParamName), TScalaInt),
        Param(gensym.fresh(outParamName), TScalaInt),
      )

      // name of all calls that end up calling a leaf pattern
      val affectedPatternNames = affectedPattern.map(_.name)

      val bodies = pattern.bodies.map { body =>
        gensym.scoped {
          var countVar = Var(countParams.head.name)

          Body(body.atoms.flatMap {
            case c: Call if affectedPatternNames.contains(c.name) =>
              val (tsOutVar, transAtom) = transformCall(c, countVar)
              countVar = tsOutVar
              transAtom
            case a => Seq(a)
          } :+ Eq(
            Var(countParams.last.name), countVar
          ))
        }
      }
      Pattern(pattern.vis, pattern.name, pattern.params ++ countParams, bodies)
        .withHints(pattern)
    }

  /**
   * Insert the counter variable into the program by modifying all root and leaf pattern and all patterns that are
   * affected by the change.
   * @param pattern A list with all pattern.
   * @return The modified pattern.
   */
    private[objectoriented] def insertCounter(pattern: Seq[Pattern]): Seq[Pattern] = {
      val leafPats = pattern.filter(_.hasHint(leafPatternHint.key)).toSet
      val rootPats = pattern.filter(_.hasHint(rootPatternHint.key)).toSet

      if (leafPats.nonEmpty && rootPats.isEmpty)
        throw new IllegalArgumentException(s"${this.getClass.getSimpleName}: Missing root annotation!")
      else if (rootPats.size > 1)
        throw new IllegalArgumentException(s"${this.getClass.getSimpleName}: Ambiguous root!")

      if (leafPats.isEmpty)
        return pattern

      // exclude leaf and root pattern from affected pattern
      val searchPattern = pattern.toSet.diff(leafPats).diff(rootPats)
      val affectedPattern = leafPats.flatMap(findAffectedPattern(_, searchPattern))
      val allAffectedPattern = affectedPattern.union(leafPats).union(rootPats)
      val unchangedPattern = searchPattern.diff(affectedPattern)

      val additionalPattern = generateAdditionalPattern(leafPats.toSeq, rootPats, affectedPattern, unchangedPattern)
      val transRootPats = rootPats.map(transformRootPattern(_, allAffectedPattern))
      val transFieldPats = leafPats.map(transformLeafPattern)
      val transAffectedPats = affectedPattern.map(transformAffectedPattern(_, allAffectedPattern))

      transRootPats.toSeq ++ transFieldPats ++ transAffectedPats ++ additionalPattern ++ unchangedPattern
    }

    /**
     * Recursively find the pattern that either call `pat` directly or indirectly.
     * @param pat              The pattern to find all callers for.
     * @param remainingPattern The search space.
     * @return Set with all pattern that directly or indirectly call `pat`.
     */
    private def findAffectedPattern(pat: Pattern, remainingPattern: Set[Pattern]): Set[Pattern] = {
      val callSides = findCallSides(pat.name, remainingPattern)
      callSides.union(callSides.flatMap { p =>
        findAffectedPattern(p, remainingPattern.diff(callSides))
      })
    }
}
