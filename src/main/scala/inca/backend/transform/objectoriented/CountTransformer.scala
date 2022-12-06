package inca.backend.transform.objectoriented

import inca.backend.hints.{Hint, Hints, MagicSetHints}
import inca.backend.ir.Datalog.{CustomAggregation, _}
import inca.backend.ir.util.CollectVars
import inca.backend.transform.Transformer
import inca.util.{Gensym, Scala}

import scala.meta.XtensionQuasiquoteTerm

/**
 * Should be applied after demand transformation.
 *
 * A CountTransformer initialized a counter with the value 0 and name `rootParamName` in all bodies of a root pattern.
 * The root pattern is defined by the `rootPatternHint` [transformRootPattern].
 *
 * All pattern that directly or indirectly call a leaf pattern (defined by the `leafPatternHint`) are calculated up to
 * the root pattern. These pattern are called affected pattern. Affected pattern are modified to take two additional
 * parameter: an input counter and an output counter [transformAffectedPattern].
 *
 * All calls that target an affected pattern, a leaf pattern or a root pattern are modified to propagate the input and
 * output counter [transformCall].
 *
 * All unaffected pattern are transformed to insert "don't care" variables when calling an affected pattern.
 *
 * The leaf pattern is modified based on behaviour defined by a concrete implementation of this class
 * [transformChildPattern].
 */
abstract class CountTransformer(val rootPatternHint: Hint,
                                val leafPatternHint: Hint,
                                val rootParamName: String,
                                val inParamName: String,
                                val outParamName: String) extends Transformer {
    val gensym = new Gensym(Seq())

  type CallSide = (Pattern, Atom)

  /**
   * Transform a leaf pattern to respect the count arguments. A subclass must override this method.
   * @param leafPat The leaf pattern to change.
   * @param affectedPattern Sequence with all affected pattern.
   * @return The modified leaf pattern.
   */
    def transformLeafPattern(leafPat: Pattern, affectedPattern: Set[Pattern]): Pattern

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

  private def transformIgnoredAtom(atom: Atom): Atom = {
    atom.asCall match {
      case Some((name, terms)) =>
        val hint = hintWithAdjustedFixedAdornment(atom, terms.size, Seq(true, false))
        val replacement = atom.replaceCall(name, terms :+ Var(gensym.fresh("_")) :+ Var(gensym.fresh("_")))
        replacement.withHints(hint)
      case _ =>
        atom
    }
  }

  private def transformNonIgnoredCall(call: Call, counterInVar: Var, counterOutVar: Var): Call = {
    val Call(name, args, trans, neg) = call
    val hint = hintWithAdjustedFixedAdornment(call, args.size, Seq(true, false))
    Call(name, args :+ counterInVar :+ counterOutVar, trans, neg).withHints(hint)
  }

  /**
   * Transform a call to respect the count arguments. A subclass can override this methods. It is possible to return
   * a sequence of atoms to replace the single call with this sequence. The default implementation will append two
   * additional arguments, one for the input counter and one for the output counter to the call.
   * @param call         The call atom to transform.
   * @param counterInVar The input counter variable.
   * @return The output count variable used as the next input and a sequence of atoms to replace the call with.
   */
    def transformCall(call: Call, counterInVar: Var): (Var, Seq[Atom]) = {
      if (isIgnored(call)) {
        (counterInVar, Seq(transformIgnoredAtom(call)))
      } else {
        val counterOutVar = Var(gensym.fresh(outParamName))
        (counterOutVar, Seq(transformNonIgnoredCall(call, counterInVar, counterOutVar)))
      }
    }

  /**
   * Fix all custom aggregations by inserting do not care values for the newly created count parameters.
   */
    def transformAgg(aggregation: CustomAggregation, counterInVar: Var): CustomAggregation = {
      val CustomAggregation(typ, description, agg, patName, args, aggregatedColumn) = aggregation
      val doNotCare = Var(gensym.fresh("_"))
      // FIXME: We put in the inVar as argument. This might be a problem in the future. For now this okay, since
      //  we only use aggregations for fold and for calculating the timestamp.
      //  The timestamp calculation is not affected by this method, since we add the timestamp agg, after the alloc
      //  transformation. If the aggregation is generated for a fold, than the aggregate pattern will always contain a
      //  dispatch call to a defunctionalized set (e.g Aux$0.apply = this.content). In this case, the
      //  the allocInVar is not needed and the tsInVar is used to filter smaller timestamps. In both cases is it
      //  therefore correct to pass in the current inVar.
      CustomAggregation(typ, description, agg, patName, args :+ counterInVar :+ doNotCare, aggregatedColumn)
    }

    override def transformModule(mod: Module): Module = {
      val transformedPattern = insertCounter(mod.pats)
      Module(mod.name, mod.imports, transformedPattern, mod.scalaContent)
    }

  /**
   * FIXME: Is there a nicer way to solve this ?
   * Append additional adornment information to an existing FixedAdornment hint if it exists.
   * @param hints The existing hint.
   * @param numArgs The original number of arguments
   * @param additionalAdornment The additional adornment information.
   * @return The modified hint.
   */
    private[objectoriented] def hintWithAdjustedFixedAdornment(hints: Hints, numArgs: Int, additionalAdornment: Seq[Boolean]): Hints = {
      val fixedAdornment = hints.hints.remove(MagicSetHints.FixedAdornmentKey)
      if (fixedAdornment.isDefined) {
        val adorn = fixedAdornment.get.asInstanceOf[MagicSetHints.FixedAdornment].adorn
        hints.addHint(MagicSetHints.FixedAdornment(adorn.slice(0, numArgs) ++ additionalAdornment))
      }
      hints
    }

    private[objectoriented] def isIgnored(atom: Atom): Boolean =
      atom.hasHint(MagicSetHints.IgnoreCallKey)

    private[objectoriented] def createScalaTermAndParam(name: String, typ: Type): (scala.meta.Term.Name, scala.meta.Term.Param) = {
      val term = scala.meta.Term.Name(name)
      val param = scala.meta.Term.Param(Nil, term, Some(typ.asScala), None)
      (term, param)
    }

  /**
   * Helper method to increase the counter.
   * @param counterIn The counter to increase.
   * @return Tuple with the output variable and the corresponding Computed to increase the counter.
   */
    private[objectoriented] def incCounter(counterIn: Var): (Var, Computed) = {
      val counterOutVar = Var(gensym.fresh(outParamName))
      val (counterInArg, counterInParam) = createScalaTermAndParam(inParamName, TScalaInt)
      (counterOutVar, Computed(
          counterOutVar, Evaluation(Seq(counterIn -> TScalaInt), TScalaInt, Scala(q"($counterInParam) => $counterInArg + 1"))
      ))
    }

  /**
   * Find all pattern that call a pattern with the name `callName`
   * @param callName The name of the called pattern to find.
   * @param pattern A set with all pattern to search.
   * @return A set with tuples of (pattern, call) for each pattern that calls the pattern with name `callName`.
   */
    private[objectoriented] def findCallSides(callName: Name, pattern: Set[Pattern]): Set[CallSide] = {
      pattern.flatMap { pat =>
        pat.bodies.flatMap { body =>
          body.atoms.flatMap { a =>
            a.asCall match {
              case Some((name, _)) if callName == name => Some((pat, a))
              case _ => None
            }
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
              val (countOutVar, transAtom) = transformCall(c, countVar)
              countVar = countOutVar
              transAtom
            case Computed(lhs, c: CustomAggregation) if affectedPatternNames.contains(c.patName) =>
              Seq(Computed(lhs, transformAgg(c, countVar)))
            case a => Seq(a)
          }).withHints(body)
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
              val (countOutVar, transAtom) = transformCall(c, countVar)
              countVar = countOutVar
              transAtom
            case Computed(lhs, c: CustomAggregation) if affectedPatternNames.contains(c.patName) =>
              Seq(Computed(lhs, transformAgg(c, countVar)))
            case a => Seq(a)
          } :+ Eq(
            Var(countParams.last.name), countVar
          )).withHints(body)
        }
      }
      Pattern(pattern.vis, pattern.name, pattern.params ++ countParams, bodies)
        .withHints(pattern)
    }

  /**
   * Transform an unaffected pattern by transforming all calls to insert dummys for the counter variable.
   *
   * @param pattern         The unaffected pattern to modify.
   * @param affectedPattern All pattern that require the counter.
   * @return The modified affected pattern.
   */
  private[objectoriented] def transformUnaffectedPattern(pattern: Pattern, affectedPattern: Set[Pattern]): Pattern = gensym.scoped {
    gensym.register(CollectVars.transPattern(pattern))

    // name of all calls that end up calling a leaf pattern
    val affectedPatternNames = affectedPattern.map(_.name)
    val bodies = pattern.bodies.map { body =>
      gensym.scoped {
        Body(body.atoms.map { atom =>
          atom.asCall match {
            case Some((name, _)) if affectedPatternNames.contains(name) => transformIgnoredAtom(atom)
            case _ => atom
          }
        }).withHints(body)
      }
    }
    Pattern(pattern.vis, pattern.name, pattern.params, bodies)
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
      val transFieldPats = leafPats.map(transformLeafPattern(_, allAffectedPattern))
      val transAffectedPats = affectedPattern.map(transformAffectedPattern(_, allAffectedPattern))
      val transUnaffectedPats = unchangedPattern.map(transformUnaffectedPattern(_, allAffectedPattern))

      transRootPats.toSeq ++ transFieldPats ++ transAffectedPats ++ additionalPattern ++ transUnaffectedPats
    }

    /**
     * Recursively find the pattern that either call `pat` directly or indirectly.
     * @param pat              The pattern to find all callers for.
     * @param remainingPattern The search space.
     * @return Set with all pattern that directly or indirectly call `pat`.
     */
    private[objectoriented] def findAffectedPattern(pat: Pattern, remainingPattern: Set[Pattern]): Set[Pattern] = {
      val callSides = findCallSides(pat.name, remainingPattern)
      val affectedPattern = callSides.flatMap { case (p, call) =>
        if (isIgnored(call)) None else Some(p)
      }
      affectedPattern.union(affectedPattern.flatMap { p =>
        findAffectedPattern(p, remainingPattern.diff(affectedPattern))
      })
    }
}
