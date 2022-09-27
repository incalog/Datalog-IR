package inca.backend.transform.objectoriented

import inca.backend.hints.{Hints, MagicSetHints, ObjectHints, OptimizationHints}
import inca.backend.hints.ObjectHints.{FieldKey, FieldRootKey}
import inca.backend.ir.Datalog._
import inca.backend.ir.util.CollectVars
import inca.backend.transform.{Transformation, Transformer}
import inca.runtime.aggregate.Aggregation
import inca.runtime.context.DataModel
import inca.util.{Gensym, Scala}

import scala.meta.XtensionQuasiquoteTerm

case class MaxAgg() extends Aggregation[Int] {
  override val name: String = "max"
  override def init: Int = Int.MinValue
  override def join(v1: Int, v2: Int): Int = v1.max(v2)
  //override def unjoin(v1: Int, v2: Int): Int = v1 - v2
  override val isAssociative: Boolean = true
  override val isCommutative: Boolean = true
  override val hasUnjoin: Boolean = false
}

object FieldTransformation extends Transformation {
  override def transformer(dataModel: DataModel): Transformer = new Transformer {

    val gensym = new Gensym(Seq())

    val rootParamName: String = "ts"
    val inParamName: String = "tsIn"
    val outParamName: String = "tsOut"

    override def transformModule(mod: Module): Module = {
      val transformedPattern = insertTimestampCount(mod.pats)
      Module(mod.name, mod.imports, transformedPattern, mod.scalaContent)
    }

    // FIXME: Is there a nicer way to solve this ?
    private def hintWithAdjustedFixedAdornment(hints: Hints, inArg: Boolean, outArg: Boolean): Hints = {
      val fixedAdornment = hints.hints.remove(MagicSetHints.FixedAdornmentKey)
      if (fixedAdornment.isDefined) {
        val adorn = fixedAdornment.get.asInstanceOf[MagicSetHints.FixedAdornment].adorn
        hints.addHint(MagicSetHints.FixedAdornment(adorn :+ inArg :+ outArg))
      }
      hints
    }

    private def isReadonlyCall(call: Call): Boolean =
      call.hasHint(MagicSetHints.IgnoreCallKey)

    private def isFieldGetCall(call: Call): Boolean =
      call.hasHint(ObjectHints.FieldGetKey)

    private def isFieldSetCall(call: Call): Boolean =
      call.hasHint(ObjectHints.FieldSetKey)

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

    def createScalaTermAndParam(name: String, typ: Type): (scala.meta.Term.Name, scala.meta.Term.Param) = {
      val term = scala.meta.Term.Name(name)
      val param = scala.meta.Term.Param(Nil, term, Some(typ.asScala), None)
      (term, param)
    }

    def filterPatternName(fieldPatName: String): String = {
      fieldPatName + "$" + "Filter"
    }

    private def generateFilterPattern(fieldPat: Pattern): Pattern = gensym.scoped {
      val tsParams = Seq(
        Param(gensym.fresh(rootParamName), TScalaInt),
        Param(gensym.fresh(rootParamName + "Max"), TScalaInt)
      )

      val fieldArgs = fieldPat.params.map(p => Var(p.name))
      val additionalArgs = tsParams.map(p => Var(p.name))

      val (tsTerm, tsParam) = createScalaTermAndParam(tsParams.head.name, tsParams.head.typ)
      val (tsMaxTerm, tsMaxParam) = createScalaTermAndParam(tsParams.last.name, tsParams.last.typ)

      val body = Body(Seq(
        Call(fieldPat.name, fieldArgs :+ additionalArgs.head)
          .addHint(MagicSetHints.IgnoreCall)
          .addHint(MagicSetHints.FixedAdornment(fieldArgs.map(_ => true) :+ true)),
        Computed(
          True,
          Evaluation(
            tsParams.map(p => Var(p.name) -> p.typ),
            TScalaBoolean,
            Scala(q"($tsParam, $tsMaxParam) => $tsTerm < $tsMaxTerm")
          )
        )
      ))

      Pattern(fieldPat.vis, filterPatternName(fieldPat.name), fieldPat.params ++ tsParams, Seq(body))
    }

    private def transformFieldPattern(fieldPat: Pattern): Pattern = gensym.scoped {
      gensym.register(CollectVars.transPattern(fieldPat))

      if (fieldPat.params.size != 2) {
        throw new IllegalArgumentException("Field pattern require exactly two parameters!")
      }

      if (fieldPat.bodies.size != 1 || fieldPat.bodies.head.atoms.nonEmpty) {
        throw new IllegalArgumentException("Field pattern require exactly one empty body!")
      }

      val tsParam = Param(gensym.fresh(rootParamName), TScalaInt)
      Pattern(fieldPat.vis, fieldPat.name, fieldPat.params :+ tsParam, Seq())
        .withHints(fieldPat)
        .addHint(OptimizationHints.NoInline)
    }

    def maxAgg(fieldPatName: String, obj: Term, outVar: Var, maxTs: Var): Computed = {
      Computed(
        outVar,
        CustomAggregation(
          TScalaInt,
          Some("Maximum aggregation"),
          Scala(q"""new inca.backend.transform.objectoriented.MaxAgg()"""),
          filterPatternName(fieldPatName),
          Seq(obj, Var(gensym.fresh("_")), Var(gensym.fresh("_")), maxTs),
          2
        )
      )
    }

    private def transformCall(atom: Atom, tsInVar: Var, affectedCallNames: Set[String]): (Var, Seq[Atom]) = {
      atom match {
        case call@Call(name, args, trans, neg) if affectedCallNames.contains(name) =>
          val hint = hintWithAdjustedFixedAdornment(call, inArg = true, outArg = false)
          if (isFieldGetCall(call)) {
            val tsMaxVar = Var(gensym.fresh(rootParamName + "Max"))
            (tsInVar, Seq(
              maxAgg(name, args.head, tsMaxVar, tsInVar),
              Call(name, args :+ tsMaxVar, trans, neg)
                .withHints(hint)
                .addHint(MagicSetHints.IgnoreCall)
            ))
          } else if (isFieldSetCall(call)) {
            val tsOutVar = Var(gensym.fresh(outParamName))
            val (tsInArg, tsInParam) = createScalaTermAndParam(inParamName, TScalaInt)
            (tsOutVar, Seq(
              Call(name, args :+ tsInVar, trans, neg),
              Computed(
                tsOutVar, Evaluation(Seq(tsInVar -> TScalaInt), TScalaInt, Scala(q"($tsInParam) => $tsInArg + 1"))
              )
            ))
          } else if (isReadonlyCall(call)) {
            (tsInVar, Seq(
              Call(name, args :+ Var(gensym.fresh("_")) :+ Var(gensym.fresh("_")), trans, neg)
                .withHints(hint)
            ))
          } else {
            val tsOutVar = Var(gensym.fresh(outParamName))
            (tsOutVar, Seq(
              Call(name, args :+ tsInVar :+ tsOutVar, trans, neg).withHints(hint)
            ))
          }
        case a => (tsInVar, Seq(a))
      }
    }

    private def transformFieldRootPattern(fieldRoot: Pattern, affectedPattern: Set[Pattern]): Pattern = gensym.scoped {
      gensym.register(CollectVars.transPattern(fieldRoot))

      // name of all calls that end up calling a constructor
      val affectedCallNames = affectedPattern.map(_.name)

      val bodies = fieldRoot.bodies.map { body =>
        gensym.scoped {
          var tsVar = Var(gensym.fresh(rootParamName))
          val tsInit = Eq(tsVar, Constant(IntLiteral(0)))

          Body(tsInit +: body.atoms.flatMap { a =>
            val (tsOutVar, transAtom) = transformCall(a, tsVar, affectedCallNames)
            tsVar = tsOutVar
            transAtom
          })
        }
      }
      Pattern(fieldRoot.vis, fieldRoot.name, fieldRoot.params, bodies).withHints(fieldRoot)
    }

    private def transformAffectedPattern(pattern: Pattern, affectedPattern: Set[Pattern]): Pattern = gensym.scoped {
      gensym.register(CollectVars.transPattern(pattern))

      val tsParams = Seq(
        Param(gensym.fresh(inParamName), TScalaInt),
        Param(gensym.fresh(outParamName), TScalaInt),
      )

      // name of all calls that end up calling a constructor
      val affectedCallNames = affectedPattern.map(_.name)
      var tsVar = Var(tsParams.head.name)

      val bodies = pattern.bodies.map { body =>
        gensym.scoped {
          Body(body.atoms.flatMap { a =>
            val (tsOutVar, transAtom) = transformCall(a, tsVar, affectedCallNames)
            tsVar = tsOutVar
            transAtom
          } :+ Eq(
            Var(tsParams.last.name), tsVar
          ))
        }
      }
      Pattern(pattern.vis, pattern.name, pattern.params ++ tsParams, bodies)
        .withHints(pattern)
    }

    private def insertTimestampCount(pattern: Seq[Pattern]): Seq[Pattern] = {
      val fieldPats = pattern.filter(_.hasHint(FieldKey)).toSet
      val fieldRootPats = pattern.filter(_.hasHint(FieldRootKey)).toSet

      if (fieldPats.nonEmpty && fieldRootPats.isEmpty)
        throw new IllegalArgumentException("Missing field root!")
      else if (fieldRootPats.size > 1)
        throw new IllegalArgumentException("Ambiguous field root!")

      if (fieldPats.isEmpty)
        return pattern

      // exclude Field and FieldRoot pattern from affected pattern
      val searchPattern = pattern.toSet.diff(fieldPats).diff(fieldRootPats)
      val affectedPattern = fieldPats.flatMap(findAffectedPattern(_, searchPattern))
      val allAffectedPattern = affectedPattern.union(fieldPats).union(fieldRootPats)
      val unchangedPattern = searchPattern.diff(affectedPattern)

      val filterPats = fieldPats.map(generateFilterPattern)
      val transRootPats = fieldRootPats.map(transformFieldRootPattern(_, allAffectedPattern))
      val transFieldPats = fieldPats.map(transformFieldPattern)
      val transAffectedPats = affectedPattern.map(transformAffectedPattern(_, allAffectedPattern))

      transRootPats.toSeq ++ transFieldPats ++ transAffectedPats ++ filterPats ++ unchangedPattern
    }

    /**
     * Recursively find the pattern that either call `pat` directly or indirectly.
     *
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
}
