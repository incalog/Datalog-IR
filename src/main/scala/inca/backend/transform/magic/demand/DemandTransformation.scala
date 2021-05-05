package inca.backend.transform.magic.demand

import inca.backend.hints.MagicSetHints.{InputCall, InputCallKey}
import inca.backend.hints.{Hints, MagicSetHints}
import inca.backend.ir.CollectVars
import inca.backend.ir.GP._
import inca.backend.transform.{FilterBodyTransformer, Transformation, Transformer}
import inca.runtime.context.DataModel
import inca.util.Gensym


// This transformation consumes MagicSetHints.IgnoreCall and MagicSetHints.NoInputRelation
object DemandTransformation extends Transformation {

  def inputPatternName(name: Name): String = "input_" + name
  def extensionalInputPatternName(name: Name): String = "ext_input_" + name

  override def transformer(dataModel: DataModel): Transformer = new Transformer {

    val gensym = new Gensym(Seq())

    override def transformModule(mod: Module): Module = {
      val insertedInputCallPats = mod.pats.flatMap(transformPattern)
      insertedInputCallPats.foreach(p => gensym.register(CollectVars.transPattern(p)))

      val inputPatterns = mod.pats.flatMap { pat =>
        val demandPats = getDemandPatterns(pat)
        demandPats.adorn.flatMap { demandPat =>
          deriveInputPattern(pat, demandPat, insertedInputCallPats)
        }
      }

      // remove bodies with calls to input relations that don't exist
      val inputPatNames = inputPatterns.map(_.name).toSet
      val allPats = insertedInputCallPats ++ inputPatterns
      val filter = new FilterBodyTransformer({ body =>
        val hasEmptyInput = body.constraints.exists { con =>
          con.hasHint(InputCallKey) && !inputPatNames.contains(con.asInstanceOf[Call].name)
        }
        !hasEmptyInput
      })
      val filteredPats = allPats.flatMap(filter.transformPattern)

      Module(mod.name, mod.imports, mod.data, filteredPats, mod.scalaContent)
    }

    override def transformPattern(pat: Pattern): Seq[Pattern] =
      if (pat.hasHint(MagicSetHints.DemandPatternsKey)) {
        val demandPats = getDemandPatterns(pat)
        demandPats.adorn.map { demandPat =>
          val extendedPatterns = insertInputCall(pat, demandPat)
          extendedPatterns
        }.toSeq
      } else {
        Seq(pat)
      }

    private def shouldDeriveInput(pat: Pattern): Boolean =
      shouldInsertInput(pat) && pat.hasHint(MagicSetHints.DemandPatternsKey)

    private def shouldInsertInput(body: Hints): Boolean =
      !body.hasHint(MagicSetHints.NoInputRelationKey)

    private def getDemandPatterns(pat: Pattern): MagicSetHints.DemandPatterns = {
      if (!pat.hasHint(MagicSetHints.DemandPatternsKey)) MagicSetHints.DemandPatterns(Set())
      else pat.hints(MagicSetHints.DemandPatternsKey).asInstanceOf[MagicSetHints.DemandPatterns]
    }

    private def insertInputCall(pat: Pattern, demandPat: Seq[Boolean]): Pattern = {
      if (!shouldInsertInput(pat))
        return pat

      if (pat.bodies.isEmpty) {
        val body = deriveInputCall(pat, demandPat).map(c => Body(Seq(c)))
        return Pattern(pat.vis, pat.name, pat.params, body.toSeq).withHints(pat)
      }

      val bodies = pat.bodies.map { b =>
        if (shouldInsertInput(b)) {
          val inputCall = deriveInputCall(pat, demandPat)
          Body(inputCall.toSeq ++ b.constraints).withHints(b)
        } else {
          b
        }
      }
      Pattern(pat.vis, pat.name, pat.params, bodies).withHints(pat)
    }

    private def deriveInputCall(pat: Pattern, demandPat: Seq[Boolean]): Option[Call] = {
      val boundParams = deriveBoundParams(pat, demandPat)
      if (boundParams.isEmpty) {
        None
      } else {
        val args = boundParams.map(p => Var(p.name))
        Some(Call(inputPatternName(pat.name), args).addHint(InputCall(pat.name)))
      }
    }

    private def deriveBoundParams(pat: Pattern, demandPat: Seq[Boolean]): Seq[Param] = {
      val indexBoundParams = deriveBoundIndices(pat, demandPat)
      indexBoundParams.map(pat.params)
    }

    private def deriveBoundIndices(pat: Pattern, demandPat: Seq[Boolean]): Seq[Int] = {
      demandPat.zipWithIndex.filter(_._1).map(_._2)
    }

    private def deriveInputPattern(pat: Pattern, demandPat: Seq[Boolean], patterns: Seq[Pattern]): Seq[Pattern] = gensym.scoped {
      if (!shouldDeriveInput(pat))
        return Seq()

      // generate new names for pattern params to avoid name collision
      val params = pat.params.map { p =>
        val name = gensym.fresh(p.name)
        Param(name, p.typ)
      }
      val boundIndices = deriveBoundIndices(pat, demandPat)
      // for each body there can be multiple input bodies (due to multiple pattern calls)
      val inputPatterns = patterns.flatMap { p =>
        p.bodies.flatMap { body =>
          body.constraints.zipWithIndex.flatMap { case (constr, constrix) =>
            constr.asCall match {
              case Some((name, args)) =>
                if (name == pat.name && !constr.hints.contains(MagicSetHints.IgnoreCallKey)) {
                  val boundParams = boundIndices.map { i =>
                    Eq(args(i), Var(params(i).name))
                  }
                  if (boundParams.isEmpty) Seq()
                  else Seq(Body(body.constraints.take(constrix) ++ boundParams).withHints(body))
                }
                else
                  Seq()
              case _ => Seq()
            }
          }
        }
      }

      val boundParams = boundIndices.map(params)

      val extensionalBody = if (pat.hasHint(MagicSetHints.MainKey)) {
        Some(Body(Seq(
          ExtensionalCall(extensionalInputPatternName(pat.name), boundParams.map(p => Var(p.name)))
        )))
      } else {
        None
      }

      val inputPat = Pattern(None, inputPatternName(pat.name), boundParams, inputPatterns ++ extensionalBody)
      if (inputPat.bodies.nonEmpty)
        Seq(inputPat)
      else
        Seq()
    }

    private def collectBodiesCallingPat(caller: Pattern, callee: Pattern): Seq[Body] =
      caller.bodies.filter {
        _.constraints.exists {
          case Call(name, _, _, _) if name == callee.name => true
          case _ => false
        }
      }
  }
}
