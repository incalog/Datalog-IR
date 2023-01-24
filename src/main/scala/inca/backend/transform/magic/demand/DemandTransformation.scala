package inca.backend.transform.magic.demand

import inca.backend.hints.Hints
import inca.backend.hints.MagicSetHints
import inca.backend.hints.MagicSetHints.InputCall
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog._
import inca.backend.transform.FilterBodyTransformer
import inca.backend.transform.Transformation
import inca.backend.transform.Transformer
import inca.runtime.context.DataModel
import inca.util.Gensym

// This transformation consumes MagicSetHints.IgnoreCall and MagicSetHints.NoInputRelation
object DemandTransformation extends Transformation {

  val demandPatternPrefix = "input$"
  val demandPatternExtensionalPrefix = "ext_input$"
  def inputPatternName(name: Name): String = demandPatternPrefix + name
  def extensionalInputPatternName(name: Name): String = demandPatternExtensionalPrefix + name

  override def transformer(dataModel: DataModel): Transformer = new Transformer {

    val gensym = new Gensym(Seq())

    override def transformModule(mod: Module): Module = {
      val insertedInputCallPats = mod.pats.flatMap(transformPattern)
      insertedInputCallPats.foreach(p => gensym.register(Datalog.collectVarNames.transPattern(p)))

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
        val hasEmptyInput = body.atoms.exists { con =>
          con.hasHint(InputCall.key) && !inputPatNames.contains(con.asInstanceOf[Call].name)
        }
        !hasEmptyInput
      })
      val filteredPats = allPats.flatMap(filter.transformPattern)

      Module(mod.name, mod.imports, filteredPats, mod.scalaContent)
    }

    override def transformPattern(pat: Pattern): Seq[Pattern] =
      if (pat.hasHint(MagicSetHints.DemandPatterns.key)) {
        val demandPats = getDemandPatterns(pat)
        demandPats.adorn.map { demandPat =>
          val extendedPatterns = insertInputCall(pat, demandPat)
          extendedPatterns
        }.toSeq
      } else {
        Seq(pat)
      }

    private def shouldDeriveInput(pat: Pattern): Boolean =
      shouldInsertInput(pat) && pat.hasHint(MagicSetHints.DemandPatterns.key)

    private def shouldInsertInput(body: Hints): Boolean =
      !body.hasHint(MagicSetHints.NoInputRelation.key)

    private def getDemandPatterns(pat: Pattern): MagicSetHints.DemandPatterns = {
      if (!pat.hasHint(MagicSetHints.DemandPatterns.key)) MagicSetHints.DemandPatterns(Set())
      else pat.hints(MagicSetHints.DemandPatterns.key).asInstanceOf[MagicSetHints.DemandPatterns]
    }

    private def insertInputCall(pat: Pattern, demandPat: Seq[Boolean]): Pattern = {
      if (!shouldInsertInput(pat))
        pat
      else if (pat.bodies.isEmpty) {
        val body = deriveInputCall(pat, demandPat).map(c => Body(Seq(c)))
        Pattern(pat.vis, pat.name, pat.params, body.toSeq).withHints(pat)
      } else {
        val bodies = pat.bodies.map { b =>
          if (shouldInsertInput(b)) {
            val inputCall = deriveInputCall(pat, demandPat)
            Body(inputCall.toSeq ++ b.atoms).withHints(b)
          } else {
            b
          }
        }
        Pattern(pat.vis, pat.name, pat.params, bodies).withHints(pat)
      }
    }

    private def deriveInputCall(pat: Pattern, demandPat: Seq[Boolean]): Option[Call] = {
      val boundParams = deriveBoundParams(pat, demandPat)
      if (boundParams.isEmpty) {
        Some(Call(inputPatternName(pat.name), List(Var("_"))).addHint(InputCall(pat.name)))
      } else {
        val args = boundParams.map(p => Var(p.name))
        Some(Call(inputPatternName(pat.name), args).addHint(InputCall(pat.name)))
      }
    }

    private def deriveBoundParams(pat: Pattern, demandPat: Seq[Boolean]): Seq[Param] = {
      val indexBoundParams = deriveBoundIndices(demandPat)
      indexBoundParams.map(pat.params)
    }

    private def deriveBoundIndices(demandPat: Seq[Boolean]): Seq[Int] = {
      demandPat.zipWithIndex.filter(_._1).map(_._2)
    }

    private def deriveInputPattern(
        pat: Pattern,
        demandPat: Seq[Boolean],
        patterns: Seq[Pattern]
      ): Seq[Pattern] = gensym.scoped {
      if (!shouldDeriveInput(pat))
        Seq()
      else {
        // generate new names for pattern params to avoid name collision
        val params = pat.params.map { p =>
          val name = gensym.fresh(p.name)
          Param(name, p.typ)
        }
        val boundIndices = deriveBoundIndices(demandPat)
        val dummyParam =
          if (boundIndices.isEmpty)
            Some(Param(gensym.fresh("dummy"), base.TScalaBoolean))
          else
            None
        val dummyBinding = dummyParam.map(p => Eq(Var(p.name), base.True))

        // for each body there can be multiple input bodies (due to multiple pattern calls)
        val inputPatterns = patterns.flatMap { p =>
          p.bodies.flatMap { body =>
            body.atoms.zipWithIndex.flatMap { case (atom, atomix) =>
              atom.asCall match {
                case Some((name, args)) =>
                  if (name == pat.name && !atom.hints.contains(MagicSetHints.IgnoreCallKey)) {
                    val bindings = boundIndices.map { i =>
                      Eq(args(i), Var(params(i).name))
                    }
                    Seq(Body(body.atoms.take(atomix) ++ bindings ++ dummyBinding).withHints(body))
                  } else
                    Seq()
                case _ => Seq()
              }
            }
          }
        }

        val boundParams = boundIndices.map(params)

        val extensionalBody = if (pat.hasHint(MagicSetHints.MainKey)) {
          val extCall = ExtensionalCall(
            extensionalInputPatternName(pat.name),
            boundParams.map(p => Var(p.name))
          )
          Some(Body(Seq(extCall) ++ dummyBinding))
        } else {
          None
        }

        val inputPat = Pattern(
          None,
          inputPatternName(pat.name),
          boundParams ++ dummyParam,
          inputPatterns ++ extensionalBody
        ).addHint(MagicSetHints.InputRelation)
        if (inputPat.bodies.nonEmpty)
          Seq(inputPat)
        else
          Seq()
      }
    }
  }
}
