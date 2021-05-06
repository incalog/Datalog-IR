package inca.backend.transform.magic

import inca.backend.hints.MagicSetHints.{InputCall, InputCallKey}
import inca.backend.hints.{Hints, MagicSetHints}
import inca.backend.ir.CollectVars
import inca.backend.ir.Datalog._
import inca.backend.transform.{FilterBodyTransformer, Transformation, Transformer}
import inca.runtime.context.DataModel
import inca.util.Gensym


// This transformation consumes MagicSetHints.IgnoreCall and MagicSetHints.NoInputRelation
object MagicSetTransformation extends Transformation {

  def inputPatternName(name: Name): String = "input_" + name
  def extensionalInputPatternName(name: Name): String = "ext_input_" + name

  override def transformer(dataModel: DataModel): Transformer = new Transformer {

    val gensym = new Gensym(Seq())

    override def transformModule(mod: Module): Module = {
      val insertedInputCallPats = mod.pats.flatMap(transformPattern)
      insertedInputCallPats.foreach(p => gensym.register(CollectVars.transPattern(p)))

      val inputPatterns = mod.pats.flatMap(deriveInputPattern(_, insertedInputCallPats))

      // remove bodies with calls to input relations that don't exist
      val inputPatNames = inputPatterns.map(_.name).toSet
      val allPats = insertedInputCallPats ++ inputPatterns
      val filter = new FilterBodyTransformer({ body =>
        val hasEmptyInput = body.atoms.exists { con =>
          con.hasHint(InputCallKey) && !inputPatNames.contains(con.asInstanceOf[Call].name)
        }
        !hasEmptyInput
      })
      val filteredPats = allPats.flatMap(filter.transformPattern)

      Module(mod.name, mod.imports, filteredPats, mod.scalaContent)
    }

    override def transformPattern(pat: Pattern): Seq[Pattern] =
      if (pat.hasHint(MagicSetHints.AdornmentKey)) {
        val extendedPattern = insertInputCall(pat)
        Seq(extendedPattern)
      } else {
        Seq(pat)
      }

    private def shouldDeriveInput(pat: Pattern): Boolean =
      shouldInsertInput(pat) && pat.hasHint(MagicSetHints.AdornmentKey)

    private def shouldInsertInput(body: Hints): Boolean =
      !body.hasHint(MagicSetHints.NoInputRelationKey)

    private def insertInputCall(pat: Pattern): Pattern = {
      if (!shouldInsertInput(pat))
        return pat

      if (pat.bodies.isEmpty) {
        val body = deriveInputCall(pat).map(c => Body(Seq(c)))
        return Pattern(pat.vis, pat.name, pat.params, body.toSeq).withHints(pat)
      }

      val bodies = pat.bodies.map { b =>
        if (shouldInsertInput(b)) {
          val inputCall = deriveInputCall(pat)
          Body(inputCall.toSeq ++ b.atoms).withHints(b)
        } else {
          b
        }
      }
      Pattern(pat.vis, pat.name, pat.params, bodies).withHints(pat)
    }

    private def deriveInputCall(pat: Pattern): Option[Call] = {
      val boundParams = deriveBoundParams(pat)
      if (boundParams.isEmpty) {
        None
      } else {
        val args = boundParams.map(p => Var(p.name))
        Some(Call(inputPatternName(pat.name), args).addHint(InputCall(pat.name)))
      }
    }

    private def deriveBoundParams(pat: Pattern): Seq[Param] = {
      val indexBoundParams = deriveBoundIndices(pat)
      indexBoundParams.map(pat.params)
    }

    private def deriveBoundIndices(pat: Pattern): Seq[Int] = {
      if (!pat.hasHint(MagicSetHints.AdornmentKey)) {
        throw new IllegalArgumentException(s"Cannot derive input pattern of non-adorned pattern ${pat.name}")
      }

      val adornment = pat.hints(MagicSetHints.AdornmentKey) match {
        case MagicSetHints.Adornment(adorn) => adorn
        case _ => throw new IllegalStateException("This cannot happen")
      }

      adornment.zipWithIndex.filter(_._1).map(_._2)
    }

    private def deriveInputPattern(pat: Pattern, patterns: Seq[Pattern]): Seq[Pattern] = gensym.scoped {
      if (!shouldDeriveInput(pat))
        return Seq()

      // generate new names for pattern params to avoid name collision
      val params = pat.params.map { p =>
        val name = gensym.fresh(p.name)
        Param(name, p.typ)
      }
      val boundIndices = deriveBoundIndices(pat)
      // for each body there can be multiple input bodies (due to multiple pattern calls)
      val inputPatterns = patterns.flatMap { p =>
        p.bodies.flatMap { body =>
          body.atoms.zipWithIndex.flatMap { case (atom, atomix) =>
            atom.asCall match {
              case Some((name, args)) =>
                if (name == pat.name && !atom.hints.contains(MagicSetHints.IgnoreCallKey)) {
                  val boundParams = boundIndices.map { i =>
                    Eq(args(i), Var(params(i).name))
                  }
                  if (boundParams.isEmpty) Seq()
                  else Seq(Body(body.atoms.take(atomix) ++ boundParams).withHints(body))
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
        _.atoms.exists {
          case Call(name, _, _, _) if name == callee.name => true
          case _ => false
        }
      }
  }
}
