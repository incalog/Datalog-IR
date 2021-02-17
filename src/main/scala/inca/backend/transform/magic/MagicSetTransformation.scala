package inca.backend.transform.magic

import inca.backend.ir.CollectVars
import inca.backend.ir.GP._
import inca.backend.transform.{Transformation, Transformer}
import inca.util.Gensym


// This transformation consumes MagicSetHints.IgnoreCall and MagicSetHints.NoInputRelation
object MagicSetTransformation extends Transformation {

  def inputPatternName(name: Name): String = "input_" + name

  override def transformer: Transformer = new Transformer {

    override def transformModule(mod: Module): Module = {
      val insertedInputCallPats = mod.pats.flatMap(transformPattern)
      val inputPatterns = mod.pats.flatMap(deriveInputPattern(_, insertedInputCallPats))
      Module(mod.name, mod.imports, insertedInputCallPats ++ inputPatterns, mod.scalaContent)
    }

    override def transformPattern(pat: Pattern): Seq[Pattern] = {
      val extendedPattern = insertInputCall(pat)
      Seq(extendedPattern)
    }
  }

  private def insertInputCall(pat: Pattern): Pattern = {
    val bodies = pat.bodies.map { b =>
      val inputCall = deriveInputCall(pat)
      Body(inputCall.toSeq ++ b.constraints)
    }
    Pattern(pat.vis, pat.name, pat.params, bodies)
  }

  private def deriveInputCall(pat: Pattern): Option[Call] = {
    val boundParams = deriveBoundParams(pat)
    if (boundParams.isEmpty) {
      None
    } else {
      val args = boundParams.map(p => Var(p.name))
      Some(Call(inputPatternName(pat.name), args, transitive = false, neg = false))
    }
  }

  private def deriveBoundParams(pat: Pattern): Seq[Param] = {
    val indexBoundParams = deriveBoundIndices(pat.name)
    indexBoundParams.map(pat.params)
  }

  private def deriveBoundIndices(name: Name): Seq[Int] = {
    val index = name.lastIndexOf("_")
    if (index == -1) {
      throw new IllegalArgumentException("Cannot derive input pattern of non-adorned pattern")
    }
    val adornmentTag = name.substring(index + 1)
    adornmentTag.zipWithIndex.filter(_._1 == 'b').map(_._2)
  }

  private def deriveInputPattern(pat: Pattern, modulePats: Seq[Pattern]): Seq[Pattern] = {
    // collect every pattern that calls pat
    val patterns = modulePats.flatMap { p =>
      val bodiesCallingPat = collectBodiesCallingPat(p, pat)
      if (bodiesCallingPat.nonEmpty)
        Some(Pattern(p.vis, p.name, p.params, bodiesCallingPat))
      else
        None
    }
    if (patterns.isEmpty)
      return Seq()

    // generate new names for pattern params to avoid name collision
    val usedVars = patterns.flatMap(CollectVars.apply).toSet
    val gensym = new Gensym(usedVars)
    val params = pat.params.map { p =>
      val name = gensym.fresh(p.name)
      Param(name, p.typ)
    }
    val boundIndices = deriveBoundIndices(pat.name)
    // for each body there can be multiple input bodies (due to multiple pattern calls)
    val inputPatterns = patterns.map { p =>
      val inputBodies = p.bodies.flatMap { body =>
        body.constraints.zipWithIndex.flatMap { case (constr, constrix) =>
          constr match {
            case Call(name, args, _, _) if name == pat.name =>
              val boundParams = boundIndices.map { i =>
                Eq(args(i), Var(params(i).name))
              }
              if (boundParams.isEmpty) Seq()
              else Seq(Body(body.constraints.take(constrix) ++ boundParams))
            case _ => Seq()
          }
        }
      }
      Pattern(p.vis, p.name, p.params, inputBodies)
    }

    // if the bodies are empty we do not create new pattern
    if (inputPatterns.isEmpty || inputPatterns.forall(_.bodies.isEmpty))
      return Seq()

    // rename so that params are the args of the call
    val renamedBodies = inputPatterns.flatMap(_.bodies)
    val boundParams = boundIndices.map(params)
    Seq(Pattern(None, inputPatternName(pat.name), boundParams, renamedBodies))
  }

  private def collectBodiesCallingPat(caller: Pattern, callee: Pattern): Seq[Body] =
    caller.bodies.filter {
      _.constraints.exists {
        case Call(name, _, _, _) if name == callee.name => true
        case _ => false
      }
    }

}
