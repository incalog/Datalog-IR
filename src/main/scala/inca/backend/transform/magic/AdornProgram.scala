package inca.backend.transform.magic

import inca.backend.hints.MagicSetHints
import inca.backend.ir.Collect
import inca.backend.ir.GP._
import inca.backend.transform.{Transformation, Transformer}



// This transformation consumes MagicSetHints.Main and MagicSetHints.FixedAdornment
// This transformation produces MagicSetHints.Adornment
object AdornProgram extends Transformation {
  type Adornment = Seq[Boolean]


  override def transformer: Transformer = new Transformer {

    override def transformModule(module: Module): Module = {
      var adornedPatterns: Set[(Pattern, Adornment)] = Set()

      var unvisitedPatterns: Set[Pattern] = module.pats.toSet

      val mainHints = collectMainPattern(module)
      val mains = mainHints.map { p =>
        val params = p.params.map(p => Var(p.name))
        val mainHint = p.hints(MagicSetHints.MainKey).asInstanceOf[MagicSetHints.Main]
        val adornment = mainHint.adorn
        (Call(p.name, params, transitive = false, neg = false), adornment)
      }
      var todo: Set[(Call, Adornment)] = mains.toSet

      def visited(call: Call, adornment: Adornment): Boolean =
        adornedPatterns.exists { case (pat, _) =>
          pat.name == adornmentName(call.name, adornment)
        }

      // TODO currently we only consider single module without imports
      // We already ignore base relations because we do not call them
      // We assume that every variable that is used is introduced beforehand (left-to-right)
      while(todo.nonEmpty) {
        val (current, currentAdorn) = todo.head
        todo = todo.tail

        if (!visited(current, currentAdorn)) {
          val pat = module.pats.find(_.name == current.name).getOrElse(sys.error(s"Pattern ${current.name} not found during adornment"))
          unvisitedPatterns -= pat
          val adornedBody = pat.bodies.map { body =>
            val adornedConstraints = body.constraints.zipWithIndex.map { case (constr, i) =>
              constr match {
                case call: Call =>
                  val (adornedCall, adorn) = deriveAdornment(i, currentAdorn, pat.params, body)
                  todo += call -> adorn
                  adornedCall.withHints(call).addHint(MagicSetHints.Adornment(adorn))
                case _ =>
                  constr
              }
            }
            Body(adornedConstraints).withHints(body)
          }
          // now we can construct the adorned pattern for this specific adornment
          val adornedPat =
            Pattern(
              pat.vis,
              adornmentName(pat.name, currentAdorn),
              pat.params,
              adornedBody
            ).withHints(pat).addHint(MagicSetHints.Adornment(currentAdorn))
          adornedPatterns += adornedPat -> currentAdorn
        }
      }

      val patterns = adornedPatterns.toSeq.map(_._1)// ++ unvisitedPatterns
      Module(module.name, module.imports, module.data, patterns, module.scalaContent)
    }
  }

  private def collectMainPattern(module: Module): Seq[Pattern]=
    module.pats.filter { p => p.hints.contains(MagicSetHints.MainKey) }

  object CollectVars extends Collect[Var] {
    override def transVar(v: Var): Seq[Var] = Seq(v)
  }

  def fixedAdornment(call: Call): Option[Seq[Boolean]] =
    call.hints.get(MagicSetHints.FixedAdornmentKey).flatMap { case MagicSetHints.FixedAdornment(adorn) =>
      Some(adorn)
    }

  def deriveAdornment(index: Int, tags: Adornment, params: Seq[Param], body: Body): (Call, Adornment) = {
    val call = body.constraints(index).asInstanceOf[Call]
    val prevConstrs = body.constraints.take(index)
    // generate adornment based on fixed adornment hint or on the already bound inputs
    fixedAdornment(call) match {
      case Some(adorn) =>
        (Call(adornmentName(call.name, adorn), call.args, call.transitive, call.neg), adorn)
      case None =>
        val boundIndices = tags.zipWithIndex.filter( _._1).map(_._2)
        val boundParams = boundIndices.map(params).map(p => Var(p.name))
        val fv = freeVars(prevConstrs, call).diff(boundParams)
        val adorn = call.args.map(a => !fv.contains(a))
        (Call(adornmentName(call.name, adorn), call.args, call.transitive, call.neg), adorn)
    }
  }

  def freeVars(prev: Seq[Constraint], constraint: Constraint): Seq[Var] = {
    val prevBound = prev.foldLeft(Seq[Var]()) { case (res, c) => res ++ CollectVars.transConstraint(c) }
    CollectVars.transConstraint(constraint).diff(prevBound)
  }

  def adornmentName(name: Name, adorn: Adornment): String =
    s"${name}_${adornmentToString(adorn)}"

  def adornmentToString(adorn: Adornment): String = adorn.map {
    case false => "f"
    case true => "b"
  }.mkString
}
