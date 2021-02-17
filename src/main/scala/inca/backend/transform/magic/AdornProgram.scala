package inca.backend.transform.magic

import inca.backend.hints.{Hint, Hints, MagicSetHints}
import inca.backend.ir.Collect
import inca.backend.ir.GP._
import inca.backend.transform.Transformation
import inca.backend.transform.Transformer

sealed trait AdornmentTag
case object Bound extends AdornmentTag
case object Free extends AdornmentTag

object AdornProgram extends Transformation {

  override def transformer: Transformer = new Transformer {

    override def transformModule(module: Module): Module = {
      var adornedPatterns: Set[(Pattern, Seq[AdornmentTag])] = Set()


      val mainHints = collectMainPattern(module)
      val mains = mainHints.map { p =>
        val params = p.params.map(p => Var(p.name))
        val mainHint = p.hints(MagicSetHints.MainKey).asInstanceOf[MagicSetHints.Main]
        val adornment = mainHint.adorn.map(a => if(a) Bound else Free)
        (Call(p.name, params, transitive = false, neg = false), adornment)
      }
      var todo: Set[(Call, Seq[AdornmentTag])] = mains.toSet

      def visited(call: Call, callTags: Seq[AdornmentTag]): Boolean =
        adornedPatterns.exists { case (pat, _) =>
          pat.name == adornmentName(call.name, callTags)
        }

      // TODO currently we only consider single module without imports
      // We already ignore base relations because we do not call them
      // We assume that every variable that is used is introduced beforehand
      while(todo.nonEmpty) {
        val (current, currentTags) = todo.head
        todo = todo.tail

        if (!visited(current, currentTags)) {
          val pat = module.pats.find(_.name == current.name).getOrElse(sys.error(s"Pattern ${current.name} not found during adornment"))
          val adornedBody = pat.bodies.map { body =>
            val adornedConstraints = body.constraints.zipWithIndex.map { case (constr, i) =>
              constr match {
                case call: Call =>
                  val prevConstrs = body.constraints.take(i)
                  val boundIndices = currentTags.zipWithIndex.filter( _._1 == Bound).map(_._2)
                  val boundParams = boundIndices.map(pat.params).map(p => Var(p.name))
                  val fv = freeVars(prevConstrs, constr).diff(boundParams)
                  val adornmentTags: Seq[AdornmentTag] = call.args.map { arg =>
                    if (fv.contains(arg)) Free
                    else Bound
                  }
                  val adornedCall = Call(adornmentName(call.name, adornmentTags), call.args, call.transitive, call.neg)
                  todo += call -> adornmentTags
                  adornedCall
                case _ =>
                  constr
              }
            }
            Body(adornedConstraints)
          }
          // now we can construct the adorned pattern for this specific adornment
          adornedPatterns += Pattern(pat.vis, adornmentName(pat.name, currentTags), pat.params, adornedBody) -> currentTags
        }
      }

      val patterns = adornedPatterns.toSeq.map { case (pat, tags) => pat }
      Module(module.name, module.imports, patterns, module.scalaContent)
    }
  }

  private def collectMainPattern(module: Module): Seq[Pattern]=
    module.pats.filter { p => p.hints.contains(MagicSetHints.MainKey) }

  object CollectVars extends Collect[Var] {
    override def transVar(v: Var): Seq[Var] = Seq(v)
  }

  def freeVars(prev: Seq[Constraint], constraint: Constraint): Seq[Var] = {
    val prevBound = prev.foldLeft(Seq[Var]()) { case (res, c) => res ++ CollectVars.transConstraint(c) }
    CollectVars.transConstraint(constraint).diff(prevBound)
  }

  def adornmentName(name: Name, tags: Seq[AdornmentTag]): String =
    name + "_" + adornmentTagsToString(tags)

  def adornmentTagsToString(tags: Seq[AdornmentTag]): String = tags.map {
    case Free => "f"
    case Bound => "b"
  }.mkString
}
