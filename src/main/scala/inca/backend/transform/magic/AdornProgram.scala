package inca.backend.transform.magic

import inca.backend.hints.MagicSetHints
import inca.backend.ir.Collect
import inca.backend.ir.GP._
import inca.backend.transform.{Transformation, Transformer}
import inca.runtime.context.DataModel

import scala.collection.mutable.ListBuffer



// This transformation consumes MagicSetHints.Main and MagicSetHints.FixedAdornment
// This transformation produces MagicSetHints.Adornment
object AdornProgram extends Transformation {
  type Adornment = Seq[Boolean]


  override def transformer(dataModel: DataModel): Transformer = new Transformer {

    override def transformModule(module: Module): Module = {
      var adornedPatterns: Set[(Pattern, Adornment)] = Set()
      var unvisitedPatterns: Set[Pattern] = module.pats.toSet

      val mainHints = collectMainPattern(module)
      val mains = mainHints.map { p =>
        val params = p.params.map(p => Var(p.name))
        val mainHint = p.hints(MagicSetHints.MainKey).asInstanceOf[MagicSetHints.Main]
        val adornment = mainHint.adorn
        (p.name, adornment)
      }
      var todo: Set[(Name, Adornment)] = mains.toSet

      def visited(name: Name, adornment: Adornment): Boolean =
        adornedPatterns.exists { case (pat, _) =>
          pat.name == adornmentName(name, adornment)
        }

      // TODO currently we only consider single module without imports
      // We already ignore base relations because we do not call them
      // We assume that every variable that is used is introduced beforehand (left-to-right)
      while(todo.nonEmpty) {
        val (current, currentAdorn) = todo.head
        todo = todo.tail

        if (!visited(current, currentAdorn)) {
          val pat = module.pats.find(_.name == current).getOrElse(sys.error(s"Pattern $current not found during adornment"))
          unvisitedPatterns -= pat
          val adornedBody = pat.bodies.map { body =>
            var previous = ListBuffer[Constraint]()
            val adornedConstraints = body.constraints.map { constr =>
              val res = constr.asCall match {
                case Some((name, args)) =>
                  val adorn = deriveAdornment(constr, args, previous.toList, currentAdorn, pat.params, body)
                  val adorned = constr.replaceCall(adornmentName(name, adorn), args)
                  todo += name -> adorn
                  adorned.withHints(constr).addHint(MagicSetHints.Adornment(adorn))
                case None =>
                  constr
              }
              previous += constr
              res
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
      Module(module.name, module.imports, patterns, module.scalaContent)
    }
  }

  private def collectMainPattern(module: Module): Seq[Pattern]=
    module.pats.filter { p => p.hints.contains(MagicSetHints.MainKey) }

  object CollectVars extends Collect[Var] {
    override def transVar(v: Var): Seq[Var] = Seq(v)
  }

  def fixedAdornment(con: Constraint): Option[Seq[Boolean]] =
    con.hints.get(MagicSetHints.FixedAdornmentKey).flatMap { case MagicSetHints.FixedAdornment(adorn) =>
      Some(adorn)
    }

  def deriveAdornment(con: Constraint, args: Seq[Term], prevConstrs: Seq[Constraint], tags: Adornment, params: Seq[Param], body: Body): Adornment = {
    // generate adornment based on fixed adornment hint or on the already bound inputs
    fixedAdornment(con) match {
      case Some(adorn) => adorn
      case None =>
        val boundIndices = tags.zipWithIndex.filter( _._1).map(_._2)
        val boundParams = boundIndices.map(params).map(p => Var(p.name))
        val fv = freeVars(prevConstrs, con).removedAll(boundParams)
        val adorn = args.map {
          case Var("_") => false
          case v: Var => !fv.contains(v)
          case _: Constant => true
        }
        adorn
    }
  }

  def freeVars(prev: Seq[Constraint], constraint: Constraint): Set[Var] = {
    val prevBound = prev.foldLeft(Set[Var]()) { case (res, c) => res ++ CollectVars.transConstraint(c) }
    val vars = CollectVars.transConstraint(constraint).toSet
    vars.diff(prevBound)
  }

  def adornmentName(name: Name, adorn: Adornment): String =
    s"${name}_${adornmentToString(adorn)}"

  def adornmentToString(adorn: Adornment): String = adorn.map {
    case false => "f"
    case true => "b"
  }.mkString
}
