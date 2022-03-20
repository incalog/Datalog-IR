package inca.backend.transform.magic

import inca.backend.hints.MagicSetHints
import inca.backend.ir.Collect
import inca.backend.ir.Datalog._
import inca.backend.transform.Transformation
import inca.backend.transform.Transformer
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
        val mainHint = p.hints(MagicSetHints.Main.key).asInstanceOf[MagicSetHints.Main]
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
      while (todo.nonEmpty) {
        val (current, currentAdorn) = todo.head
        todo = todo.tail

        if (!visited(current, currentAdorn)) {
          val pat = module.pats.find(_.name == current).getOrElse(
            sys.error(s"Pattern $current not found during adornment")
          )
          unvisitedPatterns -= pat
          val adornedBody = pat.bodies.map { body =>
            val previous = ListBuffer[Atom]()
            val adornedAtoms = body.atoms.map { atom =>
              val res = atom.asCall match {
                case Some((name, args)) =>
                  val adorn =
                    deriveAdornment(atom, args, previous.toList, currentAdorn, pat.params, body)
                  val adorned = atom.replaceCall(adornmentName(name, adorn), args)
                  todo += name -> adorn
                  adorned.withHints(atom).addHint(MagicSetHints.Adornment(adorn))
                case None =>
                  atom
              }
              previous += atom
              res
            }
            Body(adornedAtoms).withHints(body)
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

      val patterns = adornedPatterns.toSeq.map(_._1) // ++ unvisitedPatterns
      Module(module.name, module.imports, patterns, module.scalaContent)
    }
  }

  private def collectMainPattern(module: Module): Seq[Pattern] =
    module.pats.filter { p => p.hints.contains(MagicSetHints.Main.key) }

  object CollectVars extends Collect[Var] {
    override def transVar(v: Var): Seq[Var] = Seq(v)
  }

  def fixedAdornment(con: Atom): Option[Seq[Boolean]] =
    con.hints.get(MagicSetHints.FixedAdornment.key).flatMap {
      case MagicSetHints.FixedAdornment(adorn) =>
        Some(adorn)
    }

  def deriveAdornment(
      con: Atom,
      args: Seq[Term],
      prevConstrs: Seq[Atom],
      tags: Adornment,
      params: Seq[Param],
      body: Body
    ): Adornment = {
    // generate adornment based on fixed adornment hint or on the already bound inputs
    fixedAdornment(con) match {
      case Some(adorn) => adorn
      case None =>
        val boundIndices = tags.zipWithIndex.filter(_._1).map(_._2)
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

  def freeVars(prev: Seq[Atom], constraint: Atom): Set[Var] = {
    val prevBound = prev.foldLeft(Set[Var]()) { case (res, c) => res ++ CollectVars.transAtom(c) }
    val vars = CollectVars.transAtom(constraint).toSet
    vars.diff(prevBound)
  }

  def adornmentName(name: Name, adorn: Adornment): String =
    s"${name}_${adornmentToString(adorn)}"

  def adornmentToString(adorn: Adornment): String = adorn.map {
    case false => "f"
    case true => "b"
  }.mkString
}
