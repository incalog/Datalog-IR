package inca.backend.transform.magic.demand

import inca.backend.hints.MagicSetHints
import inca.backend.ir.Datalog._
import inca.backend.ir.util.Collect
import inca.backend.optimize.EliminateAliases
import inca.backend.transform.{Transformation, Transformer}
import inca.runtime.context.DataModel

import scala.collection.mutable.ListBuffer


// This transformation consumes MagicSetHints.Main and MagicSetHints.FixedAdornment
// This transformation produces MagicSetHints.Adornment
object DeriveDemandPatterns extends Transformation {
  type Adornment = Seq[Boolean]


  override def transformer(dataModel: DataModel): Transformer = new Transformer {

    // we adorn the program but then return original program where to add hints for the found demand patterns
    override def transformModule(module: Module): Module = {
      // we need to eliminate aliases beforehand such that the order or renamings by = does not matter
      val noAliasesModule = EliminateAliases.optimizer(dataModel).optimizeModule(module)
      var adornedPatterns: Set[(Name, Adornment)] = Set()

      val mainHints = collectMainPattern(noAliasesModule)
      val mains = mainHints.map { p =>
        val mainHint = p.hints(MagicSetHints.MainKey).asInstanceOf[MagicSetHints.Main]
        val adornment = mainHint.adorn
        (p.name, adornment)
      }
      var todo: Set[(Name, Adornment)] = mains.toSet

      def visited(name: Name, adornment: Adornment): Boolean =
        adornedPatterns.contains(name -> adornment)

      // TODO currently we only consider single module without imports
      // We already ignore base relations because we do not call them
      // We assume that every variable that is used is introduced beforehand (left-to-right)
      while(todo.nonEmpty) {
        val (current, currentAdorn) = todo.head
        todo = todo.tail

        if (!visited(current, currentAdorn)) {
          val pat = noAliasesModule.pats.find(_.name == current).getOrElse(sys.error(s"Pattern $current not found during adornment"))
          // unvisitedPatterns -= pat
          pat.bodies.map { body =>
            val previous = ListBuffer[Atom]()
            val adornedAtoms = body.atoms.map { atom =>
              if (!atom.hasHint(MagicSetHints.IgnoreCallKey)) {
                val res = atom.asCall match {
                  case Some((name, args)) =>
                    val adorn = deriveAdornment(atom, args, previous.toList, currentAdorn, pat.params, body)
                    todo += name -> adorn
                    val oldAdornments = atom.hints.getOrElse(MagicSetHints.AdornmentsKey, MagicSetHints.Adornments.empty).asInstanceOf[MagicSetHints.Adornments]
                    val newAdornments = MagicSetHints.Adornments(oldAdornments.adorn + adorn)
                    atom.addHint(newAdornments)
                  case None =>
                    atom
                }
                previous += atom
                res
              } else {
                previous += atom
                atom
              }
            }
            Body(adornedAtoms).withHints(body)
          }
          adornedPatterns += pat.name -> currentAdorn
        }
      }

      // annotate original pattern with found demand patterns
      noAliasesModule.pats.foreach { p =>
        val demandPats = adornedPatterns.filter {
          case(name, _) => name == p.name
        }.map(_._2)
        p.addHint(MagicSetHints.DemandPatterns(demandPats))
      }
      noAliasesModule
    }
  }

  private def collectMainPattern(module: Module): Seq[Pattern]=
    module.pats.filter { p => p.hints.contains(MagicSetHints.MainKey) }


  def fixedAdornment(con: Atom): Option[Seq[Boolean]] =
    con.hints.get(MagicSetHints.FixedAdornmentKey).flatMap { case MagicSetHints.FixedAdornment(adorn) =>
      Some(adorn)
    }

  def deriveAdornment(con: Atom, args: Seq[Term], prevConstrs: Seq[Atom], tags: Adornment, params: Seq[Param], body: Body): Adornment = {
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

  def adornmentName(name: Name, adorn: Adornment): String =
    s"${name}_${adornmentToString(adorn)}"

  def adornmentToString(adorn: Adornment): String = adorn.map {
    case false => "f"
    case true => "b"
  }.mkString

  private object CollectVars extends Collect[Var] {
    override def transVar(v: Var): Seq[Var] = Seq(v)
  }

  def freeVars(prev: Seq[Atom], constraint: Atom): Set[Var] = {
    val prevBound = prev.foldLeft(Set[Var]()) { case (res, c) => res ++ CollectVars.transAtom(c) }
    val vars = CollectVars.transAtom(constraint).toSet
    vars.diff(prevBound)
  }
}
