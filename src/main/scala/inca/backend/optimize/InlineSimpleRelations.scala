package inca.backend.optimize

import inca.backend.hints.MagicSetHints.MainKey
import inca.backend.hints.OptimizationHints.KeepPattern
import inca.backend.ir.CollectVars
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog._
import inca.backend.ir.Substitute
import inca.runtime.context.DataModel
import inca.util.Gensym

object InlineSimpleRelations extends Optimization {
  override def optimizer(dataModel: DataModel): Optimizer = new Optimizer {
    private val gensym: Gensym = new Gensym(Iterable.empty)
    private var retainInlined: Set[Name] = Set()

    def shouldInline(pat: Pattern): Boolean = {
      val isMain = pat.hasHint(MainKey)
      val keepPattern = pat.hasHint(KeepPattern.key)
      lazy val containedCalls = pat.bodies.head.atoms.collect { case call: Call => call }
      lazy val directlyRecursive = containedCalls.exists(_.name == pat.name)
      lazy val hasEvaluation = pat.bodies.head.atoms.exists {
        case Computed(_, _) => true; case _ => false
      }
      val inline =
        pat.bodies.size <= 1 && !isMain && !keepPattern && containedCalls.size <= 100 && !directlyRecursive && !hasEvaluation
      inline
    }

    override def optimizeModule(module: Datalog.Module): Datalog.Module = {
      var pats = module.pats
      var inlined: Set[Name] = Set()
      retainInlined = Set()
      var progress = true

      while (progress) {
        val candidate = pats.find(p => !inlined.contains(p.name) && shouldInline(p))
        candidate match {
          case None =>
            progress = false
          case Some(inline) =>
            gensym.scoped {
              gensym.register(CollectVars.transPattern(inline))
              pats = pats.map { pat =>
                inlineRelation(pat, inline)
              }
              inlined += inline.name
              progress = true
            }
        }
      }

      pats = pats.filter(p => !inlined.contains(p.name) || retainInlined.contains(p.name))
      Module(module.name, module.imports, pats, module.scalaContent)
    }

    def inlineRelation(pat: Pattern, inline: Pattern): Pattern = gensym.scoped {
      pat.params.foreach(p => gensym.register(p.name))
      gensym.register(CollectVars.transPattern(pat))
      val newbodies = pat.bodies.flatMap(inlineRelationBodies(_, inline))
      Pattern(pat.vis, pat.name, pat.params, newbodies).withHints(pat)
    }

    def inlineRelationBodies(body: Body, inline: Pattern): Seq[Body] = {
      var newBodies: Seq[Seq[Atom]] = Seq(Seq())
      for (atom <- body.atoms) {
        val alts = inlineRelationAtom(atom, inline)
        newBodies = for {
          body <- newBodies
          alt <- alts
        } yield body ++ alt
      }
      newBodies.map(Body)
    }

    def inlineRelationAtom(atom: Atom, inline: Pattern): Seq[Seq[Atom]] = atom match {
      case Call(name, args, transitive, negative)
          if name == inline.name && transitive || negative =>
        retainInlined += name
        Seq(Seq(atom))
      case Call(name, args, false, false) if name == inline.name =>
        val paramSubst = inline.params.map(_.name).zip(args).toMap
        var renamings: Map[String, String] = Map()
        def substFun(v: Var): Term = paramSubst.get(v.name) match {
          case Some(t) => t
          case None =>
            renamings.get(v.name) match {
              case Some(newName) =>
                Var(newName)
              case None =>
                if (gensym.isRegistered(v.name)) {
                  val newName = gensym.fresh(v.name)
                  renamings += v.name -> newName
                  Var(newName)
                } else
                  v
            }
        }
        inline.bodies.map { inlineBody =>
          val body_ = new Substitute(substFun).substBody(inlineBody)
          body_.atoms
        }

      case Computed(_, CountAggregation(name, _)) if name == inline.name =>
        retainInlined += name
        Seq(Seq(atom))
      case Computed(_, CustomAggregation(_, _, _, name, _, _)) if name == inline.name =>
        retainInlined += name
        Seq(Seq(atom))
      case _ =>
        Seq(Seq(atom))
    }
  }
}
