package inca.backend.optimize

import inca.backend.hints.MagicSetHints.MainKey
import inca.backend.ir.Datalog._
import inca.backend.ir.{CollectVars, Datalog, Substitute}
import inca.runtime.context.DataModel
import inca.util.Gensym

object InlineSimpleRelations extends Optimization {
  override def optimizer(dataModel: DataModel): Optimizer = new Optimizer {
    private val gensym: Gensym = new Gensym(Iterable.empty)
    private var retainInlined: Set[Name] = Set()

    def shouldInline(pat: Pattern): Boolean = {
      val singleBody = pat.bodies.size == 1
      val isMain = pat.hasHint(MainKey)
      lazy val calls = pat.bodies.head.atoms.collect { case call: Call => call }
      lazy val singleCall = calls.size == 1
      lazy val directlyRecursive = calls.head.name == pat.name
      lazy val hasEvaluation = pat.bodies.head.atoms.exists { case Computed(_, _) => true; case _ => false }
      singleBody && !isMain && singleCall && !directlyRecursive && !hasEvaluation
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
          case Some(inline) => gensym.scoped {
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
      val newbodies = pat.bodies.map(inlineRelationBody(_, inline))
      Pattern(pat.vis, pat.name, pat.params, newbodies).withHints(pat)
    }

    def inlineRelationBody(body: Body, inline: Pattern): Body =
      Body(body.atoms.flatMap(inlineRelationAtom(_, inline))).withHints(body)

    def inlineRelationAtom(atom: Atom, inline: Pattern): Seq[Atom] = atom match {
      case Call(name, args, false, false) if name == inline.name =>
        val body = inline.bodies.head
        val paramSubst = inline.params.map(_.name).zip(args).toMap
        var renamings: Map[String, String] = Map()
        def substFun(v: Var): Term = paramSubst.get(v.name) match {
          case Some(t) => t
          case None => renamings.get(v.name) match {
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
        val body_ = new Substitute(substFun).substBody(body)
        body_.atoms

      case Computed(_, CountAggregation(name, _)) if name == inline.name =>
        retainInlined += name
        Seq(atom)
      case Computed(_, CustomAggregation(_, _, _, name, _, _)) if name == inline.name =>
        retainInlined += name
        Seq(atom)
      case _ => Seq(atom)
    }
  }
}
