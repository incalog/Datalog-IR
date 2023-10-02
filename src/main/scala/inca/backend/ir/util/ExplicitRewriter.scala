package inca.backend.ir.util

import inca.backend.ir.Datalog

trait CoreRewriteAction
object CoreRewriteAction {
  case class InsertAtom(rel: Datalog.Name, bodyIdx: Int, atomIdx: Int, atom: Datalog.Atom) extends CoreRewriteAction
  case class DeleteAtom(rel: Datalog.Name, bodyIdx: Int, atomIdx: Int) extends CoreRewriteAction
  case class InsertRelation(rel: Datalog.Pattern) extends CoreRewriteAction
  case class DeleteRelation(name: Datalog.Name) extends CoreRewriteAction
  case class ReplaceAtoms(rel: Datalog.Name, bodyIdx: Int, fromIdx: Int, toIdx: Int, atom: Datalog.Atom) extends CoreRewriteAction
}

trait RewriteAction extends CoreRewriteAction {
  def toCore: Seq[CoreRewriteAction]
}


object ExplicitRewriter {
  import CoreRewriteAction._

  // order of rewrites is significant
  def apply(mod: Datalog.Module, actions: Seq[CoreRewriteAction]): Datalog.Module = {
    var res = mod
    actions.foreach { a =>
      res = rewriteModule(res, a)
    }
    res
  }

  def apply(mod: Datalog.Module, action: CoreRewriteAction): Datalog.Module = {
    rewriteModule(mod, action)
  }

  def rewriteModule(mod: Datalog.Module, action: CoreRewriteAction): Datalog.Module = action match {
    case InsertRelation(rel) => Datalog.Module(mod.name, mod.imports, mod.pats :+ rel, mod.scalaContent)
    case DeleteRelation(name) => Datalog.Module(mod.name, mod.imports, mod.pats.filter(_.name != name), mod.scalaContent)
    case action =>
      val rel = action match {
        case InsertAtom(rel, _, _, _) => rel
        case DeleteAtom(rel, _, _) => rel
        case ReplaceAtoms(rel, _, _, _, _) => rel
      }
      val rewritePats = mod.pats.map { pat =>
        if (pat.name == rel)
          rewritePattern(pat, action)
        else
          pat
      }
      Datalog.Module(mod.name, mod.imports, rewritePats, mod.scalaContent)
  }

  def rewritePattern(pat: Datalog.Pattern, action: CoreRewriteAction): Datalog.Pattern = {
    val bodyIdx = action match {
      case InsertAtom(_, bodyIdx, _, _) => bodyIdx
      case DeleteAtom(_, bodyIdx, _) => bodyIdx
      case ReplaceAtoms(_, bodyIdx, _, _, _) => bodyIdx
      case action => throw new IllegalArgumentException(s"Rewrite $action is not applicable within $pat")
    }
    val bodyToRewrite = pat.bodies(bodyIdx)
    val rewriteBodies = pat.bodies.patch(bodyIdx, Seq(rewriteBody(bodyToRewrite, action)), 1)
    Datalog.Pattern(pat.vis, pat.name, pat.params, rewriteBodies).withHints(pat)
  }


  def rewriteBody(body: Datalog.Body, action: CoreRewriteAction): Datalog.Body = {
    val atoms = action match {
      case InsertAtom(_, _, idx, atom) => body.atoms.patch(idx, Seq(atom), 0)
      case DeleteAtom(_, _, idx) => body.atoms.patch(idx, Nil, 1)
      case ReplaceAtoms(_, _, fromIdx, toIdx, atom) => body.atoms.patch(fromIdx, Seq(atom), toIdx - fromIdx + 1)
      case action => throw new IllegalArgumentException(s"Rewrite $action is not applicable within $body")
    }
    Datalog.Body(atoms).withHints(body)
  }
}
