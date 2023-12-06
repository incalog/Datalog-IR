package inca.ir.extension.datamatch

import inca.ir.*
import inca.ir.extension.data
import inca.ir.extension.data.*
import inca.ir.extension.typeparam
import inca.ir.extension.typeparam.TypeSubst
import inca.ir.typing.Mode


trait Typechecker extends data.Typechecker with typeparam.Typechecker:
  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Match(matchee, cases) =>
      val mathceeType = inferTerm(matchee, mode.inverted).ty

      checkAlternatives(cases) { case c@Case(ref, patVars, body) =>
        val params = lookupConstruct(ref.name, c) match
          case Some((typeParams, CaseDefinition(_, params, data))) =>
            val substMap0 = matchRef(ref, typeParams, c)

            val substMap = checkDeconstruct(mathceeType, data.ref, matchee)
            val subst = new TypeSubst(substMap)
            params.map(subst.visitType)
          case None => Seq()

        if (params.size != patVars.size)
          error(s"Expected ${params.size} arguments, but got ${patVars.size}", atom)

        scopedVariables(patVars.map(_.name).toSet) {
          patVars.zip(params).foreach { (v, ty) => checkTerm(v, ty, mode) }
          body.foreach(at => checkAtom(at, mode))
        }
      }
    case _ => super.checkAtom(atom, mode)
