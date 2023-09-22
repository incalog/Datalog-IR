package inca.ir.extension.datamatch

import inca.ir.extension.data.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.*
import inca.ir.extension.data


trait Typechecker extends data.Typechecker:
  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Match(matchee, cases) =>
      val dataType = inferTerm(matchee, mode.inverted).ty match
        case TData(name) => Some(name.name)
        case ty => error(s"Expected data type but got $ty", matchee); None

      cases.foreach {
        case Case(name, patVars, body) =>
          val params = lookupConstruct(name, atom) match
            case Some((DataDefinition(dname, _), CaseDefinition(_, params))) =>
              if (!dataType.forall(_ == dname.name))
                error(s"Constructor $name does not belong to matchee's data type $dataType", name)
              params
            case None => Seq()

          if (params.size != patVars.size)
            error(s"Expected ${params.size} arguments, but got ${patVars.size}", atom)

          scopedTypeContext {
            patVars.zip(params).foreach { (v, ty) => checkTerm(v, ty, mode) }
            body.foreach(at => checkAtom(at, mode))
          }
      }
    case _ => super.checkAtom(atom, mode)
