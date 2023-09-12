package inca.ir.extension.data

import inca.ir.extension.data.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.{Atom, ModuleEntry, Relation, TAny, Term, Type, Var}


trait Typechecker extends BaseIRTypechecker with TypeContext:
  override def typecheck(moduleEntry: ModuleEntry): Unit = moduleEntry match
    case d: DataDefinition => bindData(d)
    case _ => super.typecheck(moduleEntry)

  protected override def inferTermExtend(term: Term, mode: Mode): Type = term match
    case Construct(name, args) => lookupConstruct(name, term) match
      case None =>
        error(s"Unknown constructor $name", term)
        TAny
      case Some((DataDefinition(dataName, _), CaseDefinition(_, params))) =>
        if (args.size != params.size)
          error(s"Expected ${params.size} arguments but got: ${args.size}", term)
        args.zip(params).foreach { case (t, ty) =>
          checkTerm(t, ty, Mode.Closed)
        }
        TData(dataName)
    case _ => super.inferTermExtend(term, mode)


  /*
   *
   * not t match {
   *   case P(x) => a1, a2
   *   case Q(y) => a1, a3
   * }
   *
   * not {
   *   P(t,x), a1, a2
   *   or
   *   Q(t,y), a1, a3
   * }
   *
   * not {
   *   P(t,x), a1, a2
   * } and not {
   *   Q(t,y), a1, a3
   * }
   *
   * !P(t,x), !Q(t,y) or !a1 or !a3
   * or
   * !a1, !Q(t,y) or !a1 or !a3
   * or
   * !a2, !Q(t,y) or !a1 or !a3
   *
   * !P(t,x), !Q(t,y)
   * or
   * !P(t,x), !a1
   * or
   * !P(t,x), !a3
   * or
   * !a1, !Q(t,y)
   * or
   * !a1, !a1
   * or
   * !a1, !a3
   * or
   * !a2, !Q(t,y)
   * or
   * !a2, !a1
   * or
   * !a2, !a3
   * @param mode
   */

  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Match(matchee, cases) =>
      val dataType = inferTerm(matchee, mode.inverted) match
        case TData(name) => Some(name.name)
        case ty => error(s"Expected data type but got $ty", matchee); None

      cases.foreach {
        case Case(name, args, body) =>
          val params = lookupConstruct(name, atom) match
            case Some((DataDefinition(dname, _), CaseDefinition(_, params))) =>
              if (!dataType.forall(_ == dname.name))
                error(s"Constructor $name does not belong to matchee's data type $dataType", name)
              params
            case None => Seq()

          if (params.size != args.size)
            error(s"Expected ${params.size} arguments, but got ${args.size}", atom)

          scopedTypeContext {
            args.zip(params).foreach { (v, ty) => checkTerm(v, ty, mode) }
            body.foreach(at => checkAtom(at, mode))
          }
      }
    case _ => super.checkAtom(atom, mode)
