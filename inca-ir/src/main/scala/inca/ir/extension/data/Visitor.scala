package inca.ir.extension.data

import inca.ir.Hint.preserveHints
import inca.ir.extension.not
import inca.ir.visitors.BaseIRVisitor
import inca.ir.*

import scala.language.postfixOps

trait Visitor extends BaseIRVisitor with not.Visitor:
  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match
    case Deconstruct(t, caseRef, Seq(), neg) =>
      val eq = Eq(t, Construct(caseRef, Seq()), neg)
      this.visitAtom(eq)
    case Deconstruct(t, caseRef, args, neg) =>
      val ts = visitTerm(t)
      val aargs = args.flatMap(visitArg)
      ts.map(Deconstruct(_, visitRef(caseRef), aargs, neg))
    case _ => super.visitAtom(atom))

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = preserveHints(moduleEntry)(moduleEntry match
    case DataDefinition(name) => Seq(DataDefinition(name))
    case CaseDefinition(name, args, data) =>
      Seq(CaseDefinition(name, args.map(visitType), visitType(data).asInstanceOf[TData]))
    case ProvideDataDefinition(ref) => Seq(ProvideDataDefinition(visitRef(ref)))
    case RequireDataDefinition(name) => Seq(ProvideDataDefinition(name))
    case ProvideCaseDefinition(ref, args, data) =>
      Seq(ProvideCaseDefinition(visitRef(ref), args.map(visitType), visitType(data).asInstanceOf[TData]))
    case RequireCaseDefinition(name, args, data) =>
      Seq(RequireCaseDefinition(name, args.map(visitType), data))
    case _ => super.visitModuleEntry(moduleEntry))

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term)(term match
    case Construct(ref, data) => Seq(Construct(visitRef(ref), data.flatMap(visitTerm)))
    case _ => super.visitTerm(term))

  override def visitType(ty: Type): Type = preserveHints(ty)(ty match
    case TData(ref) => TData(visitRef(ref))
    case _ => super.visitType(ty))

  override def negateAtom(atom: Atom): Atom = atom match
    case Deconstruct(t, caseRef, args, neg) => Deconstruct(t, caseRef, args, !neg)
    case _ => super.negateAtom(atom)

