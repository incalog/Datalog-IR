package inca.ir.extension.data

import inca.ir.Hint.preserveHints
import inca.ir.extension.not
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{Atom, ModuleEntry, Relation, Term, Type, Var}

import scala.language.postfixOps

trait Visitor extends BaseIRVisitor with not.Visitor:
  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match
    case Deconstruct(t, caseName, args, neg) =>
      val ts = visitTerm(t)
      val aargs = args.flatMap(visitArg)
      ts.map(Deconstruct(_, caseName, aargs, neg))
    case _ => super.visitAtom(atom))

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = preserveHints(moduleEntry)(moduleEntry match
    case data: DataDefinition => Seq(visitDataDefinition(data))
    case _ => super.visitModuleEntry(moduleEntry))

  def visitDataDefinition(data: DataDefinition): DataDefinition = preserveHints(data) {
    DataDefinition(data.name, data.cases.map(visitDataCase))
  }

  def visitDataCase(c: CaseDefinition): CaseDefinition = preserveHints(c) {
    CaseDefinition(c.name, c.args.map(visitType))
  }

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term)(term match
    case Construct(name, data) => Seq(Construct(name, data.flatMap(visitTerm)))
    case _ => super.visitTerm(term))

  override def visitType(ty: Type): Type = preserveHints(ty)(ty match
    case TData(name) => TData(name)
    case _ => super.visitType(ty))

  override def negateAtom(atom: Atom): Atom = atom match
    case Deconstruct(t, caseName, args, neg) => Deconstruct(t, caseName, args.flatMap(visitArg), !neg)
    case _ => super.negateAtom(atom)
