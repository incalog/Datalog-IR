package inca.ir.extension.data

import inca.ir.Hint.preserveHints
import inca.ir.extension.not
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{Atom, ModuleEntry, Relation, Term, Type, Var}

trait Visitor extends BaseIRVisitor with not.Visitor:
  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match
    case NegDeconstruct(t, caseName) =>
      val ts = visitTerm(t)
      ts.map(NegDeconstruct(_, caseName))
    case Deconstruct(t, caseName, args) =>
      val ts = visitTerm(t)
      val aargs = args.flatMap(visitArg) 
      ts.map(Deconstruct(_, caseName, aargs))
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
    case Deconstruct(t, caseName, args) => NegDeconstruct(t, caseName)
    case _ => super.negateAtom(atom)
