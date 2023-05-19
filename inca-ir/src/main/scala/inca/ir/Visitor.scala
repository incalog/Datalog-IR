package inca.ir

import inca.ir
import inca.ir.extensions.*
import inca.ir.{Atom, Body, Call, ModuleEntry, Param, Relation, TInt, Term, Type, Var}

import scala.collection.immutable.Seq

trait BaseIRVisitor:
  def visit(module: ir.Module): ir.Module =
    ir.Module(module.name, module.lang, module.contents.flatMap(visitModuleEntry))

  def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match {
    case rel: Relation => visitRelation(rel)
    case _ => throw IllegalArgumentException(s"Can not visit unknown object: $moduleEntry")
  }

  def visitRelation(relation: Relation): Seq[Relation] =
    Seq(Relation(relation.name, relation.params.flatMap(visitParam), relation.bodies.flatMap(visitBody)))

  def visitParam(param: Param): Seq[Param] =
    Seq(Param(param.name, visitType(param.ty).head))

  def visitBody(body: Body): Seq[Body] =
    Seq(Body(body.atoms.flatMap(visitAtom)))

  def visitAtom(atom: Atom): Seq[Atom] = Seq(atom match {
    case Call(name, terms) => Call(name, terms.map(visitTerm))
    case _ => throw IllegalArgumentException(s"Can not visit unknown atom: $atom")
  })

  def visitTerm(term: Term): Term = term match {
    case Var(name) => Var(name)
    case _ => throw IllegalArgumentException(s"Can not visit unknown term: $term")
  }

  def visitType(ty: Type): Seq[Type] = ty match {
    case TInt => Seq(TInt)
    case _ => throw IllegalArgumentException(s"Can not visit unknown type: $ty")
  }


trait DisjunctionIRVisitor extends BaseIRVisitor:
  override def visitAtom(atom: Atom): Seq[Atom] = atom match {
    case Disjunction(as1, as2) => Seq(Disjunction(as1.flatMap(visitAtom), as2.flatMap(visitAtom)))
    case _ => super.visitAtom(atom)
  }

trait IRVisitor extends BaseIRVisitor with DisjunctionIRVisitor

// // our current module language must at least include the features of the target language
// override def loweredIRs: Set[IR] = Set(new DisjunctionIR {})
// Module(module.name, module.lang -- loweredIRs, module.contents.flatMap(visitModuleEntry))
// module.lang.includes(trg.requires)

  /*
  type Alternatives[A] = Seq[A]
  var alternativeAtoms: Alternatives[Seq[Atom]] = Alternatives()

  override def lowerBody(body: src.Body): Seq[trg.Body] = {
    // flatten all disjunctions
    val res = trg.Body(body.atoms.flatMap(lowerAtom))

    val alternativeAtoms: Alternatives[Seq[src.Atom]] = body.atoms.foldLeft[Seq[Seq[src.Atom]]](Seq(Seq())) {
        case (res, src.Disjunction(as1, as2)) => res.map(_ ++ as1) ++ res.map(_ ++ as2)
        case (res, a) => res.map(_ ++ Seq(a))
    }
    // translate the remaining atoms and generate a body for each alternative
    alternativeAtoms.map(atoms => {
      trg.Body(atoms.flatMap(lowerAtom))
    })
  }*/
