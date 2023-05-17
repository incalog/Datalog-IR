package inca.ir

import scala.collection.immutable.Seq

trait Lowering[S <: IR, T <: IR]:
  val src: S
  val trg: T
  def module: Module

  def loweredIRs: Set[IR]

  def lower: Module = {
    // our current module language must at least include the features of the target language
    module.lang.includes(trg.requires)

    Module(module.name, module.lang -- loweredIRs, module.contents.flatMap {
      case rel: src.Relation => lowerRelation(rel)
      case other => throw MatchException("Other: " + other + "   " + other.getClass)
    })
  }

  def lowerRelation(relation: src.Relation): Seq[trg.Relation] =
    Seq(trg.Relation(relation.name, relation.params.flatMap(lowerParam), relation.bodies.flatMap(lowerBody)))

  def lowerParam(param: src.Param): Seq[trg.Param] =
    Seq(trg.Param(param.name, lowerType(param.ty).head))

  def lowerBody(body: src.Body): Seq[trg.Body] =
    Seq(trg.Body(body.atoms.flatMap(lowerAtom)))

  def lowerAtom(atom: src.Atom): Seq[trg.Atom] = Seq(atom match {
    case src.Eq(lhs, rhs) => trg.Eq(lowerTerm(lhs), lowerTerm(rhs))
    case src.Neq(lhs, rhs) => trg.Neq(lowerTerm(lhs), lowerTerm(rhs))
    case src.Call(name, terms) => trg.Call(name, terms.map(lowerTerm))
  })

  def lowerTerm(term: src.Term): trg.Term = term match {
    case src.Num(value) => trg.Num(value)
    case src.Var(name) => trg.Var(name)
    case src.Add(lhs, rhs) => trg.Add(lowerTerm(lhs), lowerTerm(rhs))
    case src.Mul(lhs, rhs) => trg.Mul(lowerTerm(lhs), lowerTerm(rhs))
    case src.Abs(t) => trg.Abs(lowerTerm(t))
    case src.Min(lhs, rhs) => trg.Min(lowerTerm(lhs), lowerTerm(rhs))
  }

  def lowerType(ty: src.Type): Seq[trg.Type] = ty match {
    case src.TInt => Seq(trg.TInt)
  }


trait BooleanLowering[S <: BooleanIR, T <: IR] extends Lowering[S, T]:
  override def loweredIRs: Set[IR] = Set(new BooleanIR {})

  override def lowerAtom(atom: src.Atom): Seq[trg.Atom] = atom match {
    case src.BoolAtom(t) => Seq(trg.Eq(trg.Num(1), lowerTerm(t)))
    case _ => super.lowerAtom(atom)
  }

  override def lowerTerm(term: src.Term): trg.Term = term match {
    case src.BoolFalse => trg.Num(0)
    case src.BoolTrue => trg.Num(1)
    case src.BoolAnd(t1, t2) => trg.Mul(lowerTerm(t1), lowerTerm(t2))
    case src.BoolNot(t) => trg.Abs(trg.Add(lowerTerm(t), trg.Num(-1)))
    case src.BoolOr(t1, t2) => trg.Min(trg.Add(lowerTerm(t1), lowerTerm(t2)), trg.Num(1))
    case _ => super.lowerTerm(term)
  }

  override def lowerType(ty: src.Type): Seq[trg.Type] = ty match {
    case src.TBoolean => Seq(trg.TInt)
    case _ => super.lowerType(ty)
  }

trait DisjunctionLowering[S <: DisjunctionIR, T <: IR] extends Lowering[S, T]:
  type Alternatives[A] = Seq[A]

  override def loweredIRs: Set[IR] = Set(new DisjunctionIR {})

  var alternativeAtoms: Alternatives[Seq[src.Atom]] = Alternatives()

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
  }

  // Override this to resolve the disjunction
  override def lowerAtom(atom: src.Atom): Seq[trg.Atom] = super.lowerAtom(atom)

trait PreserveDisjunction[S <: DisjunctionIR, T <: DisjunctionIR] extends Lowering[S, T]:
  override def lowerAtom(atom: src.Atom): Seq[trg.Atom] = atom match {
    // Preserve disjunctions
    case src.Disjunction(as1, as2) => Seq(trg.Disjunction(as1.flatMap(lowerAtom), as2.flatMap(lowerAtom)))
    case _ => super.lowerAtom(atom)
  }

trait PreserveBoolean[S <: BooleanIR, T <: BooleanIR] extends Lowering[S, T]:
  // Preserve all boolean atoms and terms
  override def lowerAtom(atom: src.Atom): Seq[trg.Atom] = atom match {
    case src.BoolAtom(t) => Seq(trg.BoolAtom(lowerTerm(t)))
    case _ => super.lowerAtom(atom)
  }

  override def lowerTerm(term: src.Term): trg.Term = term match {
    case src.BoolFalse => trg.BoolFalse
    case src.BoolTrue => trg.BoolTrue
    case src.BoolAnd(t1, t2) => trg.BoolAnd(lowerTerm(t1), lowerTerm(t2))
    case src.BoolNot(t) => trg.BoolNot(trg.Add(lowerTerm(t), trg.Num(-1)))
    case src.BoolOr(t1, t2) => trg.BoolOr(lowerTerm(t1), lowerTerm(t2))
    case _ => super.lowerTerm(term)
  }