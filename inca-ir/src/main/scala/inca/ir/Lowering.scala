package inca.ir

trait Lowering[S <: IR, T <: IR](module: Module):
  def lower: Module =
    Module(module.name, module.lang, module.contents.flatMap {
      case rel: IR.Relation => lowerRelation(rel)
      case _ => ???
    })

  def lowerRelation(relation: IR.Relation): Seq[IR.Relation] =
    Seq(IR.Relation(relation.name, relation.params.flatMap(lowerParam), relation.bodies.flatMap(lowerBody)))

  def lowerParam(param: IR.Param): Seq[IR.Param] =
    Seq(IR.Param(param.name, lowerType(param.ty).head))
  def lowerBody(body: IR.Body): Seq[IR.Body] =
    Seq(IR.Body(body.atoms.flatMap(lowerAtom)))
  def lowerAtom(atom: IR.Atom): Seq[IR.Atom] = Seq(atom match {
    case BooleanIR.BoolAtom(t) => BooleanIR.BoolAtom(lowerTerm(t))
    case DisjunctionIR.Disjunction(as1, as2) => DisjunctionIR.Disjunction(lowerTerm(as1), lowerTerm(as2))
    case SetIR.SetAtom(ts) => SetIR.SetAtom(ts.map(lowerTerm))
    case TupleIR.TupleAtom(ts) => TupleIR.TupleAtom(ts.map(lowerTerm))
    case a => a
  })

  def lowerTerm(term: IR.Term): IR.Term = term

  def lowerType(ty: IR.Type): Seq[IR.Type] = Seq(ty)


class BooleanToPure(module: Module) extends Lowering[BooleanIR, IR](module):
  override def lowerAtom(atom: IR.Atom): Seq[IR.Atom] = atom match {
    case BooleanIR.BoolAtom(t) => ???
    case a => Seq(a)
  }

  override def lowerTerm(term: IR.Term): IR.Term = term match {
    case BooleanIR.BoolAnd(t1, t2) => ???
    case BooleanIR.BoolNot(t) => ???
    case BooleanIR.BoolOr(t1, t2) => ???
    case _ => ???
  }

  override def lowerType(ty: IR.Type): Seq[IR.Type] = ty match {
    case BooleanIR.TBoolean => IR.TInt
    case _ => Seq(ty)
  }

class TupleToPure(module: Module) extends Lowering[TupleIR, IR](module):
  override def lowerParam(param: IR.Param): Seq[IR.Param] =
    lowerType(param.ty).zipWithIndex.map { case (ty, i) =>
      IR.Param(Name(param.name.name + "$" + i.toString), ty)
    }

  override def lowerAtom(atom: IR.Atom): Seq[IR.Atom] = atom match {
    case TupleIR.TupleAtom(ts) => ts // TODO: flatten the tuple. In the worst case we need to match every kind of atom
    case a => super.lowerAtom(a)
  }

  override def lowerType(ty: IR.Type): Seq[IR.Type] = ty match {
    case TupleIR.TTuple(tys) => tys
    case _ => Seq(ty)
  }
