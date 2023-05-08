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
  def lowerAtom(atom: IR.Atom): Seq[IR.Atom] = Seq(atom)
  def lowerType(ty: IR.Type): Seq[IR.Type] = Seq(ty)


class TupleToPure(module: Module) extends Lowering[TupleIR, IR](module):
  override def lowerParam(param: IR.Param): Seq[IR.Param] =
    lowerType(param.ty).zipWithIndex.map { case (ty, i) =>
      IR.Param(Name(param.name.name + "$" + i.toString), ty)
    }

  override def lowerType(ty: IR.Type): Seq[IR.Type] = ty match {
    case TupleIR.TTuple(tys) => tys
    case _ => Seq(ty)
  }
