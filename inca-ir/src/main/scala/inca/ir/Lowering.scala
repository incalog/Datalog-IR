package inca.ir

trait Lowering[S <: IR, T <: IR](module: Module)(val trg: T):
  val src: S = module.lang.features.find(ir => ir.isInstanceOf[S]).get.asInstanceOf[S]

  def lower: Module = {
    // our current module language must at least include the features of the target language
    module.lang.includes(trg.requires)

    Module(module.name, module.lang, module.contents.flatMap {
      case rel: src.Relation => lowerRelation(rel)
      case other => println("Other: " + other + "   " + other.getClass)
        Seq()
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
    case src.Abs(t) => trg.Abs(lowerTerm(t))
    case src.Min(lhs, rhs) => trg.Min(lowerTerm(lhs), lowerTerm(rhs))
  }

  def lowerType(ty: src.Type): Seq[trg.Type] = ty match {
    case src.TInt => Seq(trg.TInt)
  }


class BooleanLowering[T <: IR](module: Module)(override val trg: T) extends Lowering[BooleanIR, T](module)(trg):

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