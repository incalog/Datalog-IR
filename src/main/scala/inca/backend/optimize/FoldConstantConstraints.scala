package inca.backend.optimize
import inca.backend.ir.Datalog._
import inca.backend.ir.TypeOps
import inca.runtime.context.DataModel
import inca.util.Meta.Scala

object FoldConstantConstraints extends Optimization with TypeOps {

  override def optimizer(dataModel: DataModel): Optimizer = new Optimizer {

    override def optimizeModule(module: Module): Module = {
      module.scalaContent.foreach {
        case Scala(imp: meta.Import) => registerImport(imp)
        case Scala(stat) => registerBlockDef(stat)
      }
      super.optimizeModule(module)
    }

    override def optimizeAtom(atom: Atom): Seq[Atom] = atom match {

      case Compare(EqComparator, t1, t2) if t1 == t2 => Seq()
      case Compare(EqComparator, Constant(c1), Constant(c2)) if c1 != c2 => throwBodyMustFail()

      case Compare(NeqComparator, t1, t2) if t1 == t2 => throwBodyMustFail()
      case Compare(NeqComparator, Constant(c1), Constant(c2)) if c1 == c2 => Seq()

      case HasType(t, typ) =>
        val termTyp = t match {
          case v:Var => v.typ match {
            case Some(ty: TLiteral) => ty
            case Some(ty: TLinked) => ty
            case _ => TAny
          }
          case c:Constant => c.lit.typ
        }
        if (termTyp == typ) {
          // this constraint was responsible for the inferrence of termTyp, must keep it
          Seq(atom)
        } else {
          val meetType = meet(termTyp, typ, dataModel)
          if (meetType.contains(termTyp)) {
            // upcast, always succeeds
            Seq()
            //
          } else if (meetType.contains(typ)) {
            // downcast, makes sense
            Seq(atom)
          } else if (meetType.isEmpty) {
            // cast to unrelated type, cannot succeed
            throwBodyMustFail()
          } else {
            throw new IllegalArgumentException
          }
        }

      case NotHasType(t, typ) =>
        val termTyp = t match {
          case v:Var => v.typ match {
            case Some(ty: TLiteral) => ty
            case Some(ty: TLinked) => ty
            case _ => TAny
          }
          case c:Constant => c.lit.typ
        }
        val meetType = meet(termTyp, typ, dataModel)
        if (meetType.contains(termTyp)) {
          // termTyp <: typ, hence NotHasType must fail
          throwBodyMustFail()
        } else if (meetType.contains(typ)) {
          // termTyp :> typ, hence NotHasType makes sense
          Seq(atom)
        } else if (meetType.isEmpty) {
          // termTyp and typ are unrelated, NotHasType always succeeds
          Seq()
        } else {
          throw new IllegalArgumentException
        }

      case _ => Seq(atom)
    }
  }
}
