package inca.backend.optimize
import inca.backend.ir.CollectVars
import inca.backend.ir.Datalog._
import inca.backend.ir.TypeOps
import inca.backend.optimize.Optimizer.throwBodyMustFail
import inca.runtime.context.DataModel
import inca.util.Scala

import scala.collection.immutable.MultiSet

object FoldConstantAtoms extends Optimization {

  override def optimizer(dataModel: DataModel): Optimizer = new Optimizer with TypeOps {

    override def optimizeModule(module: Module): Module = {
      module.scalaContent.foreach {
        case Scala(imp: meta.Import) => registerImport(imp)
        case Scala(stat) => registerBlockDef(stat)
      }
      super.optimizeModule(module)
    }

    private var varCount: MultiSet[Name] = MultiSet()

    override def optimizeBody(body: Body, pat: Pattern): Seq[Body] = {
      varCount = MultiSet() ++ CollectVars.transBody(body) ++ pat.params.map(_.name)
      super.optimizeBody(body, pat)
    }

    override def optimizeAtom(atom: Atom): Seq[Atom] = atom match {

      case Compare(_, v: Var, _) if varCount.get(v.name) == 1 => Seq()
      case Compare(_, _, v: Var) if varCount.get(v.name) == 1 => Seq()
      case Computed(v: Var, _) if varCount.get(v.name) == 1 => Seq()
      case Path(v: Var, _, _, _, _) if varCount.get(v.name) == 1 => Seq()
      case Path(_, _, _, v: Var, _) if varCount.get(v.name) == 1 => Seq()

      case Compare(EqComparator, t1, t2) if t1 == t2 => Seq()
      case Compare(EqComparator, Constant(c1), Constant(c2)) if c1 != c2 => throwBodyMustFail()

      case Compare(NeqComparator, t1, t2) if t1 == t2 => throwBodyMustFail()
      case Compare(NeqComparator, Constant(c1), Constant(c2)) if c1 != c2 => Seq()

      case HasType(t, typ) =>
        val termTyp = t match {
          case v: Var =>
            v.typ match {
              case Some(ty: TLiteral) => ty
              case Some(ty: TLinked) => ty
              case _ => TAny
            }
          case c: Constant => c.lit.typ
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
          case v: Var =>
            v.typ match {
              case Some(ty: TLiteral) => ty
              case Some(ty: TLinked) => ty
              case _ => TAny
            }
          case c: Constant => c.lit.typ
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
