package inca.ir.extension.aggregateset

import inca.ir
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.*
import inca.ir.extension.aggregate.{Aggregate, AggregateArg}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.set.{SetMember, TSet}
import inca.ir.lowering.BaseLowering
import inca.ir.typing.Mode

import scala.collection.mutable.ListBuffer

trait Lowering extends BaseLowering:
  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set(aggregate.IR, set.IR)

  var newrels: ListBuffer[Relation] = ListBuffer.empty

  private var currentModule: ir.Module = _

  protected override def visitModule(module: Module): Module =
    currentModule = module
    val m = super.visitModule(module)
    m.copy(contents = m.contents ++ newrels)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case AggregateSet(relname, args, op) =>
      val setRel = currentModule.relations(relname.name)
      gensym.register(setRel.params.map(_.name.name))
      val newrelName = gensym.freshGlobal(setRel.name)
      var rel = Relation(newrelName, setRel.params, Seq(Body(
        Seq(Call(setRel.name, setRel.params.map(p => Var(p.name))))
      )))
      for ((a, ix) <- args.zipWithIndex) a match
        case AggregateArg.Arg(t) => // skip
        case AggregateArg.AggregateColumn(t) =>
          val param = rel.params(ix)
          val TSet(ty) = param.ty: @unchecked
          val newparamName = gensym.freshName(param.name)
          val newparams = rel.params.updated(ix, Param(newparamName, ty))
          val memberAtom = SetMember(ir.Var(newparamName), ir.Var(param.name))
          rel = rel.copy(params = newparams, bodies = rel.bodies.map(b => Body(b.atoms :+ memberAtom)))
      newrels += rel
      preserveHints(atom) {
        Seq(Aggregate(newrelName, args, op))
      }

    case _ => super.visitAtom(atom)
