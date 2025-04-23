package inca.ir.extension.map.optimize

import inca.ir
import inca.ir.analysis.base.values.Value
import inca.ir.extension.map.analysis.interpreter.{ConstantMapFunV, ConstantMapV}
import inca.ir.extension.map as irmap
import inca.ir.*
import inca.ir.extension.map.TMap
import inca.ir.optimize.ConstantBaseIROptimizer
import inca.ir.visitors.IRVisitor

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  override def eqsToBindConstantParams(body: Body): Seq[Eq] =
    getBodyResult(body).headOption match
      case None => Seq()
      case Some(constRel) =>
        val eqs = super.eqsToBindConstantParams(body)
        constRel.cols.zip(constRel.rows).zip(eqs).flatMap { case ((c, v), eq) =>
            val isMap = v match
              case _: ConstantMapV | _: ConstantMapFunV => true
              case _ => false
            val bodyBindsVar = body.vars.map(_.name.name).contains(c)
            if (isMap && bodyBindsVar)
              None
            else
              Some(eq)
        }

  override def mayEliminate(t: Term): Boolean = t match
    case irmap.MapComprehension(k, v, ats) =>
      isConstant(t) && ats.flatMap(visitAtom).isEmpty
    case irmap.MapFun(_, valTerm) =>
      isConstant(t) && isConstant(valTerm) && mayEliminate(valTerm)
    case irmap.MapPlus(map, key, value) =>
      isConstant(t) && isConstant(key) && isConstant(value)
      && mayEliminate(map) && mayEliminate(key) && mayEliminate(value)
    case irmap.MapUnion(t1, t2) =>
      isConstant(t) && isConstant(t1) && isConstant(t2)
      && mayEliminate(t1) && mayEliminate(t2)
    case irmap.MapConcat(t1, t2) =>
      isConstant(t) && isConstant(t1) && isConstant(t2)
      && mayEliminate(t1) && mayEliminate(t2)
    case irmap.MapLookUp(map, key) => isConstant(t)
    case irmap.MapFrom(_) => isConstant(t)
    case v: Var if v.typ.exists(tty => tty.ty.isInstanceOf[TMap] && tty.mode.isBinding) => false
    case _ => super.mayEliminate(t)

  override def valueToTermInternal(value: Value): Option[Term] = value match
    case ConstantMapV(values) => Some(irmap.MapLit(values.toSeq.flatMap {
      (k, vs) => valueToTerm(k) match
        case Some(key) =>
          vs.toSeq.flatMap { v =>
            valueToTerm(v).map(key -> _)
          }
        case None => Seq()
      }))
    case ConstantMapFunV(_, _) => None
    case _ => super.valueToTermInternal(value)

  /*override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case irmap.MapContains(m, k) if k.typ.exists(_.mode.isBound) => ???
      case irmap.MapContains(m, k) if k.typ.exists(_.mode.isBinding) => ???
      case _ => super.visitAtom(atom)
  }*/
      
  private var relationsUsedInMapFrom: Set[Relation] = Set()
  override def relationIsRequired(relation: Relation): Boolean =
    if (relationsUsedInMapFrom.contains(relation))
      true
    else
      super.relationIsRequired(relation)

  override def analyzeProgram(modules: Seq[Module]): Unit =
    relationsUsedInMapFrom = Set()

    val aggVisitor = new IRVisitor {
      override def visitTerm(term: Term): Seq[Term] = term match
        case irmap.MapFrom(ref) => 
          val rel = ref.target.get
          relationsUsedInMapFrom += rel
          super.visitTerm(term)
        case _ => 
          super.visitTerm(term)
    }

    aggVisitor.visitProgram(modules)

    super.analyzeProgram(modules)




