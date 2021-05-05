package inca.backend.optimize

import inca.backend.ir.GP._
import inca.backend.ir.TypeOps
import inca.runtime.context.DataModel
import inca.util.Meta.Scala

import scala.collection.immutable.MultiDict

/**
 * Should run after `EliminateAliases` and before `FoldConstantConstraints`
 */
object InferVarTypes extends Optimization with TypeOps {

  override def optimizer(dataModel: DataModel): Optimizer = new Optimizer {

    private var funs: Map[Name, Seq[Param]] = _

    override def optimizeModule(module: Module): Module = {
      module.scalaContent.foreach {
        case Scala(imp: meta.Import) => registerImport(imp)
        case Scala(stat) => registerBlockDef(stat)
      }
      funs = module.pats.map(p => p.name -> p.params).toMap
      super.optimizeModule(module)
    }

    private var mostSpecificVarTypes: Map[Var, Type] = _
    override def optimizeBody(body: Body, pat: Pattern): Seq[Body] = {
      var vars: MultiDict[Var, Type] = MultiDict()

      def types(t: Term): collection.Set[Type] = t match {
        case v: Var => vars.get(v)
        case c: Constant => Set(c.lit.typ)
      }

      def addType(t: Term, ty: Type): Unit = t match {
        case v: Var =>
          vars += v -> ty
        case Constant(lit) =>
          val meetType = meet(lit.typ, ty, dataModel)
          if (meetType.isEmpty)
            throwBodyMustFail()
      }

      def addPatArgTypes(name: Name, args: Seq[Term]): Unit = {
        val params = funs.getOrElse(name, throw new IllegalArgumentException(s"Unbound pattern function $name"))
        if (params.size != args.size)
          throw new IllegalArgumentException(s"Pattern call of $name has wrong number of arguments $args")
        params.zip(args).foreach { case (param, arg) =>
          addType(arg, param.typ)
        }
      }

      pat.params.foreach(param => addType(Var(param.name), param.typ))

      body.constraints.foreach {
        case Compare(_, t1, t2) =>
          types(t1).foreach(ty => addType(t2, ty))
          types(t2).foreach(ty => addType(t1, ty))
        case HasType(t, typ) =>
          addType(t, typ)
        case NotHasType(t, typ) =>
          // nothing (FoldConstantConstraints will eliminate the constraint if possible)
        case Path(src, srcTy, link, trg, trgTy) =>
          addType(src, srcTy)
          addType(trg, trgTy)
        case NoPath(t, ty, link, termIsSource) =>
          addType(t, ty)
        case Call(name, args, transitive, neg) =>
          if (!neg)
            addPatArgTypes(name, args)
        case ExtensionalCall(name, args, neg) =>
          // nothing
        case Undef(t) =>
          // nothing
        case Computed(lhs, computation) =>
          computation match {
            case CountAggregation(patName, args) =>
              addPatArgTypes(patName, args)
              addType(lhs, TScalaInt)
            case Evaluation(args, resultType, _) =>
              args.foreach(a => addType(a._1, a._2))
              addType(lhs, resultType)
            case CustomAggregation(typ, _, _, patName, args, aggregatedColumn) =>
              addPatArgTypes(patName, args)
              addType(lhs, typ)
          }
      }

      try {
        mostSpecificVarTypes = Map()
        vars.sets.foreach { case (v, tys) =>
          val meetType = meet(tys, dataModel)
          meetType match {
            case Some(ty) => mostSpecificVarTypes += v -> ty
            case None => throwBodyMustFail()
          }
        }
        super.optimizeBody(body, pat)
      } finally {
        mostSpecificVarTypes = null
      }
    }

    override def optimizeTerm(term: Term): Term = term match {
      case v: Var => mostSpecificVarTypes.get(v) match {
        case Some(typ) =>
          v.typ = Some(typ)
          v
        case None => v
      }
      case c: Constant => c
    }
  }
}
