package inca.backend.optimize

import inca.backend.ir.GP._
import inca.backend.ir.TypeOps
import inca.frontend.core.CompileToGP.BodyMustFail
import inca.runtime.context.LanguageMetaInfo

import scala.collection.immutable.MultiDict

/**
 * Should run after `EliminateAliases` and before `FoldConstantConstraints`
 */
// TODO Unbounded Type was added, need to check if this algorithm needs to be adapted
object InferVarTypes extends Optimization with TypeOps {

  override def optimizer(languageMetaInfo: LanguageMetaInfo): Optimizer = new Optimizer {

    private var funs: Map[Name, Seq[Param]] = _

    override def optimizeModule(module: Module): Module = {
      initializeScala(module)
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
          val meetType = meet(lit.typ, ty, languageMetaInfo)
          if (meetType.isEmpty)
            throw BodyMustFail
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
        case Computed(lhs, computation) =>
          computation match {
            case CountAggregation(patName, args) =>
              addPatArgTypes(patName, args)
              addType(lhs, TScalaInt)
            case Evaluation(args, resultType, _) =>
              args.foreach(a => addType(a._1, a._2))
              addType(lhs, resultType)
            case CustomAggregation(typ, agg, patName, args, aggregatedColumn) =>
              addPatArgTypes(patName, args)
              addType(lhs, typ)
          }
      }

      try {
        mostSpecificVarTypes = Map()
        vars.sets.foreach { case (v, tys) =>
          val meetType = meet(tys, languageMetaInfo)
          meetType match {
            case Some(ty) => mostSpecificVarTypes += v -> ty
            case None => throw BodyMustFail
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
