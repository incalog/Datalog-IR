package inca.backend.optimize

import inca.backend.ir.GP._
import inca.backend.ir.{GP, TypeOps}
import inca.frontend.fun.CompileToGP.BodyMustFail
import inca.runtime.context.LanguageMetaInfo

import scala.collection.immutable.MultiDict

object FoldTypeConstraints extends Optimization {

  override def optimizer(languageMetaInfo: LanguageMetaInfo): Optimizer = new Optimizer {

    private var funs: Map[Name, Seq[Param]] = _

    override def optimizeModule(module: Module): Module = {
      funs = module.pats.map(p => p.name -> p.params).toMap
      super.optimizeModule(module)
    }

    override def optimizeBody(body: Body): Seq[Body] = {
      var vars: MultiDict[Var, TypeAnno] = MultiDict()

      return Seq(body)

      def types(t: Term): collection.Set[TypeAnno] = t match {
        case v: Var => vars.get(v)
        case c: Constant => Set(c.lit.typ)
      }

      def addType(t: Term, ty: TypeAnno): Unit = t match {
        case v: Var =>
          vars += v -> ty
        case Constant(lit) =>
          if (lit.typ != ty)
            throw BodyMustFail
      }

      def addPatArgTypes(name: Name, args: Seq[Term]): Unit = {
        val params = funs.getOrElse(name, throw new IllegalArgumentException(s"Unbound pattern function $name"))
        if (params.size != args.size)
          throw new IllegalArgumentException(s"Pattern call of $name has wrong number of arguments $args")
        params.zip(args).foreach { case (param, arg) =>
          param.typ.map(addType(arg, _))
        }
      }

      body.constraints.foreach {
        case Compare(_, t1, t2) =>
          types(t1).foreach(ty => addType(t2, ty))
          types(t2).foreach(ty => addType(t1, ty))
        case HasType(t, typ) =>
          addType(t, typ)
        case Path(src, trg, link, targetType) =>
          link match {
            case GP.ParentLink => addType(src, TAnyLinked)
            case GP.NextLink => addType(src, TAnyLinked)
            case GP.SizeLink => addType(src, TInt)
            case NamedLink(nodeType, _) => addType(src, nodeType)
          }
          addType(trg, targetType)
        case Call(name, args, transitive, neg) =>
          addPatArgTypes(name, args)
        case Computed(lhs, computation) =>
          computation match {
            case CountAggregation(patName, args) =>
              addPatArgTypes(patName, args)
              addType(lhs, TInt)
            case Evaluation(args, resultType, _) =>
              args.foreach(a => addType(a._1, a._2))
              addType(lhs, resultType)
            case LatticeAggregation() =>
              ???
          }
      }

      var bestVarType: Map[Var, TypeAnno] = Map()
      vars.sets.foreach { case (v, tys) =>
        TypeOps.meet(tys, languageMetaInfo) match {
          case Some(ty) => bestVarType += v -> ty
          case None => throw BodyMustFail
        }
      }

      Seq(body)
    }
  }
}
