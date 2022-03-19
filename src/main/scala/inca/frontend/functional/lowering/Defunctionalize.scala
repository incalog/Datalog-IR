package inca.frontend.functional.lowering

import inca.frontend.functional.core._
import inca.util.Gensym

import scala.collection.mutable.ListBuffer

object Defunctionalize {
  def transformModule(module: Module): Module =
    new Defunctionalize(module).transModule()

  def transformModules(modules: Seq[Module]): Seq[Module] =
    modules.map(transformModule)
}

class Defunctionalize(module: Module) {

  private val gensym: Gensym = new Gensym(Iterable.empty)

  private case class AnonFun(typ: Type, vs: Seq[Name], body: Expression, defunName: String, freevars: Seq[Var])
  private val anonymousFunctions = ListBuffer[AnonFun]()

  private var funTypeDefun: Map[TFun, String] = Map()
  private def getFunTypeDefun(tfun: TFun): String = funTypeDefun.get(tfun) match {
    case Some(s) => s
    case None =>
      val sym = gensym.freshGlobal("Defun")
      funTypeDefun += tfun -> sym
      sym
  }

  private def funData(tfun: TFun): String =
    getFunTypeDefun(transformNested(tfun))
  private def funApply(tfun: TFun): String =
    "apply" + getFunTypeDefun(transformNested(tfun))

  private var newVarTargets: Map[Name, (Var.Target, Type)] = Map()
  private var newTDataTargets: Map[Name, TData.Target] = Map()

  def transModule(): Module = {
    val Module(name, imports, contents) = module
    gensym.register(module.usedModuleNames.map(_.name))
    gensym.register(module.usedDefNames.map(_.name))

    val newcontents = contents.map(transformModuleContent)

    val defunFuns = anonymousFunctions.toList.groupBy(_.typ).flatMap {
      case (tfun@TFun(from, to), funs) =>
        val data = DataDef(Seq(), None, Name(funData(tfun)),
          funs.map { case AnonFun(_, _, _, defunName, freevars) =>
            val constr = DataConstructor(Name(defunName), freevars.map(v => transformType(v.typ.getOrElse(TAny))))
            newVarTargets += Name(defunName) -> ((constr, constr.constructorType(Name(funData(tfun)))))
            constr
          }
        )
        newTDataTargets += data.name -> data
        val apply = FunctionDef(Seq(), None, Name(funApply(tfun)),
          Seq(Param(Name("fun"), TData(data.name)), Param(Name("arg"), TTuple.from(from.map(transformType)))),
          transformType(to),
          Match(Var(Name("fun")),
            funs.map { case AnonFun(_, vs, body, defunName, freevars) =>
              body.freevars.foreach{ v => v.target = None; v.typ = None }
              body.freeTvars.foreach(_.target = None)
              ConstructorPattern(Name(defunName), freevars.map(_.name)) -> Let(vs, Some(TTuple.from(tfun.from)), Var(Name("arg")), body)
            }
          )
        )
        newVarTargets += apply.name -> ((apply, apply.funType))
        Seq(data, apply)
      case (ty, funs) =>
        throw new IllegalStateException(s"We cannot have non-function type $ty for functions $funs")
    }

    val newModule = Module(name, imports, newcontents ++ defunFuns).sourceLocFrom(module)
    newModule
  }

  def transformModuleContent(moduleContent: ModuleContent): ModuleContent = moduleContent match {
    case DataDef(annos, vis, name, constrs) =>
      DataDef(annos, vis, name, constrs.map {
        case DataConstructor(name, paramTypes) =>
          DataConstructor(name, paramTypes.map(transformType))
      }).sourceLocFrom(moduleContent)
    case FunctionDef(annos, vis, name, params, outType, body) =>
      FunctionDef(annos, vis, name,
        params.map(p => Param(p.name, transformType(p.typ))),
        transformType(outType),
        transformExp(body)
      ).sourceLocFrom(moduleContent)
  }

  def transformType(t: Type): Type = t match {
    case TFun(from, to) =>
      val fromTrans = from.map(transformType)
      val toTrans = transformType(to)
      TData(Name(funData(TFun(fromTrans, toTrans)))).sourceLocFrom(t)
    case TTuple(ts) => TTuple(ts.map(transformType)).sourceLocFrom(t)
    case TOption(ty) => TOption(transformType(ty)).sourceLocFrom(t)
    case TSet(ty) =>
      // TData(Name(relData(transformType(ty))))
      TSet(transformType(ty)).sourceLocFrom(t)
    case _ => t
  }

  def transformNested(tfun: TFun): TFun =
    TFun(tfun.from.map(transformType), transformType(tfun.to)).sourceLocFrom(tfun)
  def transformNested(tset: TSet): TSet =
    TSet(transformType(tset.ty)).sourceLocFrom(tset)

  def transformExp(exp: Expression): Expression = exp match {
    case v@Var(name) =>
      v.target match {
        case Some(fun: FunctionDef) =>
          val constrSym = gensym.freshGlobal("Funref")
          val ty = transformNested(fun.funType)
          val afun = AnonFun(ty,
            fun.params.map(_.name),
            Call(
              Var(fun.name),
              fun.params.map(p => Var(p.name))),
            constrSym, Seq())
          anonymousFunctions += afun
          Call(Var(Name(constrSym).sourceLocFrom(v)).sourceLocFrom(v), Seq()).sourceLocFrom(v)
        case Some(constr: DataConstructor) =>
          val constrSym = gensym.freshGlobal("Relref")
          val ty = TFun(constr.paramTypes, transformType(exp.typ.get))
          val paramIndices = constr.paramTypes.indices
          val afun = AnonFun(ty,
            paramIndices.map(ix => Name(s"_$ix")),
            Call(
              Var(constr.name),
              paramIndices.map(ix => Var(Name(s"_$ix")))).mtyped(v.typ.map(transformType)),
            constrSym, Seq())
          anonymousFunctions += afun
          Call(Var(Name(constrSym).sourceLocFrom(v)).sourceLocFrom(v), Seq()).sourceLocFrom(v)
        case _ =>
          Var(name).sourceLocFrom(v)
      }
    case TypeCast(e, ty) =>
      TypeCast(transformExp(e), transformType(ty)).sourceLocFrom(exp)
    case Let(names, anno, bound, body) =>
      Let(names,
        anno.map(transformType),
        transformExp(bound),
        transformExp(body)
      ).sourceLocFrom(exp)
    case If(cnd, thn, els) =>
      If(
        transformExp(cnd),
        transformExp(thn),
        transformExp(els)
      ).sourceLocFrom(exp)
    case Call(fun, args, transitive) =>
      val argTrans = args.map(a => transformExp(a))
      fun match {
        case v@Var(name)
          if v.target.forall(_.isInstanceOf[FunctionDef]) || v.target.forall(_.isInstanceOf[DataConstructor]) =>
          // regular call to first-order function
          Call(Var(name).sourceLocFrom(v), argTrans, transitive).sourceLocFrom(exp)
        case _ =>
          val tfun@TFun(_, _) = fun.typ.getOrElse(throw new IllegalStateException(s"Expression at function position of call has to be of function type")).asInstanceOf[TFun]
          // call defun apply
          Call(Var(Name(funApply(tfun)).sourceLocFrom(exp)).sourceLocFrom(exp), Seq(
            transformExp(fun),
            Tuple.from(argTrans)
          )).sourceLocFrom(exp)
      }
    case lam: Lambda =>
      val constrSym = gensym.freshGlobal("Lambda")
      val free = lam.freevars.distinct
      val body = transformExp(lam.body)
      val tfun@TFun(_, _) = lam.typ.getOrElse(throw new IllegalStateException(s"Lambda has to be of function type")).asInstanceOf[TFun]
      anonymousFunctions += AnonFun(
        transformNested(tfun),
        lam.vs.map(_._1),
        body, constrSym, free)
      Call(
        Var(Name(constrSym).sourceLocFrom(exp)).sourceLocFrom(exp),
        free.map(_.copy())).sourceLocFrom(exp)
    case Tuple(exps) =>
      Tuple(exps.map(e => transformExp(e))).sourceLocFrom(exp)
    case Match(matchee, cases) =>
      Match(transformExp(matchee), cases.map(c => c._1 -> transformExp(c._2))).sourceLocFrom(exp)
    case BaseLit(_) =>
      exp
    case BaseApply(fun, args) =>
      BaseApply(fun, args.map(a => transformExp(a))).sourceLocFrom(exp)
    case BaseApplyUnary(op, exp) =>
      BaseApplyUnary(op, transformExp(exp)).sourceLocFrom(exp)
    case BaseApplyMethod(recv, meth, args) =>
      BaseApplyMethod(transformExp(recv), meth, args.map(_.map(transformExp))).sourceLocFrom(exp)
    case BaseApplyInfix(left, op, right) =>
      BaseApplyInfix(transformExp(left), op, transformExp(right)).sourceLocFrom(exp)

    case NoneExp() =>
      exp
    case SomeExp(e) =>
      SomeExp(transformExp(e)).sourceLocFrom(exp)
    case SetExp(es) =>
      SetExp(es.map(e => transformExp(e))).sourceLocFrom(exp)
    case SetComprehension(build, predicates) =>
      SetComprehension(transformExp(build), predicates.map(p => transformExp(p))).sourceLocFrom(exp)

    case mem@SetMember(tup, set, neg) if mem.isTypeMember =>
      val m = SetMember(transformExp(tup), set, neg).sourceLocFrom(exp)
      m.isTypeMember = true
      m
    case SetMember(tup, set, neg) =>
      SetMember(transformExp(tup), transformExp(set), neg).sourceLocFrom(exp)

    case SetFold(anno, init, op, set) => op match {
      case Var(name) =>
        SetFold(anno, transformExp(init), Var(name), transformExp(set)).sourceLocFrom(exp)
      case _ =>
        throw new UnsupportedOperationException("Only function references allowed as fold operation currently.")
//        SetFold(anno, transformExp(init), transformExp(op), transformExp(set))
    }
  }

}
