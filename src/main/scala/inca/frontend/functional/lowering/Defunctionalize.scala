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

  private var relTypeDefun: Map[Type, String] = Map()
  private def getRelTypeDefun(ty: Type): String = relTypeDefun.get(ty) match {
    case Some(s) => s
    case None =>
      val sym = gensym.freshGlobal("Derel")
      relTypeDefun += ty -> sym
      sym
  }

  private def funData(tfun: TFun): String =
    getFunTypeDefun(transformNested(tfun))
  private def funApply(tfun: TFun): String =
    "apply" + getFunTypeDefun(transformNested(tfun))

  private def relData(tcontent: Type) =
    getRelTypeDefun(tcontent)
  private def relApply(tcontent: Type): String =
    "query" + getRelTypeDefun(tcontent)

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
            newVarTargets += Name(defunName) -> (constr, constr.constructorType(Name(funData(tfun))))
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
              ConstructorPattern(Name(defunName), freevars.map(v => VarPattern(v.name))) ->
              Let(vs, Some(TTuple.from(tfun.from)), Var(Name("arg")), body)
            }
          )
        )
        newVarTargets += apply.name -> (apply, apply.funType)
        Seq(data, apply)
    }

    val newModule = Module(name, imports, newcontents ++ defunFuns)
    newModule
  }

  def transformModuleContent(moduleContent: ModuleContent): ModuleContent = moduleContent match {
    case DataDef(annos, vis, name, constrs) =>
      DataDef(annos, vis, name, constrs.map {
        case DataConstructor(name, paramTypes) =>
          DataConstructor(name, paramTypes.map(transformType))
      })
    case FunctionDef(annos, vis, name, params, outType, body) =>
      FunctionDef(annos, vis, name,
        params.map(p => Param(p.name, transformType(p.typ))),
        transformType(outType),
        transformExp(body))
  }

  def transformType(t: Type): Type = t match {
    case TFun(from, to) =>
      val fromTrans = from.map(transformType)
      val toTrans = transformType(to)
      TData(Name(funData(TFun(fromTrans, toTrans))))
    case TTuple(ts) => TTuple(ts.map(transformType))
    case TOption(ty) => TOption(transformType(ty))
    case TSet(ty) =>
      // TData(Name(relData(transformType(ty))))
      TSet(transformType(ty))
    case _ => t
  }

  def transformNested(tfun: TFun): TFun =
    TFun(tfun.from.map(transformType), transformType(tfun.to))
  def transformNested(tset: TSet): TSet =
    TSet(transformType(tset.ty))

  def transformExp(exp: Expression): Expression = (exp match {
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
          Call(Var(Name(constrSym)), Seq())
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
          Call(Var(Name(constrSym)), Seq())
        case _ =>
          Var(name)
      }
    case Wildcard() =>
      Wildcard()
    case TypeCast(e, ty) =>
      TypeCast(transformExp(e), transformType(ty))
    case Let(names, anno, bound, body) =>
      Let(names,
        anno.map(transformType),
        transformExp(bound),
        transformExp(body))
    case If(cnd, thn, els) =>
      If(
        transformExp(cnd),
        transformExp(thn),
        transformExp(els))
    case Call(fun, args, transitive) =>
      val argTrans = args.map(a => transformExp(a))
      fun match {
        case v@Var(name)
          if v.target.forall(_.isInstanceOf[FunctionDef]) || v.target.forall(_.isInstanceOf[DataConstructor]) =>
          // regular call to first-order function
          Call(Var(name), argTrans, transitive)
        case _ =>
          val tfun@TFun(_, _) = fun.typ.get
          // call defun apply
          Call(Var(Name(funApply(tfun))), Seq(
            transformExp(fun),
            Tuple.from(argTrans)
          ))
      }
    case lam: Lambda =>
      val constrSym = gensym.freshGlobal("Lambda")
      val free = lam.freevars.distinct
      val body = transformExp(lam.body)
      val tfun@TFun(_, _) = lam.typ.get
      anonymousFunctions += AnonFun(
        transformNested(tfun),
        lam.vs.map(_._1),
        body, constrSym, free)
      Call(
        Var(Name(constrSym)),
        free.map(_.copy()))
    case Tuple(exps) =>
      Tuple(exps.map(e => transformExp(e)))
    case Match(matchee, cases) =>
      Match(transformExp(matchee), cases.map(c => c._1 -> transformExp(c._2)))
    case BaseLit(code) =>
      exp
    case BaseApply(fun, args) =>
      BaseApply(fun, args.map(a => transformExp(a)))
    case BaseApplyMethod(recv, method, argsOpt) =>
      val transformedArgs = argsOpt match {
        case Some(args) => Some(args.map(transformExp))
        case None => None
      }
      BaseApplyMethod(transformExp(recv), method, transformedArgs)
    case BaseApplyInfix(left, op, right) =>
      BaseApplyInfix(transformExp(left), op, transformExp(right))

    case NoneExp() =>
      exp
    case SomeExp(e) =>
      SomeExp(transformExp(e))
    case SetExp(es) =>
      SetExp(es.map(e => transformExp(e)))
    case SetComprehension(build, predicates) =>
      SetComprehension(transformExp(build), predicates.map(p => transformExp(p)))

    case mem@SetMember(tup, set, neg) if mem.isTypeMember =>
      val m = SetMember(transformExp(tup), set, neg)
      m.isTypeMember = true
      m
    case SetMember(tup, set, neg) =>
      SetMember(transformExp(tup), transformExp(set), neg)

    case SetFold(anno, init, op, set) => op match {
      case Var(name) =>
        SetFold(anno, transformExp(init), Var(name), transformExp(set))
      case _ =>
        throw new UnsupportedOperationException("Only function references allowed as fold operation currently.")
//        SetFold(anno, transformExp(init), transformExp(op), transformExp(set))
    }
  })

}
