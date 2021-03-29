package inca.frontend.lowering

import inca.frontend.core._
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

  private case class AnonRel(typ: Type, exp: Expression, defunName: String, freevars: Seq[Var])
  private val anonymousRelations = ListBuffer[AnonRel]()

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
              ConstructorPattern(Name(defunName), freevars.map(_.name))
                .resolved(newVarTargets(Name(defunName))._1.asInstanceOf[DataConstructor.Target]) ->
              Let(vs, Some(TTuple.from(tfun.from)), Var(Name("arg")), body)
            }
          )
        )
        newVarTargets += apply.name -> (apply, apply.funType)
        Seq(data, apply)
    }

    val defunRels = anonymousRelations.toList.groupBy(_.typ).flatMap {
      case (TSet(tcontent), funs) =>
        val data = DataDef(Seq(), None, Name(relData(tcontent)),
          funs.map { case AnonRel(_, _, defunName, freevars) =>
            val constr = DataConstructor(Name(defunName), freevars.map(v => transformType(v.typ.getOrElse(TAny))))
            newVarTargets += Name(defunName) -> (constr, constr.constructorType(Name(relData(tcontent))))
            constr
          }
        )
        newTDataTargets += data.name -> data
        val apply = FunctionDef(Seq(), None, Name(relApply(tcontent)),
          Seq(Param(Name("fun"), TData(data.name))),
          TSet(transformType(tcontent)),
          Match(Var(Name("fun")),
            funs.map { case AnonRel(_, body, defunName, freevars) =>
              ConstructorPattern(Name(defunName), freevars.map(_.name))
                .resolved(newVarTargets(Name(defunName))._1.asInstanceOf[DataConstructor.Target]) ->
              body
            }
          )
        )
        newVarTargets += apply.name -> (apply, apply.funType)
        Seq(data, apply)
    }

    val newModule = Module(name, imports, newcontents ++ defunFuns ++ defunRels)

    newModule.content.foreach {
      case fun: FunctionDef =>
        fun.freevars.foreach(v => newVarTargets.get(v.name) match {
          case Some((applyFun, tfun)) =>
            v.resolved(applyFun)
            v.orTyped(tfun)
          case None => // nothing
        })
        fun.freeTvars.foreach(t => newTDataTargets.get(t.name) match {
          case Some(data) =>
            t.resolved(data)
          case None => // nothing
        })
      case data: DataDef =>
        data.freeTvars.foreach(t => newTDataTargets.get(t.name) match {
          case Some(data) =>
            t.resolved(data)
          case None => // nothing
        })
    }

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
    case v:Var =>
      v.target match {
        case Some(fun: FunctionDef) =>
          val constrSym = gensym.freshGlobal("Funref")
          val ty = transformNested(fun.funType)
          val afun = AnonFun(ty,
            fun.params.map(_.name),
            Call(
              Var(fun.name).typed(ty),
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
              Var(constr.name).typed(ty),
              paramIndices.map(ix => Var(Name(s"_$ix")))).mtyped(v.typ.map(transformType)),
            constrSym, Seq())
          anonymousFunctions += afun
          Call(Var(Name(constrSym)), Seq())
        case _ =>
          v.typ = v.typ.map(transformType)
          v
      }
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
        case v: Var
          if v.target.forall(_.isInstanceOf[FunctionDef]) || v.target.forall(_.isInstanceOf[DataConstructor]) =>
          // regular call to first-order function
          Call(fun, argTrans, transitive)
        case _ =>
          val tfun@TFun(_, _) = fun.typ.get
          // call defun apply
          Call(Var(Name(funApply(tfun))).typed(transformNested(tfun)), Seq(
            transformExp(fun),
            Tuple.from(argTrans)
          ))
      }
    case lam: Lambda =>
      val constrSym = gensym.freshGlobal("Lambda")
      val free = lam.freevars.toSeq
      val body = transformExp(lam.body)
      val tfun@TFun(_, _) = lam.typ.get
      anonymousFunctions += AnonFun(
        transformNested(tfun),
        lam.vs.map(_._1),
        body, constrSym, free)
      Call(
        Var(Name(constrSym)).typed(TFun(free.map(_.typ.get), TData(Name(funData(tfun))))),
        free)
    case Tuple(exps) =>
      Tuple(exps.map(e => transformExp(e)))
    case Match(matchee, cases) =>
      Match(transformExp(matchee), cases.map(c => c._1 -> transformExp(c._2)))
    case BaseLit(code) =>
      exp
    case BaseApply(fun, args) =>
      BaseApply(fun, args.map(a => transformExp(a)))
    case BaseApplyInfix(left, op, right) =>
      BaseApplyInfix(transformExp(left), op, transformExp(right))

    case NoneExp() =>
      exp
    case SomeExp(e) =>
      SomeExp(transformExp(e))
    case SetExp(es) =>
      defunRel(exp.typ.get, SetExp(es.map(e => transformExp(e))))
    case SetComprehension(build, predicates) =>
      defunRel(exp.typ.get, SetComprehension(transformExp(build), predicates.map(p => transformExp(p))))

    case mem@SetMember(tup, set, neg) if mem.isTypeMember =>
      val m = SetMember(transformExp(tup), set, neg)
      m.isTypeMember = true
      m
    case SetMember(tup, set, neg) =>
      SetMember(transformExp(tup), defunQuery(set), neg)

    case SetFold(anno, init, op, set) =>
      SetFold(anno, transformExp(init), op, defunQuery(set))
  }).mtyped(exp.typ.map(transformType))

  private def defunRel(typ: Type, rel: Expression): Expression = {
    // TODO
//    val constrSym = gensym.freshGlobal("Rel")
//    val free = rel.freevars.toSeq
//    anonymousRelations += AnonRel(typ, rel, constrSym, free)
//    Call(Var(Name(constrSym)), free)
    rel
  }
  
  private def defunQuery(rel: Expression): Expression = {
    // TODO
//    Call(
//      Var(Name(relApply(rel.typ.get))).typed(transformNested(rel.typ.get.asInstanceOf[TSet])),
//      Seq(transformExp(rel))
//    )
   rel
  }

}
