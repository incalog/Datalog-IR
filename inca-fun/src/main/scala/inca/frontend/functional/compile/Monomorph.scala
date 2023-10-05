package inca.frontend.functional.compile

import inca.frontend.functional.syntax.*
import inca.frontend.functional.typechecker.TypeUtil
import inca.ir.Name
import inca.util.Gensym

import scala.collection.mutable

class Monomorph {

  private val gensym: Gensym = new Gensym(Iterable.empty)

  private val constructToDataDef = mutable.Map[Name, DataDef]()
  private val polymorphicFunctionDefs = mutable.Map[Name, FunctionDef]()
  private val polymorphicDataDefs = mutable.Map[Name, DataDef]()
  private val polymorphicCallSites = mutable.Map[Name, Seq[Seq[Type]]]()

  private val polymorphicToMonomorphic = mutable.Map[(Name, Seq[Type]), Name]()

  private val monomorphicFunctionDefs = mutable.Map[(Name, Seq[Type]), FunctionDef]()
  private val monomorphicDataDefs = mutable.Map[(Name, Seq[Type]), DataDef]()

  private val monomorphicContent = mutable.ListBuffer[ModuleContent]()

  def transModule(module: Module): Module = {
    module.content.foreach {
      case df@DataDef(_, _, _, _, constrs) =>
        constrs.foreach { constr =>
          constructToDataDef(constr.name) = df
        }
      case _ => // do nothing
    }
    // collect all polymorphic function and data defs
    collectPolyModuleContent(module)
    // collect all type applications of polymorphic functions and data defa
    polymorphicCallSites ++= collectTypeApplications(module).groupMap(_._1)(_._2.distinct)

    // generate names of monomorphic versions
    generateMonomorphicVersionNames()

    // actually generating monomorphic versions
    generateMonomorphicVersions()

    // replace polymorphic function/constructor calls and types
    val transformedMonomorphicContent = monomorphicContent.map {
      case ddef: DataDef => ddef // we do nothing
      case fdef: FunctionDef =>

        val subst = Map[TName, Type]()
        val monoParams = fdef.params.map { p =>
          Param(p.name, monomorph(p.typ)(subst))
        }
        val monoOutType = monomorph(fdef.outType)(subst)
        val monoBody = monomorph(fdef.body)(subst)
        fdef.copy(params = monoParams, outType = monoOutType, body = monoBody)
    }.toSeq
    Module(module.name, module.imports,
      monomorphicDataDefs.values.toSeq ++
        transformedMonomorphicContent ++
        monomorphicFunctionDefs.values)
  }

  private def collectPolyModuleContent(module: Module): Unit = {
    module.content.foreach {
      case fdef@FunctionDef(_, _, name, tyParams, _, _, _) if tyParams.nonEmpty =>
        polymorphicFunctionDefs(name) = fdef
      case ddef@DataDef(_, _, name, tyParams, _) if tyParams.nonEmpty =>
        polymorphicDataDefs(name) = ddef
      case mono =>
        monomorphicContent += mono
    }
  }

  private def collectTypeApplications(module: Module): Seq[(Name, Seq[Type])] = {
    val collector = new Collect[(Name, Seq[Type])] {
      override def collectFunctionDef(fun: FunctionDef): Seq[(Name, Seq[Type])] = {
        val tys = fun.params.map(_.typ) :+ fun.outType
        val dataDefTypeApps = tys.flatMap {
          case tc@TApply(TName(name), argTys) if isMonomorphic(tc) => Some(name -> argTys)
          case _ => None
        }
        dataDefTypeApps ++ super.collectFunctionDef(fun)
      }

      override def collectDataDef(data: DataDef): Seq[(Name, Seq[Type])] = {
        super.collectDataDef(data)
      }

      override def collectExpression(exp: Expression): Seq[(Name, Seq[Type])] = exp match {
        case Call(v@Var(name), tyArgs, args) if tyArgs.nonEmpty && tyArgs.forall(isMonomorphic) =>
          val fun = v.target match {
            case Some(_: FunctionDef) =>
              Seq(name -> tyArgs)
            case Some(dc:DataConstructor) =>
              Seq(constructToDataDef(dc.name).name -> tyArgs)
            case _ => Seq()
          }
          fun ++ args.flatMap(collectExpression)
        case _ => super.collectExpression(exp)
      }
    }
    collector(module)
  }

  private def generateMonomorphicVersionNames(): Unit = {
    polymorphicCallSites.foreach { case (name, callSites) =>
      polymorphicFunctionDefs.get(name) match {
        case Some(funDef) => callSites.foreach { typeArgs =>
          val monoName = monomorphName(funDef.name, typeArgs)
          polymorphicToMonomorphic(funDef.name -> typeArgs) = monoName
        }
        case None => polymorphicDataDefs.get(name) match {
          case Some(dataDef) =>
            callSites.foreach { typeArgs =>
              val names = dataDef.name +: dataDef.constrs.map(_.name)
              names.foreach { name =>
                val monoName = monomorphName(name, typeArgs)
                polymorphicToMonomorphic(name -> typeArgs) = monoName
              }
            }
          case None => throw new IllegalArgumentException(s"Cannot monomorphize $name")
        }
      }
    }
  }

  private def monomorphName(name: Name, typeArgs: Seq[Type]): Name =
    Name(gensym.freshGlobal(name.name + typeArgs.map(typeToString).mkString))

  private def typeToString(ty: Type): String = ty match {
    case TAny => "Any"
    case TNothing => "Nothing"
    case TFun(from, to) => s"Fun(${from.map(typeToString).mkString}, ${typeToString(to)})"
    case TTuple(ts) => s"Tuple(${ts.map(typeToString).mkString})"
    case TName(name) => name.name
    case TApply(TName(name), tys) => s"Constr($name, ${tys.map(typeToString).mkString})"
    case TApply(_, _) => throw new IllegalArgumentException(ty.toString)
    case TSet(ty) => s"Set(${typeToString(ty)})"
  }

  private def generateMonomorphicVersions(): Unit = {
    polymorphicFunctionDefs.foreach { case (_, funDef) =>
      polymorphicCallSites.get(funDef.name) match {
        case Some(callSites) =>
          callSites.foreach(generateMonoFunctionDef(funDef, _))
        case None => // do nothing
      }
    }
    polymorphicDataDefs.foreach { case (_, dataDef) =>
      polymorphicCallSites.get(dataDef.name) match {
        case Some(callSites) =>
          callSites.foreach(generateMonoDataDef(dataDef, _))
        case None => // do nothing
      }
    }
  }

  private def generateMonoFunctionDef(funDef: FunctionDef, typeArgs: Seq[Type]): Unit = {
    val monoName = polymorphicToMonomorphic(funDef.name, typeArgs)
    val subst = funDef.tyVars.map (x => TName (x.name) ).zip (typeArgs).toMap

    val monoParams = funDef.params.map {param =>
      Param (param.name, monomorph(param.typ)(subst))
    }
    val monoOutType = monomorph(funDef.outType)(subst)
    val monoFunDef = FunctionDef (funDef.annos, funDef.vis, monoName, Seq (), monoParams, monoOutType, monomorph (funDef.body) (subst) )
    monomorphicFunctionDefs(funDef.name -> typeArgs) = monoFunDef
  }


  private def generateMonoDataDef(dataDef: DataDef, typeArgs: Seq[Type]): Unit = {
    val monoName = polymorphicToMonomorphic(dataDef.name, typeArgs)
    val subst = dataDef.tyVars.map(x => TName(x.name)).zip(typeArgs).toMap

    val monoConstrs = dataDef.constrs.map { constr =>
      val monoConstrName = polymorphicToMonomorphic(constr.name, typeArgs)
      val monoParamTypes = constr.paramTypes.map(monomorph(_)(subst))
      DataConstructor(monoConstrName, monoParamTypes)
    }
    val monoDataDef = DataDef(dataDef.annos, dataDef.vis, monoName, Seq(), monoConstrs)
    monomorphicDataDefs(dataDef.name -> typeArgs) = monoDataDef
  }

  private def monomorph(ty: Type)(implicit subst: Map[TName, Type]): Type = ty match {
    case TAny => TAny
    case TNothing => TNothing
    case TFun(from, to) => TFun(from.map(monomorph), monomorph(to))
    case TTuple(ts) => TTuple(ts.map(monomorph))
    case TName(_) => TypeUtil.substitute(ty, subst)
    case TApply(TName(name), tys) => TName(polymorphicToMonomorphic(name, tys.map(monomorph)))
    case TApply(_, _) => throw new IllegalArgumentException(ty.toString)
    case TSet(ty) => TSet(monomorph(ty))
  }

  private def monomorph(exp: Expression)(implicit subst: Map[TName, Type]): Expression = exp match {
    case Call(Var(name), tyArgs, args) if tyArgs.nonEmpty =>
      val tyArgsSubst = tyArgs.map(TypeUtil.substitute(_, subst))
      val monoName = polymorphicToMonomorphic(name, tyArgsSubst)
      Call(Var(monoName), Seq(), args.map(monomorph))
    case Call(fun, tyArgs, args) => Call(monomorph(fun), tyArgs, args.map(monomorph))
    case Var(name) => Var(name)
    case Tuple(es) => Tuple(es.map(monomorph))
    case Let(names, anno, bound, body) => Let(names, anno, monomorph(bound), monomorph(body))
    case If(cnd, thn, els) => If(monomorph(cnd), monomorph(thn), monomorph(els))
    case Match(matchee, cases) =>
      val matcheeTy = matchee.typ.get
      val monoCases = cases.map { case (pat, body) => monomorph(pat, body, matcheeTy) }
      Match(monomorph(matchee), monoCases)
    case Lambda(vs, body) => Lambda(vs, monomorph(body))
    case SetExp(es) => SetExp(es.map(monomorph))
    case SetFold(anno, init, op, set) => SetFold(anno, monomorph(init), monomorph(op), monomorph(set))
    case SetMember(tup, set, neg) => SetMember(monomorph(tup), monomorph(set), neg)
    case SetComprehension(build, predicates) => SetComprehension(monomorph(build), predicates.map(monomorph))
    case _: (IntLit | DoubleLit | StringLit | BoolLit) => exp
    case UnOp(op, e) => UnOp(op, monomorph(e))
    case BinOp(e1, op, e2) => BinOp(monomorph(e1), op, monomorph(e2))
  }

  private def monomorph(pat: Pattern, body: Expression, ty: Type)(implicit subst: Map[TName, Type]): (Pattern, Expression) = {
    (monomorph(pat, ty), monomorph(body))
  }

  private def monomorph(pat: Pattern, ty: Type)(implicit subst: Map[TName, Type]): Pattern = pat match {
    case ConstructorPattern(constr, args) =>
      ty match {
        case TApply(_, typeArgs) =>
          val substTypeArgs = typeArgs.map(TypeUtil.substitute(_, subst))
          val monoConstrName = polymorphicToMonomorphic(constr, substTypeArgs)
          ConstructorPattern(monoConstrName, args)
        case _ => pat
      }
    case _ => throw new IllegalArgumentException(s"Currently cannot monomorph pattern $pat")
  }

  private def isMonomorphic(ty: Type): Boolean = ty match {
    case TAny => true
    case TNothing => true
    case TFun(from, to) => from.forall(isMonomorphic) && isMonomorphic(to)
    case TTuple(ts) => ts.forall(isMonomorphic)
    case tn@TName(_) => tn.target match {
      case Some(ParametricType(_)) => false
      case Some(DataDef(_, _, _, Seq(), _)) => true
      case Some(DataDef(_, _, _, _, _)) => false
      case None => true
      case _ => throw new IllegalArgumentException(ty.toString)
    }
    case TApply(_, tys) => tys.forall(isMonomorphic)
    case TSet(ty) => isMonomorphic(ty)
  }
}

