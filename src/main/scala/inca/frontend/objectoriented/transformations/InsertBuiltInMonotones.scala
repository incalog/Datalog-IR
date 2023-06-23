package inca.frontend.objectoriented.transformations

import inca.frontend.objectoriented.core.{ClassDef, ClassRef, ConstructorExpr, Expression, MethodDef, Module, MonotoneMapAnnotation, Name, NullExpr, Param, ReturnStmt, SetExpr, TClass, TSet, TTuple, TUnit, Type}
import inca.frontend.objectoriented.transformations.InsertBuiltInMonotones.monoMapName


object InsertBuiltInMonotones {
  def transformModule(module: Module): Module =
    new InsertBuiltInMonotones(module).transModule()

  def transformModules(modules: Seq[Module]): Seq[Module] =
    modules.map(transformModule)

  val monoMapName = Name("MonoMap")
}

/**
 * Just to satisfy the typechecker...
 */
class InsertBuiltInMonotones(val module: Module) extends ModuleLowering {
  var buildInMonotones: Map[String, ClassDef] = Map()

  override private[transformations] def transModuleInternal(module: Module): Module = {
    val Module(name, imports, classes) = module
    val transClasses = classes.map(transClass)

    Module(name, imports, buildInMonotones.values.toSeq ++ transClasses)
  }

  private def monomorphClassName(name: String, tyParams: Seq[Type]): String = if (tyParams.isEmpty)
      name
    else
      name + tyParams.map {
        case ty@TClass(ClassRef(clsName)) => monomorphClassName(clsName.raw, ty.tyParams)
        case ty => ty.toString.replace("`", "")
      }.mkString("$", "$", "")

  private def createBuildInMonotone(name: Name, tyParams: Seq[Type]): TClass = {
    val clsName = monomorphClassName(name.raw, tyParams)

    var monoCls = buildInMonotones.get(clsName)

    if (monoCls.isEmpty) {
      val tys = tyParams.map(transType)
      monoCls = Some(ClassDef(
        // hardcore the map types for now
        Seq(MonotoneMapAnnotation(tys)),
        None,
        Name(clsName),
        Seq(), // No parent class for now
        Seq(
          MethodDef(Nil, None, Name("get"), Seq(Param(Name("key"), tys.head)), tys.last, Seq(
            // We implement this in GenerateDatalog
            ReturnStmt(NullExpr())
          )),
          MethodDef(Nil, None, Name("keys"), Seq(), TSet(tys.head), Seq(
            // We implement this in GenerateDatalog
            ReturnStmt(SetExpr(Seq(), tty = Some(tys.head)))
          )),
          MethodDef(Nil, None, Name("__plus__"), Seq(Param(Name("kv"), TTuple(tys))), TUnit, Seq(
            // We implement this in GenerateDatalog
          ))/*,
          MethodDef(Nil, None, Name("values"), Seq(), TSet(tys.last), Seq(
            SetComprehension(Seq(
              SetMemberExpr(Name("result"), VarReadExpr())
            ))
          ))*/
        )
      ))
      buildInMonotones += (clsName -> monoCls.get)
    }

    monoCls.get.typ
  }

  override def transExpressionInternal(expression: Expression): Seq[Expression] = expression match {
    case constr@ConstructorExpr(ClassRef(name), args) =>
      // Monomorph constructor expression
      Seq(ConstructorExpr(ClassRef(Name(monomorphClassName(name.raw, constr.tyParams))), transExpressions(args)))
    case _ =>
      super.transExpressionInternal(expression)
  }

  override def transTypeInternal(typ: Type): Type = {
    typ match {
      case tyCls@TClass(ClassRef(name)) if name.raw == monoMapName.raw =>
        createBuildInMonotone(name, tyCls.tyParams)
      case _ =>
        super.transTypeInternal(typ)
    }
  }
}