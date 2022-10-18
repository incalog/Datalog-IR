package inca.frontend.objectoriented.lowering

import inca.frontend.objectoriented.core._
import inca.util.Gensym

object Defunctionalize {
  def transformModule(module: Module): Module =
    new StaticSingleAssignment(module).transModule()

  def transformModules(modules: Seq[Module]): Seq[Module] =
    modules.map(transformModule)
}

class Defunctionalize(val module: Module) extends ModuleLowering {
  private val gensym: Gensym = new Gensym(Iterable.empty)

  private lazy val defunClassDef: ClassDef =
    ClassDef(Seq(), Some(Private), Name(gensym.fresh("Defun")), Seq(), Seq())

  var auxClassDefs: Seq[ClassDef] = Seq()

  private def genNewAux(fieldParams: Map[Name, Option[Type]], tySet: Type, exprs: Seq[Expression]): ClassDef = {
    // transform local variables to fields
    val fields = fieldParams.map { case (name, ty) =>
      val typ = transType(ty.getOrElse(throw new IllegalArgumentException(s"Untyped param $name")))
      FieldDef(Seq(), None, name, typ, None, immutable = false)
    }.toSeq
    val ret = ReturnStmt(SetExpr(exprs.map {
      case VarReadExpr(targetName) => // all reads to local variables should now access the correct field instead
        FieldReadExpr(VarReadExpr(Name("this")), targetName)
      case e =>
        e
    }))
    val apply = MethodDef(Seq(), None, Name("apply"), Seq(), tySet, Seq(ret))
    // create a default constructor
    val constrParams = fields.map(f => Param(f.name, f.typ))
    val constrBody = fields.map { f =>
      FieldAssignStmt(
        VarReadExpr(Name("this")), f.name, VarReadExpr(f.name)
      )
    }
    val constr = ConstructorDef(Seq(), None, constrParams, constrBody)
    val clsName = Name(gensym.fresh("Aux"))
    ClassDef(Seq(), Some(Private), clsName, Seq(defunClassDef.typ.ref), fields :+ apply :+ constr)
  }

  override private[lowering] def transModuleInternal(module: Module): Module = {
    val Module(name, imports, classes) = module
    // make sure the defun class name is unique
    gensym.register(module.usedModuleNames.map(_.raw))
    gensym.register(classes.map(_.name.raw))
    val transClasses = classes.map(transClass)
    Module(name, imports, (transClasses :+ defunClassDef) ++ auxClassDefs)
  }

  override private[lowering] def transStatementInternal(stmt: Statement): Statement = stmt match {
    case ReturnStmt(expr@SetExpr(_)) =>
      // keep the set without applying any transformation
      // TODO: Allow other set expr such as set comprehension as well
      ReturnStmt(preserveLoc(expr.asInstanceOf[Expression])(super.transExpressionInternal))
    case _ =>
      super.transStatementInternal(stmt)
  }

  override private[lowering] def transParamInternal(param: Param): Param = param match {
    case Param(name, TSet(ty)) =>
      // get a copy
      val typ = transType(defunClassDef.typ)
      // store the original set type
      typ.setType = Some(TSet(ty))
      // create the new param
      Param(name, typ)
    case p => p
  }

  override private[lowering] def transExpressionInternal(expression: Expression): Expression = expression match {
    case SetExpr(exps) =>
      val vars = exps.flatMap(_.vars).toMap
      val typ = expression.typ.getOrElse(throw new IllegalArgumentException(s"Untyped expression $expression"))
      val auxClass = genNewAux(vars, typ, exps)
      auxClassDefs :+= auxClass
      val constr = ConstructorExpr(auxClass.typ.ref, vars.map { case (k, v) => VarReadExpr(k) }.toSeq )
      MethodCallExpr(constr, Name("apply"), Seq())

    case _ => super.transExpressionInternal(expression)
  }
}
