package inca.frontend.objectoriented.lowering

import inca.compiler.SourceLocation
import inca.frontend.objectoriented.core._
import inca.util.Gensym


object AddMissingDefinitions {
  def transformModule(module: Module): Module =
    new AddMissingDefinitions(module).transModule()

  def transformModules(modules: Seq[Module]): Seq[Module] =
    modules.map(transformModule)
}

/**
 * This class adds missing definitions, such as an empty constructor definition to the module. This should be executed
 * before we attempt to typecheck a module.
 */
class AddMissingDefinitions(val module: Module) extends ModuleLowering {

  override def transClassInternal(classDef: ClassDef): ClassDef = {
    val ClassDef(annos, vis, name, parents, content) = classDef
    val missingConstructor = generateMissingConstructor(classDef)
    super.transClassInternal(ClassDef(annos, vis, name, parents, content ++ missingConstructor))
  }

  private def generateMissingConstructor(classDef: ClassDef): Option[ConstructorDef] = {
    if (classDef.constructors.isEmpty)
      Some(ConstructorDef(Seq(), None, Seq(), Seq()))
    else
      None
  }

  override def transMethodInternal(methodDef: MethodDef, classDef: ClassDef): MethodDef = {
    val MethodDef(annos, vis, name, params, outType, content) = methodDef
    val missingReturn = generateReturnStatement(content)
    super.transMethodInternal(MethodDef(annos, vis, name, params, outType, content.dropRight(1) ++ missingReturn), classDef)
  }

  private def generateReturnStatement(content: Seq[Statement]): Seq[Statement] = {
    val unitStmt = ReturnStmt(TupleExpr())
    val lastStmt = content.lastOption.getOrElse(unitStmt)
    lastStmt match {
      case ReturnStmt(_) =>
        Seq(lastStmt)
      case ExprStmt(expression) =>
        Seq(ReturnStmt(expression))
      case IfStmt(cnd, thn, els) =>
        val returnThn = generateReturnStatement(thn)
        val returnEls = generateReturnStatement(els)
        Seq(IfStmt(cnd, thn.dropRight(1) ++ returnThn, els.dropRight(1) ++ returnEls))
      case _ =>
        Seq(lastStmt, unitStmt)
      case VarPhiAssignStmt(_, _, _, _, _) =>
        throw new IllegalArgumentException("Can not implicitly return a VarPhiAssignment!")
    }
  }
}
