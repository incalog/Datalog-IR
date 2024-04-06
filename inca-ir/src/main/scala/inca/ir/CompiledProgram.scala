package inca.ir.CompiledProgram
import inca.ir
import inca.ir.*
import inca.ir.extension.*
import inca.ir.CompiledModule.*
import inca.ir.analysis.{BaseIROptimizer, IRAbstractInterpreter, IROptimizer}
import inca.ir.lowering.BaseLowering
import inca.ir.typing.{BaseIRTypechecker, DependencyGraph, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.ir.visitors.{BaseIRVisitor, IRVisitor, StatisticsCollector}
import inca.util.CompilationMessage
import inca.util.compileroptions.CompilerOptions
import inca.ir.Hint.preserveHints

case class Link(fromModule: Name, exportEntry: Name, toModule: Name, importEntry: Name)

trait CompiledProgram:
  val linkSet: Set[Link]
  val modules: Seq[CompiledModule]
  var tempLinkedModules: Seq[CompiledModule] = Seq()
  val linkedModule: Module = ???

  protected def typechecker: BaseIRTypechecker = new IRTypechecker

  def getModule(name: Name): Module =
    (tempLinkedModules ++ modules).find(m => m.ir.name == name) match
      case Some(module: Module) => module
      case None => throw IllegalArgumentException(s"Module $name not found")

  def intraTypecheck: Unit =
    linkSet.foreach(link =>
      getModule(link.fromModule).entries(link.exportEntry) match
        case mExp: ModuleExport =>
          getModule(link.toModule).entries(link.importEntry) match
            case mImp: ModuleImport => typechecker.checkImportExport(mImp, mExp)
            case _ => throw IllegalArgumentException(s"${link.importEntry} is not a valid Import")
        case _ => throw IllegalArgumentException(s"${link.exportEntry} is not a valid Export")
      )
  
  object ReplaceImport extends IRVisitor:
    var currentModule: Module = Module("", BaseIR.language, Seq())

    override def visitProgram(modules: Seq[ir.Module]): Seq[ir.Module] =
      modules.map(m => 
        currentModule = m
        visitModule(m))

    override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = preserveHints(moduleEntry)(moduleEntry match {
      case mImp: ModuleImport => Seq()
      case _ => super.visitModuleEntry(moduleEntry)
    })

    override def visitRef[Target](ref: Ref[Target]): Ref[Target] = preserveHints(ref)(ref match
      case RefByName(name) => RefByName(name) match
        case mImport: ModuleImport => 
          linkSet.find(link => link.toModule == currentModule && link.importEntry == mImport) match
            case Some(curlink) => getExportRef[Target](curlink.fromModule, curlink.exportEntry)
            case None => throw IllegalArgumentException(s"Imported entry does not exist")

      case _ => super.visitRef(ref)
    )

  def getExportRef[Target](module: Name, exportEntry: Name): Ref[Target] = {
   val targetModule = getModule(module)

   val originalEntry = targetModule.contents.find { entry =>
     entry.name == exportEntry && !entry.isInstanceOf[ModuleExport]
   }

   originalEntry match {
     case Some(entry) =>
       RefByName[Target](entry.name)
     case None =>
       throw new IllegalArgumentException(s"Original definition for export $exportEntry not found in module $module")
   }
  }
