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
  val linkSet: Seq[Link]
  val modules: Seq[CompiledModule]
  def rootModule: Module = modules.find(m => linkSet.forall(l => l.fromModule != m.ir.name)) match
    case Some(module: CompiledModule) => module.ir
    case None => throw IllegalArgumentException(s"No root module could be found for linkset $linkSet")
  def linkedModule: Module = ApplyLinking.visitModule(rootModule)

  protected def typechecker: BaseIRTypechecker = new IRTypechecker

  def getModule(name: Name): Module =
    modules.find(m => m.ir.name == name) match
      case Some(module: CompiledModule) => module.ir
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
  
  def getExportEntry(module: Name, exportEntry: Name): ModuleEntry = {
   val targetModule = getModule(module)

   val originalEntry = targetModule.contents.find { entry =>
     entry.name == exportEntry && !entry.isInstanceOf[ModuleExport]
   }

   originalEntry match {
     case Some(entry) =>
        entry
     case None =>
       throw new IllegalArgumentException(s"Original definition for export $exportEntry not found in module $module")
   }
  }

  def nameAlreadyExtended(name: Name): Boolean =
    val suffix = name.name.reverse.takeWhile(c => c != '_')
    if name.name.exists(c => c == '_') then modules.exists(m => m.name.name == suffix.reverse) else false

  object ApplyLinking extends IRVisitor:
    var currentModule: Module = Module("", BaseIR.language, Seq())
    var alreadyImportedModules: Seq[(Name, Name)] = Seq() // Seq[(fromModule, exportEntry)]

    override def visitModule(module: Module): Module = 
      currentModule = module
      Module(module.name, module.lang, module.contents.flatMap(visitModuleEntry))

    override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = 
      preserveHints(moduleEntry)(moduleEntry match {
        case mImp: ModuleImport => Seq()
        case _ => 
          val tempCurrentModule = currentModule
          val processedExportModules = linkSet.flatMap(l => if l.toModule == tempCurrentModule.name
            then {
              if !alreadyImportedModules.contains((l.fromModule, l.exportEntry)) then { 
                alreadyImportedModules = alreadyImportedModules.appended(l.fromModule, l.exportEntry)
                currentModule = getModule(l.fromModule)
                visitModuleEntry(getExportEntry(l.fromModule, l.exportEntry)).map(m => 
                  if !nameAlreadyExtended(m.name) then m.withExtendedName(s"_${l.fromModule}") else m)
              } else Seq()
            } else Seq())
          currentModule = tempCurrentModule
          processedExportModules ++ super.visitModuleEntry(moduleEntry)
      })

    override def visitRef[Target](ref: Ref[Target]): Ref[Target] = preserveHints(ref)(ref match
      case RefByName(name) => currentModule.entries.get(name) match
        case Some(mImport: ModuleImport) => 
          linkSet.find(link => link.toModule == currentModule.name && link.importEntry == mImport.name) match
            case Some(curlink) => RefByName[Target](getExportEntry(curlink.fromModule, curlink.exportEntry).name + s"_${curlink.fromModule}")
            case None => throw IllegalArgumentException(s"Imported entry does not exist")
        case _ => super.visitRef(ref)
      case _ => super.visitRef(ref)
    )
