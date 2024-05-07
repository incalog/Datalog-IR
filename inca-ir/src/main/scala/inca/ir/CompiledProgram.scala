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
  val compiledModules: Seq[CompiledModule]
  def rootModule: Module = compiledModules.find(m => linkSet.forall(l => l.fromModule != m.ir.name)) match
    case Some(module: CompiledModule) => module.ir
    case None => throw IllegalArgumentException(s"No root module could be found for linkset $linkSet")
  def linkedModule: Module = ApplyLinking.visitModule(rootModule)

  protected def typechecker: BaseIRTypechecker = new IRTypechecker

  lazy val modulesMap: Map[Name, Module] = compiledModules.map(m => m.ir.name -> m.ir).toMap

  def validateLinkset: Unit =
    linkSet.foreach(link =>
      modulesMap(link.fromModule).exports(link.exportEntry) match
        case mExp: ModuleExport =>
          modulesMap(link.toModule).imports(link.importEntry) match
            case mImp: ModuleImport => typechecker.checkImportExport(mImp, mExp)
            case null => throw IllegalArgumentException(s"${link.importEntry} is not a valid Import")
        case null => throw IllegalArgumentException(s"${link.exportEntry} is not a valid Export")
      )
  
  def getExportEntry(module: Name, exportEntry: Name): ModuleEntry = {
    val targetModule = modulesMap(module)
    val contentsNoExport = targetModule.contents.filterNot(m => targetModule.exports.values.toSeq.contains(m))
    contentsNoExport.find(e => e.name == exportEntry) match {
      case Some(entry) =>
         entry
      case None =>
        throw new IllegalArgumentException(s"Original definition for export $exportEntry not found in module $module")
    }
  }

  def nameAlreadyExtended(name: Name): Boolean =
    val suffix = name.name.reverse.takeWhile(c => c != '_')
    if name.name.exists(c => c == '_') then compiledModules.exists(m => m.name.name == suffix.reverse) else false

  object ApplyLinking extends IRVisitor:
    var currentModule: Name = ""
    var alreadyImportedEntries: Seq[(Name, Name)] = Seq() // Seq[(fromModule, exportEntry)]
    var features: Language = BaseIR.language

    override def visitModule(module: Module): Module = 
      currentModule = module.name
      val contents = module.contents.flatMap(visitModuleEntry)
      Module(module.name, features, contents)

    override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = 
      preserveHints(moduleEntry)(moduleEntry match {
        case mImp: ModuleImport => Seq()
        case _ => 
          val tempCurrentModule = currentModule
          val processedImportedEntries = linkSet.flatMap(l => if l.toModule == tempCurrentModule
            then {
              if !alreadyImportedEntries.contains((l.fromModule, l.exportEntry)) then { 
                alreadyImportedEntries = alreadyImportedEntries.appended(l.fromModule, l.exportEntry)
                currentModule = modulesMap(l.fromModule).name
                visitModuleEntry(getExportEntry(l.fromModule, l.exportEntry)).map(m => 
                  if !nameAlreadyExtended(m.name) then m.withExtendedName(s"_${l.fromModule}") else m)
              } else Seq()
            } else Seq())
          currentModule = tempCurrentModule
          features = features ++ modulesMap(currentModule).lang.features
          processedImportedEntries ++ super.visitModuleEntry(moduleEntry)
      })

    override def visitRef[Target](ref: Ref[Target]): Ref[Target] = preserveHints(ref)(ref match
      case RefByName(name) => modulesMap(currentModule).entries.get(name) match
        case Some(mImport: ModuleImport) => 
          linkSet.find(link => link.toModule == currentModule && link.importEntry == mImport.name) match
            case Some(curlink) => RefByName[Target](getExportEntry(curlink.fromModule, curlink.exportEntry).name + s"_${curlink.fromModule}")
            case None => throw IllegalArgumentException(s"Imported entry does not exist")
        case _ => if linkSet.exists(l => l.fromModule == currentModule && l.exportEntry == name) then RefByName(name + s"_${currentModule.name}") else super.visitRef(ref)
      case _ => super.visitRef(ref)
    )
