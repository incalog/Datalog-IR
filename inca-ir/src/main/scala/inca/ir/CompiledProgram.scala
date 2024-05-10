package inca.ir

import inca.ir
import inca.ir.*
import inca.ir.CompiledModule.*
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker}
import inca.ir.visitors.IRVisitor
import inca.ir.Hint.preserveHints

case class Link(fromModule: Name, exportEntry: Name, toModule: Name, importEntry: Name)

private case class PrefixModuleEntries(prefix: Name) extends IRVisitor:
  private var renamings: Map[ModuleEntry, Name] = _

  def extend(module: Module): Module = visitModule(module)

  def updateModuleEntryName(moduleEntry: ModuleEntry): ModuleEntry =
    val prefixedName = CompiledProgram.prefixName(moduleEntry.name, prefix)
    moduleEntry.withName(prefixedName)

  override def visitModule(module: Module): Module =
    renamings = module.contents.flatMap {
      case _: ModuleImport => None
      case e => Some(e -> CompiledProgram.prefixName(e.name, prefix))
    }.toMap

    super.visitModule(module)

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match
    case _: ModuleImport => super.visitModuleEntry(moduleEntry)
    case _: ModuleExport => Seq()
    case _ => super.visitModuleEntry(moduleEntry).map(updateModuleEntryName)

  override def visitRef[Target](ref: Ref[Target]): Ref[Target] = preserveHints(ref) {
    ref.target match
      case Some(value: ModuleEntry) => renamings.get(value) match
        case Some(name) => RefByName[Target](name)
        case _ => super.visitRef(ref)
      case _ => super.visitRef(ref)
  }


private case class ResolveImports(linkSet: Seq[Link]) extends IRVisitor:
  var currentModule: Module = _
  // This assumes that all references are unique given their name, e.g. no Datatype and relation must have the same name
  var renamings: Map[Name, Name] = Map()

  def resolve(module: Module): Module =
    currentModule = module
    // find all links that are required by this modules imports
    val links = linkSet.filter(_.toModule == module.name)
    renamings = links.map { l =>
      l.importEntry ->  CompiledProgram.prefixName(l.exportEntry, l.fromModule)
    }.toMap

    visitModule(module)

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match
    case _: ModuleImport => Seq()
    case _ => super.visitModuleEntry(moduleEntry)

  override def visitRef[Target](ref: Ref[Target]): Ref[Target] = preserveHints(ref) {
    // TODO: Targets to ModuleImports are not correctly resolved
    //  Calls should have a ref to either a relation, ext relation or import
    /*ref.target match
      case Some(imp: ModuleImport) => renamings.get(imp.name) match
        case Some(name) => RefByName[Target](name)
        case None => super.visitRef(ref)
      case _ => super.visitRef(ref)*/

    renamings.get(ref.name) match
      case Some(name) => RefByName[Target](name)
      case _ => super.visitRef(ref)
  }

trait CompiledProgram:
  def linkSet: Seq[Link]
  def compiledModules: Seq[CompiledModule]

  private lazy val rootModule: Module = compiledModules.find(m => linkSet.forall(l => l.fromModule != m.ir.name)) match
    case Some(module: CompiledModule) => module.checked
    case None => throw IllegalArgumentException(s"No root module could be found for linkset $linkSet")

  lazy val linkedModule: Module = link(rootModule)

  protected def typechecker: BaseIRTypechecker = new IRTypechecker

  lazy val modulesMap: Map[Name, Module] =
    compiledModules.map {
      m => m.ir.name -> m.checked
    }.toMap

  type Prefix = Name
  type ModuleName = Name

  var alreadyExtended: Map[ModuleName, Prefix] = Map()

  def validateLinkSet(): Unit =
    linkSet.foreach(link =>
      val exp =  modulesMap(link.fromModule).exports.get(link.exportEntry)
      val imp =  modulesMap(link.toModule).imports.get(link.importEntry)
      (exp, imp) match
        case (Some(mExp: ModuleExport), Some(mImp: ModuleImport)) => typechecker.checkImportExport(mImp, mExp)
        case (Some(_), _) => throw IllegalArgumentException(s"${link.exportEntry} is not a valid Export")
        case (_, Some(_)) => throw IllegalArgumentException(s"${link.importEntry} is not a valid Import")
        case _ => throw IllegalArgumentException(s"${link.importEntry}, nor ${link.exportEntry} is valid")
      )

  def link(module: Module): Module =
    var lang: Language = BaseIR.language
    val newFeatures = modulesMap.flatMap((_, m) => m.lang.features).toSet
    lang ++= newFeatures

    // Prefix all module entries by their module name
    var renamedModules = modulesMap.map((n, mod) =>
      if n != rootModule.name then
        PrefixModuleEntries(n).extend(mod)
      else
        rootModule
    ).toSeq

    // Follow transitive links
    var links: Map[(Name, Name), (Name, Name)] = Map()
    var changed = true
    while (changed) {
      val newLinks = linkSet.map { l =>
        links.get((l.fromModule, l.exportEntry)) match
          case Some((newFrom, newExport)) => (l.toModule, l.importEntry) -> (newFrom, newExport)
          case _ => (l.toModule, l.importEntry) -> (l.fromModule, l.exportEntry)
      }.toMap
      changed = newLinks != links
      links = newLinks
    }
    val resolvedLinkSet = links.map { case ((to, imp), (from, exp)) => Link(from, exp, to, imp) }.toSeq

    // Resolve the imports
    renamedModules = renamedModules.map(m => ResolveImports(resolvedLinkSet).resolve(m))

    // We over approximate by copying over all relations, we could refine this to only copy over transitively
    // required relations from the import
    Module(module.name, lang, renamedModules.flatMap(_.contents))

object CompiledProgram:
  def prefixName(name: Name, prefix: Name): Name = Name(s"$prefix$$$name")