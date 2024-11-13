package inca.ir.extension.module

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.{BaseIR, Import, Module, ModuleEntry, Name, Provide, Ref, RefByName, RefByQualifiedName, Require, Substitution}
import inca.ir.lowering.BaseLowering
import inca.ir.visitors.IRVisitor

object PrefixModuleEntries:
  def prefixName(name: Name, prefix: String): String = s"$prefix$$$name"
import PrefixModuleEntries.prefixName

// modify a provided Module to be imported in the main module
private case class ExtractModuleContent(prefix: String, subst: Seq[Substitution[_, _]]) extends IRVisitor:
  private var renamings: Map[Name, Name] = Map()
  // if we provide a required entry we just created an alias
  private var aliases: Map[Name, Name] = Map()

  def extract(module: Module): (Seq[ModuleEntry], Map[Name, Name]) =
    (visitModule(module).contents, aliases)

  private def updateModuleEntryName(moduleEntry: ModuleEntry): ModuleEntry =
    moduleEntry.withName(prefixName(moduleEntry.name, prefix))

  private def pathComponents(ref: Ref[_]): (Seq[Name], Name) = ref match
    case RefByName(n) => (Seq(), n)
    case RefByQualifiedName(ns) =>
      (ns.dropRight(1), ns.last)
    // TODO: Handle type substitutions
    case _ => ???

  override def visitModule(module: Module): Module =
    // we need to rename refs to require module entries differently
    val requirementsRenaming = subst.map {
      case s: Substitution[_, _] =>
        val (path, unqualifiedName) = pathComponents(s.from)
        s.to.name -> path.foldRight(unqualifiedName) {
          case (refName, acc) => Name(prefixName(acc, refName.name))
        }
    }.toMap

    // we prefix all other module entries
    renamings = module.contents.flatMap {
      case _: Require => None
      case _: Provide[_] => None
      case _: Import => None
      case e => Some(e.name -> Name(prefixName(e.name, prefix)))
    }.toMap ++ requirementsRenaming

    super.visitModule(module)

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = preserveHints(moduleEntry) {
    moduleEntry match
      case p: Provide[_] =>
        p.exportRef.target match
          case Some(r: Require) => aliases += Name(prefixName(p.name, prefix)) -> renamings(r.name)
          case _ => // nothing
        Seq()
      case _: Require => Seq()
      case _ => super.visitModuleEntry(moduleEntry).map(updateModuleEntryName)
  }

  override def visitRef[Target](ref: Ref[Target]): Ref[Target] = preserveHints(ref) {
    // this assumes that all module entries in a module have unique names (which is enforced by the typechecker)
    ref.target match
      case Some(_: ModuleEntry) => renamings.get(ref.name) match
        case Some(name) => RefByName[Target](name)
        case _ => throw IllegalStateException(s"No renaming found for ${ref.name}")
      case _ => super.visitRef(ref)
  }

trait Lowering extends BaseLowering:
  enum Phase:
    case PrefixModules
    case ResolveAliases

  override val name: String = "Module"
  override val loweredIRs: Set[BaseIR] = Set()
  override val requiredIRs: Set[BaseIR] = Set()
  var phase: Phase = Phase.PrefixModules

  var moduleMap: Map[Name, ir.Module] = Map()
  private var aliases: Map[Name, Name] = Map()

  override def visitProgram(modules: Seq[ir.Module], dependencies: Seq[ir.Module] = Seq()): Seq[ir.Module] =
    moduleMap = dependencies.map(m => m.name -> m).toMap
    phase = Phase.PrefixModules
    val newMods = modules.map(visitModule)
    phase = Phase.ResolveAliases
    newMods.map(visitModule)

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match
    case Import(moduleRef, as, subst) if phase == Phase.PrefixModules =>
      val module = moduleMap(moduleRef.name)
      val extractor = ExtractModuleContent(as.name, subst)
      val (content, modAliases) = extractor.extract(module)
      aliases ++= modAliases
      content
    case _ => super.visitModuleEntry(moduleEntry)

  override def visitRef[Target](ref: Ref[Target]): Ref[Target] = ref match
    case RefByQualifiedName(ns) if phase == Phase.PrefixModules =>
      val name = ns.dropRight(1).foldRight(ns.last) {
        case (refName, acc) => Name(prefixName(acc, refName.name))
      }
      RefByName(name)
    case RefByName(name) if phase == Phase.ResolveAliases =>
      aliases.get(name) match
        case Some(alias) => RefByName(alias)
        case _ => super.visitRef(ref)
    case _ => super.visitRef(ref)