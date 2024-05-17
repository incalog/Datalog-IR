package inca.ir.extension.module

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.extension.data.{CaseDefinition, CaseDefinitionSubstitution, DataDefinition, DataDefinitionSubstitution, RequireCaseDefinition, RequireDataDefinition, TData}
import inca.ir.extension.typeparam.TypeApplication
import inca.ir.{BaseIR, Body, Call, Import, Module, ModuleEntry, Name, Provide, Ref, RefByName, RefByQualifiedName, Relation, RelationSubstitution, Require, RequireRelation, Substitution, Var}
import inca.ir.lowering.BaseLowering
import inca.ir.visitors.IRVisitor

object PrefixModuleEntries:
  def prefixName(name: Name, prefix: String): String = s"$prefix$$$name"
import PrefixModuleEntries.prefixName

// modify a provided Module to be imported in the main module
private case class ExtractModuleContent(prefix: String, subst: Seq[Substitution[_, _]]) extends IRVisitor:
  private var renamings: Map[Name, Name] = _

  def extract(module: Module): Seq[ModuleEntry] = visitModule(module).contents

  private def updateModuleEntryName(moduleEntry: ModuleEntry): ModuleEntry =
    moduleEntry.withName(prefixName(moduleEntry.name, prefix))

  private def pathComponents(ref: Ref[_]): (Seq[Name], Name) = ref match
    case RefByName(n) => (Seq(), n)
    case RefByQualifiedName(ns) => (ns.dropRight(1), ns.last)
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

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match
    case prov: Provide[_] =>
      prov.exportRef.target match
        case Some(RequireRelation(name, params)) =>
          // we need to manually create a relation for this, since it does not really exist yet
          val fromName = renamings(name)
          val rel = Relation(Name(prefixName(name, prefix)), params.flatMap(visitParam), Seq(Body(Seq(
            Call(fromName, params.map(p => Var(p.name).arg))
          ))))
          Seq(rel)
        // TODO: Are these two cases needed / correct?
        case Some(RequireDataDefinition(name)) =>
          val fromName = renamings(name)
          val dd = DataDefinition(Name(prefixName(name, prefix)))
          Seq(dd)
        case Some(RequireCaseDefinition(name, rArgs, data)) =>
          val fromName = renamings(name)
          val args = rArgs.map(visitType)
          val cd = CaseDefinition(Name(prefixName(name, prefix)), args, visitType(data).asInstanceOf[TData])
          Seq(cd)
        case _ => Seq() // nothing, since we already copied this one over by copying all relations
    case _: Require => Seq()
    case _ => super.visitModuleEntry(moduleEntry).map(updateModuleEntryName)

  override def visitRef[Target](ref: Ref[Target]): Ref[Target] = preserveHints(ref) {
    // this assumes that all module entries in a module have unique names (which is enforced by the typechecker)
    ref.target match
      case Some(_: ModuleEntry) => renamings.get(ref.name) match
        case Some(name) => RefByName[Target](name)
        case _ => throw IllegalStateException(s"No renaming found for ${ref.name}")
      case _ => super.visitRef(ref)
  }

trait Lowering extends BaseLowering:
  override val name: String = "Module"
  override val loweredIRs: Set[BaseIR] = Set()
  override val requiredIRs: Set[BaseIR] = Set()

  var moduleMap: Map[Name, ir.Module] = Map()

  override def visitProgram(modules: Seq[ir.Module], dependencies: Seq[ir.Module] = Seq()): Seq[ir.Module] =
    moduleMap = dependencies.map(m => m.name -> m).toMap
    modules.map(visitModule)

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match
    case Import(moduleRef, as, subst) =>
      val module = moduleMap(moduleRef.name)
      val extractor = ExtractModuleContent(as.name, subst)
      extractor.extract(module)
    case _ => super.visitModuleEntry(moduleEntry)

  override def visitRef[Target](ref: Ref[Target]): Ref[Target] = ref match
    case RefByQualifiedName(ns) =>
      val name = ns.dropRight(1).foldRight(ns.last) {
        case (refName, acc) => Name(prefixName(acc, refName.name))
      }
      RefByName(name)
    case _ => super.visitRef(ref)