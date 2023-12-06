package inca.ir.extension.typeparam

import inca.ir.lowering.BaseLowering
import inca.ir.*

import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable.ListBuffer

trait Lowering extends BaseLowering:

  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set()

  private var groundUsages: Map[Name, Set[Seq[Type]]] = _

  def monoNameSuffix(usage: Seq[Type]): String =
    if (usage.isEmpty)
      ""
    else
      s"$$${usage.mkString("_")}"

  override def visitModule(module: Module): Module =
    groundUsages = Map().withDefault(_ => Set())
    val mod = super.visitModule(module)

    val nonParametricEntries = mod.contents.filter(!_.isInstanceOf[ParametricModuleEntry])
    val instantiatedEntries: ListBuffer[ModuleEntry] = ListBuffer.empty

    var seenUsages: Map[Name, Set[Seq[Type]]] = Map().withDefault(_ => Set())
    while (groundUsages.nonEmpty) {
      val newUsages = groundUsages
      groundUsages = Map().withDefault(_ => Set())

      instantiatedEntries ++= newUsages.flatMap { case (name, usages) =>
        mod.entries.get(name) match
          case Some(ParametricModuleEntry(params, entry)) => usages.flatMap { usage =>
            if (seenUsages(name).contains(usage))
              None
            else {
              seenUsages += name -> (seenUsages(name) + usage)
              Some(instantiateEntry(entry, params, usage))
            }
          }
          case e =>
            Seq()
      }
    }
    mod.copy(contents = nonParametricEntries ++ instantiatedEntries)

  var currentSubst: Map[Name, Type] = Map()

  def instantiateEntry(entry: ModuleEntry, typeParams: Seq[Name], usage: Seq[Type]): ModuleEntry =
    val suffix = monoNameSuffix(usage)
    currentSubst = typeParams.zip(usage).toMap
    val e = visitModuleEntry(entry).head
    e.withExtendedName(suffix)

  override def visitType(ty: Type): Type = ty match
    case TypeVar(name) => currentSubst.getOrElse(name, ty)
    case _ => super.visitType(ty)

  override def visitRef[Target](ref: Ref[Target]): Ref[Target] = ref match
    case TypeApplication(name, args) =>
      val subst = new TypeSubst(currentSubst)
      val newArgs = args.map(subst.visitType)

      if (!newArgs.exists(_.isInstanceOf[TypeVar])) {
        groundUsages += name -> (groundUsages(name) + newArgs)
        val suffix = monoNameSuffix(newArgs)
        RefByName(Name(name.name + suffix))
      } else {
        ref
      }
    case _ => super.visitRef(ref)