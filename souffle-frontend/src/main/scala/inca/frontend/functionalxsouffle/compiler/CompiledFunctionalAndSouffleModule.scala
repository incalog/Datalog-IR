package inca.frontend.functionalxsouffle.compiler

import inca.backend.hints.MagicSetHints
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.Name
import inca.compiler.{CompiledModule, CompilerFlags, SourceLocation}
import inca.frontend.functional.compiler.FunctionalOptions
import inca.frontend.functional.core
import inca.frontend.functional.core.{Module, Type}
import inca.frontend.functional.lowering.{Defunctionalize, GenerateDataModel}
import inca.frontend.functionalxsouffle.lowering.GenerateDatalog
import inca.frontend.functionalxsouffle.typechecker.{ExtractRelationSignatures, Typechecker}
import inca.frontend.souffle.Souffle
import inca.frontend.souffle.lowering.SouffleToIncaBackendCompiler
import inca.runtime.context.DataModel

case class CompiledFunctionalAndSouffleModule(fun: Module, souffle: Souffle.Module, options: FunctionalOptions) extends CompiledModule {
  override def name: Name = fun.name.name

  override def sourceLocation: SourceLocation = fun.name

  // at first we need to load souffle and get signatures of relations
  // TODO do we only want input and output relations or intermediate relations as well? Currently we get all relations
  lazy val souffleSigs: Seq[Souffle.RuleSignature] = ExtractRelationSignatures.extract(souffle)

  lazy val _externalSignautres: Map[core.Name, Seq[Type]] = souffleSigs.map { sig =>
    core.Name(sig.name)-> sig.parameters.map(_.typ).map(transformSouffleTypeToIRType)
  }.toMap

  private def transformSouffleTypeToIRType(ty: Souffle.Type): Type = ty match {
    case Souffle.DeclaredType(_) => core.TScalaString
    case Souffle.SymbolType => core.TScalaString
    case Souffle.NumberType => core.TScalaInt
    case Souffle.UnsignedType => core.TScalaLong
    case Souffle.FloatType => core.TScalaDouble
  }

  lazy val typer = new Typechecker {
    override val externalSignatures: Map[core.Name, Seq[Type]] = _externalSignautres
  }

  // Type fun module with the souffleSigs
  lazy val typed: Module = {
    typer.typecheck(fun)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    fun
  }

  lazy val coreModule: Module = {
    val module = new Defunctionalize(typed).transModule()
    typer.typecheck(module)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    if (CompilerFlags.DEBUGMODE) {
      println(s"Core Module")
      println(module)
    }
    module
  }

  lazy val (
    souffleIR,
    souffleDatamodel,
    souffleInputs): (Datalog.Module, DataModel, Seq[(Souffle.RuleSignature, Souffle.Input)]) = {
    val module = new SouffleToIncaBackendCompiler().compile("SouffleIR", souffle)
    if (CompilerFlags.DEBUGMODE) {
      println(s"Souffle Intermediate Representation")
      println(module)
    }
    // We want to avoid generating input relations for all external relations
    val transPats = module.ir.pats.map(_.addHint(MagicSetHints.NoInputRelation))
    val transModuleIR = Datalog.Module(module.ir.name, module.ir.imports, transPats, module.ir.scalaContent)
    (transModuleIR, module.dataModel, module.inputs)
  }

  lazy val ir: Datalog.Module = {
    val funModule = new GenerateDatalog(coreModule, _externalSignautres).transModule()
    if (CompilerFlags.DEBUGMODE) {
      println(s"Intermediate Representation")
      println(funModule)
    }
    val combinedModule = Datalog.Module(funModule.name, Seq(), funModule.pats ++ souffleIR.pats, funModule.scalaContent ++ souffleIR.scalaContent)
    combinedModule
  }

  lazy val dataModel: DataModel = {
    val funDataModel = new GenerateDataModel(coreModule).transModule()
    funDataModel ++ souffleDatamodel
  }
}
