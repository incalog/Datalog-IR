package inca.compiler

import inca.backend.ir.GP
import inca.backend.ir.GP.Name
import inca.frontend.core.{CompileToGP, Frontend}
import inca.frontend.core.tree._
import inca.frontend.desugar.Desugar
import inca.frontend.parser.SourceLocation
import inca.metamodel.MetaModel

case class CompiledFunModule(fun: Module, options: Options) extends CompiledModule {

  override def name: Name = fun.name.name

  def metaModelName = fun.usingMetaModel.name.name

  val frontend: Frontend = if(metaModelName.isEmpty) options.frontend else {
    val metaModel = new MetaModel("./src/test/scala/inca/analyzedLangs/GoLang.json", "./src/test/scala/inca/analyzedLangs/tokenNodes" + metaModelName)
    options.frontendFactory(metaModel.getLanguageMetaInfo)
  }

  override def sourceLocation: SourceLocation = fun.name

  lazy val typed: Module = {
    val frontend = options.frontend
    frontend.typecheck(fun)
    messages ++= frontend.getErrors
    messages ++= frontend.getWarnings
    stopIfNeeded()
    fun
  }

  lazy val desugared: Module = {
    val frontend = options.frontend
    val module = Desugar(frontend.allDesugarables)(typed)
    frontend.typecheck(module)
    messages ++= frontend.getErrors
    messages ++= frontend.getWarnings
//    println(module)
    stopIfNeeded()
    module
  }

  lazy val ir: GP.Module = {
    val module = new CompileToGP().transformModule(desugared)
//    println(module)
    module
  }
}
