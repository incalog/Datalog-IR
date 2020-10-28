package inca

import inca.backend.ir.{CompileToPSystem, GP, PSystem}
import inca.frontend.core.{CompileToGP, Core}
import inca.frontend.desugar.Desugar
import inca.util.Meta

import scala.collection.mutable
import scala.meta._

object Compiler {
  def compileFunModule(module: Core.Module,
                       pkg: Option[String] = None,
                       compilerOptions: CompilerOptions): meta.Source = {
//    println(module)
    val desugared = Desugar(compilerOptions.frontend.allDesugarables)(module)
//    println(desugared)
    val gp = CompileToGP.transformModule(desugared)
//    println(gp)
    compileGPModule(gp, pkg, compilerOptions)
  }

  def optimize(module: GP.Module, compilerOptions: CompilerOptions): GP.Module = {
    var optimized = module
    // println(optimized)
    for (op <- compilerOptions.optimizations) {
      optimized = op.optimizer(compilerOptions.languageMetaInfo).optimizeModule(optimized)
      // println(optimized)
    }
    optimized
  }

  def compileGPModule(module: GP.Module,
                      pkg: Option[String] = None,
                      compilerOptions: CompilerOptions): meta.Source = {
    val optimized = optimize(module, compilerOptions)
    val Seq(source) = CompileToPSystem.compileModules(Seq(optimized))
    pkg match {
      case Some(name) => source"package ${Meta.mkQualName(name)};..${source.stats}"
      case None => source
    }
  }

  def compileAndLoadFunModule(module: Core.Module,
                              pkg: Option[String] = None,
                              compilerOptions: CompilerOptions): PSystem.Module = {
    val source = compileFunModule(module, pkg, compilerOptions)
    val loadSource = source"..${source.stats}; ${Term.Name(module.name.name)}"
    compileAndLoadScala[PSystem.Module](loadSource.syntax)()
  }

  def compileAndLoadGPModule(module: GP.Module,
                             pkg: Option[String] = None,
                             compilerOptions: CompilerOptions): PSystem.Module = {
    val source = compileGPModule(module, pkg, compilerOptions)
    val loadSource = source"..${source.stats}; ${Term.Name(module.name)}"
    compileAndLoadScala[PSystem.Module](loadSource.syntax)()
  }


  private val compilerCache: mutable.Map[String, () => Any] = mutable.Map()
  def compileAndLoadScala[A](source: String): () => A = {
    compilerCache.get(source).map(v => return v.asInstanceOf[() => A])

    import reflect.runtime.currentMirror
    import tools.reflect.ToolBox

//    println(source)

    val toolbox = currentMirror.mkToolBox()
    val tree = toolbox.parse(source)
    val compiled = toolbox.compile(tree)
    val result = () => compiled()
    compilerCache += source -> result
    result.asInstanceOf[() => A]
  }

}
