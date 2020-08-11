package inca

import inca.backend.ir.{CompileToPSystem, GP, PSystem}
import inca.backend.optimize.{ConstantConstraintFolding, ConstantPropagation, EliminateAliases, Optimizer}
import inca.frontend.desugar.{Desugar, Desugarable}
import inca.frontend.fun.{CompileToGP, Fun}
import inca.frontend.funext.{BoolOps, Cast, Enum, ForallExists, Foreach, IfThenElse, Match, Switch}
import inca.util.Meta

import scala.collection.mutable
import scala.meta._

object Compiler {
  val defaultDesugarables = Seq(
    BoolOps,
    Cast,
    Enum,
    ForallExists,
    Foreach,
    IfThenElse,
    Match,
    Switch)

  val defaultOptimizations = Seq(
    ConstantPropagation,
    EliminateAliases,
    ConstantConstraintFolding
  )

  def compileFunModule(module: Fun.Module,
                       pkg: Option[String] = None,
                       desugarables: Seq[Desugarable] = defaultDesugarables,
                       optimizations: Seq[Optimizer] = defaultOptimizations): meta.Source = {
    val desugared = Desugar(desugarables:_*)(module)
    val gp = CompileToGP.transformModule(desugared)
    compileGPModule(gp, pkg, optimizations)
  }

  def compileGPModule(module: GP.Module,
                      pkg: Option[String] = None,
                      optimizations: Seq[Optimizer] = defaultOptimizations): meta.Source = {
    println(module)
    val optimized = optimizations.foldLeft(module)((mod, opt) => opt.optimizeModule(mod))
    println(optimized)
    val Seq(source) = CompileToPSystem.compileModules(Seq(optimized))
    pkg match {
      case Some(name) => source"package ${Meta.mkQualName(name)};..${source.stats}"
      case None => source
    }
  }

  def compileAndLoadFunModule(module: Fun.Module,
                              pkg: Option[String] = None,
                              desugarables: Seq[Desugarable] = defaultDesugarables,
                              optimizations: Seq[Optimizer] = defaultOptimizations): PSystem.Module = {
    val source = compileFunModule(module, pkg, desugarables, optimizations)
    val loadSource = source"..${source.stats}; ${Term.Name(module.name)}"
    compileAndLoadScala[PSystem.Module](loadSource.syntax)()
  }

  def compileAndLoadGPModule(module: GP.Module,
                             pkg: Option[String] = None,
                             optimizations: Seq[Optimizer] = defaultOptimizations): PSystem.Module = {
    val source = compileGPModule(module, pkg, optimizations)
    val loadSource = source"..${source.stats}; ${Term.Name(module.name)}"
    compileAndLoadScala[PSystem.Module](loadSource.syntax)()
  }


  private val compilerCache: mutable.Map[String, () => Any] = mutable.Map()
  def compileAndLoadScala[A](source: String): () => A = {
    compilerCache.get(source).map(v => return v.asInstanceOf[() => A])

    import reflect.runtime.currentMirror
    import tools.reflect.ToolBox

    val toolbox = currentMirror.mkToolBox()
    val tree = toolbox.parse(source)
    val compiled = toolbox.compile(tree)
    val result = () => compiled()
    compilerCache += source -> result
    result.asInstanceOf[() => A]
  }

}
