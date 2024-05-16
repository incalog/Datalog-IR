package inca.ir
import inca.ir.Module as IRModule
import inca.util.Graph

trait CompiledProgram:
  def irModules: Seq[IRModule]
  def createCompiledUnit(module: Module, otherUnits: Seq[CompiledUnit], isClosedWorld: Boolean): CompiledUnit

  lazy val compiledUnits: Seq[CompiledUnit] =
    val mods = topologicalOrderedModules
    val tl = mods.dropRight(1).foldLeft(Seq[CompiledUnit]()) {
      case (units, m) => units :+ createCompiledUnit(m, units, false)
    }
    tl ++ mods.lastOption.map(m => createCompiledUnit(m, tl, true))

  private enum Edge:
    case Import

  lazy val topologicalOrderedModules: Seq[IRModule] = importDependencyGraph.topologicalSort

  private lazy val moduleMap: Map[Name, IRModule] = irModules.map(m => m.name -> m).toMap

  private lazy val importDependencyGraph: Graph[IRModule, Edge] =
    val g = new Graph[IRModule, Edge] {
      override protected def cloneGraph(): Graph[IRModule, Edge] = throw NotImplementedError()
      override protected def nodeToGraphViz(n: IRModule): String = n.name.toString
      override protected def edgeGraphVizAttributes(from: IRModule, to: IRModule, info: Edge): String = ""
      override protected def nodeGraphVizAttributes(from: IRModule): String = ""
    }
    for m <- irModules do
      g.addNode(m)
      for imp <- m.imports do
        g.addEdge(moduleMap(imp.module.name), m, Edge.Import)
    g




