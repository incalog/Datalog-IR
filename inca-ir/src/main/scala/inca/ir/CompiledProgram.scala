package inca.ir

import inca.ir.Module as IRModule
import inca.ir.visitors.BaseIRVisitor
import inca.util.Graph

private enum Edge:
  case Import

private case class ModuleGraph(modules: Seq[IRModule]) extends Graph[IRModule, Edge]:
  private lazy val moduleMap: Map[Name, IRModule] = modules.map(m => m.name -> m).toMap

  for m <- modules do
    addNode(m)
    for imp <- m.imports do
      addEdge(moduleMap(imp.module.name), m, Edge.Import)

  override protected def cloneGraph(): Graph[IRModule, Edge] = throw NotImplementedError()

  override protected def nodeToGraphViz(n: IRModule): String = n.name.toString

  override protected def edgeGraphVizAttributes(from: IRModule, to: IRModule, info: Edge): String = ""

  override protected def nodeGraphVizAttributes(from: IRModule): String = ""


trait CompiledProgram:
  def irModules: Seq[IRModule]

  def createCompiledUnit(modules: Seq[Module], otherUnits: Seq[CompiledUnit], isClosedWorld: Boolean): CompiledUnit

  def setPipeline(pipeline: List[() => BaseIRVisitor]): Unit =
    compiledUnits.foreach(_.setPipeline(pipeline))

  private lazy val moduleGraph: ModuleGraph = ModuleGraph(irModules)

  lazy val topologicalOrderedModules: Seq[IRModule] = moduleGraph.topologicalSort

  lazy val compiledUnits: Seq[CompiledUnit] =
    val mods = topologicalOrderedModules
    val tl = mods.dropRight(1).foldLeft(Seq[CompiledUnit]()) {
      case (units, m) => units :+ createCompiledUnit(Seq(m), units, false)
    }
    tl ++ mods.lastOption.map(m => createCompiledUnit(Seq(m), tl, true))

  lazy val mainUnit: CompiledUnit = compiledUnits.last




