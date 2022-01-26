package inca.debugger
import inca.backend.ir.Datalog
import inca.compiler.CompiledModule
import inca.debugger.table.Table

final class IRDebugger(compiled: CompiledModule) extends Debugger {
  super.initialize(compiled)

  override def entry(name: Datalog.Name, bindings: Table[Value]): Unit =
    super.entry(name, bindings)

  override val frontend: DebuggerFrontend = new DebuggerFrontend {
    override type FrontendPoint = ControlPoint
    override def frontendPoint(cp: ControlPoint): Option[FrontendPoint] = Some(cp)

    override type FrontendValue = Value
    override def frontendTable(fp: FrontendPoint, bound: Table[Value]): Table[Value] = bound
  }

  override def stepIntoFrontend(): Unit = stepInto()
}