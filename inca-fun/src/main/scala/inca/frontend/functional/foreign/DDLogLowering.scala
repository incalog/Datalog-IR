package inca.frontend.functional.foreign

import inca.foreign.ddlog.ir.primitive
import inca.foreign.ddlog.ir.primitive.{DDLogAggregationOperator, DDLogDefnModuleEntry, DDLogInca}
import inca.frontend.functional.compile.{GenerateDDLog, GenerateIR}
import inca.frontend.functional.syntax.{DataDef, FunctionDef}
import inca.ir
import inca.ir.extension.aggregate.{Aggregate, IR as iragg}
import inca.ir.lowering.BaseLowering
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.typing.IRTypechecker


trait DDLogTypechecker extends IRTypechecker:
  override def checkModuleEntry(moduleEntry: ModuleEntry): Unit = moduleEntry match
    case _: DDLogDefnModuleEntry => // nothing
    case _ => super.checkModuleEntry(moduleEntry)


trait DDLogLowering extends BaseLowering:
  override val name: String = "Foreign"
  override def loweredIRs: Set[BaseIR] = Set(iragg)
  override def requiredIRs: Set[BaseIR] = Set(iragg, primitive.IR)

  val generateDDLog = new GenerateDDLog

  var ddLogModules: Set[DDLogDefnModuleEntry] = Set()

  override def visitProgram(modules: Seq[Module], dependencies: Seq[Module] = Seq()): Seq[Module] =
    val prog = super.visitProgram(modules)
    val checker = new DDLogTypechecker {}
    checker.checkProgram(prog)
    prog

  override def visitModule(module: ir.Module): ir.Module = preserveHints(module) {
    ddLogModules = Set()
    val mod = super.visitModule(module)
    ir.Module(mod.name, mod.lang, mod.contents ++ ddLogModules)
  }

  override def visitAtom(atom: Atom): Seq[Atom] =
    atom match
      case Aggregate(rel, args, aggOp@FunctionalIncaAggregationOperator(fun, init, op)) =>
        // TODO: Lower type e.g. Boolean to int before passing it into this function
        generateDDLog.genFunDef(fun)
        val generatedCode = generateDDLog.generated.mkString("\n")

        ddLogModules += DDLogDefnModuleEntry("Aggregation$" + fun.name, generatedCode)

        val initCode = generateDDLog.transExp(init)
        val addCode = generateDDLog.transExp(op)
        val ddlogAggOp = DDLogAggregationOperator(fun.name, aggOp.resultType, initCode, addCode)
        Seq(Aggregate(rel, args, ddlogAggOp))
      case _ =>
        super.visitAtom(atom)
