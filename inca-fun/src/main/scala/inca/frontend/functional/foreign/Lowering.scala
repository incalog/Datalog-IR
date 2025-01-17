package inca.frontend.functional.foreign

import inca.foreign.scala.ir.primitive.{ScalaAggregationOperator, ScalaDefnModuleEntry, ScalaInca, ScalaType}
import inca.frontend.functional.compile.{GenerateIR, GenerateScala}
import inca.frontend.functional.syntax.{DataDef, FunctionDef}
import inca.ir.{Atom, BaseIR, ModuleEntry, name2string, string2name}
import inca.ir.lowering.BaseLowering
import inca.ir.extension.aggregate.{Aggregate, IR as iragg}
import inca.foreign.scala.ir.primitive
import inca.ir
import inca.ir.Hint.preserveHints

trait Lowering extends BaseLowering:
  override val name: String = "Foreign"

  override def loweredIRs: Set[BaseIR] = Set(iragg)

  override def requiredIRs: Set[BaseIR] = Set(iragg, primitive.IR)

  val generateScala = new GenerateScala

  var scalaModules: Set[ScalaDefnModuleEntry] = Set()

  override def visitModule(module: ir.Module): ir.Module = preserveHints(module) {
    scalaModules = Set()
    val mod = super.visitModule(module)
    ir.Module(mod.name, mod.lang, mod.contents ++ scalaModules)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case Aggregate(rel, args, aggOp@FunctionalIncaAggregationOperator(fun, init, op)) =>
        // TODO: Lower type e.g. Boolean to int before passing it into this function

        generateScala.genFunDef(fun)
        val generatedCode = generateScala.generated.mkString("\n")

        scalaModules = scalaModules + ScalaDefnModuleEntry("Aggregation$" + fun.name, generatedCode)

        val initCode = generateScala.transExp(init)
        val addCode = generateScala.transExp(op)
        val scalaAggOp = ScalaAggregationOperator(fun.name, aggOp.resultType, initCode, addCode)
        Seq(Aggregate(rel, args, scalaAggOp))
      case _ =>
        super.visitAtom(atom)
  }
