package inca.frontend.functional.foreign

import inca.foreign.scala.ir.primitive.{ScalaAggregationOperator, ScalaDefnModuleEntry, ScalaInca, ScalaType}
import inca.frontend.functional.compile.{GenerateIR, GenerateScala}
import inca.frontend.functional.syntax.{DataDef, FunctionDef}
import inca.ir.{Atom, BaseIR, ModuleEntry, name2string, string2name}
import inca.ir.lowering.BaseLowering
import inca.ir.extension.aggregate.{Aggregate, IR as iragg}
import inca.foreign.scala.ir.primitive

trait Lowering extends BaseLowering:
  override val name: String = "Foreign"
  override def loweredIRs: Set[BaseIR] = Set(iragg)
  override def requiredIRs: Set[BaseIR] = Set(iragg, primitive.IR)

  val generateScala = new GenerateScala

  var scalaModules: Set[ScalaDefnModuleEntry] = Set()

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] =
    scalaModules = Set()
    super.visitModuleEntry(moduleEntry) ++ scalaModules.toSeq

  override def visitAtom(atom: Atom): Seq[Atom] =
    atom match
      case Aggregate(rel, args, aggOp@FunctionalIncaAggregationOperator(fun, init, op)) =>
        val irType = FunctionalInca.compileType(aggOp.to)
        val scalaType = primitive.ScalaInca.compileType(aggOp.resultType)

        generateScala.genFunDef(fun)
        val generatedCode = generateScala.generated.mkString("\n")

        scalaModules = scalaModules + ScalaDefnModuleEntry("Aggregation$" + fun.name, generatedCode)

        val initCode = generateScala.transExp(init)
        val addCode = generateScala.transExp(op)
        val aggCode = generateScala.genAggregation(fun.name, init, op, aggOp.to)
        val scalaAggOp = ScalaAggregationOperator(fun.name, scalaType, initCode, addCode)
        Seq(Aggregate(rel, args, scalaAggOp))
      case _ =>
        super.visitAtom(atom)
