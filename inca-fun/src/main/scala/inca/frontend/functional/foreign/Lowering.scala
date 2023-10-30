package inca.frontend.functional.foreign

import inca.foreign.scala.ir.primitive.{ScalaAggregationOperator, ScalaDefnModuleEntry, ScalaInca, ScalaType}
import inca.frontend.functional.compile.{GenerateIR, GenerateScala}
import inca.frontend.functional.syntax.{DataDef, FunctionDef}
import inca.ir.{Atom, BaseIR, ModuleEntry, name2string, string2name}
import inca.ir.lowering.BaseLowering
import inca.ir.extension.aggregate.{Aggregate, IR as iragg}

trait Lowering extends BaseLowering:
  override def loweredIRs: Set[BaseIR] = Set(iragg)
  override def requiredIRs: Set[BaseIR] = Set(iragg)

  val generateScala = new GenerateScala

  var scalaModules: Set[ScalaDefnModuleEntry] = Set()

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] =
    scalaModules = Set()
    super.visitModuleEntry(moduleEntry) ++ scalaModules.toSeq

  override def visitAtom(atom: Atom): Seq[Atom] =
    atom match
      case Aggregate(rel, args, aggOp@FunctionalIncaAggregationOperator(fun, init, op)) =>
        val ty = FunctionalInca.compileType(aggOp.to)
        // TODO: Lower the type

        generateScala.genFunDef(fun)
        val generatedCode = generateScala.generated.mkString("\n")

        scalaModules = scalaModules + ScalaDefnModuleEntry("Aggregation$" + fun.name, generatedCode)

        val aggCode = generateScala.genAggregation(fun.name, init, op, aggOp.to)
        val scalaAggOp = ScalaAggregationOperator(ScalaInca.compileType(ty), aggCode + ".aggregator")
        Seq(Aggregate(rel, args, scalaAggOp))
      case _ =>
        super.visitAtom(atom)
