package inca.frontend.oodl.foreign

import inca.ir.lowering.BaseLowering
import inca.ir.BaseIR
import inca.ir.{Atom, BaseIR, ModuleEntry, name2string, string2name}
import inca.ir.lowering.BaseLowering
import inca.ir.extension.aggregate.{Aggregate, IR as iragg}
import inca.foreign.scala.ir.primitive.IR as irprimitive
import inca.foreign.scala.ir.primitive.{ScalaAggregationOperator, ScalaDefnModuleEntry, ScalaInca}
import inca.frontend.oodl.compile.GenerateScala
import inca.frontend.oodl.syntax.Module as OODLModule
import inca.ir

class Lowering(oodlModule: OODLModule) extends BaseLowering:
  override def loweredIRs: Set[BaseIR] = Set(iragg)
  override def requiredIRs: Set[BaseIR] = Set(iragg, irprimitive)

  val generateScala = new GenerateScala

  override def visitModule(module: ir.Module): ir.Module =
    val scalaCode = generateScala.transModule(oodlModule)
    val scalaContent = ScalaDefnModuleEntry(s"${module.name}$$Scala", scalaCode)
    val ir.Module(name, lang, content) = super.visitModule(module)
    ir.Module(name, lang, scalaContent +: content)

  override def visitAtom(atom: Atom): Seq[Atom] =
    atom match
      case Aggregate(rel, args, aggOp@OODLAggregationOperator(fun, init, op)) =>
        // TODO: Lower the type
        val ty = OODL.compileType(aggOp.to)
        val aggCode = generateScala.genAggregation(fun.name, init, op, aggOp.to)
        val scalaAggOp = ScalaAggregationOperator(ScalaInca.compileType(ty), aggCode + ".aggregator")
        Seq(Aggregate(rel, args, scalaAggOp))
      case _ =>
        super.visitAtom(atom)
