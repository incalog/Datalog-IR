package inca.viatra.structuralUID

import inca.foreign.scala.ir.primitive.{ScalaMakeUID, ScalaType, IR as primitiveIR, Typechecker}
import inca.ir.execution.UnitRelation
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.ir.{Body, CompiledModule, Eq, Call, Language, Module, Name, Param, Relation, Var, string2name, term2Arg}
import inca.util.compileroptions.CompilerOptions
import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite:

  val mod = Module("Path",
    Language(primitiveIR, inca.ir.extension.arithmetic.IR),
    Seq(
      Relation(
        "zero",
        Seq(Param("x", TInt), Param("y", ScalaType.uid)),
        Seq(
          Body(Seq(
            Eq(Var("x"), IntNum(1)),
            Eq(Var("y"), ScalaMakeUID("Zero", Seq()))
          )),
          Body(Seq(
            Eq(Var("x"), IntNum(1)),
            Eq(Var("y"), ScalaMakeUID("Zero", Seq()))
          ))
        )
      ),
      Relation(
        "succ",
        Seq(Param("x", TInt), Param("y", ScalaType.uid)),
        Seq(
          Body(Seq(
            Eq(Var("x"), IntNum(1)),
            Call("zero", Seq(Var("x"), Var("pred"))),
            Eq(Var("y"), ScalaMakeUID("Succ", Seq(Var("pred"))))
          )),
        )
      )
    ))

  class ScalaTypeChecker extends IRTypechecker with Typechecker

  class Compiled(val ir: Module) extends CompiledModule:
    override def compilerOptions: CompilerOptions =
      val opt = CompilerOptions.default
      opt.irLogging.logModule = false
      opt

    override def name: Name = ir.name
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation

    override def typechecker: BaseIRTypechecker = new ScalaTypeChecker
    override def optimize(p: Seq[Module]): Seq[Module] = p

  test("simple example") {
    val options = CompilerOptions.default

    val viatraLogging = options("viatra_logging")
    viatraLogging.update("typed", false)
    viatraLogging.update("module", true)
    viatraLogging.update("lowerings", false)
    viatraLogging.update("psystem", false)

    val viatraOptions = options("viatra_options")
    viatraOptions.update("apply_double_aggregation_rewrite", true)

    val compiled = Compiled(mod)
    compiled.setPipeline(List())

    val engine = new inca.viatra.Executor().instantiate(compiled)

    val res = engine.read(UnitRelation("succ"))
    println(res.asTable)
  }


