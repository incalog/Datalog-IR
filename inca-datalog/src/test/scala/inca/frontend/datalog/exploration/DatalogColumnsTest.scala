package inca.frontend.datalog.exploration

import inca.frontend.datalog.compile.DatalogCompilerOptions
import inca.ir.execution.{IRExecutor, UnitRelation, Relation2 as ExecRelation2}
import org.scalatest.funsuite.AnyFunSuite
import inca.{ir as base}
import inca.ir.extension.arithmetic as irarith
import inca.ir.util.SourceLocation
import inca.ir.{CompiledUnit, Name, string2name, term2Arg}
import inca.util.compileroptions.CompilerOptions
import inca.viatra.backend.Executor

class DatalogColumnsTest extends AnyFunSuite:
  val options: DatalogCompilerOptions = DatalogCompilerOptions.fromResource("datalog/Options.ini")
  val exec: IRExecutor = new Executor()

  test("Path") {
    // Note: Deactivate optimizations before experiments

    // Config
    val numExec = 10
    val numNodes = 100
    val numUnusedParams = 400

    val additionalParams =
      for i <- 0.until(numUnusedParams) yield
        base.Param(s"p$i", irarith.TInt)
    val additionalParamEqs =
      for i <- 0.until(numUnusedParams) yield
        base.Eq(base.Var(s"p$i"), base.Var("x"))

    val mod = base.Module(
      "PathModule",
      irarith.IR.language,
      Seq(
        base.ExtensionalRelation("edge", Seq(base.Param("x", irarith.TInt), base.Param("y", irarith.TInt))),
        base.Relation(
          "path",
          Seq(base.Param("x", irarith.TInt), base.Param("y", irarith.TInt)) ++ additionalParams,
          Seq(
            base.Body(Seq(
              base.ExtensionalCall("edge", Seq(base.Var("x"), base.Var("y")))
            ) ++ additionalParamEqs),
            base.Body(Seq(
              base.ExtensionalCall("edge", Seq(base.Var("x"), base.Var("z"))),
              base.Call("path", Seq(base.Var("z").arg, base.Var("y").arg) ++ additionalParams.map(p => base.Var(p.name).arg))
            ) ++ additionalParamEqs)
          )
        )
      )
    )

    val compiledMod = new CompiledUnit:
      override def compilerOptions: CompilerOptions = options

      override def name: Name = mod.name

      override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation

      override def irModules: Seq[base.Module] = Seq(mod)

      override val isClosedWorld: Boolean = true

      override def otherUnits: Seq[CompiledUnit] = Seq()

    val graph = (for i <- 0.until(numNodes) yield Seq(i, i + 1)) :+ Seq(numNodes, 0)
    val edbRels = ExecRelation2("edge", Seq("x", "y"), graph)

    val dts = for (i <- 0.until(numExec)) yield {
      val engine = exec.instantiate(compiledMod)
      engine.insert(edbRels)

      val start = System.currentTimeMillis()
      val res = engine.read(UnitRelation("path"))
      val dt = System.currentTimeMillis() - start
      //println(res.asTable)
      dt
    }

    println(s"Execution time ${dts.sum / dts.size}ms")
  }
