package inca.casestudy.doop

import inca.ir.analysis.IRTerminationAnalysis
import inca.ir.analysis.base.values.{FiniteAbstractRelation, Value}
import inca.ir.execution.ThreadCount.{Auto, Fixed}
import inca.ir.execution.{IRExecutor, ThreadCount, UnitRelation}
import inca.ir.extension.data.{CaseDefinitionReference, TData}
import inca.ir.analysis.EdbConfig
import inca.ir.extension.arithmetic.analysis.interpreter.{IntervalDoubleV, IntervalIntV}
import inca.ir.extension.arithmetic.{TDouble, TInt}
import inca.ir.extension.data.analysis.interpreter.{FiniteCaseV, FiniteDataV}
import inca.ir.extension.string.TString
import inca.ir.extension.string.analysis.interpreter.FiniteStringV
import inca.ir.extension.{aggregategeneric, block, bool, disjunction, module, not}
import inca.ir.optimize.AliasElimination
import inca.ir.{CompiledUnit, Name, Param, Type, string2name}
import inca.souffle.frontend.compile.CompiledSouffleProgram
import inca.util.compileroptions.CompilerOptions
import sturdy.values.Topped

import scala.io.Source

class DoopSensitiveTerminationAnalysis extends IRTerminationAnalysis:
  /*
   * Source: https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.11
   *
   * 1. Bytecode index (BCI) of the JVM is 2 bytes, aka 2^16 = 65536:
   * 2. The max arity of methods is 255
   * 3. Most string, e.g. class names are limited by 2 bytes, aka. 2^16 = 65536 characters assuming ASCII encoding
   */

  def jvmBciLimit: IntervalIntV = IntervalIntV.finite(0, 65536 - 1)
  def jvmMethodArityLimit: IntervalIntV = IntervalIntV.finite(0, 256 - 1)
  def jvmTagSize: IntervalIntV = IntervalIntV.finite(0, 256 - 1)
  def lineNumberLimit: IntervalIntV = IntervalIntV.finite(0, 65536 - 1) // technically there is no limit on line numbers
  def jvmUpperStringLengthLimit: Int = 65536 - 1

  override def newEdbConfig(dataTypes: Map[TData, Set[CaseDefinitionReference]]): EdbConfig[FiniteAbstractRelation] =
    (n: Name, params: Seq[Param]) =>
      def abstractEDBValue(relName: Name, paramName: Name, ty: Type, at: Int): Value =
        (relName.name, paramName.name, ty, at) match
          /* Integer */
          case ("_ReturnVoid", "Q_index$param", TInt, 1) => jvmBciLimit // limit in mini javac 8337
          case ("_ExitMonitor", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_StaticMethodInvocation", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_StoreArrayIndex", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_VirtualMethodInvocation", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_Goto", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_Goto", "Q_to$param", TInt, 2) => jvmBciLimit // jump target, which is also a BCI
          case ("_AssignCast", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_MethodTypeConstant", "Q_arity$param", TInt, 1) => jvmMethodArityLimit
          case ("_FormalParam", "Q_index$param", TInt, 0) => jvmMethodArityLimit // index in the method signature
          case ("_AssignInstanceOf", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_AssignBinop", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_AssignCastNumConstant", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_LoadStaticField", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_StoreStaticField", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_Return", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_AssignCastNull", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_BootstrapParam", "Q_index$param", TInt, 0) => jvmBciLimit
          case ("_ThrowNull", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_ExceptionHandler", "Q_index$param", TInt, 2) => jvmBciLimit
          case ("_ExceptionHandler", "Q_begin$param", TInt, 4) => jvmBciLimit
          case ("_ExceptionHandler", "Q_end$param", TInt, 5) => jvmBciLimit
          case ("_BreakpointStmt", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_AssignUnop", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_UnsupportedInstruction", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_SpecialMethodInvocation", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_If", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_If", "Q_to$param", TInt, 2) => jvmBciLimit
          case ("Param_Annotation", "Q_index$param", TInt, 1) => jvmMethodArityLimit
          case ("_TableSwitch", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_MethodInvocation_Line", "line$param", TInt, 1) => lineNumberLimit // 7030
          case ("_StoreInstanceField", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_AssignNull", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_DynamicMethodInvocation", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_DynamicMethodInvocation", "Q_dynArity$param", TInt, 5) => jvmMethodArityLimit
          case ("_DynamicMethodInvocation", "Q_tag$param", TInt, 7) => jvmTagSize
          case ("_EnterMonitor", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_AssignNumConstant", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_SuperMethodInvocation", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_AssignLocal", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_Throw", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_ActualParam", "Q_index$param", TInt, 0) => jvmMethodArityLimit
          case ("_LoadArrayIndex", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_AssignHeapAllocation", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_AssignHeapAllocation", "Q_linenumber$param", TInt, 5) => lineNumberLimit
          case ("_MethodHandleConstant", "Q_arity$param", TInt, 4) => jvmMethodArityLimit
          case ("_LookupSwitch", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_AssignPhantomInvoke", "Q_index$param", TInt, 1) => jvmBciLimit
          case ("_Method", "Q_arity$param", TInt, 6) => jvmMethodArityLimit
          case ("_LoadInstanceField", "Q_index$param", TInt, 1) => jvmBciLimit
          /* String */
          case (_, _, TString, _) => FiniteStringV.edb(jvmUpperStringLengthLimit) // miniJavac max length: 29965
          case (_, _, TDouble, _)  => throw IllegalStateException("Not required!")
          case (_, _, _: TData, _) => throw IllegalStateException("Not required!")
          case _ => Value.Top

      val (aCols, aRows) = params.zipWithIndex.map((p, i) => (p.name.name, abstractEDBValue(n, p.name, p.ty, i))).unzip
      FiniteAbstractRelation(aCols, aRows, Topped.Actual(false), Topped.Actual(true))


object TwoTypeSensitiveHeap:
  private def runContextSensitiveDL(createEngine: (compiled: CompiledUnit) => IRExecutor#Engine, file: String): Unit =
    val baseDir = "doop"
    val source = Source.fromResource(baseDir + "/" + file)
    val options = CompilerOptions.default
    options.irLogging.logModule = false
    options.irLogging.logLowerings = false
    options.irLogging.logTypeInformation = false
    val compiled = CompiledSouffleProgram.fromSource("TwoTypeSensitiveHeap", source, options)
    compiled.setOptimizationPipeline(List())
    compiled.setPipeline(List(
      () => new aggregategeneric.Lowering {},
      () => new bool.Lowering {},
      () => new block.Lowering {},
      () => new disjunction.Lowering {},
      () => new not.Lowering {},
      () => new AliasElimination {},
      () => new module.Lowering {},
      () => new DoopSensitiveTerminationAnalysis
    ))

    println("Load edb from files...")
    val edbFacts = compiled.loadEdbInputs(baseDir)
    val outputRels = compiled.outputRelations

    println("Populate edb...")
    val engine = createEngine(compiled.mainUnit)
    edbFacts.foreach(engine.insert)

    println("Execute...")
    val execTime = outputRels.map { rel =>
      val start = System.currentTimeMillis()
      val res = engine.read(rel)
      val end = System.currentTimeMillis()
      println(res.name -> res.size)
      end - start
    }.sum

    /*val start = System.currentTimeMillis()
    engine.read(UnitRelation("VarPointsTo"))
    val end = System.currentTimeMillis()
    val execTime = end - start

    println(execTime / 1000.0)*/

  @main
  def runTwoTypeSensitiveHeapDL(): Unit = {
    runContextSensitiveDL(
      compiled => inca.souffle.backend.Executor(Auto).instantiate(compiled),
      "2-type-sensitive+heap-flatten.dl"
    )
  }

  @main
  def runTwoTypeSensitiveHeapDefensiveDL(): Unit = {
    runContextSensitiveDL(
      compiled => inca.souffle.backend.Executor(Auto).instantiate(compiled),
      "2-type-sensitive+heap-flatten - Defensive.dl"
    )
  }
