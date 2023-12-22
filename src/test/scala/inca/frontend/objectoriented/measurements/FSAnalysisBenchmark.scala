package inca.frontend.objectoriented.measurements

import inca.backend.analyze.DependencyGraph.{DependencyEdge, NegativeCall}
import inca.backend.analyze.{DependencyGraph, Graph}
import inca.frontend.ir.Relation
import inca.frontend.objectoriented
import inca.frontend.objectoriented.compiler.{CompiledObjectModule, ObjectOptions}
import inca.frontend.objectoriented.core.{ClassDef, ClassRef, FieldDef, MethodDef, Module, Name, Param, ReturnStmt, TClass, TScalaString, TSet}
import inca.frontend.objectoriented.datalog.ObjectOrientedDatalog
import inca.frontend.objectoriented.interpreter._
import inca.frontend.objectoriented.parser.Parser
import inca.frontend.objectoriented.transformations.{AddMissingDefinitions, InsertBuiltInMonotones}
import inca.frontend.objectoriented.typechecker.Typechecker
import inca.runtime.EnginePool
import inca.util.FileUtil
import inca.util.measurement.CSVUtil.{CSV, csvToString}
import inca.util.measurement.MemoryUtil

import scala.meta.Term

case class FSAnalysisBenchmark(warmups: Int, runs: Int) {
  val progFolder: String = s"objectoriented/measurements/"
  val resultPath: String = "benchmark/objectoriented"

  trait Config {
    val warmup: Int
    val runs: Int
    val name: String
  }
  case class FSConfig(warmup: Int, runs: Int, name: String, numAssign: Int, numWhiles: Int) extends Config


  def options: ObjectOptions = ObjectOptions()
  val typechecker = new Typechecker {}

  private def toCSV(vals: Seq[(Int, IndexedSeq[Long])]): CSV = {
    val header = vals.map(_._1).toIndexedSeq
    // we assume that each list has same number of elements
    val rowLength = vals.head._2.size
    val rows = for (i <- 0 until rowLength) yield vals.map(_._2(i)).toIndexedSeq
    header +: rows
  }

  private def insertProgIntoMudoule(mod: Module, numAssigns: Int, numWhiles: Int): Module = {
    val astProg = GenerateWhileLanguageProg.generateProgramAst(numAssigns, numWhiles)
    val varSetExpr = GenerateWhileLanguageProg.generateVarSetExr(numAssigns, numWhiles)
    val classes = mod.classes.filter(_.name.raw != "Examples")
    val exampleMethods = mod.classes.filter(_.name.raw == "Examples").head.methods.filter(_.name.raw != "nestedWhile")
    val exampleClass = ClassDef(Seq(), None, Name("Examples"), Seq(), Seq(
      FieldDef(Seq(), None, Name("varNames"), TSet(TScalaString), Some(varSetExpr), immutable = true),
      //FieldDef(Seq(), None, Name("nestedWhile"), TClass(ClassRef(Name("Stm"))), Some(astProg), immutable = true),
      /*MethodDef(Seq(), None, Name("nestedWhile"), Seq(), TClass(ClassRef(Name("Stm"))), Seq(
        ReturnStmt(astProg)
      ))*/
    ) ++ exampleMethods)
    Module(mod.name, mod.imports, classes :+ exampleClass)
  }

  private def measureDatalog(c: FSConfig, prog: String, edb: Seq[Relation], args: Seq[Term]): IndexedSeq[Long] = {
    val code = FileUtil.readFile(prog)
    val parsed = objectoriented.parser.Parser.parse(code)

    val mod = insertProgIntoMudoule(parsed, c.numAssign, c.numWhiles)
    val module = CompiledObjectModule(mod, options)

    for (i <- 0 until c.warmup) yield {
      println(s"Warmup Datalog ${c.name}: ${i + 1}")
      val datalog = new ObjectOrientedDatalog(module)
      datalog.measure("ConstantAnalysis", "main", edb, args:_*)

      EnginePool.disposeAllEngines()
      MemoryUtil.collectGarbage()
    }
    for (i <- 0 until c.runs) yield {
      println(s"Run Datalog ${c.name}: ${i + 1}")

      val datalog = new ObjectOrientedDatalog(module)

      val diff = datalog.measure("ConstantAnalysis", "main", edb, args:_*)
      println("diff: " + diff.toDouble/1000000000d)

      EnginePool.disposeAllEngines()
      MemoryUtil.collectGarbage()

      diff
    }
  }

  private def measureInterpreter(c: FSConfig, prog: String, edb: Map[String, Value], args: Seq[Value]): IndexedSeq[Long] = {
    val code = FileUtil.readFile(prog)
    var mod = Parser.parse(code)

    // Insert input data into program
    mod = insertProgIntoMudoule(mod, c.numAssign, c.numWhiles)
    //println(mod)
    //System.exit(1)

    mod = InsertBuiltInMonotones.transformModule(mod)
    mod = AddMissingDefinitions.transformModule(mod)
    typechecker.typecheck(mod)

    val mainClass = "ConstantAnalysis"
    val mainMethod = "main"
    val mainClasses = mod.classes.filter(_.name.raw == mainClass)
    val mainMethods = mainClasses.flatMap(c => c.methods.filter(_.name.raw == mainMethod))
    if (mainMethods.size < 1) {
      throw new IllegalArgumentException(s"No main method with name $mainMethod for class $mainClass found!")
    } else if (mainMethods.size > 1) {
      throw new IllegalArgumentException(s"Ambiguous method with name $mainMethod for class $mainClass found!")
    }

    val main = mainMethods.head
    if (!main.isMain) {
      throw new IllegalArgumentException(s"Method $mainMethod for class $mainClass is not a main method!")
    }

    for (i <- 0 until c.warmup) yield {
      println(s"Warmup Interpreter ${c.name}: ${i + 1}")
      new Interpreter(mod, edb).run(main, args)

      MemoryUtil.collectGarbage()
    }
    for (i <- 0 until c.runs) yield {
      println(s"Run Interpreter ${c.name}: ${i + 1}")
      val interp = new Interpreter(mod, edb)

      println("Run.....")
      val start = System.nanoTime()
      interp.run(main, args)
      val diff = System.nanoTime() - start
      println("diff: " + diff.toDouble/1000000000d)

      MemoryUtil.collectGarbage()

      diff
    }
  }

  def runConstant(): Unit = {
    run("FSConstantAnalysis")
  }

  def runSign(): Unit = {
    run("FSSignAnalysis")
  }

  def run(progName: String): Unit = {
    val configs = for (i <- 2 to 21 by 2) yield {
      FSConfig(warmups, runs, progName, 10, i)
    }

    val prog = progFolder + progName + ".oinca"

    // OODL - Datalog
    val datalogMeasurements = for (c <- configs) yield {
      c.numWhiles -> measureDatalog(c, prog, Seq(), Seq(meta.Lit.Int(c.numWhiles), meta.Lit.Int(c.numAssign)))
    }
    FileUtil.writeFile(s"$resultPath/fsc/${progName}_Datalog.csv", csvToString(toCSV(datalogMeasurements)))

    // OODL - Interp
    val interpreterMeasurements = for (c <- configs) yield {
      c.numWhiles -> measureInterpreter(c, prog, Map(), Seq(ScalaValue(c.numWhiles), ScalaValue(c.numAssign)))
    }
    FileUtil.writeFile(s"$resultPath/fsc/${progName}_Interpreter.csv", csvToString(toCSV(interpreterMeasurements)))
  }
}
