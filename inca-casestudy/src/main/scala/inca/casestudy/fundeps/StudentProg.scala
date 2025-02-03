package inca.casestudy.fundeps

import inca.ir.{BaseIR, Body, Call, CompiledUnit, Eq, ExtensionalCall, ExtensionalRelation, FunctionalDependencyHint, Module, Name, Param, Relation, Var, execution, string2name, term2Arg, termList2ArgList}
import inca.ir.execution.{IRExecutor, UnitRelation}
import inca.ir.extension.arithmetic.TInt
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.{arithmetic, block, bool, demand, disjunction, string, tuple}
import inca.ir.optimize.AliasElimination
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions

import scala.util.Random

object StudentProg:

  val enrollment = ExtensionalRelation(
    "enrollment",
    Seq(
      Param("studentID", TString),
      Param("courseID", TString),
      Param("grade", TInt)
    )
  )

  val student = ExtensionalRelation(
    "student",
    Seq(
      Param("studentID", TString),
      Param("name", TString),
      Param("major", TString)
    )
  )

  val studentIDB = Relation(
    "studentIDB",
    Seq(
      Param("studentID", TString),
      Param("name", TString),
      Param("major", TString)
    ),
    Seq(
      Body(Seq(ExtensionalCall("student", Seq(Var("studentID"), Var("name"), Var("major")))))
    )
  )

  def main =
    Relation("main",
      Seq(
        Param("name", TString),
        Param("grade", TInt)
      ),
      Seq(
        Body(Seq(
          ExtensionalCall("enrollment", Seq(Var("studentID"), Var("course"), Var("grade"))),
          Call("studentIDB", Seq(Var("studentID"), Var("name"), Var("major"))),
        ))
      )
    )


  def createMod() = Module("StudentProg", BaseIR.language + arithmetic.IR + string.IR,
    Seq(
      enrollment,
      student,
      studentIDB,//.addHint(FunctionalDependencyHint(Seq("studentID"), Seq("name", "major"))),
      main
    )
  )


  def createCompiled(mod: Module): CompiledUnit = new CompiledUnit:
    override def name: Name = mod.name

    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation

    override val irModules: Seq[Module] = Seq(mod)

    override val otherUnits: Seq[CompiledUnit] = Seq()

    override val isClosedWorld = true

    override def compilerOptions: CompilerOptions = {
      val opt = CompilerOptions.default
      opt.irLogging.logModule = true
      opt.irLogging.logLowerings = true
      opt.irLogging.logTypeInformation = false
      opt.irLogging.logStatsAfterOptimizations = false
      opt
    }

    setPipeline(List(
      () => new bool.Lowering {},
      () => new block.Lowering {},
      () => new disjunction.Lowering {},
      () => new demand.Lowering {},
      () => new tuple.Lowering {},
      () => new AliasElimination {}
    ))


  def generateFakeData(studentCount: Int, courseCount: Int, enrollmentCount: Int): Seq[execution.Relation] =
    val random = new Random(1)

    // Generate students
    val studentData = (1 to studentCount).map { i =>
      val studentID = i.toString
      val name = s"Student_$studentID"
      val major = Seq("CS", "Math", "Physics", "Biology", "Chemistry")(random.nextInt(5))
      Seq(studentID, name, major)
    }

    // Generate courses
    val courseIDs = (1 to courseCount).map(i => s"Course_$i")

    // Generate enrollments
    val enrollmentData = (1 to enrollmentCount).map { _ =>
      val studentID = (1 + random.nextInt(studentCount)).toString
      val courseID = courseIDs(random.nextInt(courseCount))
      val grade = 50 + random.nextInt(51)
      Seq(studentID, courseID, grade)
    }

    val studentRel = execution.Relation3("student", Seq("studentID", "name", "major"), studentData)
    val enrollmentRel = execution.Relation3("enrollment", Seq("studentID", "courseID", "grade"), enrollmentData)

    Seq(studentRel, enrollmentRel)

  private def runModInEngine(executor: IRExecutor, mod: Module): execution.Relation =
    val compiled = createCompiled(mod)

    val data = generateFakeData(100, 100, 100)

    // Get result
    var engine = executor.instantiate(compiled)
    data.foreach(engine.insert)
    val res = engine.read(UnitRelation("main"))

    // Measure
    engine = executor.instantiate(compiled)
    data.foreach(engine.insert)
    println(engine.measure(UnitRelation("main")))
    res


  @main def runStudentProg() =
    inca.viatra.backend.Executor.initializeLogging()
    inca.viatra.backend.Executor.enableDebugLogging()

    val viatraRes = runModInEngine(inca.viatra.backend.Executor(), createMod())
    //println(viatraRes.asTable)