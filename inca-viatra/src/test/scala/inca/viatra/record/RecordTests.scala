package inca.viatra.record

import inca.ir.execution.{Relation2, UnitRelation}
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.block.Lowering as BlockLowering
import inca.ir.extension.data.TData
import inca.ir.extension.record.{FieldDefinition, FieldLookup, RecordDefinition, RecordLit, TRecord, Lowering as RecordLowering}
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.{block, data, record}
import inca.ir.util.SourceLocation
import inca.ir.{BaseIR, Body, Call, CompiledModule, Eq, Language, Module, Name, Param, RefByName, Relation, TermArg, Var, string2name}
import inca.util.compileroptions.CompilerOptions
import org.scalatest.funsuite.AnyFunSuite

class RecordTests extends AnyFunSuite:
  class Compiled(val ir: Module) extends CompiledModule:
    setPipeline(List(
      () => new RecordLowering {},
      () => new BlockLowering {}
    ))

    override def compilerOptions: CompilerOptions =
      val opt = CompilerOptions.default
      opt.irLogging.logModule = false
      opt.irLogging.logLowerings = false
      opt
    override def name: Name = ir.name
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
    override def optimize(p: Seq[Module]): Seq[Module] = p

  val mod1 = Module("Test02", BaseIR.language + record.IR, Seq(
    RecordDefinition("Company"),
    FieldDefinition("EmployeeCount", TInt, TRecord("Company")),
    Relation("R", Seq(Param("x", TRecord("Company")), Param("f", TInt)), Seq(
      Body(Seq(
        Eq(Var("x"), RecordLit("Company",Seq((RefByName(Name("EmployeeCount")), IntNum(4))))),
        Eq(Var("f"), FieldLookup(Var("x"), Name("EmployeeCount"))),
      ))
    ))
  ))

//  val baseIR = new BaseIR {}
  val slang = BaseIR.language + record.IR

  test("Simple Record and FieldLookup") {
    val compiled = Compiled(mod1)
    val engine = new inca.viatra.Executor().instantiate(compiled)
    val lit = engine.read(UnitRelation("R")).project(1, 2)
    assertResult(Set(4))(lit.toSet)
  }


  test("Terms: recordLit") {
    val input = Module("test", slang, Seq(
      RecordDefinition("Person"),
      FieldDefinition("haircolor", TString, TRecord(RefByName(Name("Person")))),
      Relation(Name("R"), Seq(Param("X", TRecord("Person"))), Seq(Body(Seq(
        Eq(Var("X"), RecordLit("Person", Seq((RefByName(Name("haircolor")), StringLit("green"))))),
      ))))
    ))
    val compiled = Compiled(input)
    val engine = new inca.viatra.Executor().instantiate(compiled)
    val lit = engine.read(UnitRelation("R"))
    assertResult("Person(green)")(lit.entries.head.toString)
  }

  test("Deconstruct") {
    val test_record = RecordDefinition("Person")
    val test_type = TRecord(RefByName(Name("Person")))
    val test_field0 = FieldDefinition("height", TInt, test_type)
    val test_field1 = FieldDefinition("haircolor", TString, test_type)
    val test_recordlit = RecordLit("Person", Seq(
      (RefByName(Name("haircolor")), StringLit("green")),
      (RefByName(Name("height")), IntNum(180))
    ))

    val input = Module("test", slang, Seq(
      test_record,
      test_field0,
      test_field1) ++
      Seq(Relation(Name("R"), Seq(Param("HairColor", TString), Param("Height", TInt)), Seq(Body(Seq(
        Eq(Var("PersonVar"), test_recordlit),
        record.Deconstruct(Var("PersonVar"), RefByName(Name("Person")), Seq(
          (Name("haircolor"), TermArg(Var("HairColor"))),
          (Name("height"), TermArg(Var("Height"))),
        )) // Deconstruct(record: Term, name: Ref[RecordDefinition], fields: Seq[(Name, Arg)], neg: Boolean)
      )))))
    )

    val compiled = Compiled(input)
    val engine = new inca.viatra.Executor().instantiate(compiled)
    val lit = engine.read(UnitRelation("R"))
    assertResult(Set(("green", 180)))(lit.toSet)
  }

  test("Deconstruct more complex"){
    val test_record = RecordDefinition("Person")
    val test_type = TRecord(RefByName(Name("Person")))
    val test_field0 = FieldDefinition("height", TInt, test_type)
    val test_field1 = FieldDefinition("haircolor", TString, test_type)
    val test_field2 = FieldDefinition("employed_at", TRecord("Company"), test_type)
    val test_recordlit = RecordLit("Person", Seq(
      (RefByName(Name("haircolor")), StringLit("green")),
      (RefByName(Name("height")), IntNum(180)),
      (RefByName(Name("employed_at")), RecordLit("Company", Seq((RefByName(Name("Name")), StringLit("Apple")))))
    ))
    val companyRecord = Seq(
      RecordDefinition("Company"),
      FieldDefinition("Name", TString, TRecord("Company"))
    )

    val input = Module("test", slang, Seq(
      test_record,
      test_field0,
      test_field1,
    ) ++
      companyRecord ++
      Seq(test_field2) ++
      Seq(Relation(Name("R"), Seq(
        Param("HairColor", TString),
        Param("Height", TInt),
        Param("EmployedAt", TRecord("Company"))),
        Seq(Body(Seq(
          Eq(Var("PersonVar"), test_recordlit),
          record.Deconstruct(Var("PersonVar"), RefByName(Name("Person")), Seq(
            (Name("haircolor"), TermArg(Var("HairColor"))),
            (Name("height"), TermArg(Var("Height"))),
            (Name("employed_at"), TermArg(Var("EmployedAt")))
          )) // Deconstruct(record: Term, name: Ref[RecordDefinition], fields: Seq[(Name, Arg)], neg: Boolean)
        )))
      ))
    )
    val compiled = Compiled(input)
    val engine = new inca.viatra.Executor().instantiate(compiled)
    val lit = engine.read(UnitRelation("R")).project(0, 2)
    assertResult(Set(("green", 180)))(lit.toSet)
  }
