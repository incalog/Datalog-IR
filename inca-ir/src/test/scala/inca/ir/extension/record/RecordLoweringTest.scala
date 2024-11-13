package inca.ir.extension.record

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.{block, data, record}
import inca.ir.extension.data.{CaseDefinition, DataDefinition, TData}
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.extension.record.Lowering
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.typing.IRTypechecker
import inca.ir.{BaseIR, Body, Eq, Language, Module, Name, Param, RefByName, Relation, TermArg, Var, string2name}


class RecordLoweringTest extends AnyFunSuite {

  val baseIR = new BaseIR {}
  private val expected_lang = Language(baseIR, block.IR, data.IR)
  private val slang = BaseIR.language + record.IR

  val companyRecord = Seq(
    RecordDefinition("Company"),
    FieldDefinition("Name", TString, TRecord("Company"))
  )

  test("Terms: recordLit") {
    val lowering = new Lowering {}
    val test_type = TRecord(RefByName(Name("Person")))
    val test_field = FieldDefinition("haircolor", TString, test_type)
    val test_record = RecordDefinition("Person")
    val test_recordlit = RecordLit("Person", Seq((RefByName(Name("haircolor")), StringLit("green"))))

    val input = Module("test", slang, Seq(
      test_record,
      test_field,
      Relation(Name("R"), Seq(Param("X", TRecord("Person"))), Seq(Body(Seq(
        Eq(Var("X"), test_recordlit),
      ))))
    ))

    val lowered = lowering.visitProgram(Seq(input)).head
    val expected = Module("test", expected_lang, Seq(
      data.DataDefinition("Person$Record"),
      data.CaseDefinition("Person", Seq(TString), TData("Person$Record")),
      Relation(Name("R"), Seq(Param("X", TData("Person$Record"))), Seq(Body(Seq(
        Eq(Var("X"), data.Construct("Person", Seq(StringLit("green"))))
      )))),
    ))

    //println(lowered)
    assertResult(expected.lang)(lowered.lang)
    assertResult(expected.contents.toSet)(lowered.contents.toSet)
  }


  test("Terms: FieldLookup") {
    val lowering = new Lowering {}
    val typechecker = new IRTypechecker
    val test_record = RecordDefinition("Person")
    val test_type = TRecord(RefByName(Name("Person")))
    val test_field1 = FieldDefinition("haircolor", TString, test_type)
    val test_recordlit = RecordLit("Person", Seq(
      (RefByName(Name("haircolor")), StringLit("green"))
    ))

    val input = Module("test", slang, Seq(
      test_record,
      test_field1,
      Relation(Name("R"), Seq(Param("X", TString)), Seq(Body(Seq(
        Eq(Var("PersonVar"), test_recordlit),
        Eq(Var("HairColorVar"), FieldLookup(Var("PersonVar"), RefByName(Name("haircolor")))),
        Eq(Var("HairColorVar"), Var("X"))
      ))))
    ))
    typechecker.checkProgram(Seq(input))
    val lowered = lowering.visitProgram(Seq(input))
    val expected = Module("test", expected_lang, Seq(
      data.DataDefinition("Person$Record"),
      Relation(Name("R"), Seq(Param("X", TString)), Seq(Body(Seq(
        Eq(Var("PersonVar"), data.Construct("Person", Seq(StringLit("green")))),
        Eq(Var("HairColorVar"), block.Block(data.Deconstruct(Var("PersonVar"), Name("Person"), Seq(TermArg(Var("field$0")))), Var("field$0"))),
        Eq(Var("HairColorVar"), Var("X"))
      )))),
      data.CaseDefinition("Person", Seq(TString), TData("Person$Record")),
    ))
    //println(lowered)
    assertResult(expected.lang)(lowered.head.lang)
    assertResult(expected.contents.toSet)(lowered.head.contents.toSet)
  }

  test("Terms: FieldLookup with two fields") {
    val lowering = new Lowering {}
    val typechecker = new IRTypechecker
    val test_record = RecordDefinition("Person")
    val test_type = TRecord(RefByName(Name("Person")))
    val test_field0 = FieldDefinition("height", TInt, test_type)
    val test_field1 = FieldDefinition("haircolor", TString, test_type)
    val test_recordlit = RecordLit("Person", Seq(
      (RefByName(Name("height")), IntNum(180)),
      (RefByName(Name("haircolor")), StringLit("green"))
    ))

    val input = Module("test", slang, Seq(
      test_record,
      test_field0,
      test_field1,
      Relation(Name("R"), Seq(Param("X", TString)), Seq(Body(Seq(
        Eq(Var("PersonVar"), test_recordlit),
        Eq(Var("HairColorVar"), FieldLookup(Var("PersonVar"), RefByName(Name("haircolor")))),
        Eq(Var("HairColorVar"), Var("X"))

      ))))
    ))
    typechecker.checkProgram(Seq(input))
    //println(typechecker.getErrors)
    val lowered = lowering.visitProgram(Seq(input))
    val expected = Module("test", expected_lang, Seq(
      data.DataDefinition("Person$Record"),
      Relation(Name("R"), Seq(Param("X", TString)), Seq(Body(Seq(
        Eq(Var("PersonVar"), data.Construct("Person", Seq(IntNum(180), StringLit("green")))),
        Eq(Var("HairColorVar"), block.Block(data.Deconstruct(Var("PersonVar"), Name("Person"), Seq(TermArg(Var("field$0")), TermArg(Var("field$1")))), Var("field$1"))),
        Eq(Var("HairColorVar"), Var("X"))
      )))),
      data.CaseDefinition("Person", Seq(TInt, TString), TData("Person$Record")),
    ))
    //println(lowered)
    assertResult(expected.lang)(lowered.head.lang)
    assertResult(expected.contents.toSet)(lowered.head.contents.toSet)
  }

  test("Terms: FieldLookup with two fields in different order") {
    val lowering = new Lowering {}
    val typechecker = new IRTypechecker
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
      test_field1,
      Relation(Name("R"), Seq(Param("X", TString)), Seq(Body(Seq(
        Eq(Var("PersonVar"), test_recordlit),
        Eq(Var("HairColorVar"), FieldLookup(Var("PersonVar"), RefByName(Name("haircolor")))),
        Eq(Var("HairColorVar"), Var("X"))

      ))))
    ))
    typechecker.checkProgram(Seq(input))
    val lowered = lowering.visitProgram(Seq(input))
    val expected = Module("test", expected_lang, Seq(
      data.DataDefinition("Person$Record"),
      Relation(Name("R"), Seq(Param("X", TString)), Seq(Body(Seq(
        Eq(Var("PersonVar"), data.Construct("Person", Seq(IntNum(180), StringLit("green")))),
        Eq(Var("HairColorVar"), block.Block(data.Deconstruct(Var("PersonVar"), Name("Person"), Seq(TermArg(Var("field$0")), TermArg(Var("field$1")))), Var("field$1"))),
        Eq(Var("HairColorVar"), Var("X"))
      )))),
      data.CaseDefinition("Person", Seq(TInt, TString), TData("Person$Record")),
    ))
    //println(lowered)
    assertResult(expected.lang)(lowered.head.lang)
    assertResult(expected.contents.toSet)(lowered.head.contents.toSet)
  }

  test("Atoms: Deconstruct simple") {
    val lowering = new Lowering {}
    val typechecker = new IRTypechecker
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
                                      Seq(Relation(Name("R"), Seq(Param("HairColor", TString)), Seq(Body(Seq(
                                        Eq(Var("PersonVar"), test_recordlit),
                                        Deconstruct(Var("PersonVar"), RefByName(Name("Person")), Seq(
                                          (Name("haircolor"), TermArg(Var("HairColor"))),
                                          (Name("height"), TermArg(Var("Height")))
                                        )) // Deconstruct(record: Term, name: Ref[RecordDefinition], fields: Seq[(Name, Arg)], neg: Boolean)
                                      )))))
    )
    typechecker.checkProgram(Seq(input))
    val lowered = lowering.visitProgram(Seq(input))
    val expected = Module("test", expected_lang, Seq(
      data.DataDefinition("Person$Record"),
      Relation(Name("R"), Seq(Param("HairColor", TString)), Seq(Body(Seq(
        Eq(Var("PersonVar"), data.Construct("Person", Seq(
          IntNum(180),
          StringLit("green"),
        ))),
        data.Deconstruct(Var("PersonVar"), Name("Person"), Seq(
          TermArg(Var("Height")),
          TermArg(Var("HairColor")),
        ))
      )))),
      data.CaseDefinition("Person", Seq(TInt, TString), TData("Person$Record")),
    ))
    //println(lowered)
    assertResult(expected.lang)(lowered.head.lang)
    assertResult(expected.contents.toSet)(lowered.head.contents.toSet)

  }


  test("Atoms: Deconstruct more complex") {
    val lowering = new Lowering {}
    val typechecker = new IRTypechecker
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

    val input = Module("test", slang, Seq(
      test_record,
      test_field0,
      test_field1,
      test_field2,
    ) ++
                                      companyRecord ++
                                      Seq(Relation(Name("R"), Seq(Param("HairColor", TString)), Seq(Body(Seq(
                                        Eq(Var("PersonVar"), test_recordlit),
                                        Deconstruct(Var("PersonVar"), RefByName(Name("Person")), Seq(
                                          (Name("haircolor"), TermArg(Var("HairColor"))),
                                          (Name("height"), TermArg(Var("Height"))),
                                          (Name("employed_at"), TermArg(Var("EmployedAt")))
                                        )) // Deconstruct(record: Term, name: Ref[RecordDefinition], fields: Seq[(Name, Arg)], neg: Boolean)
                                      )))))
    )
    typechecker.checkProgram(Seq(input))
    val lowered = lowering.visitProgram(Seq(input))
    val expected = Module("test", expected_lang, Seq(
      data.DataDefinition("Person$Record"),
      data.DataDefinition("Company$Record"),
      Relation(Name("R"), Seq(Param("HairColor", TString)), Seq(Body(Seq(
        Eq(Var("PersonVar"), data.Construct("Person", Seq(
          IntNum(180),
          StringLit("green"),
          data.Construct("Company", Seq(StringLit("Apple")))
        ))),
        data.Deconstruct(Var("PersonVar"), Name("Person"), Seq(
          TermArg(Var("Height")),
          TermArg(Var("HairColor")),
          TermArg(Var("EmployedAt"))
        ))
      )))),
      data.CaseDefinition("Company", Seq(TString), TData("Company$Record")),
      data.CaseDefinition("Person", Seq(TInt, TString, TData("Company$Record")), TData("Person$Record")),
    ))
    //println(lowered)
    assertResult(expected.lang)(lowered.head.lang)
    assertResult(expected.contents.toSet)(lowered.head.contents.toSet)

  }
}
