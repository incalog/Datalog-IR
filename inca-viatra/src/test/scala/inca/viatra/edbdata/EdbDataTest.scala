package inca.viatra.edbdata

import inca.ir.execution.{Relation1, Relation2}
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.edbdata.*
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.typing.IRTypechecker
import inca.ir.util.SourceLocation
import inca.ir.{Body, Call, CompiledUnit, Eq, Language, Module, ModuleEntry, Name, Param, Relation, TNothing, Var, string2name}
import inca.util.compileroptions.CompilerOptions
import inca.viatra.backend.Executor
import inca.viatra.runtime.context.DataModel
import org.scalatest.funsuite.AnyFunSuiteLike
import truechange.*

class EdbDataTest extends AnyFunSuiteLike:
  val exec = new Executor()

  class EdbCompiledUnit(val ir: Module, dataModel: DataModel) extends CompiledUnit:
    override def name: Name = ir.name

    override val isClosedWorld: Boolean = true

    override def otherUnits: Seq[CompiledUnit] = Seq()

    lazy val irModules: Seq[Module] = Seq(ir)

    override def compilerOptions: CompilerOptions = CompilerOptions.default

    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation

    lazy val engine: exec.Engine = exec.instantiate(this, dataModel)

  def module(dataModel: DataModel, relations: ModuleEntry*): EdbCompiledUnit =
    val typechecker = new IRTypechecker {}
    val mod = Module("M", Language(IR), relations)
    typechecker.checkProgram(Seq(mod))
    //try typechecker.checkProgram(Seq(mod))
    //finally println(mod)
    new EdbCompiledUnit(mod, dataModel)

  test("Type enumerate") {
    val mod = module(Nat.dataModel,
      EdbNodeDefinition("Nat"),
      EdbNodeDefinition("Zero", "Nat"),
      EdbNodeDefinition("Succ", "Nat"),
      EdbFieldDefinition("Succ", "pred", TEdbNode("Nat")),

      Relation("nats", Seq(Param("n", TEdbNode("Nat"))), Seq(
        Body(Seq(
          Eq(Var("n"), LookupEdbType(TEdbNode("Nat")))
        ))
      )),
      Relation("succs", Seq(Param("n", TEdbNode("Succ"))), Seq(
        Body(Seq(
          Eq(Var("n"), LookupEdbType(TEdbNode("Succ")))
        ))
      ))
    )

    import Nat.*
    val n = Succ(Succ(Zero()))
    val edits = n.load()
    mod.engine.feed.processCoreEditScript(edits)

    //edits.print()

    val succs = mod.engine.read(Relation1("succs", Seq("n"), Seq()))
    //println(succs.asTable)
    assertResult(2)(succs.size)

    val nats = mod.engine.read(Relation1("nats", Seq("n"), Seq()))
    //println(nats.asTable)
    assertResult(3)(nats.size)
  }

  test("Type field enumerate") {
    val mod = module(Nat.dataModel,
      EdbNodeDefinition("Nat"),
      EdbNodeDefinition("Zero", "Nat"),
      EdbNodeDefinition("Succ", "Nat"),
      EdbFieldDefinition("Succ", "pred", TEdbNode("Nat")),

      Relation("preds", Seq(Param("s", TEdbNode("Succ")), Param("p", TEdbNode("Nat"))), Seq(
        Body(Seq(
          Eq(Var("p"), LookupEdbField(Var("s"), "pred"))
        ))
      ))
    )

    import Nat.*
    val n = Succ(Succ(Zero()))
    val edits = n.load()
    mod.engine.feed.processCoreEditScript(edits)

    //edits.print()

    val preds = mod.engine.read(Relation2("preds", Seq("s", "p"), Seq()))
    //println(preds.asTable)
    assertResult(2)(preds.size)
  }

  test("Type field lookup") {
    val mod = module(Nat.dataModel,
      EdbNodeDefinition("Nat"),
      EdbNodeDefinition("Zero", "Nat"),
      EdbNodeDefinition("Succ", "Nat"),
      EdbFieldDefinition("Succ", "pred", TEdbNode("Nat")),

      Relation("succs", Seq(Param("n", TEdbNode("Succ"))), Seq(
        Body(Seq(
          Eq(Var("n"), LookupEdbType(TEdbNode("Succ")))
        ))
      )),

      Relation("preds", Seq(Param("s", TEdbNode("Succ")), Param("p", TEdbNode("Nat"))), Seq(
        Body(Seq(
          Call("succs", Seq(Var("s").arg)),
          Eq(Var("p"), LookupEdbField(Var("s"), "pred"))
        ))
      ))
    )

    import Nat.*
    val n = Succ(Succ(Zero()))
    val edits = n.load()
    mod.engine.feed.processCoreEditScript(edits)

    //edits.print()

    val preds = mod.engine.read(Relation2("preds", Seq("s", "p"), Seq()))
    //println(preds.asTable)
    assertResult(2)(preds.size)
  }

  test("Type field inverse lookup") {
    val mod = module(Nat.dataModel,
      EdbNodeDefinition("Nat"),
      EdbNodeDefinition("Zero", "Nat"),
      EdbNodeDefinition("Succ", "Nat"),
      EdbFieldDefinition("Succ", "pred", TEdbNode("Nat")),

      Relation("nats", Seq(Param("n", TEdbNode("Nat"))), Seq(
        Body(Seq(
          Eq(Var("n"), LookupEdbType(TEdbNode("Nat")))
        ))
      )),

      Relation("preds", Seq(Param("s", TEdbNode("Succ")), Param("p", TEdbNode("Nat"))), Seq(
        Body(Seq(
          Call("nats", Seq(Var("p").arg)),
          Eq(Var("p"), LookupEdbField(Var("s"), "pred"))
        ))
      ))
    )

    import Nat.*
    val n = Succ(Succ(Zero()))
    val edits = n.load()
    mod.engine.feed.processCoreEditScript(edits)

    //edits.print()

    val preds = mod.engine.read(Relation2("preds", Seq("s", "p"), Seq()))
    //println(preds.asTable)
    assertResult(2)(preds.size)
  }

  test("Exp schema") {
    module(new DataModel(),
      EdbNodeDefinition("Exp"),
      EdbNodeDefinition("Var", "Exp"),
      EdbFieldDefinition("Var", "name", TEdbValue(TString)),
      EdbNodeDefinition("Num", "Exp"),
      EdbFieldDefinition("Num", "value", TEdbValue(TInt)),
      EdbNodeDefinition("Add", "Exp"),
      EdbFieldDefinition("Add", "lhs", TEdbNode("Exp")),
      EdbFieldDefinition("Add", "rhs", TEdbNode("Exp"))
    )
  }

