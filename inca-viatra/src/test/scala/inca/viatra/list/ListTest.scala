package inca.viatra.list

import inca.ir.execution.UnitRelation
import inca.ir.extension.arithmetic.DoubleNum
import inca.ir.extension.bool.TBoolean
import inca.ir.typing.BaseIRTypechecker
import inca.ir.util.SourceLocation
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{BaseIR, Body, Call, Cast, CompiledModule, Eq, Language, Module, Name, Param, Relation, TAny, TNothing, Type, Var, string2name}
import inca.util.compileroptions.CompilerOptions
import inca.viatra.backend.ViatraRelation
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.extension.*
import inca.ir.extension.arithmetic.{Add, IntNum, TDouble, TInt}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.list.*
import inca.ir.lowering.BaseLowering
import inca.ir.typing.{IRTypechecker, TypeErrorException}
import inca.ir.{Body, Eq, Language, Module, Param, Relation, TNothing, Var, string2name}

class ListTest extends AnyFunSuite {

   class Compiled(val ir: Module) extends CompiledModule:
      setPipeline(List(
         () => new list.Lowering {},
         () => new block.Lowering {},
         () => new bool.Lowering {},
         () => new disjunction.Lowering {},
         () => new demand.Lowering {}
      ))

      override def compilerOptions: CompilerOptions =
         val opt = CompilerOptions.default
         opt.irLogging.logModule = false
         opt.irLogging.logLowerings = false
         opt

      override def name: Name = ir.name
      override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
      override def typechecker: BaseIRTypechecker = new IRTypechecker
      override def optimize(p: Seq[Module]): Seq[Module] = p

   def executeMain(relations: Relation*): inca.ir.execution.Relation =
      val module = Module("M", Language(IR, arithmetic.IR, block.IR, list.IR), relations)
      val compiled = Compiled(module)
      val engine = new inca.viatra.backend.Executor().instantiate(compiled)
      engine.read(UnitRelation("main"))

   test("IsEmpty of an empty list literal"){
      val outParams = Seq(Param("empty", TBoolean))
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("x"), ListLit(Seq())),
            Eq(Var("empty"), IsEmpty(Var("x")))
         )))
      )
      assertResult(1)(executeMain(mainRelation).entries.head)
   }
   
   test("tail of an empty list literal") {
      val outParams = Seq(Param("x", TList(TAny)))
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("x"), Tail(ListLit(Seq()))
         )))
      ))
      val res = executeMain(mainRelation)
      assert(res.isEmpty)
   }

   test("head of an empty list literal") {
      val outParams = Seq(Param("x", TAny)) 
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("x"), Head(ListLit(Seq())))
         ))
      ))
      val res = executeMain(mainRelation)
      assert(res.isEmpty)
   }

   test("size of an empty list literal") {
      val mainRelation = Relation("main", Seq(Param("x", TInt)), Seq(
         Body(Seq(
            Eq(Var("x"), Size(ListLit(Seq())))
         ))
      ))
      val res = executeMain(mainRelation)
      assertResult(0)(res.entries.head)
   }

   test("append to an empty list literal") {
      val outParams = Seq(Param("x", TList(TDouble)))
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("x"), Append(
               Cast(
                  ListLit(Seq()), TList(TDouble)
               ), DoubleNum(2.4))
            )
         ))
      ))
      val res = executeMain(mainRelation)
      assertResult("Cons$TDouble(2.4,Nil$TDouble())")(res.entries.head.toString)
   }

   test("prepend to an empty list literal") {
      val outParams = Seq(Param("x", TList(TDouble)))
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("x"), Prepend(Cast(ListLit(Seq()), TList(TDouble)), DoubleNum(2.4)))
         ))
      ))
      val res = executeMain(mainRelation)
      assertResult("Cons$TDouble(2.4,Nil$TDouble())")(res.entries.head.toString)
   }
   
   test("list literal") {
      val outParam = Param("x", TList(TDouble))
      val mainRelation = Relation("main", Seq(outParam), Seq(
         Body(Seq(
            Eq(Var("y"), DoubleNum(0)),
            Eq(Var("x"), ListLit(Seq(Var("y"), DoubleNum(1.4), DoubleNum(2.4))))
         ))
      ))
      val res = executeMain(mainRelation)
      assertResult("Cons$TDouble(0.0,Cons$TDouble(1.4,Cons$TDouble(2.4,Nil$TDouble())))")(res.entries.head.toString)
   }

   test("IsEmpty of a non-empty list literal"){
      val outParams = Seq(Param("empty", TBoolean))
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("y"), DoubleNum(0)),
            Eq(Var("x"), ListLit(Seq(Var("y"), DoubleNum(1.4), DoubleNum(2.4)))),
            Eq(Var("empty"), IsEmpty(Var("x")))
         )))
      )
      val res = executeMain(mainRelation)
      assertResult(0)(res.entries.head)
   }

   test("tail of list literal") {
      val outParams = Seq(
         Param("x", TList(TDouble))
      )
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("y"), DoubleNum(0.0)),
            Eq(Var("x"), Tail(ListLit(Seq(Var("y"), DoubleNum(1.4), DoubleNum(2.4)))))
         ))
      ))
      val res = executeMain(mainRelation)
      assertResult("Cons$TDouble(1.4,Cons$TDouble(2.4,Nil$TDouble()))")(res.entries.head.toString)
   }

   test("head of list literal") {
      val outParams = Seq(Param("x", TDouble))
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("y"), DoubleNum(0.0)),
            Eq(Var("x"), Head(ListLit(Seq(Var("y"), DoubleNum(1.4), DoubleNum(2.4)))))
         ))
      ))
      val res = executeMain(mainRelation)
      assertResult(0.0)(res.entries.head)
   }

   test("size of list literal") {
      val outParams = Seq(Param("x", TInt))
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("x"), Size(ListLit(Seq(DoubleNum(2.3), DoubleNum(1.4), DoubleNum(2.4)))))
         ))
      ))
      val res = executeMain(mainRelation)
      assertResult(3)(res.entries.head)
   }

   test("append list literal") {
      val outParams = Seq(
         Param("x", TList(TDouble)),
      )
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("y"), ListLit(Seq(DoubleNum(3.4), DoubleNum(0)))),
            Eq(Var("x"), Append(Var("y"), DoubleNum(2.4)))
         ))
      ))
      val res = executeMain(mainRelation)
      assertResult("Cons$TDouble(3.4,Cons$TDouble(0.0,Cons$TDouble(2.4,Nil$TDouble())))")(res.entries.head.toString)
   }

   test("prepend list literal") {
      val outParams = Seq(
         Param("x", TList(TDouble)),
      )
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("y"), ListLit(Seq(DoubleNum(3.4), DoubleNum(0)))),
            Eq(Var("x"), Prepend(Var("y"), DoubleNum(2.4)))
         ))
      ))
      val res = executeMain(mainRelation)
      assertResult("Cons$TDouble(2.4,Cons$TDouble(3.4,Cons$TDouble(0.0,Nil$TDouble())))")(res.entries.head.toString)
   }
}

