package inca.ir.extension.list

import inca.ir.extension.arithmetic.DoubleNum
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.extension.*
import inca.ir.extension.arithmetic.{IntNum, TDouble, TInt}
import inca.ir.extension.demand.TDemand
import inca.ir.typing.IRTypechecker
import inca.ir.lowering.BaseLowering
import inca.ir.{BaseIR, TAny, Body, Eq, Language, Module, Param, Relation, Var, string2name}

class ListLoweringTest extends AnyFunSuite {

   def createModuleAndLower(relations: Relation*): Module =
      val typecheckerBefore = new IRTypechecker

      val lowerings = Seq(
         new list.Lowering {},
         new block.Lowering {},
         new disjunction.Lowering {},
         new bool.Lowering {},
         new demand.Lowering {}
      )

      def lower(l: BaseLowering, m: Module): Module =
         val checker = new IRTypechecker
         val lowered = l.lower(m)
         checker.checkProgram(Seq(lowered))
         if checker.getErrors.nonEmpty then
            checker.getErrors.foreach(println)
         lowered

      val mod = Module("M", Language(IR, arithmetic.IR, block.IR, list.IR, disjunction.IR, bool.IR), relations)
      typecheckerBefore.checkProgram(Seq(mod))

      val lowered = lowerings.foldLeft(mod)((mod, l) => lower(l, mod))
      lowered

   test("IsEmpty of an empty list literal"){
      val outParams = Seq(Param("x", TList(TAny)))
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("x"), ListLit(Seq())),
            Eq(Var("empty"), IsEmpty(Var("x")))
         )))
      )
      val mod = createModuleAndLower(mainRelation)
   }

   test("tail of an empty list literal") {
      val outParams = Seq(Param("x", TList(TAny)))
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("x"), Tail(ListLit(Seq()))
         )))
      ))
      val mod = createModuleAndLower(mainRelation)
   }

   test("head of an empty list literal") {
      val outParams = Seq(Param("x", TAny))
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("x"), Head(ListLit(Seq())))
         ))
      ))
      val mod = createModuleAndLower(mainRelation)
   }

   test("size of an empty list literal") {
      val mainRelation = Relation("main", Seq(), Seq(
         Body(Seq(
            Eq(Var("x"), Size(ListLit(Seq())))
         ))
      ))
      val mod = createModuleAndLower(mainRelation)
   }

   test("list literal") {
      val outParam = Param("x", TList(TDouble))
      val mainRelation = Relation("main", Seq(outParam), Seq(
         Body(Seq(
            Eq(Var("y"), DoubleNum(0)),
            Eq(Var("x"), ListLit(Seq(Var("y"), DoubleNum(1.4), DoubleNum(2.4))))
         ))
      ))
      val mod = createModuleAndLower(mainRelation)
   }

   test("IsEmpty of a non-empty list literal"){
      val outParams = Seq(Param("x", TList(TDouble)))
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("y"), DoubleNum(0)),
            Eq(Var("x"), ListLit(Seq(Var("y"), DoubleNum(1.4), DoubleNum(2.4)))),
            Eq(Var("empty"), IsEmpty(Var("x")))
         )))
      )
      val mod = createModuleAndLower(mainRelation)
   }

   test("tail of list literal") {
      val outParams = Seq(
         Param("x", TDemand(TList(TDouble))),
         Param("y", TDemand(TDouble))
      )
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("y"), DoubleNum(0.0)),
            Eq(Var("x"), Tail(ListLit(Seq(Var("y"), DoubleNum(1.4), DoubleNum(2.4)))))
         ))
      ))
      val mod = createModuleAndLower(mainRelation)
   }

   test("head of list literal") {
      val outParams = Seq(Param("x", TDouble), Param("y", TDemand(TDouble)))
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("y"), DoubleNum(0.0)),
            Eq(Var("x"), Head(ListLit(Seq(Var("y"), DoubleNum(1.4), DoubleNum(2.4)))))
         ))
      ))
      val mod = createModuleAndLower(mainRelation)
   }

   test("size of list literal") {
      val outParams = Seq(Param("x", TInt))
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("x"), Size(ListLit(Seq(DoubleNum(2.3), DoubleNum(1.4), DoubleNum(2.4)))))
         ))
      ))
      val mod = createModuleAndLower(mainRelation)
   }

   test("append sth") {
      val outParam = Param("x", TList(TInt))
      val mainRelation = Relation("main", Seq(outParam), Seq(
         Body(Seq(
            Eq(Var("x"), Append(ListLit(Seq(IntNum(2), IntNum(3))), IntNum(4)))
         ))
      ))
      val mod = createModuleAndLower(mainRelation)
   }

   test("prepend sth") {
      val outParam = Param("x", TList(TInt))
      val mainRelation = Relation("main", Seq(outParam), Seq(
         Body(Seq(
            Eq(Var("x"), Prepend(ListLit(Seq(IntNum(2), IntNum(3))), IntNum(4)))
         ))
      ))
      val mod = createModuleAndLower(mainRelation)
   }

   test("append list literal") {
      val outParams = Seq(
         Param("x", TList(TDouble)),
         Param("y", TList(TDouble))
      )
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("y"), ListLit(Seq(DoubleNum(3.4), DoubleNum(0)))),
            Eq(Var("x"), Append(Var("y"), DoubleNum(2.4)))
         ))
      ))
      val mod = createModuleAndLower(mainRelation)
   }

   test("prepend list literal") {
      val outParams = Seq(
         Param("x", TList(TDouble)),
         Param("y", TList(TDouble))
      )
      val mainRelation = Relation("main", outParams, Seq(
         Body(Seq(
            Eq(Var("y"), ListLit(Seq(DoubleNum(3.4), DoubleNum(0)))),
            Eq(Var("x"), Prepend(Var("y"), DoubleNum(2.4)))
         ))
      ))
      val mod = createModuleAndLower(mainRelation)
   }
}
