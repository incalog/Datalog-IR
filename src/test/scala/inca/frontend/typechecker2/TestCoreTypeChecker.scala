package inca.frontend.typechecker2

import fastparse.parse
import inca.analyzedLangs
import inca.frontend.core.Core._
import inca.frontend.parser.CoreParser
import org.scalatest.flatspec.AnyFlatSpec

class TestCoreTypeChecker extends AnyFlatSpec{

  def parseExp(str: String): Exp = {
    val parser = CoreParser(Seq())
    // programs are always syntactically correct
    parse(str, parser.exp(_)).get.value
  }

  "checkExp" should "type var correctly" in {
    val typer = new TypeChecker(Nil)(analyzedLangs.Exp.languageMetaInfo)
    val varExp = parseExp("x")

    assertResult(typer.checkExp(typer.TypeContext(Map("x" -> TBool), Map(), null))(varExp))(TBool)
    assertThrows[IllegalArgumentException](typer.checkExp(typer.TypeContext(Map(), Map(), null))(varExp))
  }

  "checkExp" should "type constant literals correctly" in {
    val typer = new TypeChecker(Nil)(analyzedLangs.Exp.languageMetaInfo)
    val boolConst = parseExp("true")
    val intConst = parseExp("12")
    val longConst = parseExp("12L")
    val doubleConst = parseExp("12.0")
    val stringConst = parseExp("\"str\"")

    val ctx = typer.TypeContext(Map(), Map(), null)
    assertResult(typer.checkExp(ctx)(boolConst))(TBool)
    assertResult(typer.checkExp(ctx)(intConst))(TInt)
    assertResult(typer.checkExp(ctx)(longConst))(TLong)
    assertResult(typer.checkExp(ctx)(doubleConst))(TDouble)
    assertResult(typer.checkExp(ctx)(stringConst))(TString)
  }

  "checkExp" should "type wildcard correctly" in {
    val typer = new TypeChecker(Nil)(analyzedLangs.Exp.languageMetaInfo)
    val wildcard = parseExp("_")

    val ctx = typer.TypeContext(Map(), Map(), null)
    assertResult(typer.checkExp(ctx)(wildcard))(TAny)
  }

  "checkExp" should "type path access named link correctly" in {
    val typer = new TypeChecker(Nil)(analyzedLangs.Exp.languageMetaInfo)

    val pathAccess = parseExp("add.lhs")
    val ctx = typer.TypeContext(Map("add" -> TNode(analyzedLangs.Exp.addTag)), Map(), null)
    assertResult(typer.checkExp(ctx)(pathAccess))(TNode(analyzedLangs.Exp.expTag))

    val emptyCtx = typer.TypeContext(Map(), Map(), null)
    assertThrows[IllegalArgumentException](typer.checkExp(emptyCtx)(pathAccess))

    val invalidPathAccess = parseExp("add.vl")
    assertThrows[IllegalArgumentException](typer.checkExp(ctx)(invalidPathAccess))

    val listPathAccess = parseExp("many.exps")
    val listCtx = typer.TypeContext(Map("many" -> TNode(analyzedLangs.Exp.manyTag)), Map(), null)
    assertResult(typer.checkExp(listCtx)(listPathAccess))(TList(TNode(analyzedLangs.Exp.expTag)))
    val listSize = parseExp("many.exps.size")
    assertResult(typer.checkExp(listCtx)(listSize))(TInt)

    val listChilds = parseExp("many.exps.children")
    assertResult(typer.checkExp(listCtx)(listChilds))(TNode(analyzedLangs.Exp.expTag))
  }

  "checkExp" should "type path access parent link correctly" in {
    val typer = new TypeChecker(Nil)(analyzedLangs.Exp.languageMetaInfo)

    val parent = parseExp("many.parent")
    val ctx = typer.TypeContext(Map("many" -> TNode(analyzedLangs.Exp.manyTag)), Map(), null)
    assertResult(typer.checkExp(ctx)(parent))(TAny)
  }

  "checkExp" should "type path access size link correctly" in {
    val typer = new TypeChecker(Nil)(analyzedLangs.Exp.languageMetaInfo)

    val listSize = parseExp("many.exps.size")
    val ctx = typer.TypeContext(Map("many" -> TNode(analyzedLangs.Exp.manyTag)), Map(), null)
    assertResult(typer.checkExp(ctx)(listSize))(TInt)
  }

  "checkExp" should "type path access children link correctly" in {
    val typer = new TypeChecker(Nil)(analyzedLangs.Exp.languageMetaInfo)

    val listChilds = parseExp("many.exps.children")
    val ctx = typer.TypeContext(Map("many" -> TNode(analyzedLangs.Exp.manyTag)), Map(), null)
    assertResult(typer.checkExp(ctx)(listChilds))(TNode(analyzedLangs.Exp.expTag))
  }

  "checkExp" should "type path access next link correctly" in {
    val typer = new TypeChecker(Nil)(analyzedLangs.Exp.languageMetaInfo)

    val listChildsNext = parseExp("many.exps.children.next")
    val ctx = typer.TypeContext(Map("many" -> TNode(analyzedLangs.Exp.manyTag)), Map(), null)
    assertResult(typer.checkExp(ctx)(listChildsNext))(TNode(analyzedLangs.Exp.expTag))
  }

  "checkExp" should "type path access previous link correctly" in {
    val typer = new TypeChecker(Nil)(analyzedLangs.Exp.languageMetaInfo)

    val ctx = typer.TypeContext(Map("many" -> TNode(analyzedLangs.Exp.manyTag)), Map(), null)
    val listChildsPrev = parseExp("many.exps.children.previous")
    assertResult(typer.checkExp(ctx)(listChildsPrev))(TNode(analyzedLangs.Exp.expTag))
  }

  "checkExp" should "type tuple correctly" in {
    val typer = new TypeChecker(Nil)(analyzedLangs.Exp.languageMetaInfo)

    val tuple = parseExp("(1, x)")
    val ctx = typer.TypeContext(Map("x" -> TString), Map(), null)
    assertResult(typer.checkExp(ctx)(tuple))(TTuple(Seq(TInt, TString)))

    val tuple2 = parseExp("(1, x, 2L, true)")
    assertResult(typer.checkExp(ctx)(tuple2))(TTuple(Seq(TInt, TString, TLong, TBool)))

    val tuple3 = parseExp("(1, x, 2L, y)")
    assertThrows[IllegalArgumentException](typer.checkExp(ctx)(tuple3))
  }

  "checkExp" should "type call correctly" in {
    val typer = new TypeChecker(Nil)(analyzedLangs.Exp.languageMetaInfo)

    val funEnv = Map("fun" -> PatternFunction(None, "fun", Seq(Param("x", TInt), Param("y", TInt)), Seq(AnnoParam(None, TBool)), Seq()))
    val ctx = typer.TypeContext(Map(), funEnv, null)

    val call = parseExp("fun(1, 2)")
    assertResult(typer.checkExp(ctx)(call))(TBool)

    val wrongNumArgs = parseExp("fun(1, 2, 2)")
    assertThrows[IllegalArgumentException](typer.checkExp(ctx)(wrongNumArgs))

    val wrongArgType = parseExp("fun(1, true)")
    assertThrows[IllegalArgumentException](typer.checkExp(ctx)(wrongArgType))

    val undefinedCall = parseExp("other(1, x, 2L, y)")
    assertThrows[IllegalArgumentException](typer.checkExp(ctx)(undefinedCall))

    val funEnv2 = Map("fun" -> PatternFunction(None, "fun", Seq(Param("x", TNode(analyzedLangs.Exp.expTag))), Seq(), Seq()))
    val ctx2 = typer.TypeContext(Map("x" -> TNode(analyzedLangs.Exp.addTag)), funEnv2, null)
    val callWithNodeArgs = parseExp("fun(x)")
    assertResult(typer.checkExp(ctx2)(callWithNodeArgs))(TUnit)

    val callWithNodeArgWrongType = parseExp("fun(1)")
    assertThrows[IllegalArgumentException](typer.checkExp(ctx2)(callWithNodeArgWrongType))

    val funEnv3 = Map("fun" -> PatternFunction(None, "fun", Seq(Param("x", TNode(analyzedLangs.Exp.addTag))), Seq(AnnoParam(None, TNode(analyzedLangs.Exp.expTag)), AnnoParam(None, TNode(analyzedLangs.Exp.expTag))), Seq()))
    val ctx3 = typer.TypeContext(Map("x" -> TNode(analyzedLangs.Exp.intTag), "y" -> TNode(analyzedLangs.Exp.addTag)), funEnv3, null)

    val callWithNodeArgWrongType2 = parseExp("fun(x)")
    assertThrows[IllegalArgumentException](typer.checkExp(ctx3)(callWithNodeArgWrongType2))

    val callMultipleOutputTypes = parseExp("fun(y)")
    assertResult(typer.checkExp(ctx3)(callMultipleOutputTypes))(TTuple(Seq(TNode(analyzedLangs.Exp.expTag), TNode(analyzedLangs.Exp.expTag))))
  }

  "checkExp" should "type count correctly" in {
    val typer = new TypeChecker(Nil)(analyzedLangs.Exp.languageMetaInfo)

    val funEnv = Map("fun" -> PatternFunction(None, "fun", Seq(Param("x", TInt), Param("y", TInt)), Seq(AnnoParam(None, TBool)), Seq()))
    val ctx = typer.TypeContext(Map(), funEnv, null)

    val count = parseExp("count fun(1, 2)")
    assertResult(typer.checkExp(ctx)(count))(TInt)
  }
}
