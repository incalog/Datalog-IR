package inca.ir.extension.arithmetic

//import inca.Scala
import inca.ir.*
import inca.ir.extension.*
//import inca.ir.extension.primitiveScala.{Application, Constant, TScala, IR as ScalaIR}
import inca.ir.typing.Typechecker
import inca.util.CompilationMessage
import org.scalatest.funsuite.AnyFunSuiteLike


//class ArithmeticLoweringTest extends AnyFunSuiteLike:
//  case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))
//
//  trait Stage1IR extends BaseIR with ScalaIR with block.IR:
//    override val name: String = "ScalaBlock"
//    override def language: Language = super.language
//    override def requires: Language = Language(IR)
//  object Stage1IR extends Stage1IR {}
//
//  val stage1lowering = ScalaLowering
//  val stage2lowering = new block.Lowering {}
//
//  val typechecker = new Typechecker {}
//
//  def atom(i: Int): Atom = Call(s"A_$i", Seq())
//  def term(i: Int): Term = Var(s"x_$i")
//  def module(language: Language, atoms: Seq[Atom]): Module =
//    val mod = Module("Test", language, Seq(
//      Relation("test", Seq(), Seq(Body(atoms))),
//      //Relation("test2", Seq(Param("a", TInt), Param("b", TInt)), Seq(Body(Seq())))
//    ))
//    typechecker.typecheck(mod)
//    println(mod)
//    typechecker.failOnError()
//    mod
//
//  test("simple 1") {
//    //val t = term(3)
//    val mAdd = module(IR.language, Seq(
//      Eq(term(0), IntNum(4)),
//      Eq(term(1), IntNum(2)),
//      Eq(term(2), Add(term(0), term(1))),
//      //Call("test2", Seq(term(1), t))
//    ))
//    val lowered = stage2lowering.lower(stage1lowering.lower(mAdd))
//
//    val intTy = Scala.TypeName("Int")
//    val lam = Scala.Lam(
//      Seq("lhs" -> Some(intTy), "rhs" -> Some(intTy)),
//      Scala.AppInfix(Scala.Id("lhs"), "+", Scala.Id("rhs"))
//    )
//    val tmpVar = Var("Arithmetic$0")
//
//    val bAdd = module(ScalaIR.language, Seq(
//      Eq(term(0), Constant(Scala.IntLiteral(4), TScala.int)),
//      Eq(term(1), Constant(Scala.IntLiteral(2), TScala.int)),
//      Application(tmpVar, TScala.int, lam, Seq(term(0), term(1))),
//      Eq(term(2), tmpVar)
//    ))
//    println(mAdd)
//    println()
//    println(lowered)
//    println()
//    println(bAdd)
//
//    assertResult(bAdd)(lowered)
//  }

