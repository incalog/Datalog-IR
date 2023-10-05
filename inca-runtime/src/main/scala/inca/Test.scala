package inca

import inca.backend.lowering.{GeneratePSystem, PSystem}
import inca.ir.{Atom, Body, Call, Eq, Language, Module, Param, Relation, TAny, Term, Var, string2name}
import inca.ir.extension.arithmetic.{Add, IR, IntNum, ScalaLowering, TInt}
import inca.runtime.context.{DataModel, QueryScope}
import inca.util.{CompilationMessage, ScalaCompiler}
import inca.ir.extension.primitiveScala
import inca.ir.extension.block
import inca.ir.typing

import scala.collection.immutable.Seq
import inca.runtime.EnginePool
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

@main
def main() = {

  def atom(i: Int): Atom = Call(s"A_$i", Seq())

  def term(i: Int): Term = Var(s"x_$i")

  def module(language: Language, atoms: Seq[Atom]): Module = Module("MyModule", language, Seq(
    Relation("Test", Seq(Param("a", TInt), Param("b", TInt)), Seq(Body(atoms))),
    //Relation("Test2", Seq(Param("p", TAny), Param("q", TAny)), Seq(Body(atoms))),
  ))

  //val t = term(3)
  val mod = module(IR.language, Seq(
    //Eq(term(0), IntNum(4)),
    Eq(IntNum(1), Var("a")),
    //Eq(IntNum(3), Var("a")),
    Eq(Var("b"), Add(Var("a"), IntNum(2))),
    //Call("Test2", Seq(term(1), term(2)))
  ))

  trait Lowering extends block.Lowering with primitiveScala.Visitor
  trait Typechecker extends typing.IRTypechecker with primitiveScala.Typechecker

  println(mod)

  val typechecker = new Typechecker {}
  typechecker.typecheck(mod)

  // This lowering introduces blocks
  var loweredMod = new ScalaLowering {}.lower(mod)
  typechecker.typecheck(loweredMod)

  // Get rid of the blocks
  loweredMod = new Lowering {}.lower(loweredMod)
  typechecker.typecheck(loweredMod)



  // Get required type information



  println(loweredMod)

  var code = GeneratePSystem.compileModules(Seq(loweredMod))
  code = s"$code; MyModule"
  //println(code)

  val compiler = new ScalaCompiler()
  val psystemModule: PSystem.Module = compiler.compileAndLoadScala(code)
  val mainSpec = psystemModule.patterns("Test")()

  val scope = new QueryScope(new DataModel())
  val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
  val mainMatcher = engine.getMatcher(mainSpec)
  val res = mainMatcher.getAllMatches

  println()
  //println(s"PSystemModule: $psystemModule")
  //println(s"PSystemModule: ${psystemModule.patterns}")
  //println(s"PSystemModule: ${mainSpec}")
  println(s"Result: $res")
}
