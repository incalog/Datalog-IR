package inca.ir

import inca.backend.lowering.{GeneratePSystem, PSystem}
import inca.ir.extension.arithmetic.{Add, IR, IntNum, ScalaLowering}
import inca.ir.typing.{CompilationMessage, Typechecker}
import inca.runtime.context.{DataModel, QueryScope}
import inca.util.ScalaCompiler

import scala.collection.immutable.Seq
import scala.quoted.*
import scala.quoted.staging
import inca.runtime
import inca.runtime.EnginePool
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

@main
def main() = {

  def atom(i: Int): Atom = Call(s"A_$i", Seq())

  def term(i: Int): Term = Var(s"x_$i")

  def module(language: Language, atoms: Seq[Atom]): Module = Module("MyModule", language, Seq(
      Relation("Test", Seq(Param("a", TAny), Param("b", TAny)), Seq(Body(atoms), Body(atoms))),
      //Relation("Test2", Seq(Param("p", TAny), Param("q", TAny)), Seq(Body(atoms))),
    ))

  //val t = term(3)
  val mod = module(IR.language, Seq(
    //Eq(term(0), IntNum(4)),
    Eq(IntNum(1), Var("b")),
    Eq(term(2), Var("a")),
    //Call("Test2", Seq(term(1), term(2)))
  ))


    //import quotes.reflect.*

  /*val relCode: Quotes ?=> Expr[Any] = '{
    var relations = Map[String, App]()
    relations += ${Expr(relName)} -> new App {
      println("Hello world")
    }
  }

  val relBodyCode: Quotes ?=> Expr[Any] = '{
    new App {
      println("foo")
      ${ reflect.Ref(relName) }
    }
  }
  val relation = reflect.ValDef(relName, relBodyCode)*/
  /*
  val R = ... call Q
  val Q = ... call P

   */

  /*given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

  val res = staging.run {
    GeneratePSystem_Meta.compileModules(Seq(mod)).head
  }*/

  val loweredMod = ScalaLowering.lower(mod)

  var code = GeneratePSystem.compileModules(Seq(loweredMod))
  code = s"$code; MyModule"
  //println(code)

  // TODO: This should work, but currently the module is doing nothing useful
  // TODO: Support arithmetics
  val compiler = new ScalaCompiler()
  val psystemModule: PSystem.Module = compiler.compileAndLoadScala(code)
  val mainSpec = psystemModule.patterns("Test")()

  val scope = new QueryScope(new DataModel())
  val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
  val mainMatcher = engine.getMatcher(mainSpec)
  val res = mainMatcher.getAllMatches

  println()
  println(s"PSystemModule: $psystemModule")
  println(s"PSystemModule: ${psystemModule.patterns}")
  println(s"Result: $res")

}