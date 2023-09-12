package inca.ir

import inca.backend.lowering.GeneratePSystem
import inca.ir.extension.arithmetic.{Add, IR, IntNum}
import inca.ir.typing.{CompilationMessage, Typechecker}

import scala.collection.immutable.Seq
import scala.quoted.*
import scala.quoted.staging

case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

@main
def main() = {
  def atom(i: Int): Atom = Call(s"A_$i", Seq())

  def term(i: Int): Term = Var(s"x_$i")

  def module(language: Language, atoms: Seq[Atom]): Module = Module("MyModule", language, Seq(
      Relation("Test", Seq(Param("a", TAny), Param("b", TAny)), Seq(Body(atoms))),
      Relation("Test2", Seq(Param("p", TAny), Param("q", TAny)), Seq(Body(atoms))),
      //Relation("test2", Seq(Param("a", TInt), Param("b", TInt)), Seq(Body(Seq())))
    ))

  //val t = term(3)
  val mod = module(IR.language, Seq(
    Eq(term(0), IntNum(4)),
    Eq(term(1), IntNum(2)),
    Eq(term(2), Add(term(0), term(1))),
    //Call("test2", Seq(term(1), t))
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

  given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

  val res = staging.run {
    GeneratePSystem.compileModules(Seq(mod)).head
  }

  println()
  println("Staging result:")
  println(res)
}