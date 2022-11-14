package inca.embedded2
import inca.embedded2.Operations._

object Test extends App {
  val x = 1
  println(x)
  import inca.embedded2.CompilerCombinationImplicit._
  import inca.embedded2.FunCompilerImplicit._
  import inca.embedded2.IRCompilerImplicit._
  import inca.embedded2.InterpreterImplicit.ScalaInterpreter

  val funLang: Fun[Scala] = null.asInstanceOf[Fun[Scala]]
  val prog = null.asInstanceOf[funLang.Program]
  val entry = null.asInstanceOf[funLang.EntryPoint]
  val input = null.asInstanceOf[funLang.Input]
//  val funComp = null.asInstanceOf[Compiler[Scala, Fun[Scala], Scala, ASTIR[Scala]]]
//  val irComp = null.asInstanceOf[Compiler[Scala, ASTIR[Scala], Scala, PSystem[Scala]]]
  val db =
    solve[Scala, Fun[Scala]](prog, entry, input) // (compose(funComp, irComp), ScalaInterpreter)
  // val compiler: Fun[Scala]#Program = null.asInstanceOf[Fun[Scala]#Program]
}
