package inca

//import dotty.tools.dotc.ast.untpd.{Ident, Literal, Select}
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.semanticdb.Descriptor.Term
//import dotty.tools.dotc.core.Decorators.toTermName
//import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.util.SourceFile

import scala.quoted.*

// make available the necessary compiler for runtime code generation
given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

//case class Constant[T](value: T)

def foo(): String = "foo"

object Inspect {

  def test(): Any = {

    // Not possible to create function as expr...
    // https://stackoverflow.com/questions/67870834/scala-3-create-expr-int-int

    val x: Int = 4
    //val closure = '{ (x: Int) => x + 1 }
    //val c = Constant(x)
    val fun = "toString"

    //val toStringAST = Select(Ident("y".toTermName), "toString".toTermName)
    //toStringAST.asExpr[Fun]
    //val src = new SourceFile(null, null)

    /*val apl: (Int, String) => String = staging.run {
      import quotes.reflect.*
      val stringAST = Literal(StringConstant("Hello"))

      val addSuffix: Expr[(Int, String) => String] = '{
        (y: Int, suffix: String) => $ { stringAST.asExprOf[String] } + "_" + suffix
      }
      //val tree: Tree = addSuffix.asTerm
      //val res = tree.show(using Printer.TreeStructure)
      addSuffix
    }
    apl.apply(x, "test")*/

    // Call function
    /*val apl: (Int, String) => Any = staging.run {
      import quotes.reflect.*

      val addSuffix: Expr[(Int, String) => Any] = '{
        (y: Int, suffix: String) => $ {
          //Apply(Select('{y}.asTerm, Symbol.requiredMethod(s"foo")), List()).asExprOf[String]
          Apply(Ref(Symbol.requiredMethod(s"inca.foo")), List()).asExprOf[String]
        }
      }
      //val tree: Tree = addSuffix.asTerm
      //val res = tree.show(using Printer.TreeStructure)
      addSuffix
    }*/

    // Call method
    val apl: (Int, String) => Any = staging.run {
      import quotes.reflect.*

      val addSuffix: Expr[(Int, String) => Any] = '{
        (y: Int, suffix: String) =>
          $ {
            Apply(Select.unique('{y}.asTerm, "toString"), List()).asExprOf[String]
          } + "_Hello_there"
      }
      addSuffix
    }

    apl.apply(3, "blub")
  }
}
