package org.inca.incer

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

@compileTimeOnly("enable macro paradise to expand macro annotations")
class IncrementalIndex extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro IncrementalIndexMacro.impl
}

object IncrementalIndexMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    c.Expr[Any](q"{..$annottees}")
  }
}


