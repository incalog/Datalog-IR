package org.inca.incer

import org.inca.incer.indices.Indices

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

@compileTimeOnly("enable macro paradise to expand macro annotations")
class IncrementalIndex extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro IncrementalIndexMacro.impl
}

object IncrementalIndexMacro {
  def impl(c: Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    if (annottees.size > 1) {
      throw new RuntimeException("Expected a single annotated element!")
    }

    val symIndices = symbolOf[Indices]
    val symIncrementalizable = symbolOf[Incrementalizable]

    val inputElement = annottees.head
    println("Input: " + inputElement)

    val outputElement = inputElement match {
      case q"$modifiers class $className[..$typeParameters] $constructorModifiers(...$constructorParameters) extends { ..$earlyDefinitions } with ..$superTypes { $self => ..$statements }" =>
        val newSuperTypes = superTypes :+ tq"$symIncrementalizable"
        val res =
          q"""
            $modifiers class $className[..$typeParameters] $constructorModifiers(...$constructorParameters) extends { ..$earlyDefinitions } with ..$newSuperTypes { $self =>
              ..$statements

              override def insert(indices: $symIndices): Unit = {
                // NodeType instance
                indices.insertNodeTypeInstance(NodeType(this.getClass), this)
              }

              override def delete(indices: $symIndices): Unit = {
                // NodeType instance
                indices.deleteNodeTypeInstance(NodeType(this.getClass), this)
              }
            }"""
        res
      case _ => inputElement
    }

    println("Output: " + outputElement)
    outputElement
  }
}


