package org.inca.incer

import org.inca.util.Macros
import org.inca.incer.indices.Indices
import org.inca.meta.MetaElements.NodeType

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.io.NoAbstractFile
import scala.reflect.macros.whitebox.Context

@compileTimeOnly("enable macro paradise to expand macro annotations")
class IncrementalIndex extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro IncrementalIndexMacro.impl
}

object IncrementalIndexMacro {
  def impl(c: Context)(annottees: c.Tree*): c.Tree = {
    if (annottees.size > 1) {
      throw new RuntimeException("Expected a single annotated element!")
    }

    val inputElement = annottees.head
//    println("Input: " + inputElement)
    val outputElement = rewrite(c)(inputElement)
//    println("Output: " + outputElement)
    outputElement
  }


  def rewrite(c: Context)(inputElement: c.Tree): c.Tree = {
    import c.universe._

    val symNodeType = symbolOf[NodeType].companion
    val symIndices = symbolOf[Indices]
    val companionIndices = symIndices.companion
    val symIncrementalizable = symbolOf[Incrementalizable]
    val tyIncrementalizable = typeOf[Incrementalizable]

    val outputElement = inputElement match {
      case q"$modifiers class $className[..$typeParameters] $constructorModifiers(...$constructorParameters) extends { ..$earlyDefinitions } with ..$superTypes { $self => ..$statements }" =>
        val newSuperTypes = superTypes :+ tq"$symIncrementalizable"
        val obj = TermName(className.toString)
        val resultClass =
          q"""
            $modifiers class $className[..$typeParameters] $constructorModifiers(...$constructorParameters) extends { ..$earlyDefinitions } with ..$newSuperTypes { $self =>
              ..$statements

              override def insert(indices: $symIndices): Unit = {
                this.insert(indices, false)
              }

              override def insert(indices: $symIndices, recursive : Boolean): Unit = {
                super.insert(indices, false)
                if (!recursive) {
                  indices.insertNodeTypeInstance($symNodeType(classOf[$className[..$typeParameters]]), this)
                }

                ..${
            Macros.mapParams(c)(constructorParameters, tyIncrementalizable,
              p => q"this.$p.insert(indices)",
              p => q"indices.insertDataTypeInstance(this.$p)",
              p => q"{if (!this.$p.isEmpty) this.$p.get.insert(indices)}",
              p => q"{this.$p.foreach((x: $symIncrementalizable) => x.insert(indices))}"
            )
          }

                ..${
            Macros.mapParams(c)(constructorParameters, tyIncrementalizable,
              p => q"indices.insertNodeLinkInstance(this, $symNodeType(classOf[$className[..$typeParameters]])(${p.toString}), this.$p)",
              p => q"indices.insertNodeLinkInstance(this, $symNodeType(classOf[$className[..$typeParameters]])(${p.toString}), this.$p)",
              p => q"{if (!this.$p.isEmpty) indices.insertNodeLinkInstance(this, $symNodeType(classOf[$className[..$typeParameters]])(${p.toString}), this.$p.get)}",
              p => q"{this.$p.foreach((x: $symIncrementalizable) => indices.insertNodeLinkInstance(this, $symNodeType(classOf[$className[..$typeParameters]])(${p.toString}), x))}"
            )
          }
              }

              override def delete(indices: $symIndices): Unit = {
                this.delete(indices, false)
              }

              override def delete(indices: $symIndices, recursive : Boolean): Unit = {
                super.delete(indices, true)
                if (!recursive) {
                  indices.deleteNodeTypeInstance($symNodeType(classOf[$className[..$typeParameters]]), this)
                }

                ..${
            Macros.mapParams(c)(constructorParameters, tyIncrementalizable,
              p => q"this.$p.delete(indices)",
              p => q"indices.deleteDataTypeInstance(this.$p)",
              p => q"{if (!this.$p.isEmpty) this.$p.get.delete(indices)}",
              p => q"{this.$p.foreach((x: $symIncrementalizable) => x.delete(indices))}"
            )
          }

                ..${
            Macros.mapParams(c)(constructorParameters, tyIncrementalizable,
              p => q"indices.deleteNodeLinkInstance(this, $symNodeType(classOf[$className[..$typeParameters]])(${p.toString}), this.$p)",
              p => q"indices.deleteNodeLinkInstance(this, $symNodeType(classOf[$className[..$typeParameters]])(${p.toString}), this.$p)",
              p => q"{if (!this.$p.isEmpty) indices.deleteNodeLinkInstance(this, $symNodeType(classOf[$className[..$typeParameters]])(${p.toString}), this.$p.get)}",
              p => q"{this.$p.foreach((x: $symIncrementalizable) => indices.deleteNodeLinkInstance(this, $symNodeType(classOf[$className[..$typeParameters]])(${p.toString}), x))}"
            )
          }
              }

              // this forces the registration of the types, but only once!
              $obj.getClass
            }
          """
        val resultClassCompanion =
          q"""
            object $obj {
              ..${
            val filteredSuperTypes = superTypes.filter(t => Macros.treeType(c)(t) <:< tyIncrementalizable)
            if (filteredSuperTypes.isEmpty) {
              Seq(q"$companionIndices.registerType(classOf[$className[..$typeParameters]], null)")
            } else {
              filteredSuperTypes.map(s => q"$companionIndices.registerType(classOf[$className[..$typeParameters]], classOf[$s])")
            }
          }
            }
          """
        q"{$resultClass; $resultClassCompanion}"
      case q"$modifiers trait $traitName[..$typeParameters] extends { ..$earlyDefinitions } with ..$superTypes { $self => ..$statements }" =>
        val newSuperTypes = superTypes :+ tq"$symIncrementalizable"
        val obj = TermName(traitName.toString)
        val resultTrait =
          q"""
            $modifiers trait $traitName[..$typeParameters] extends { ..$earlyDefinitions } with ..$newSuperTypes { $self =>
              ..$statements

              override def insert(indices: $symIndices): Unit = {
                this.insert(indices, false)
              }

              override def insert(indices: $symIndices, recursive : Boolean): Unit = {
                super.insert(indices, false)
                if (!recursive) {
                  indices.insertNodeTypeInstance($symNodeType(classOf[$traitName[..$typeParameters]]), this)
                }
              }

              override def delete(indices: $symIndices): Unit = {
                this.delete(indices, false)
              }

              override def delete(indices: $symIndices, recursive : Boolean): Unit = {
                super.delete(indices, true)
                if (!recursive) {
                  indices.deleteNodeTypeInstance($symNodeType(classOf[$traitName[..$typeParameters]]), this)
                }
              }

              // this forces the registration of the types, but only once!
              $obj.getClass
            }
          """
        val resultTraitCompanion =
          q"""
            object $obj {
              ..${
            val filteredSuperTypes = superTypes.filter(t => Macros.treeType(c)(t) <:< tyIncrementalizable)
            if (filteredSuperTypes.isEmpty) {
              Seq(q"$companionIndices.registerType(classOf[$traitName[..$typeParameters]], null)")
            } else {
              filteredSuperTypes.map(s => q"$companionIndices.registerType(classOf[$traitName[..$typeParameters]], classOf[$s])")
            }
          }
            }
          """
        q"{$resultTrait; $resultTraitCompanion}"
      case q"$modifiers object $objectName extends { ..$earlyDefinitions } with ..$superTypes { $self => ..$statements }" =>
        val newStatements = statements.map(t => rewrite(c)(t))
        val resultObject =
          q"""
            $modifiers object $objectName extends { ..$earlyDefinitions } with ..$superTypes { $self =>
              ..$newStatements
            }
          """
        resultObject
      case _ => inputElement
    }

    outputElement
  }
}


