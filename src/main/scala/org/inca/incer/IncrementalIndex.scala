package org.inca.incer

import org.inca.diff.macros.Util
import org.inca.incer.indices.Indices
import org.inca.meta.MetaElements.NodeType

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

    val symNodeType = symbolOf[NodeType].companion
    val symIndices = symbolOf[Indices]
    val companionIndices = symIndices.companion
    val symIncrementalizable = symbolOf[Incrementalizable]
    val tyIncrementalizable = typeOf[Incrementalizable]

    val inputElement = annottees.head
    println("Input: " + inputElement)

    val outputElement = inputElement match {
      case q"$modifiers class $className[..$typeParameters] $constructorModifiers(...$constructorParameters) extends { ..$earlyDefinitions } with ..$superTypes { $self => ..$statements }" =>
        val newSuperTypes = superTypes :+ tq"$symIncrementalizable"
        val obj = TermName(className.toString)
        val resultClass =
          q"""
            $modifiers class $className[..$typeParameters] $constructorModifiers(...$constructorParameters) extends { ..$earlyDefinitions } with ..$newSuperTypes { $self =>
              ..$statements

              override def insert(indices: $symIndices): Unit = {
                super.insert(indices)

                indices.insertNodeTypeInstance($symNodeType(classOf[$className[..$typeParameters]]), this)

                ..${Util.mapParams(c)(constructorParameters, tyIncrementalizable,
                  p => q"this.$p.insert(indices)",
                  p => q"indices.insertDataTypeInstance(this.$p)",
                  p => q"{if (!this.$p.isEmpty) this.$p.get.insert(indices)}",
                  p => q"{this.$p.foreach((x: $symIncrementalizable) => x.insert(indices))}"
                )}

                ..${Util.mapParams(c)(constructorParameters, tyIncrementalizable,
                  p => q"indices.insertNodeLinkInstance(this, $symNodeType(classOf[$className[..$typeParameters]])(${p.toString}), this.$p)",
                  p => q"indices.insertNodeLinkInstance(this, $symNodeType(classOf[$className[..$typeParameters]])(${p.toString}), this.$p)",
                  p => q"{if (!this.$p.isEmpty) indices.insertNodeLinkInstance(this, $symNodeType(classOf[$className[..$typeParameters]])(${p.toString}), this.$p.get)}",
                  p => q"{this.$p.foreach((x: $symIncrementalizable) => indices.insertNodeLinkInstance(this, $symNodeType(classOf[$className[..$typeParameters]])(${p.toString}), x))}"
                )}
              }

              override def delete(indices: $symIndices): Unit = {
                super.delete(indices)

                indices.deleteNodeTypeInstance($symNodeType(classOf[$className[..$typeParameters]]), this)

                ..${Util.mapParams(c)(constructorParameters, tyIncrementalizable,
                  p => q"this.$p.delete(indices)",
                  p => q"indices.deleteDataTypeInstance(this.$p)",
                  p => q"{if (!this.$p.isEmpty) this.$p.get.delete(indices)}",
                  p => q"{this.$p.foreach((x: $symIncrementalizable) => x.delete(indices))}"
                )}

                ..${Util.mapParams(c)(constructorParameters, tyIncrementalizable,
                  p => q"indices.deleteNodeLinkInstance(this, $symNodeType(classOf[$className[..$typeParameters]])(${p.toString}), this.$p)",
                  p => q"indices.deleteNodeLinkInstance(this, $symNodeType(classOf[$className[..$typeParameters]])(${p.toString}), this.$p)",
                  p => q"{if (!this.$p.isEmpty) indices.deleteNodeLinkInstance(this, $symNodeType(classOf[$className[..$typeParameters]])(${p.toString}), this.$p.get)}",
                  p => q"{this.$p.foreach((x: $symIncrementalizable) => indices.deleteNodeLinkInstance(this, $symNodeType(classOf[$className[..$typeParameters]])(${p.toString}), x))}"
                )}
              }

              // this forces the registration of the types, but only once!
              $obj.getClass
            }
          """
        val resultClassCompanion =
          q"""
            object $obj {
              ..${
                val filteredSuperTypes = superTypes.filter(t => Util.treeType(c)(t) <:< tyIncrementalizable)
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
                super.insert(indices)
                indices.insertNodeTypeInstance($symNodeType(classOf[$traitName[..$typeParameters]]), this)
              }

              override def delete(indices: $symIndices): Unit = {
                super.delete(indices)
                indices.deleteNodeTypeInstance($symNodeType(classOf[$traitName[..$typeParameters]]), this)
              }

              // this forces the registration of the types, but only once!
              $obj.getClass
            }
          """
        val resultTraitCompanion =
          q"""
            object $obj {
              ..${
                val filteredSuperTypes = superTypes.filter(t => Util.treeType(c)(t) <:< tyIncrementalizable)
                if (filteredSuperTypes.isEmpty) {
                  Seq(q"$companionIndices.registerType(classOf[$traitName[..$typeParameters]], null)")
                } else {
                  filteredSuperTypes.map(s => q"$companionIndices.registerType(classOf[$traitName[..$typeParameters]], classOf[$s])")
                }
              }
            }
          """
        q"{$resultTrait; $resultTraitCompanion}"
      case _ => inputElement
    }

    println("Output: " + outputElement)
    outputElement
  }
}


