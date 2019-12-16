package org.inca.generators.gp

import org.inca.gp.Content.GraphPattern
import scala.meta._

class GPGenerator {
  val filename = "test.scala"

  def generate(pattern: GraphPattern, collectionName: String): Unit = {
    val fullyQualifiedName = s"""$collectionName.${pattern.name}"""
    val className = s"""${pattern.name}_${collectionName}QuerySpecification"""
    var program =
      s"""
      class ${className} extends ScalaQuerySpecification {

        class GeneratedPQuery extends AbstractPQuery {
      """

    for (param <- pattern.parameters) {
      program += // todo FTR append ConceptKey()
        s"""
          val p_${param.name}: PParameter = PParameter("${param.name}", "${param.typ.get.toString.substring(1)}", ConceptKey())"""
    }

    program +=
      s"""

          val INSTANCE: ${className}.GeneratedPQuery = ${className}.GeneratedPQuery()"""

    program +=
      s"""

          def doGetContainedBodies(): Set[PBody] = {"""

    for (body <- pattern.bodies) {
      program +=
        s"""
            {
              var body = PBody(this) """

      for (param <- pattern.parameters) {
        program +=
          s"""
              val var_${param.name}: PVariable = body.getOrCreateVariableByName("${param.name}")"""
      }
      program +=
        s"""
            }"""
    }

    program +=
      s"""
          }
        }

        def getFullyQualifiedName(): String = "$fullyQualifiedName"
      }"""
    val tree = program.parse[Source].get

    println(tree)
//    println(tree.structure)
  }

  def printFl(text: String): Unit = {

  }
}
