package org.inca.generators.gp

import org.inca.lang.gp.Content.GraphPattern
import org.inca.generators.gp.util.sdkStuff._

import scala.meta._


class GPGenerator {
  val filename = "test.scala"

  /**
   * Generates a ast with quasiquotes of scalameta from a IncA graph pattern
   * @param pattern        GraphPattern which will be transformed into scalameta tree
   * @param collectionName Name of the file (mps) where pattern is saved
   * @return
   */
  def generate(pattern: GraphPattern, collectionName: String): Defn.Class = {


    val className = Type.Name(s"${pattern.name}_${collectionName}QuerySpecification")

    val containedBodies = createDoGetContainedBodies(pattern)
    val doGetContainedBodiesMethod = q"override def doGetContainedBodies(): Set[PBody] = $containedBodies"
    val generatedPQuery =
      getGeneratedPQuery(pattern.parameters).toList ++
        List(doGetContainedBodiesMethod) ++
        createOverrideFuns(pattern, collectionName)
    val generatedPQueryClass = q"class GeneratedPQuery extends AbstractPQuery { ..$generatedPQuery }"
//    val imports = createImportStatements()

    val rootClass = q"class $className extends ScalaQuerySpecification { $generatedPQueryClass }"


    // todo remove, just simple test
    println(rootClass)
    rootClass
  }
}