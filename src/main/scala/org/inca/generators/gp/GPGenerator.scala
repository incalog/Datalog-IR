package org.inca.generators.gp

import org.inca.lang.gp.Content.GraphPattern
import org.inca.generators.gp.util.sdkStuff._

import scala.meta._


class GPGenerator {
  val filename = "test.scala"

  /**
   * Generates a ast with quasiquotes of scalameta from a IncA graph pattern
   *
   * @param pattern        GraphPattern which will be transformed into scalameta tree
   * @param collectionName Name of the file (mps) where pattern is saved
   * @return
   */
  def generate(pattern: GraphPattern, collectionName: String): Source = {


    val className = Type.Name(s"${pattern.name}_${collectionName}QuerySpecification")
    val classTermName = Term.Name(s"${pattern.name}_${collectionName}QuerySpecification")

    val containedBodies = createDoGetContainedBodies(pattern)
    val doGetContainedBodiesMethod = q"override def doGetContainedBodies(): Set[PBody] = $containedBodies"
    val generatedPQuery =
      getGeneratedPQuery(pattern.parameters).toList ++
        List(doGetContainedBodiesMethod) ++
        createOverrideFuns(pattern, collectionName)
    val generatedPQueryClass = q"class GeneratedPQuery extends AbstractPQuery { ..$generatedPQuery }"

    val genericQuerySpecificationFunctions =
      List(
        q"override def instantiate(viatraQueryEngine: ViatraQueryEngine): ScalaPatternMatcher",
        q"override def getPreferredScopeClass: Class[_ <: QueryScope]")

    val superClassParam = Template(
      List(),
      List(
        Init(
          Type.Name("ScalaQuerySpecification"),
          Name.Anonymous(),
          List(
            List(
              Term.New(
                Init(
                  Type.Select(classTermName, Type.Name("GeneratedPQuery")),
                  Name.Anonymous(),
                  List())
              )
            )
          )
        )
      ),
      Self(Name.Anonymous(), None),
      genericQuerySpecificationFunctions)

    val stats = createImportStatements() ++
      List(q"class $className extends $superClassParam") ++
      List(q"object $classTermName { $generatedPQueryClass }")

    val source = source"..$stats"

    // todo remove, just simple test
    println(source)
    source
  }
}