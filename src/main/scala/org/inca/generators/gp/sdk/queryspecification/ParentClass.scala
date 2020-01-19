package org.inca.generators.gp.sdk.queryspecification

import org.inca.lang.gp.Content.GraphPattern

import scala.meta._

object ParentClass {

  def generateParentClass(pattern: GraphPattern, collectionName: String): Stat = {

    val className = Type.Name(s"${pattern.name}_${collectionName}QuerySpecification")
    val classTermName = Term.Name(s"${pattern.name}_${collectionName}QuerySpecification")


    val superClassParam = Init(
      Type.Select(classTermName, Type.Name("GeneratedPQuery")),
      Name.Anonymous(),
      List()
    )

    q"""
       class $className extends ScalaQuerySpecification(new $superClassParam) {
         override def instantiate(viatraQueryEngine: ViatraQueryEngine): ScalaPatternMatcher = ???
         override def getPreferredScopeClass: Class[_ <: QueryScope] = ???
       }
       """
  }
}
