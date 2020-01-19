package org.inca.generators.gp.sdk.queryspecification

import org.inca.generators.gp.sdk.queryspecification.QuerySpecificationGenerator._
import org.inca.lang.gp.Content.GraphPattern

import scala.meta._

object ParentClass {

  def generateParentClass(pattern: GraphPattern, collectionName: String): Stat = {

    val name = classTypeName(pattern, collectionName)
    val nameTerm = classTermName(pattern, collectionName)

    q"""
       class $name extends ScalaQuerySpecification(new ${superClassParam(nameTerm)}) {
         override def instantiate(viatraQueryEngine: ViatraQueryEngine): ScalaPatternMatcher = ???
         override def getPreferredScopeClass: Class[_ <: QueryScope] = ???
       }
       """
  }

  private def superClassParam(classTermName: Term.Name) = Init(
    Type.Select(classTermName, Type.Name("GeneratedPQuery")),
    Name.Anonymous(),
    List()
  )
}
