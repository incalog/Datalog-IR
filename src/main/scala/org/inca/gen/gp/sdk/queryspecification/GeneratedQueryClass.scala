package org.inca.gen.gp.sdk.queryspecification

import org.inca.lang.gp.Content.GraphPattern

import scala.meta._

object GeneratedQueryClass {

  def generateParentClass(pattern: GraphPattern): Stat = {

    val name = Type.Name(pattern.name)
    val nameTerm = Term.Name(pattern.name)
    val superClassParam = Init(
      Type.Name("TFQuerySpecification"),
      Name.Anonymous(),
      List(List(q"$nameTerm.GeneratedPQuery.INSTANCE"))
    )

    q"""
       class $name extends $superClassParam {
         override def instantiate(engine: ViatraQueryEngine): GenericPatternMatcher = {
            var matcher: GenericPatternMatcher = engine.getExistingMatcher(this)
            if (matcher == null) matcher = engine.getMatcher(this)
            matcher
         }
         override def getPreferredScopeClass: Class[_ <: QueryScope] = classOf[TFQueryScope]
       }"""
  }
}
