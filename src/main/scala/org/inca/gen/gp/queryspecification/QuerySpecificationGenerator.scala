package org.inca.gen.gp.queryspecification

import GeneratedQueryObject.{createGraphPatternBodies, pparams}
import org.inca.lang.gp.Content.GraphPattern

import scala.meta._

object QuerySpecificationGenerator {

  def generateQuerySpecification(pattern: GraphPattern): Source = {

    val fileNameType = Type.Name(pattern.name)
    val fileNameTerm = Term.Name(pattern.name)
    val pFullyQualifiedName = Lit.String(pattern.name)

    val superClassParam = Init(
      Type.Name("TFQuerySpecification"),
      Name.Anonymous(),
      List(List(q"$fileNameTerm.GeneratedPQuery.INSTANCE"))
    )

    val pParamPNames = pattern.parameters.map { p => Term.Name(s"p_${p.name}")}.toList
    val pParamNamesString = pattern.parameters.map { p => Lit.String(p.name)}.toList

    source"""
            import org.eclipse.viatra.query.runtime.api.{GenericPatternMatcher, ViatraQueryEngine}
            import org.eclipse.viatra.query.runtime.api.scope.QueryScope
            import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables.TypeConstraint
            import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
            import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}
            import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
            import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.ExportedParameter

            import java.util

            import org.inca.gen.gp.model.keys.{ClassKey, LinkKey}
            import org.inca.gen.gp.sdk.queryspecification.PrimitiveConstants
            import org.inca.incer.indices.{TFInputKey, TFQueryScope, TFQuerySpecification}
            import org.inca.meta.MetaElements
            import org.inca.meta.MetaElements.NodeType

            class $fileNameType extends $superClassParam {
               override def instantiate(engine: ViatraQueryEngine): GenericPatternMatcher = {
                  var matcher: GenericPatternMatcher = engine.getExistingMatcher(this)
                  if (matcher == null) matcher = engine.getMatcher(this)
                  matcher
               }
               override def getPreferredScopeClass: Class[_ <: QueryScope] = classOf[TFQueryScope]
            }

            object $fileNameTerm {
              def instance(): $fileNameType = LazyHolder.INSTANCE

              private final class LazyHolder
              private final object LazyHolder {
                val INSTANCE: $fileNameType = make()
                def make(): $fileNameType = new $fileNameType()
              }


              final class GeneratedPQuery extends BasePQuery(PVisibility.PUBLIC) {
                  private val that = this
                  ..${pparams(pattern.parameters)}
                  {}
                  override protected def doGetContainedBodies(): util.Set[PBody] = {
                    val bodies: util.Set[PBody] = util.Set.of(
                      ..${createGraphPatternBodies(pattern)}
                    )
                    bodies
                  }

                  override def getFullyQualifiedName: String = $pFullyQualifiedName
                  override def getParameters: util.List[PParameter] = util.List.of(..$pParamPNames)
                  override def getParameterNames: util.List[String] = util.List.of(..$pParamNamesString)
              }

              final object GeneratedPQuery {
                val INSTANCE = new GeneratedPQuery
              }
            }
          """
  }
}
