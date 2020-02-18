package org.inca.gen.gp

import org.inca.gen.gp.helper.GenTypeConstraints._
import org.inca.gen.gp.helper.GenVariables._
import org.inca.lang.gp.Content.GraphPattern

import scala.meta._

object GeneratorGP {

  def querySpecification(pattern: GraphPattern): Source = {

    val fileNameType = Type.Name(pattern.name)
    val fileNameTerm = Term.Name(pattern.name)
    val fileNameLit = Lit.String(pattern.name)

    val superClassParam = Init(
      Type.Name("TFQuerySpecification"),
      Name.Anonymous(),
      List(List(q"$fileNameTerm.GeneratedPQuery.INSTANCE")))

    val paramTermName = pattern.parameters.toList map { p => Term.Name(s"p_${p.name}")}
    val paramLitName = pattern.parameters.toList map {p => Lit.String(p.name)}

    source"""
            package org.inca.generator.generated

            import org.eclipse.viatra.query.runtime.api.{GenericPatternMatcher, ViatraQueryEngine}
            import org.eclipse.viatra.query.runtime.api.scope.QueryScope
            import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables.TypeConstraint
            import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
            import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}
            import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
            import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.ExportedParameter

            import java.util

            import org.inca.gen.gp.model._
            import org.inca.gen.gp.model.keys.{ClassKey, LinkKey}
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
                      ..${pattern.bodies.toList map { body =>
                            q"""{
                                val body: PBody = new PBody(that)
                                ..${localGlobalVariables(pattern.parameters)}
                                ()
                                val exportedParams = new util.ArrayList[ExportedParameter]()
                                ..${pattern.parameters.toList map { gp =>
                                  q"""exportedParams.add(new ExportedParameter(body,
                                    ${Term.Name(s"var_${gp.name}")},
                                    ${Term.Name(s"p_${gp.name}")}))"""
                                }}
                                body.setSymbolicParameters(exportedParams)

                                ..${(temporaryVariables _ andThen createTemporaryVariables)(body.contents)}
                                ..${(generatedTemporaryVariables _ andThen contextPointers)(body.contents)}
                                ..${(uniquePrimitives _ andThen primitivesToParams)(body.contents)}
                                ..${typeConstraintsParameters(pattern.parameters)}
                                ..${typeConstraints(body.contents)}
                                body
                              }"""
                          }}
                    )
                    bodies
                  }

                  override def getFullyQualifiedName: String = $fileNameLit
                  override def getParameters: util.List[PParameter] = util.List.of(..$paramTermName)
                  override def getParameterNames: util.List[String] = util.List.of(..$paramLitName)
              }

              final object GeneratedPQuery {
                val INSTANCE = new GeneratedPQuery
              }
            }"""
  }
}
