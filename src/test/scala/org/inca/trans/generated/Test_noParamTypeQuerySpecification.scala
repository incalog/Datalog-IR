package org.inca.generator.generated
import org.eclipse.viatra.query.runtime.api.{ GenericPatternMatcher, ViatraQueryEngine }
import org.eclipse.viatra.query.runtime.api.scope.QueryScope
import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables.{ PositivePatternCall, BinaryTransitiveClosure, TypeConstraint }
import org.eclipse.viatra.query.runtime.matchers.psystem.{ PBody, PVariable }
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{ BasePQuery, PParameter, PVisibility }
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred._
import org.eclipse.viatra.query.runtime.matchers.context.common.JavaTransitiveInstancesKey
import java.util
import org.inca.incer.indices.{ TFInputKey, TFQueryScope, TFQuerySpecification }
import org.inca.meta.MetaElements
class Test_noParamTypeQuerySpecification extends TFQuerySpecification(Test_noParamTypeQuerySpecification.GeneratedPQuery.INSTANCE) {
  override def instantiate(engine: ViatraQueryEngine): GenericPatternMatcher = {
    var matcher: GenericPatternMatcher = engine.getExistingMatcher(this)
    if (matcher == null) matcher = engine.getMatcher(this)
    matcher
  }
  override def getPreferredScopeClass: Class[_ <: QueryScope] = classOf[TFQueryScope]
}
object Test_noParamTypeQuerySpecification {
  def instance(): Test_noParamTypeQuerySpecification = LazyHolder.INSTANCE
  private final object LazyHolder {
    val INSTANCE: Test_noParamTypeQuerySpecification = make()
    def make(): Test_noParamTypeQuerySpecification = new Test_noParamTypeQuerySpecification()
  }
  private final object GeneratedPQuery extends BasePQuery(PVisibility.PUBLIC) {
    val INSTANCE: GeneratedPQuery.type = this
    private val param_add: PParameter = new PParameter("add")
    {}
    override protected def doGetContainedBodies(): util.Set[PBody] = {
      val bodies: util.Set[PBody] = util.Set.of {
        val body: PBody = new PBody(this)
        val var_add: PVariable = body.getOrCreateVariableByName("add")
        ()
        val exportedParams = new util.ArrayList[ExportedParameter]()
        exportedParams.add(new ExportedParameter(body, var_add, param_add))
        body.setSymbolicParameters(exportedParams)
        new TypeConstraint(body, Tuples.flatTupleOf(var_add), new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[org.inca.analyzedLangs.expLang.Add])))
        body
      }
      bodies
    }
    override def getFullyQualifiedName: String = "Test_noParamTypeQuerySpecification"
    override def getParameters: util.List[PParameter] = util.List.of(param_add)
    override def getParameterNames: util.List[String] = util.List.of("add")
  }
}