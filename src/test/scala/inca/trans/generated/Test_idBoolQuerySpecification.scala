package inca.trans.generated

import java.util

import inca.MetaElements
import inca.backend.indices.{TFInputKey, TFQueryScope, TFQuerySpecification}
import org.eclipse.viatra.query.runtime.api.scope.QueryScope
import org.eclipse.viatra.query.runtime.api.{GenericPatternMatcher, ViatraQueryEngine}
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred._
import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables.TypeConstraint
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}
import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
class Test_idBoolQuerySpecification extends TFQuerySpecification(Test_idBoolQuerySpecification.GeneratedPQuery.INSTANCE) {
  override def instantiate(engine: ViatraQueryEngine): GenericPatternMatcher = {
    var matcher: GenericPatternMatcher = engine.getExistingMatcher(this)
    if (matcher == null) matcher = engine.getMatcher(this)
    matcher
  }
  override def getPreferredScopeClass: Class[_ <: QueryScope] = classOf[TFQueryScope]
}
object Test_idBoolQuerySpecification {
  def instance(): Test_idBoolQuerySpecification = LazyHolder.INSTANCE
  private final object LazyHolder {
    val INSTANCE: Test_idBoolQuerySpecification = make()
    def make(): Test_idBoolQuerySpecification = new Test_idBoolQuerySpecification()
  }
  private final object GeneratedPQuery extends BasePQuery(PVisibility.PUBLIC) {
    val INSTANCE: GeneratedPQuery.type = this
    private val param_in: PParameter = new PParameter("in", "java.lang.Boolean", new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[java.lang.Boolean])))
    private val param_out: PParameter = new PParameter("out", "java.lang.Boolean", new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[java.lang.Boolean])))
    {}
    override protected def doGetContainedBodies(): util.Set[PBody] = {
      val bodies: util.Set[PBody] = util.Set.of {
        val body: PBody = new PBody(this)
        val var_in: PVariable = body.getOrCreateVariableByName("in")
        val var_out: PVariable = body.getOrCreateVariableByName("out")
        ()
        val exportedParams = new util.ArrayList[ExportedParameter]()
        exportedParams.add(new ExportedParameter(body, var_in, param_in))
        exportedParams.add(new ExportedParameter(body, var_out, param_out))
        body.setSymbolicParameters(exportedParams)
        new TypeConstraint(body, Tuples.flatTupleOf(var_in), new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[java.lang.Boolean])))
        new TypeConstraint(body, Tuples.flatTupleOf(var_out), new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[java.lang.Boolean])))
        new Equality(body, var_in, var_out)
        body
      }
      bodies
    }
    override def getFullyQualifiedName: String = "Test_idBoolQuerySpecification"
    override def getParameters: util.List[PParameter] = util.List.of(param_in, param_out)
    override def getParameterNames: util.List[String] = util.List.of("in", "out")
  }
}