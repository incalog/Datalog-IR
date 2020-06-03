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
class Test_callLhChildQuerySpecification extends TFQuerySpecification(Test_callLhChildQuerySpecification.GeneratedPQuery.INSTANCE) {
  override def instantiate(engine: ViatraQueryEngine): GenericPatternMatcher = {
    var matcher: GenericPatternMatcher = engine.getExistingMatcher(this)
    if (matcher == null) matcher = engine.getMatcher(this)
    matcher
  }
  override def getPreferredScopeClass: Class[_ <: QueryScope] = classOf[TFQueryScope]
}
object Test_callLhChildQuerySpecification {
  def instance(): Test_callLhChildQuerySpecification = LazyHolder.INSTANCE
  private final object LazyHolder {
    val INSTANCE: Test_callLhChildQuerySpecification = make()
    def make(): Test_callLhChildQuerySpecification = new Test_callLhChildQuerySpecification()
  }
  private final object GeneratedPQuery extends BasePQuery(PVisibility.PUBLIC) {
    val INSTANCE: GeneratedPQuery.type = this
    private val param_add: PParameter = new PParameter("add", "org.inca.analyzedLangs.expLang.Add", new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[org.inca.analyzedLangs.expLang.Add])))
    private val param_out: PParameter = new PParameter("out", "org.inca.analyzedLangs.expLang.Exp", new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[org.inca.analyzedLangs.expLang.Exp])))
    {}
    override protected def doGetContainedBodies(): util.Set[PBody] = {
      val bodies: util.Set[PBody] = util.Set.of {
        val body: PBody = new PBody(this)
        val var_add: PVariable = body.getOrCreateVariableByName("add")
        val var_out: PVariable = body.getOrCreateVariableByName("out")
        ()
        val exportedParams = new util.ArrayList[ExportedParameter]()
        exportedParams.add(new ExportedParameter(body, var_add, param_add))
        exportedParams.add(new ExportedParameter(body, var_out, param_out))
        body.setSymbolicParameters(exportedParams)
        val var_arg: PVariable = body.getOrCreateVariableByName("arg")
        val var_lhschild: PVariable = body.getOrCreateVariableByName("lhschild")
        new TypeConstraint(body, Tuples.flatTupleOf(var_add), new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[org.inca.analyzedLangs.expLang.Add])))
        new TypeConstraint(body, Tuples.flatTupleOf(var_out), new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[org.inca.analyzedLangs.expLang.Exp])))
        new PositivePatternCall(body, Tuples.flatTupleOf(var_add, var_arg), Test_lhChildQuerySpecification.instance().getInternalQueryRepresentation())
        new Equality(body, var_lhschild, var_arg)
        new Equality(body, var_lhschild, var_out)
        body
      }
      bodies
    }
    override def getFullyQualifiedName: String = "Test_callLhChildQuerySpecification"
    override def getParameters: util.List[PParameter] = util.List.of(param_add, param_out)
    override def getParameterNames: util.List[String] = util.List.of("add", "out")
  }
}