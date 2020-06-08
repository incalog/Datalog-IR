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
import org.inca.incer.indices.{ ParentKey, TFInputKey, TFQueryScope, TFQuerySpecification }
import org.inca.meta.MetaElements
class Test_parentQuerySpecification extends TFQuerySpecification(Test_parentQuerySpecification.GeneratedPQuery.INSTANCE) {
  override def instantiate(engine: ViatraQueryEngine): GenericPatternMatcher = {
    var matcher: GenericPatternMatcher = engine.getExistingMatcher(this)
    if (matcher == null) matcher = engine.getMatcher(this)
    matcher
  }
  override def getPreferredScopeClass: Class[_ <: QueryScope] = classOf[TFQueryScope]
}
object Test_parentQuerySpecification {
  def instance(): Test_parentQuerySpecification = LazyHolder.INSTANCE
  private final object LazyHolder {
    val INSTANCE: Test_parentQuerySpecification = make()
    def make(): Test_parentQuerySpecification = new Test_parentQuerySpecification()
  }
  private final object GeneratedPQuery extends BasePQuery(PVisibility.PUBLIC) {
    val INSTANCE: GeneratedPQuery.type = this
    private val param_in: PParameter = new PParameter("in")
    private val param_out: PParameter = new PParameter("out", "org.inca.analyzedLangs.expLang.Exp", new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[org.inca.analyzedLangs.expLang.Exp])))
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
        val var_trg: PVariable = body.getOrCreateVariableByName("trg")
        new TypeConstraint(body, Tuples.flatTupleOf(var_out), new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[org.inca.analyzedLangs.expLang.Exp])))
        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var_in, var_trg), new ParentKey(new MetaElements.ParentLink()))
        new Equality(body, var_trg, var_out)
        body
      }
      bodies
    }
    override def getFullyQualifiedName: String = "Test_parentQuerySpecification"
    override def getParameters: util.List[PParameter] = util.List.of(param_in, param_out)
    override def getParameterNames: util.List[String] = util.List.of("in", "out")
  }
}