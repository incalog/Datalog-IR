package inca.trans.generated
import org.eclipse.viatra.query.runtime.api.{ GenericPatternMatcher, ViatraQueryEngine }
import org.eclipse.viatra.query.runtime.api.scope.QueryScope
import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables.{ PositivePatternCall, BinaryTransitiveClosure, TypeConstraint }
import org.eclipse.viatra.query.runtime.matchers.psystem.{ PBody, PVariable }
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{ BasePQuery, PParameter, PVisibility }
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred._
import org.eclipse.viatra.query.runtime.matchers.context.common.JavaTransitiveInstancesKey
import java.util
import inca.backend.indices.{ TFInputKey, TFQueryScope, TFQuerySpecification }
import inca.backend.virtual.VirtualKey
import inca.backend.virtual.ParentKey
import inca.MetaElements
class Test_isBooleanQuerySpecification extends TFQuerySpecification(Test_isBooleanQuerySpecification.GeneratedPQuery.INSTANCE) {
  override def instantiate(engine: ViatraQueryEngine): GenericPatternMatcher = {
    var matcher: GenericPatternMatcher = engine.getExistingMatcher(this)
    if (matcher == null) matcher = engine.getMatcher(this)
    matcher
  }
  override def getPreferredScopeClass: Class[_ <: QueryScope] = classOf[TFQueryScope]
}
object Test_isBooleanQuerySpecification {
  def instance(): Test_isBooleanQuerySpecification = LazyHolder.INSTANCE
  private final object LazyHolder {
    val INSTANCE: Test_isBooleanQuerySpecification = make()
    def make(): Test_isBooleanQuerySpecification = new Test_isBooleanQuerySpecification()
  }
  private final object GeneratedPQuery extends BasePQuery(PVisibility.PUBLIC) {
    val INSTANCE: GeneratedPQuery.type = this
    private val param_in: PParameter = new PParameter("in", "inca.analyzedLangs.expLang.BooleanLit", new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[inca.analyzedLangs.expLang.BooleanLit])))
    private val param_out: PParameter = new PParameter("out", "java.lang.Boolean", new JavaTransitiveInstancesKey(classOf[java.lang.Boolean]))
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
        val var_tmp: PVariable = body.getOrCreateVariableByName("tmp")
        val lit_boolean1231 = body.newConstantVariable(true)
        new TypeConstraint(body, Tuples.flatTupleOf(var_in), new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[inca.analyzedLangs.expLang.BooleanLit])))
        new Equality(body, var_tmp, lit_boolean1231)
        new Equality(body, var_tmp, var_out)
        body
      }
      bodies
    }
    override def getFullyQualifiedName: String = "Test_isBooleanQuerySpecification"
    override def getParameters: util.List[PParameter] = util.List.of(param_in, param_out)
    override def getParameterNames: util.List[String] = util.List.of("in", "out")
  }
}