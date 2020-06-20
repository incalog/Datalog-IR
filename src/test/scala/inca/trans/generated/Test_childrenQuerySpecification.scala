package inca.trans.generated
import org.eclipse.viatra.query.runtime.api.{ GenericPatternMatcher, ViatraQueryEngine }
import org.eclipse.viatra.query.runtime.api.scope.{QueryScope => ViatraQueryScope}
import org.eclipse.viatra.query.runtime.matchers.psystem.{ PBody, PVariable }
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{ BasePQuery, PParameter, PVisibility }
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.ExportedParameter
import org.eclipse.viatra.query.runtime.matchers.context.common.JavaTransitiveInstancesKey
import java.util
import inca.backend.indices.{ InputKey, QueryScope, TFQuerySpecification }
import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables.TypeConstraint
import inca.MetaElements.NodeType
import inca.MetaElements.NamedLink
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.Equality
class Test_childrenQuerySpecification extends TFQuerySpecification(Test_childrenQuerySpecification.GeneratedPQuery.INSTANCE) {
  override def instantiate(engine: ViatraQueryEngine): GenericPatternMatcher = {
    var matcher: GenericPatternMatcher = engine.getExistingMatcher(this)
    if (matcher == null) matcher = engine.getMatcher(this)
    matcher
  }
  override def getPreferredScopeClass: Class[_ <: ViatraQueryScope] = classOf[QueryScope]
}
object Test_childrenQuerySpecification {
  def instance(): Test_childrenQuerySpecification = LazyHolder.INSTANCE
  private final object LazyHolder {
    val INSTANCE: Test_childrenQuerySpecification = make()
    def make(): Test_childrenQuerySpecification = new Test_childrenQuerySpecification()
  }
  private final object GeneratedPQuery extends BasePQuery(PVisibility.PUBLIC) {
    val INSTANCE: GeneratedPQuery.type = this
    private val param_add: PParameter = new PParameter("add", "inca.analyzedLangs.expLang.Add", new InputKey.NodeTypeKey(NodeType("inca.analyzedLangs.expLang.Add")))
    private val param_out: PParameter = new PParameter("out", "inca.analyzedLangs.expLang.Exp", new InputKey.NodeTypeKey(NodeType("inca.analyzedLangs.expLang.Exp")))
    {}
    override protected def doGetContainedBodies(): util.Set[PBody] = {
      val bodies: util.Set[PBody] = util.Set.of({
        val body: PBody = new PBody(this)
        val var_add: PVariable = body.getOrCreateVariableByName("add")
        val var_out: PVariable = body.getOrCreateVariableByName("out")
        ()
        val exportedParams = new util.ArrayList[ExportedParameter]()
        exportedParams.add(new ExportedParameter(body, var_add, param_add))
        exportedParams.add(new ExportedParameter(body, var_out, param_out))
        body.setSymbolicParameters(exportedParams)
        val var_trg: PVariable = body.getOrCreateVariableByName("trg")
        new TypeConstraint(body, Tuples.flatTupleOf(var_add), new InputKey.NodeTypeKey(NodeType("inca.analyzedLangs.expLang.Add")))
        new TypeConstraint(body, Tuples.flatTupleOf(var_out), new InputKey.NodeTypeKey(NodeType("inca.analyzedLangs.expLang.Exp")))
        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var_add, var_trg), new InputKey.LinkKey(NamedLink(NodeType("inca.analyzedLangs.expLang.Add"), "lhs")))
        new Equality(body, var_trg, var_out)
        body
      }, {
        val body: PBody = new PBody(this)
        val var_add: PVariable = body.getOrCreateVariableByName("add")
        val var_out: PVariable = body.getOrCreateVariableByName("out")
        ()
        val exportedParams = new util.ArrayList[ExportedParameter]()
        exportedParams.add(new ExportedParameter(body, var_add, param_add))
        exportedParams.add(new ExportedParameter(body, var_out, param_out))
        body.setSymbolicParameters(exportedParams)
        val var_trg0: PVariable = body.getOrCreateVariableByName("trg0")
        new TypeConstraint(body, Tuples.flatTupleOf(var_add), new InputKey.NodeTypeKey(NodeType("inca.analyzedLangs.expLang.Add")))
        new TypeConstraint(body, Tuples.flatTupleOf(var_out), new InputKey.NodeTypeKey(NodeType("inca.analyzedLangs.expLang.Exp")))
        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var_add, var_trg0), new InputKey.LinkKey(NamedLink(NodeType("inca.analyzedLangs.expLang.Add"), "rhs")))
        new Equality(body, var_trg0, var_out)
        body
      })
      bodies
    }
    override def getFullyQualifiedName: String = "Test_childrenQuerySpecification"
    override def getParameters: util.List[PParameter] = util.List.of(param_add, param_out)
    override def getParameterNames: util.List[String] = util.List.of("add", "out")
  }
}