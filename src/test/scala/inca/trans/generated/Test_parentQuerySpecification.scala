package inca.trans.generated
import java.util

import inca.runtime.context.QueryScope
import inca.runtime.{IncaQuerySpecification, index}
import inca.runtime.index.MetaElements
import org.eclipse.viatra.query.runtime.api.scope.{QueryScope => ViatraQueryScope}
import org.eclipse.viatra.query.runtime.api.{GenericPatternMatcher, ViatraQueryEngine}
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.{ExportedParameter, _}
import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables._
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}
import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
class Test_parentQuerySpecification extends IncaQuerySpecification(Test_parentQuerySpecification.GeneratedPQuery.INSTANCE) {
  override def instantiate(engine: ViatraQueryEngine): GenericPatternMatcher = {
    var matcher: GenericPatternMatcher = engine.getExistingMatcher(this)
    if (matcher == null) matcher = engine.getMatcher(this)
    matcher
  }
  override def getPreferredScopeClass: Class[_ <: ViatraQueryScope] = classOf[QueryScope]
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
    private val param_out: PParameter = new PParameter("out", MetaElements.NodeType("inca.analyzedLangs.expLang.Exp").toString, new inca.runtime.indices.InputKey.NodeTypeKey(index.MetaElements.NodeType("inca.analyzedLangs.expLang.Exp")))
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
        val var_p: PVariable = body.getOrCreateVariableByName("p")
        new TypeConstraint(body, Tuples.flatTupleOf(var_out), new inca.runtime.indices.InputKey.NodeTypeKey(index.MetaElements.NodeType("inca.analyzedLangs.expLang.Exp")))
        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var_in, var_trg), inca.runtime.virtual.tree.ParentKey)
        new Equality(body, var_p, var_trg)
        new TypeConstraint(body, Tuples.flatTupleOf(var_p), new inca.runtime.indices.InputKey.NodeTypeKey(index.MetaElements.NodeType("inca.analyzedLangs.expLang.Exp")))
        new Equality(body, var_p, var_out)
        body
      }
      bodies
    }
    override def getFullyQualifiedName: String = "Test_parentQuerySpecification"
    override def getParameters: util.List[PParameter] = util.List.of(param_in, param_out)
    override def getParameterNames: util.List[String] = util.List.of("in", "out")
  }
}