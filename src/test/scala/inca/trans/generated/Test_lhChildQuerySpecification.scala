package inca.trans.generated
import java.util

import inca.runtime.IncaQuerySpecification
import inca.runtime.context.QueryScope
import org.eclipse.viatra.query.runtime.api.scope.{QueryScope => ViatraQueryScope}
import org.eclipse.viatra.query.runtime.api.{GenericPatternMatcher, ViatraQueryEngine}
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.{ExportedParameter, _}
import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables._
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}
import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
class Test_lhChildQuerySpecification extends IncaQuerySpecification(Test_lhChildQuerySpecification.GeneratedPQuery.INSTANCE) {
  override def instantiate(engine: ViatraQueryEngine): GenericPatternMatcher = {
    var matcher: GenericPatternMatcher = engine.getExistingMatcher(this)
    if (matcher == null) matcher = engine.getMatcher(this)
    matcher
  }
  override def getPreferredScopeClass: Class[_ <: ViatraQueryScope] = classOf[QueryScope]
}
object Test_lhChildQuerySpecification {
  def instance(): Test_lhChildQuerySpecification = LazyHolder.INSTANCE
  private final object LazyHolder {
    val INSTANCE: Test_lhChildQuerySpecification = make()
    def make(): Test_lhChildQuerySpecification = new Test_lhChildQuerySpecification()
  }
  private final object GeneratedPQuery extends BasePQuery(PVisibility.PUBLIC) {
    val INSTANCE: GeneratedPQuery.type = this
    private val param_add: PParameter = new PParameter("add", inca.runtime.index.MetaElements.NodeType("inca.analyzedLangs.expLang.Add").toString, new inca.runtime.index.NodeTypeKey(inca.runtime.index.MetaElements.NodeType("inca.analyzedLangs.expLang.Add")))
    private val param_out: PParameter = new PParameter("out", inca.runtime.index.MetaElements.NodeType("inca.analyzedLangs.expLang.Exp").toString, new inca.runtime.index.NodeTypeKey(inca.runtime.index.MetaElements.NodeType("inca.analyzedLangs.expLang.Exp")))
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
        val var_trg: PVariable = body.getOrCreateVariableByName("trg")
        new TypeConstraint(body, Tuples.flatTupleOf(var_add), new inca.runtime.index.NodeTypeKey(inca.runtime.index.MetaElements.NodeType("inca.analyzedLangs.expLang.Add")))
        new TypeConstraint(body, Tuples.flatTupleOf(var_out), new inca.runtime.index.NodeTypeKey(inca.runtime.index.MetaElements.NodeType("inca.analyzedLangs.expLang.Exp")))
        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var_add, var_trg), new inca.runtime.index.LinkKey(inca.runtime.index.MetaElements.NamedLink(inca.runtime.index.MetaElements.NodeType("inca.analyzedLangs.expLang.Add"), "lhs")))
        new Equality(body, var_trg, var_out)
        body
      }
      bodies
    }
    override def getFullyQualifiedName: String = "Test_lhChildQuerySpecification"
    override def getParameters: util.List[PParameter] = util.List.of(param_add, param_out)
    override def getParameterNames: util.List[String] = util.List.of("add", "out")
  }
}