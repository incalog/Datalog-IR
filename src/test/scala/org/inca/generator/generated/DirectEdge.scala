
package org.inca.generator.generated
import org.eclipse.viatra.query.runtime.api.{ GenericPatternMatcher, ViatraQueryEngine }
import org.eclipse.viatra.query.runtime.api.scope.QueryScope
import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables.TypeConstraint
import org.eclipse.viatra.query.runtime.matchers.psystem.{ PBody, PVariable }
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{ BasePQuery, PParameter, PVisibility }
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.ExportedParameter
import java.util
import org.inca.gen.gp.model._
import org.inca.gen.gp.model.keys.{ ClassKey, LinkKey }
import org.inca.incer.indices.{ TFInputKey, TFQueryScope, TFQuerySpecification }
import org.inca.meta.MetaElements
import org.inca.meta.MetaElements.NodeType
class DirectEdge extends TFQuerySpecification(DirectEdge.GeneratedPQuery.INSTANCE) {
  override def instantiate(engine: ViatraQueryEngine): GenericPatternMatcher = {
    var matcher: GenericPatternMatcher = engine.getExistingMatcher(this)
    if (matcher == null) matcher = engine.getMatcher(this)
    matcher
  }
  override def getPreferredScopeClass: Class[_ <: QueryScope] = classOf[TFQueryScope]
}
object DirectEdge {
  def instance(): DirectEdge = LazyHolder.INSTANCE
  private final object LazyHolder {
    val INSTANCE: DirectEdge = make()
    def make(): DirectEdge = new DirectEdge()
  }
  private final object GeneratedPQuery extends BasePQuery(PVisibility.PUBLIC) {
    val INSTANCE = this
    private val p_src: PParameter = new PParameter("src",
      MetaElements.NodeType(classOf[analyzedLangs.Node]).toString,
      new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[analyzedLangs.Node])))
    private val p_trg: PParameter = new PParameter("trg",
      MetaElements.NodeType(classOf[analyzedLangs.Node]).toString,
      new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[analyzedLangs.Node])))
    {}
    override protected def doGetContainedBodies(): util.Set[PBody] = {
      val bodies: util.Set[PBody] = util.Set.of {
        val body: PBody = new PBody(this)
        val var_src: PVariable = body.getOrCreateVariableByName("src")
        val var_trg: PVariable = body.getOrCreateVariableByName("trg")
        ()
        val exportedParams = new util.ArrayList[ExportedParameter]()
        exportedParams.add(new ExportedParameter(body, var_src, p_src))
        exportedParams.add(new ExportedParameter(body, var_trg, p_trg))
        body.setSymbolicParameters(exportedParams)

        val var__graph: PVariable = body.getOrCreateVariableByName("graph")
        val var__edge: PVariable = body.getOrCreateVariableByName("edge")

        new TypeConstraint(body, Tuples.flatTupleOf(var_src),
          new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[analyzedLangs.Node])))
        new TypeConstraint(body, Tuples.flatTupleOf(var_trg), new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[analyzedLangs.Node])))
        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var_src, var__graph),
          new TFInputKey.NodeLinkKey(MetaElements.NodeType(classOf[analyzedLangs.Node])("parent")))
        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var__graph, var__edge), new TFInputKey.NodeLinkKey(MetaElements.NodeType(classOf[analyzedLangs.Graph])("edges")))
        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var__edge, var_src), new TFInputKey.NodeLinkKey(MetaElements.NodeType(classOf[analyzedLangs.Edge])("from")))
        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var__edge, var_trg), new TFInputKey.NodeLinkKey(MetaElements.NodeType(classOf[analyzedLangs.Edge])("to")))
        body
      }
      bodies
    }
    override def getFullyQualifiedName: String = "DirectEdge"
    override def getParameters: util.List[PParameter] = util.List.of(p_src, p_trg)
    override def getParameterNames: util.List[String] = util.List.of("src", "trg")
  }
}