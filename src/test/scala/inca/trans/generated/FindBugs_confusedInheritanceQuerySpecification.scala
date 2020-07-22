package inca.trans.generated
import java.util

import inca.MetaElements.{NamedLink, NodeType}
import inca.backend.indices.{InputKey, QueryScope, TFQuerySpecification}
import inca.backend.virtual.tree.ParentKey
import org.eclipse.viatra.query.runtime.api.scope.{QueryScope => ViatraQueryScope}
import org.eclipse.viatra.query.runtime.api.{GenericPatternMatcher, ViatraQueryEngine}
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.{Equality, ExportedParameter}
import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables.TypeConstraint
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}
import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
class FindBugs_confusedInheritanceQuerySpecification extends TFQuerySpecification(FindBugs_confusedInheritanceQuerySpecification.GeneratedPQuery.INSTANCE) {
  override def instantiate(engine: ViatraQueryEngine): GenericPatternMatcher = {
    var matcher: GenericPatternMatcher = engine.getExistingMatcher(this)
    if (matcher == null) matcher = engine.getMatcher(this)
    matcher
  }
  override def getPreferredScopeClass: Class[_ <: ViatraQueryScope] = classOf[QueryScope]
}
object FindBugs_confusedInheritanceQuerySpecification {
  def instance(): FindBugs_confusedInheritanceQuerySpecification = LazyHolder.INSTANCE
  private final object LazyHolder {
    val INSTANCE: FindBugs_confusedInheritanceQuerySpecification = make()
    def make(): FindBugs_confusedInheritanceQuerySpecification = new FindBugs_confusedInheritanceQuerySpecification()
  }
  private final object GeneratedPQuery extends BasePQuery(PVisibility.PUBLIC) {
    val INSTANCE: GeneratedPQuery.type = this
    private val param_class: PParameter = new PParameter("class", "inca.analyzedLangs.ClassDeclaration", new InputKey.NodeTypeKey(NodeType("inca.analyzedLangs.ClassDeclaration")))
    {}
    override protected def doGetContainedBodies(): util.Set[PBody] = {
      val bodies: util.Set[PBody] = util.Set.of {
        val body: PBody = new PBody(this)
        val var_class: PVariable = body.getOrCreateVariableByName("class")
        ()
        val exportedParams = new util.ArrayList[ExportedParameter]()
        exportedParams.add(new ExportedParameter(body, var_class, param_class))
        body.setSymbolicParameters(exportedParams)
        val var_trg: PVariable = body.getOrCreateVariableByName("trg")
        val var_tmp: PVariable = body.getOrCreateVariableByName("tmp")
        val var_trg0: PVariable = body.getOrCreateVariableByName("trg0")
        val var_members: PVariable = body.getOrCreateVariableByName("members")
        val var_trg1: PVariable = body.getOrCreateVariableByName("trg1")
        val var_member: PVariable = body.getOrCreateVariableByName("member")
        val var_trg2: PVariable = body.getOrCreateVariableByName("trg2")
        val lit_boolean1231 = body.newConstantVariable(true)
        new TypeConstraint(body, Tuples.flatTupleOf(var_class), new InputKey.NodeTypeKey(NodeType("inca.analyzedLangs.ClassDeclaration")))
        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var_class, var_trg), new InputKey.LinkKey(NamedLink(NodeType("inca.analyzedLangs.ClassDeclaration"), "isFinal")))
        new Equality(body, var_tmp, lit_boolean1231)
        new Equality(body, var_trg, var_tmp)
        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var_class, var_trg0), new InputKey.LinkKey(NamedLink(NodeType("inca.analyzedLangs.ClassDeclaration"), "members")))
        new Equality(body, var_members, var_trg0)
        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var_trg1, var_members), ParentKey)
        new Equality(body, var_member, var_trg1)
        new TypeConstraint(body, Tuples.flatTupleOf(var_member), new InputKey.NodeTypeKey(NodeType("inca.analyzedLangs.FieldDeclaration")))
        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var_member, var_trg2), new InputKey.LinkKey(NamedLink(NodeType("inca.analyzedLangs.FieldDeclaration"), "visibility")))
        new TypeConstraint(body, Tuples.flatTupleOf(var_trg2), new InputKey.NodeTypeKey(NodeType("inca.analyzedLangs.ProtectedVisibility")))
        body
      }
      bodies
    }
    override def getFullyQualifiedName: String = "FindBugs_confusedInheritanceQuerySpecification"
    override def getParameters: util.List[PParameter] = util.List.of(param_class)
    override def getParameterNames: util.List[String] = util.List.of("class")
  }
}