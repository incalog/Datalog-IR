package inca.trans.generated
import java.util

import inca.runtime.Query
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.{ExportedParameter, _}
import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables._
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}
import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
object FindBugs_confusedInheritanceQuerySpecification {
  lazy val instance: Query.Specification = new Query.Specification(GeneratedPQuery)
  private final object GeneratedPQuery extends BasePQuery(PVisibility.PUBLIC) {
    private val param_class: PParameter = new PParameter("class", truechange.SortType("inca.analyzedLangs.ClassDeclaration").toString, inca.runtime.index.NodeTypeKey(truechange.SortType("inca.analyzedLangs.ClassDeclaration")))
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
        val var_trg_0: PVariable = body.getOrCreateVariableByName("trg_0")
        val var_members: PVariable = body.getOrCreateVariableByName("members")
        val var_trg_1: PVariable = body.getOrCreateVariableByName("trg_1")
        val var_member: PVariable = body.getOrCreateVariableByName("member")
        val var_trg_2: PVariable = body.getOrCreateVariableByName("trg_2")
        val lit_boolean1231 = body.newConstantVariable(true)
        new TypeConstraint(body, Tuples.flatTupleOf(var_class), inca.runtime.index.NodeTypeKey(truechange.SortType("inca.analyzedLangs.ClassDeclaration")))
        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var_class, var_trg), inca.runtime.index.LinkPrimitiveKey(("inca.analyzedLangs.ClassDeclaration", "isFinal")))
        new Equality(body, var_tmp, lit_boolean1231)
        new Equality(body, var_trg, var_tmp)
        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var_class, var_trg_0), inca.runtime.index.LinkNodeKey(("inca.analyzedLangs.ClassDeclaration", "members")))
        new Equality(body, var_members, var_trg_0)
        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var_trg_1, var_members), inca.runtime.index.dynamic.ParentIndex.Key)
        new Equality(body, var_member, var_trg_1)
        new TypeConstraint(body, Tuples.flatTupleOf(var_member), inca.runtime.index.NodeTypeKey(truechange.SortType("inca.analyzedLangs.FieldDeclaration")))
        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var_member, var_trg_2), inca.runtime.index.LinkNodeKey(("inca.analyzedLangs.FieldDeclaration", "visibility")))
        new TypeConstraint(body, Tuples.flatTupleOf(var_trg_2), inca.runtime.index.NodeTypeKey(truechange.SortType("inca.analyzedLangs.ProtectedVisibility")))
        body
      }
      bodies
    }
    override def getFullyQualifiedName: String = "FindBugs_confusedInheritanceQuerySpecification"
    override def getParameters: util.List[PParameter] = util.List.of(param_class)
    override def getParameterNames: util.List[String] = util.List.of("class")
  }
}