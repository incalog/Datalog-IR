package inca.trans.generated
import java.util

import inca.runtime.Query
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.ExportedParameter
import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables._
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}
import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
object Test_noParamTypeQuerySpecification {
  lazy val instance: Query.Specification = new Query.Specification(GeneratedPQuery)
  private final object GeneratedPQuery extends BasePQuery(PVisibility.PUBLIC) {
    private val param_add: PParameter = new PParameter("add")
    {}
    override protected def doGetContainedBodies(): util.Set[PBody] = {
      val bodies: util.Set[PBody] = util.Set.of {
        val body: PBody = new PBody(this)
        val var_add: PVariable = body.getOrCreateVariableByName("add")
        ()
        val exportedParams = new util.ArrayList[ExportedParameter]()
        exportedParams.add(new ExportedParameter(body, var_add, param_add))
        body.setSymbolicParameters(exportedParams)
        new TypeConstraint(body, Tuples.flatTupleOf(var_add), inca.runtime.index.NodeTypeKey(truechange.SortType("inca.analyzedLangs.Add")))
        body
      }
      bodies
    }
    override def getFullyQualifiedName: String = "Test_noParamTypeQuerySpecification"
    override def getParameters: util.List[PParameter] = util.List.of(param_add)
    override def getParameterNames: util.List[String] = util.List.of("add")
  }
}