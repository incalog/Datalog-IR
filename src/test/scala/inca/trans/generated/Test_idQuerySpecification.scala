package inca.trans.generated
import java.util

import inca.runtime.Query
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.{ExportedParameter, _}
import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables._
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}
import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
object Test_idQuerySpecification {
  lazy val instance: Query.Specification = new Query.Specification(GeneratedPQuery)
  private final object GeneratedPQuery extends BasePQuery(PVisibility.PUBLIC) {
    private val param_add: PParameter = new PParameter("add", truechange.SortType("inca.analyzedLangs.Add").toString, inca.runtime.index.NodeTypeKey(truechange.SortType("inca.analyzedLangs.Add")))
    private val param_out: PParameter = new PParameter("out", truechange.SortType("inca.analyzedLangs.Exp").toString, inca.runtime.index.NodeTypeKey(truechange.SortType("inca.analyzedLangs.Exp")))
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
        new TypeConstraint(body, Tuples.flatTupleOf(var_add), inca.runtime.index.NodeTypeKey(truechange.SortType("inca.analyzedLangs.Add")))
        new TypeConstraint(body, Tuples.flatTupleOf(var_out), inca.runtime.index.NodeTypeKey(truechange.SortType("inca.analyzedLangs.Exp")))
        new Equality(body, var_add, var_out)
        body
      }
      bodies
    }
    override def getFullyQualifiedName: String = "Test_idQuerySpecification"
    override def getParameters: util.List[PParameter] = util.List.of(param_add, param_out)
    override def getParameterNames: util.List[String] = util.List.of("add", "out")
  }
}