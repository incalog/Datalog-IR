package inca.trans.generated
import java.util

import inca.runtime.Query
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.{ExportedParameter, _}
import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables._
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}
import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
object Test_isBooleanQuerySpecification {
  lazy val instance: Query.Specification = new Query.Specification(GeneratedPQuery)
  private final object GeneratedPQuery extends BasePQuery(PVisibility.PUBLIC) {
    private val param_in: PParameter = new PParameter("in", truechange.SortType("inca.analyzedLangs.BooleanLit").toString, inca.runtime.index.NodeTypeKey(truechange.SortType("inca.analyzedLangs.BooleanLit")))
    private val param_out: PParameter = new PParameter("out", truechange.JavaLitType(classOf[java.lang.Boolean]).toString, inca.runtime.index.PrimitiveTypeKey(truechange.JavaLitType(classOf[java.lang.Boolean])))
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
        new TypeConstraint(body, Tuples.flatTupleOf(var_in), inca.runtime.index.NodeTypeKey(truechange.SortType("inca.analyzedLangs.BooleanLit")))
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