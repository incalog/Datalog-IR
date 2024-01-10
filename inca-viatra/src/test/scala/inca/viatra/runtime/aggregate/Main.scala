package inca.viatra.runtime.aggregate

object Main {
  def main(args: Array[String]): Any = {

    import org.eclipse.viatra.query.runtime.api.{GenericPatternMatcher, ViatraQueryEngine}
    import org.eclipse.viatra.query.runtime.api.scope.{QueryScope => ViatraQueryScope}
    import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
    import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}
    import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
    import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.ExportedParameter

    import org.eclipse.viatra.query.runtime.matchers.context.common.JavaTransitiveInstancesKey

    import java.util

    import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred._
    import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables._

    import inca.viatra.compile.PSystem
    import inca.viatra.runtime.Query.Specification
    import inca.viatra.runtime.index.NamedRelationKey

    import inca.viatra.runtime.aggregate.builtin
    import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.AggregatorConstraint
    import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.BoundAggregator

    object Path extends PSystem.Module {

      override val patterns: Map[String, () => Specification] = Map("edge" -> (() => edge.instance),"path" -> (() => path.instance))

      object edge {
        lazy val instance: Specification = new Specification(generatedPQuery)

        private object generatedPQuery extends BasePQuery(PVisibility.PUBLIC) {
          private val param_x: PParameter = new PParameter("x")
          private val param_y: PParameter = new PParameter("y")

          override protected def doGetContainedBodies(): util.Set[PBody] = util.Set.of({
            val body: PBody = new PBody(this)
            val var_x: PVariable = body.getOrCreateVariableByName("x")
            val var_y: PVariable = body.getOrCreateVariableByName("y")
            val exportedParams = new util.ArrayList[ExportedParameter]()
            exportedParams.add(new ExportedParameter(body, var_x, param_x))
            exportedParams.add(new ExportedParameter(body, var_y, param_y))

            body.setSymbolicParameters(exportedParams)


            val eval_out$0: PVariable = body.getOrCreateVariableByName("out$0")
            val eval_out$1: PVariable = body.getOrCreateVariableByName("out$1")

            new ExpressionEvaluation(body, new org.eclipse.viatra.query.runtime.matchers.psystem.IExpressionEvaluator {
              override def getShortDescription: String = """"eval(`((s: Any) => s.toString)(`1`)`)""""
              override def getInputParameterNames: java.lang.Iterable[String] = java.util.Arrays.asList()
              override def evaluateExpression(env: org.eclipse.viatra.query.runtime.matchers.psystem.IValueProvider): Any = {
                ((s: Any) => s.toString)(1)
              }
            }, eval_out$0)

            new ExpressionEvaluation(body, new org.eclipse.viatra.query.runtime.matchers.psystem.IExpressionEvaluator {
              override def getShortDescription: String = """"eval(`((s: Any) => s.toString)(`2`)`)""""
              override def getInputParameterNames: java.lang.Iterable[String] = java.util.Arrays.asList()
              override def evaluateExpression(env: org.eclipse.viatra.query.runtime.matchers.psystem.IValueProvider): Any = {
                ((s: Any) => s.toString)(2)
              }
            }, eval_out$1)
            new Equality(body, var_x, eval_out$0)
            new Equality(body, var_y, eval_out$1)
            body
          }, {
            val body: PBody = new PBody(this)
            val var_x: PVariable = body.getOrCreateVariableByName("x")
            val var_y: PVariable = body.getOrCreateVariableByName("y")
            val exportedParams = new util.ArrayList[ExportedParameter]()
            exportedParams.add(new ExportedParameter(body, var_x, param_x))
            exportedParams.add(new ExportedParameter(body, var_y, param_y))

            body.setSymbolicParameters(exportedParams)


            val eval_out$2: PVariable = body.getOrCreateVariableByName("out$2")
            val eval_out$3: PVariable = body.getOrCreateVariableByName("out$3")

            new ExpressionEvaluation(body, new org.eclipse.viatra.query.runtime.matchers.psystem.IExpressionEvaluator {
              override def getShortDescription: String = """"eval(`((s: Any) => s.toString)(`2`)`)""""
              override def getInputParameterNames: java.lang.Iterable[String] = java.util.Arrays.asList()
              override def evaluateExpression(env: org.eclipse.viatra.query.runtime.matchers.psystem.IValueProvider): Any = {
                ((s: Any) => s.toString)(2)
              }
            }, eval_out$2)

            new ExpressionEvaluation(body, new org.eclipse.viatra.query.runtime.matchers.psystem.IExpressionEvaluator {
              override def getShortDescription: String = """"eval(`((s: Any) => s.toString)(`3`)`)""""
              override def getInputParameterNames: java.lang.Iterable[String] = java.util.Arrays.asList()
              override def evaluateExpression(env: org.eclipse.viatra.query.runtime.matchers.psystem.IValueProvider): Any = {
                ((s: Any) => s.toString)(3)
              }
            }, eval_out$3)
            new Equality(body, var_x, eval_out$2)
            new Equality(body, var_y, eval_out$3)
            body
          }, {
            val body: PBody = new PBody(this)
            val var_x: PVariable = body.getOrCreateVariableByName("x")
            val var_y: PVariable = body.getOrCreateVariableByName("y")
            val exportedParams = new util.ArrayList[ExportedParameter]()
            exportedParams.add(new ExportedParameter(body, var_x, param_x))
            exportedParams.add(new ExportedParameter(body, var_y, param_y))

            body.setSymbolicParameters(exportedParams)


            val eval_out$4: PVariable = body.getOrCreateVariableByName("out$4")
            val eval_out$5: PVariable = body.getOrCreateVariableByName("out$5")

            new ExpressionEvaluation(body, new org.eclipse.viatra.query.runtime.matchers.psystem.IExpressionEvaluator {
              override def getShortDescription: String = """"eval(`((s: Any) => s.toString)(`3`)`)""""
              override def getInputParameterNames: java.lang.Iterable[String] = java.util.Arrays.asList()
              override def evaluateExpression(env: org.eclipse.viatra.query.runtime.matchers.psystem.IValueProvider): Any = {
                ((s: Any) => s.toString)(3)
              }
            }, eval_out$4)

            new ExpressionEvaluation(body, new org.eclipse.viatra.query.runtime.matchers.psystem.IExpressionEvaluator {
              override def getShortDescription: String = """"eval(`((s: Any) => s.toString)(`4`)`)""""
              override def getInputParameterNames: java.lang.Iterable[String] = java.util.Arrays.asList()
              override def evaluateExpression(env: org.eclipse.viatra.query.runtime.matchers.psystem.IValueProvider): Any = {
                ((s: Any) => s.toString)(4)
              }
            }, eval_out$5)
            new Equality(body, var_x, eval_out$4)
            new Equality(body, var_y, eval_out$5)
            body
          })

          override def getFullyQualifiedName: String = "Path_edge"
          override def getParameters: util.List[PParameter] = util.List.of(param_x,param_y)
          override def getParameterNames: util.List[String] = util.List.of("x","y")
        }
      }


      object path {
        lazy val instance: Specification = new Specification(generatedPQuery)

        private object generatedPQuery extends BasePQuery(PVisibility.PUBLIC) {
          private val param_x: PParameter = new PParameter("x")
          private val param_y: PParameter = new PParameter("y")

          override protected def doGetContainedBodies(): util.Set[PBody] = util.Set.of({
            val body: PBody = new PBody(this)
            val var_x: PVariable = body.getOrCreateVariableByName("x")
            val var_y: PVariable = body.getOrCreateVariableByName("y")
            val exportedParams = new util.ArrayList[ExportedParameter]()
            exportedParams.add(new ExportedParameter(body, var_x, param_x))
            exportedParams.add(new ExportedParameter(body, var_y, param_y))

            body.setSymbolicParameters(exportedParams)




            new PositivePatternCall(body, Tuples.flatTupleOf(var_x,var_y), Path.edge.instance.getInternalQueryRepresentation)
            body
          }, {
            val body: PBody = new PBody(this)
            val var_x: PVariable = body.getOrCreateVariableByName("x")
            val var_y: PVariable = body.getOrCreateVariableByName("y")
            val exportedParams = new util.ArrayList[ExportedParameter]()
            exportedParams.add(new ExportedParameter(body, var_x, param_x))
            exportedParams.add(new ExportedParameter(body, var_y, param_y))

            body.setSymbolicParameters(exportedParams)
            val var_z: PVariable = body.getOrCreateVariableByName("z")



            new PositivePatternCall(body, Tuples.flatTupleOf(var_x,var_z), Path.path.instance.getInternalQueryRepresentation)
            new PositivePatternCall(body, Tuples.flatTupleOf(var_z,var_y), Path.path.instance.getInternalQueryRepresentation)
            body
          })

          override def getFullyQualifiedName: String = "Path_path"
          override def getParameters: util.List[PParameter] = util.List.of(param_x,param_y)
          override def getParameterNames: util.List[String] = util.List.of("x","y")
        }
      }

    }
    ; Path
  }
}