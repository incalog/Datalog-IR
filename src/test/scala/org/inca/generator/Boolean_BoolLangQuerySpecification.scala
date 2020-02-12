//package org.inca.generator
//
//import org.eclipse.viatra.query.runtime.api.{GenericPatternMatcher, ViatraQueryEngine}
//import org.eclipse.viatra.query.runtime.api.scope.QueryScope
//import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables.TypeConstraint
//import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
//import org.eclipse.viatra.query.runtime.matchers.psystem.queries.{BasePQuery, PParameter, PVisibility}
//import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
//import org.inca.generators.gp.model.keys.{ClassKey, LinkKey}
//import org.inca.meta.MetaElements.{MetaElement, NodeType}
//import java.util
//
//import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.ExportedParameter
//import org.inca.findbugs.ClassDeclaration
//import org.inca.gen.gp.model.keys.{ClassKey, LinkKey}
//import org.inca.gen.gp.queryspecification.PrimitiveConstants
//import org.inca.gen.gp.sdk.queryspecification.PrimitiveConstants.Primitive
//import org.inca.incer.indices.{TFInputKey, TFQueryScope, TFQuerySpecification}
//import org.inca.meta.MetaElements
//
//class Boolean_BoolLangQuerySpecification() extends TFQuerySpecification(Boolean_BoolLangQuerySpecification.GeneratedPQuery.INSTANCE) {
//
//  // adjust
//  override def instantiate(engine: ViatraQueryEngine): GenericPatternMatcher = {
//    var matcher: GenericPatternMatcher = engine.getExistingMatcher(this)
//    if (matcher == null) matcher = engine.getMatcher(this)
//    matcher
//  }
//  override def getPreferredScopeClass: Class[_ <: QueryScope] = classOf[TFQueryScope]
//  // end adjust
//}
//
//
//
//
//object Boolean_BoolLangQuerySpecification {
//  // add
//  def instance(): Boolean_BoolLangQuerySpecification = LazyHolder.INSTANCE
//
//  private final class LazyHolder {}
//  private final object LazyHolder {
//    val INSTANCE: Boolean_BoolLangQuerySpecification = make()
//    def make(): Boolean_BoolLangQuerySpecification = new Boolean_BoolLangQuerySpecification()
//  }
//
//  // end add
//
//  // add superclass param
//  final class GeneratedPQuery extends BasePQuery(PVisibility.PUBLIC) {
//    private val that = this
//
//    private val p_expression: PParameter =
//      new PParameter("p_expression",
//        "org.inca.meta.MetaElements.MetaElement",
//        new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[MetaElement])))
//    private val p_value: PParameter =
//      new PParameter("p_value",
//        "org.inca.generators.gp.sdk.queryspecification.PrimitiveConstants.Primitive",
//        new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[Primitive])))
//
//    override def getFullyQualifiedName: String = "BoolLang.Boolean"
//    override def getParameterNames: util.List[String] = util.List.of("expression", "value")
//    override def getParameters: util.List[PParameter] = util.List.of(p_expression, p_value)
//
//    override protected def doGetContainedBodies(): util.Set[PBody] = {
//      val bodies: util.Set[PBody] = util.Set.of {
//        val body: PBody = new PBody(that)
//        val var_expression: PVariable = body.getOrCreateVariableByName("expression")
//        val var_value: PVariable = body.getOrCreateVariableByName("value")
//
//        // add
//        val exportedParameters = new util.ArrayList[ExportedParameter]()
//        exportedParameters.add(new ExportedParameter(body, var_expression, p_expression))
//        exportedParameters.add(new ExportedParameter(body, var_value, p_value))
//        body.setSymbolicParameters(exportedParameters)
//        // end add
//
//        new TypeConstraint(body, Tuples.flatTupleOf(var_expression), new ClassKey(NodeType(classOf[org.inca.meta.MetaElements.MetaElement])))
//        new TypeConstraint(body, Tuples.flatTupleOf(var_expression), new ClassKey(NodeType(classOf[PrimitiveConstants.Primitive])))
//        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var_expression, var_value), new LinkKey(NodeType(classOf[PrimitiveConstants.BooleanConstant])("value")))
//        body
//      }
//      bodies
//    }
//  }
//  final object GeneratedPQuery  {
//    // add
//    val INSTANCE = new GeneratedPQuery
//    // end add
//  }
//}