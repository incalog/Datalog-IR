//package org.inca.generator
//
//import org.eclipse.viatra.query.runtime.api.ViatraQueryEngine
//import org.eclipse.viatra.query.runtime.api.scope.QueryScope
//import org.eclipse.viatra.query.runtime.matchers.psystem.{PBody, PVariable}
//import org.eclipse.viatra.query.runtime.matchers.psystem.queries.PParameter
//import org.inca.generators.gp.model.psystem.{AbstractPQuery, ScalaPatternMatcher, ScalaQuerySpecification}
//import java.util
//
//import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.Equality
//import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables.{PositivePatternCall, TypeConstraint}
//import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
//import org.inca.generators.gp.model.keys.{ClassKey, LinkKey, PlaceholderConceptKey}
//import org.inca.meta.MetaElements.NodeType
//
//class FalseInitializer_BoolLangQuerySpecification extends ScalaQuerySpecification(new FalseInitializer_BoolLangQuerySpecification.GeneratedPQuery) {
//  override def instantiate(viatraQueryEngine: ViatraQueryEngine): ScalaPatternMatcher = ???
//  override def getPreferredScopeClass: Class[_ <: QueryScope] = ???
//}
//object FalseInitializer_BoolLangQuerySpecification {
//  final class GeneratedPQuery extends AbstractPQuery {
//    private val that = this
//    private val p_var: PParameter = new PParameter("p_var", "analyzedLangs.BinaryExpLang.VariableDeclaration", new PlaceholderConceptKey())
//    private val p_initializer: PParameter = new PParameter("p_initializer", "org.inca.generators.gp.sdk.queryspecification.PrimitiveConstants.Primitive", new PlaceholderConceptKey())
//    {}
//    override protected def doGetContainedBodies(): util.Set[PBody] = {
//      val bodies: util.Set[PBody] = util.Set.of {
//        val body: PBody = new PBody(that)
//        val var_var: PVariable = body.getOrCreateVariableByName("var")
//        val var_initializer: PVariable = body.getOrCreateVariableByName("initializer")
//        val var__expression: PVariable = body.getOrCreateVariableByName("expression")
//        val var___i552556757 = body.newConstantVariable(false)
//        val var___1282437555 = body.newConstantVariable(true)
//        new TypeConstraint(body, Tuples.flatTupleOf(var_var), new ClassKey(NodeType(classOf[analyzedLangs.BinaryExpLang.VariableDeclaration])))
//        new TypeConstraint(body, Tuples.flatTupleOf(var_initializer), new ClassKey(NodeType(classOf[org.inca.generators.gp.sdk.queryspecification.PrimitiveConstants.Primitive])))
//        new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var_var, var__expression), new LinkKey(NodeType(classOf[analyzedLangs.BinaryExpLang.VariableDeclaration])("initializer")))
////        new PositivePatternCall(body, Tuples.flatTupleOf(var__expression, var_initializer), new Boolean_BoolLangQuerySpecification().instance().getInternalQueryRepresentation())
//        new Equality(body, var_initializer, var___i552556757)
//        new Equality(body, var___1282437555, var___1282437555)
//        body
//      }
//      bodies
//    }
//    override def getFullyQualifiedName: String = "BoolLang.FalseInitializer"
//    override def getParameterNames: util.List[String] = util.List.of("var", "initializer")
//    override def getParameters: util.List[PParameter] = util.List.of(p_var, p_initializer)
//  }
//}
