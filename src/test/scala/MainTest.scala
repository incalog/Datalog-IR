import org.inca.core.`type`.compileTime.{ConceptReferenceType, JoinType, PatternVisibility}
import org.inca.core.content.JoinTypeDef
import org.inca.core.reference.VariableReference
import org.inca.gp.constraints.GraphPatternConceptConstraint
import org.inca.gp.content.{GraphPattern, GraphPatternBody, GraphPatternParameter}
import org.inca.mps.ConceptDeclaration
import org.inca.mps.binaryOperations.PlusExpression

object MainTest {
  var PlusMinusExpression = JoinType(JoinTypeDef("PlusMinusExpression", List(/* TODO insert stuff */)))
  var patter = GraphPattern("PlusMinus",
    List(GraphPatternParameter("e", PlusMinusExpression)),
      List(
        GraphPatternBody(
          List(GraphPatternConceptConstraint(
                VariableReference(GraphPatternParameter("e", PlusMinusExpression)), PlusMinusExpression)
            )),
        GraphPatternBody(
          List(GraphPatternConceptConstraint(
            VariableReference(GraphPatternParameter("e", ConceptReferenceType(ConceptDeclaration()))), PlusMinusExpression)
          )),
        GraphPatternBody(
          List(GraphPatternConceptConstraint(
            VariableReference(GraphPatternParameter("e", PlusMinusExpression)), PlusMinusExpression)
          ))
      ),
    PatternVisibility(true))


  new IPatternVisibilty {
    var visible = true
  }
}
