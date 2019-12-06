import org.inca.core_old.typ.compileTime.{ConceptReferenceType, JoinType, PatternVisibility}
import org.inca.core_old.content.JoinTypeDef
import org.inca.core_old.reference.VariableReference
import org.inca.gp_old.constraints.GraphPatternConceptConstraint
import org.inca.gp_old.content.{GraphPattern, GraphPatternBody, GraphPatternParameter}
import org.inca.mps.ConceptDeclaration
import org.inca.mps.binaryOperations.PlusExpression

object MainTest {

  // PlusMinus

  val PlusMinusExpression = JoinType(JoinTypeDef("PlusMinusExpression", List()))
  val pattern = GraphPattern("PlusMinus",
    List(GraphPatternParameter("e", PlusMinusExpression)),
      List(
        GraphPatternBody(
          List(GraphPatternConceptConstraint(
                VariableReference(GraphPatternParameter("e", PlusMinusExpression)), PlusMinusExpression)
            )),
        GraphPatternBody(
          List(GraphPatternConceptConstraint(
            VariableReference(GraphPatternParameter("e",   NodeType(Node.getClass))), PlusMinusExpression)
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


  // MulDiv

  var MulDivExpression = JoinType(JoinTypeDef("MulDivExpression", List()))
  var mulDiv = GraphPattern("MulDiv"
    List(GraphPatternParameter("e"),
          List(
            GraphPatternBody(
              GraphPatternConceptConstraint(VariableReference(GraphPatternParameter("e")), MulDivExpression)
            )
          )
    )
  )




}
