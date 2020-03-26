package org.inca.generator

import org.inca.analyzedLangs.{ClassDeclaration, ClassMember, FieldDeclaration, ProtectedVisibility}
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.inca.gen.Pipeline.generateGraphPattern
import org.inca.gen.gp.model.BooleanConstant
import org.inca.generator.generated.ConfusedInheritance
import org.inca.incer.indices.{EnginePool, TFQueryScope}
import org.inca.lang.core.Constraints.EqualityCompareFeature
import org.inca.lang.core.Content.CoreTemporaryVariable
import org.inca.lang.core.Reference.CoreVariableReference
import org.inca.lang.gp.Constraints.{GraphPatternCompareConstraint, PathExpressionConstraint}
import org.inca.lang.gp.Content.{GraphPattern, GraphPatternBody, GraphPatternParameter}
import org.inca.lang.gp.Virtual.ParentPathElement
import org.inca.meta.MetaElements.NodeType
import org.scalatest.funsuite.AnyFunSuite

class JavaLangTest extends AnyFunSuite {

  private val classDeclType = NodeType(classOf[ClassDeclaration])
  private val fieldDeclType = NodeType(classOf[FieldDeclaration])
  private val classMemberType = NodeType(classOf[ClassMember])

  private val classDeclarationIsFinalLink = classDeclType("isFinal")
  private val classDeclarationMembersLink = classDeclType("members")
  private val fieldDeclarationVisibilityLink = fieldDeclType("visibility")

  private val classGPP = GraphPatternParameter("class", Some(classDeclType))

  private val memberTV = CoreTemporaryVariable("member", Some(classMemberType))

  private val temp_1TV = CoreTemporaryVariable("temp_1", None)
  private val temp_2TV = CoreTemporaryVariable("temp_2", None)
  private val temp_3TV = CoreTemporaryVariable("temp_3", None)
  private val temp_4TV = CoreTemporaryVariable("temp_4", None)

  private val confusedInheritance = GraphPattern(
    "ConfusedInheritance",
    Seq(
      classGPP
    ),
    Seq(
      GraphPatternBody(
        Seq(
          PathExpressionConstraint(
            CoreVariableReference(classGPP),
            temp_1TV,
            ParentPathElement(None, classDeclarationIsFinalLink),
            classDeclType
          ),
          GraphPatternCompareConstraint(
            EqualityCompareFeature(),
            CoreVariableReference(temp_2TV),
            BooleanConstant(true)
          ),
          GraphPatternCompareConstraint(
            EqualityCompareFeature(),
            CoreVariableReference(temp_1TV),
            CoreVariableReference(temp_2TV)
          ),
          PathExpressionConstraint(
            CoreVariableReference(classGPP),
            temp_3TV,
            ParentPathElement(None, classDeclarationMembersLink),
            classDeclType
          ),
          GraphPatternCompareConstraint(
            EqualityCompareFeature(),
            CoreVariableReference(memberTV),
            CoreVariableReference(temp_3TV)
          ),
          PathExpressionConstraint(
            CoreVariableReference(memberTV),
            temp_4TV,
            ParentPathElement(None, fieldDeclarationVisibilityLink),
            fieldDeclType
          )
        )
      )
    ),
    None
  )

  test("Generate and write confusedInheritance graph pattern") {
    //  generate(greatGrandParent, "GPLang")
//    writeClass(generateGraphPattern(confusedInheritance), confusedInheritance.name)
    println(generateGraphPattern(confusedInheritance))
    //    writeClass(generate(path), path.name)
  }


  val clazz = ClassDeclaration("Foo", true, List(FieldDeclaration("bar", ProtectedVisibility())))

  test("Confused Inheritance") {
    val scope = new TFQueryScope(clazz)
    val matcher = EnginePool.getMatcher(ConfusedInheritance.instance(), scope, DifferentialReteBackendFactory.INSTANCE)
    println(matcher.getAllMatches)

    val indices = scope.getEngineContext.getBaseIndex
    indices.update(() => {
      indices.deleteNodeLinkInstance(clazz, NodeType(classOf[ClassDeclaration])("isFinal"), true)
      indices.deleteDataTypeInstance(true)
      indices.insertNodeLinkInstance(clazz, NodeType(classOf[ClassDeclaration])("isFinal"), false)
      indices.insertDataTypeInstance(false)
    })
    println(matcher.getAllMatches)

    indices.update(() => {
      indices.deleteNodeLinkInstance(clazz, NodeType(classOf[ClassDeclaration])("isFinal"), false)
      indices.deleteDataTypeInstance(false)
      indices.insertNodeLinkInstance(clazz, NodeType(classOf[ClassDeclaration])("isFinal"), true)
      indices.insertDataTypeInstance(true)
    })
    println(matcher.getAllMatches)

    EnginePool.disposeAllEngines()
  }
}
