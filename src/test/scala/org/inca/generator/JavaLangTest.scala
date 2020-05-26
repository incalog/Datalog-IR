package org.inca.generator

import org.inca.analyzedLangs.{BooleanConstant, ClassDeclaration, ClassMember, FieldDeclaration, ProtectedVisibility}
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.inca.gen.Pipeline.generateGraphPattern
import org.inca.generator.Util.writeClass
import org.inca.generator.generated.ConfusedInheritance
import org.inca.incer.indices.{EnginePool, TFQueryScope}
import org.inca.lang.Core._
import org.inca.lang.Gp._
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

  private val memberTV = TemporaryVariable("member", Some(classMemberType))

  private val temp_1TV = TemporaryVariable("temp_1", None)
  private val temp_2TV = TemporaryVariable("temp_2", None)
  private val temp_3TV = TemporaryVariable("temp_3", None)
  private val temp_4TV = TemporaryVariable("temp_4", None)

  /**
   * pattern ConfusedInheritance(class: Class) {
   *   Class.isFinal(class, temp1)
   *   temp2 = BooleanConstant(true)
   *   temp1 = temp2
   *   Class.members(class, temp3)
   *   member = temp3
   *   FieldDeclaration.visibility(member, temp4)
   * }
   */
  private val confusedInheritance = GraphPattern(
    "ConfusedInheritance",
    Seq(
      classGPP
    ),
    Seq(
      GraphPatternBody(
        Seq(
          PathExpressionConstraint(
            VariableReference(classGPP),
            temp_1TV,
            ParentPathElement(None, classDeclarationIsFinalLink),
            classDeclType
          ),
          CompareConstraint(
            EqualityCompareFeature(),
            VariableReference(temp_2TV),
            BooleanLiteral(true)
          ),
          CompareConstraint(
            EqualityCompareFeature(),
            VariableReference(temp_1TV),
            VariableReference(temp_2TV)
          ),
          PathExpressionConstraint(
            VariableReference(classGPP),
            temp_3TV,
            ParentPathElement(None, classDeclarationMembersLink),
            classDeclType
          ),
          CompareConstraint(
            EqualityCompareFeature(),
            VariableReference(memberTV),
            VariableReference(temp_3TV)
          ),
          PathExpressionConstraint(
            VariableReference(memberTV),
            temp_4TV,
            ParentPathElement(None, fieldDeclarationVisibilityLink),
            fieldDeclType
          )
        )
      )
    ),
    None
  )



  val clazz = ClassDeclaration("Foo", true, List(FieldDeclaration("bar", ProtectedVisibility())))

  test("Confused Inheritance") {
    writeClass(confusedInheritance)

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
