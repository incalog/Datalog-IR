package inca.abstractTreesitterAPI

import com.sun.jna.Pointer
import inca.abstractTreesitterAPI.editscriptMappings.{ChildPrototype, EditScript, EditTag, SugaredEdit}
import inca.abstractTreesitterAPI.treesitterMappings.{TSDiffResult, TSNode}
import inca.abstractTreesitterAPI.util.{CLibrary, FILE}
import inca.codeExamples.JavaCodeExamples._
import inca.treesitterLanguage.JavaTreeSitter.{createDiffResult, freeResources, lang, litMap, parser}
import inca.utils.JSONReader.{getFieldChildren, getFieldEntries}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

class AbstractTreesitterAPITest extends AnyFunSuite with BeforeAndAfterAll {

  override protected def afterAll(): Unit = {
    TreeSitterTruediffLibrary.lib.ts_parser_delete(parser)
    TreeSitterTruediffLibrary.lib.ts_literal_map_destroy(litMap)
  }

  def checkChildSymbol(editArray: Seq[SugaredEdit], childId: Pointer): String = {
    val childEdit: Option[SugaredEdit] = editArray.find { edit => edit.getId == childId.toString }
    childEdit match {
      case Some(value) => TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, value.getTag)
      case None => throw new NoSuchElementException(s"No such node id exists: ${childId.toString}.")
    }
  }

  def correctChildrenFields(edit: SugaredEdit, editArray: Seq[SugaredEdit]): Boolean = {
    edit.edit_tag match {
      case EditTag.ATTACH => true
      case EditTag.DETACH => true
      case EditTag.UNLOAD =>
        val nodeFields: Array[String] = getFieldEntries(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.getTag))
        val unloadKids: Array[ChildPrototype] = {
          if (edit.sugar_edit.unload.kids.size == 0) {
            Array[ChildPrototype]()
          } else {
            val unloadChild: ChildPrototype = new ChildPrototype(edit.sugar_edit.unload.kids.content)
            unloadChild.toArray(edit.sugar_edit.unload.kids.size).asInstanceOf[Array[ChildPrototype]]
          }
        }
//        unloadKids.forall { child =>
//          if (child.is_field == 1) {
//            val tempEditArray: Seq[SugaredEdit] = editArray.filter { edits => edits.getId != edit.getId }
//            val childSymbol: String = checkChildSymbol(tempEditArray, child.child_id)
//            val fieldChildren: Array[String] = getFieldChildren(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.getTag), TreeSitterTruediffLibrary.lib.ts_language_field_name_for_id(lang, child.child_name.field_id))
//            fieldChildren.contains(childSymbol)
//          } else {
//            true
//          }
//        }
        unloadKids.forall { child =>
          if (child.is_field == 1) {
            nodeFields.contains(TreeSitterTruediffLibrary.lib.ts_language_field_name_for_id(lang, child.child_name.field_id))
          } else {
            true
          }
        }
      case EditTag.LOAD =>
        if (edit.sugar_edit.load.is_leaf == 1 || edit.sugar_edit.load.kids.size == 0) {
          true
        } else {
          val nodeFields: Array[String] = getFieldEntries(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.getTag))
          val loadChild: ChildPrototype = new ChildPrototype(edit.sugar_edit.load.kids.content)
          val loadKids: Array[ChildPrototype] = loadChild.toArray(edit.sugar_edit.load.kids.size).asInstanceOf[Array[ChildPrototype]]
//          if (loadKids.nonEmpty) {
//            loadKids.forall { child =>
//              if (child.is_field == 1) {
//                val tempEditArray: Seq[SugaredEdit] = editArray.filter { edits => edits.getId != edit.getId }
//                val childSymbol: String = checkChildSymbol(tempEditArray, child.child_id)
//                val fieldChildren: Array[String] = getFieldChildren(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.getTag), TreeSitterTruediffLibrary.lib.ts_language_field_name_for_id(lang, child.child_name.field_id))
//                fieldChildren.contains(childSymbol)
//              } else {
//                true
//              }
//            }
//          }
          loadKids.forall { child =>
            if (child.is_field == 1) {
              nodeFields.contains(TreeSitterTruediffLibrary.lib.ts_language_field_name_for_id(lang, child.child_name.field_id))
            } else {
              true
            }
          }
        }
      case EditTag.LOAD_ATTACH =>
        if (edit.sugar_edit.load_attach.is_leaf == 1 || edit.sugar_edit.load_attach.kids.size == 0) {
          true
        } else {
          val nodeFields: Array[String] = getFieldEntries(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.getTag))
          val loadAttachChild: ChildPrototype = new ChildPrototype(edit.sugar_edit.load_attach.kids.content)
          val loadAttachKids: Array[ChildPrototype] = loadAttachChild.toArray(edit.sugar_edit.load_attach.kids.size).asInstanceOf[Array[ChildPrototype]]
//          loadAttachKids.forall { child =>
//            if (child.is_field == 1) {
//              val tempEditArray: Seq[SugaredEdit] = editArray.filter { edits => edits.getId != edit.getId }
//              val childSymbol: String = checkChildSymbol(tempEditArray, child.child_id)
//              val fieldChildren: Array[String] = getFieldChildren(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.getTag), TreeSitterTruediffLibrary.lib.ts_language_field_name_for_id(lang, child.child_name.field_id))
//              fieldChildren.contains(childSymbol)
//            } else {
//              true
//            }
//          }
          loadAttachKids.forall { child =>
            if (child.is_field == 1) {
              nodeFields.contains(TreeSitterTruediffLibrary.lib.ts_language_field_name_for_id(lang, child.child_name.field_id))
            } else {
              true
            }
          }
        }
      case EditTag.DETACH_UNLOAD =>
        val nodeFields: Array[String] = getFieldEntries(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.getTag))
        val detachUnloadKids: Array[ChildPrototype] = {
          if (edit.sugar_edit.detach_unload.kids.size == 0) {
            Array[ChildPrototype]()
          } else {
            val detachUnloadChild: ChildPrototype = new ChildPrototype(edit.sugar_edit.detach_unload.kids.content)
            detachUnloadChild.toArray(edit.sugar_edit.detach_unload.kids.size).asInstanceOf[Array[ChildPrototype]]
          }
        }
//        detachUnloadKids.forall { child =>
//          if (child.is_field == 1) {
//            val tempEditArray: Seq[SugaredEdit] = editArray.filter { edits => edits.getId != edit.getId }
//            val childSymbol: String = checkChildSymbol(tempEditArray, child.child_id)
//            val fieldChildren: Array[String] = getFieldChildren(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.getTag), TreeSitterTruediffLibrary.lib.ts_language_field_name_for_id(lang, child.child_name.field_id))
//            fieldChildren.contains(childSymbol)
//          } else {
//            true
//          }
//        }
        detachUnloadKids.forall { child =>
          if (child.is_field == 1) {
            nodeFields.contains(TreeSitterTruediffLibrary.lib.ts_language_field_name_for_id(lang, child.child_name.field_id))
          } else {
            true
          }
        }
      case EditTag.UPDATE => true
      case _ =>
        throw new IllegalArgumentException(s"Unexpected edit tag: ${edit.edit_tag}")
    }
  }

  def testFields(editScript: EditScript): Unit = {
    val editArray: Seq[SugaredEdit] = {
      if (editScript.edits.size == 0) {
        Array[SugaredEdit]()
      } else {
        editScript.edits.content.toArray(editScript.edits.size).asInstanceOf[Array[SugaredEdit]]
      }
    }
    assert(editArray.forall { edit => correctChildrenFields(edit, editArray) })
  }

  def testEditScript(srcCode: String, destCode: String): Unit = {
    val diffResult: TSDiffResult = createDiffResult(srcCode, destCode)
    val rootNode: TSNode.ByValue = TreeSitterTruediffLibrary.lib.ts_tree_root_node(diffResult.constructed_tree)
    val file: FILE = CLibrary.lib.fopen("abstract-treesitter-truediff-API/src/main/treeGraph/constructedTreeGraph.txt", "w")
    TreeSitterTruediffLibrary.lib.ts_tree_diff_graph(rootNode, lang, file)
    CLibrary.lib.fclose(file)
    testFields(diffResult.edit_script)
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, diffResult.edit_script)
    freeResources(diffResult.constructed_tree, diffResult.edit_script)
  }

  test("TODO: Fix attaching of node without loading it before") {
    testEditScript(attachWithoutLoadSrcCode, attachWithoutLoadDestCode)
  }

  test("TODO: Fix link tracking when multiple Attach and Detach operations occur in EditScript") {
    testEditScript(incorrectLinkTrackingWithMultipleAttachAndDetachOperationsSrcCode, incorrectLinkTrackingWithMultipleAttachAndDetachOperationsDestCode)
  }

  test("Correct fields in annotation") {
    testEditScript(annotationSrcCode, annotationDestCode)
  }

  test("Correct fields in annotation_type_declaration") {
    testEditScript(annotationTypeDeclarationSrcCode, annotationTypeDeclarationDestCode)
  }

  test("Correct fields in annotation_type_element_declaration") {
    testEditScript(annotationTypeElementDeclarationSrcCode, annotationTypeElementDeclarationDestCode)
  }

  test("Correct fields in array_access") {
    testEditScript(arrayAccessSrcCode, arrayAccessDestCode)
  }

  test("Correct fields in array_creation_expression") {
    testEditScript(arrayCreationExpressionSrcCode, arrayCreationExpressionDestCode)
  }

  test("Correct fields in array_type") {
    testEditScript(arrayTypeSrcCode, arrayTypeDestCode)
  }

  test("Correct fields in assignment_expression") {
    testEditScript(assignmentExpressionSrcCode, assignmentExpressionDestCode)
  }

  test("Correct fields in binary_expression") {
    testEditScript(binaryExpressionSrcCode, binaryExpressionDestCode)
  }

  test("Correct fields in cast_expression") {
    testEditScript(castExpressionSrcCode, castExpressionDestCode)
  }

  test("Correct fields in catch_clause") {
    testEditScript(catchClauseSrcCode, catchClauseDestCode)
  }

  test("Correct fields in catch_formal_parameter") {
    testEditScript(catchFormalParameterSrcCode, catchFormalParameterDestCode)
  }

  test("Correct fields in class_declaration") {
    testEditScript(classDeclarationSrcCode, classDeclarationDestCode)
  }

  test("Correct fields in constant_declaration") {
    testEditScript(constantDeclarationSrcCode, constantDeclarationDestCode)
  }

  test("Correct fields in constructor_declaration") {
    testEditScript(constructorDeclarationSrcCode, constructorDeclarationDestCode)
  }

  test("Correct fields in do_statement") {
    testEditScript(doStatementSrcCode, doStatementDestCode)
  }

  test("Correct fields in element_value_pair") {
    testEditScript(elementValuePairSrcCode, elementValuePairDestCode)
  }

  test("Correct fields in enhanced_for_statement") {
    testEditScript(enhancedForStatementSrcCode, enhancedForStatementDestCode)
  }

  test("Correct fields in enum_constant") {
    testEditScript(enumConstantSrcCode, enumConstantDestCode)
  }

  test("Correct fields in enum_declaration") {
    testEditScript(enumDeclarationSrcCode, enumDeclarationDestCode)
  }

  test("Correct fields in explicit_constructor_invocation") {
    testEditScript(explicitConstructorInvocationSrcCode, explicitConstructorInvocationDestCode)
  }

  test("Correct fields in field_access") {
    testEditScript(fieldAccessSrcCode, fieldAccessDestCode)
  }

  test("Correct fields in field_declaration") {
    testEditScript(fieldDeclarationSrcCode, fieldDeclarationDestCode)
  }

  test("Correct fields in for_statement") {
    testEditScript(forStatementSrcCode, forStatementDestCode)
  }

  test("Correct fields in formal_parameter") {
    testEditScript(formalParameterSrcCode, formalParameterDestCode)
  }

  test("Correct fields in if_statement") {
    testEditScript(ifStatementSrcCode, ifStatementDestCode)
  }

  test("Correct fields in instanceof_expression") {
    testEditScript(instanceofExpressionSrcCode, instanceofExpressionDestCode)
  }

  test("Correct fields in interface_declaration") {
    testEditScript(interfaceDeclarationSrcCode, interfaceDeclarationDestCode)
  }

  test("Correct fields in lambda_expression") {
    testEditScript(lambdaExpressionSrcCode, lambdaExpressionDestCode)
  }

  test("Correct fields in local_variable_declaration") {
    testEditScript(localVariableDeclarationSrcCode, localVariableDeclarationDestCode)
  }

  test("Correct fields in marker_annotation") {
    testEditScript(markerAnnotationSrcCode, markerAnnotationDestCode)
  }

  test("Correct fields in method_declaration") {
    testEditScript(methodDeclarationSrcCode, methodDeclarationDestCode)
  }

  test("Correct fields in method_invocation") {
    testEditScript(methodInvocationSrcCode, methodInvocationDestCode)
  }

  test("Correct fields in module_declaration") {
    testEditScript(moduleDeclarationSrcCode, moduleDeclarationDestCode)
  }

  test("Correct fields in object_creation_expression") {
    testEditScript(objectCreationExpressionSrcCode, objectCreationExpressionDestCode)
  }

  test("Correct fields in record_declaration") {
    testEditScript(recordDeclarationSrcCode, recordDeclarationDestCode)
  }

  test("Correct fields in resource") {
    testEditScript(resourceSrcCode, resourceDestCode)
  }

  test("Correct fields in scoped_identifier") {
    testEditScript(scopedIdentifierSrcCode, scopedIdentifierDestCode)
  }

  test("Correct fields in switch_expression") {
    testEditScript(switchExpressionSrcCode, switchExpressionDestCode)
  }

  test("Correct fields in synchronized_statement") {
    testEditScript(synchronizedStatementSrcCode, synchronizedStatementDestCode)
  }

  test("Correct fields in ternary_expression") {
    testEditScript(ternaryExpressionSrcCode, ternaryExpressionDestCode)
  }

  test("Correct fields in try_statement") {
    testEditScript(tryStatementSrcCode, tryStatementDestCode)
  }

  test("Correct fields in try_with_resources_statement") {
    testEditScript(tryWithResourcesStatementSrcCode, tryWithResourcesStatementDestCode)
  }

  test("Correct fields in unary_expression") {
    testEditScript(unaryExpressionSrcCode, unaryExpressionDestCode)
  }

  test("Correct fields in variable_declarator") {
    testEditScript(variableDeclaratorSrcCode, variableDeclaratorDestCode)
  }

  test("Correct fields in while_statement") {
    testEditScript(whileStatementSrcCode, whileStatementDestCode)
  }

  test("No Changes") {
    testEditScript(noChangesSrcCode, noChangesDestCode)
  }

  test("Updated Integer") {
    testEditScript(updatedIntegerSrcCode, updatedIntegerDestCode)
  }

  test("Updated variable name") {
    testEditScript(updatedVariableNameSrcCode, updatedVariableNameDestCode)
  }

  test("Updated identifier and Integer") {
    testEditScript(updatedIdentifierAndIntegerSrcCode, updatedIdentifierAndIntegerDestCode)
  }

  test("Updated multiple identifier and Integer") {
    testEditScript(updatedMultipleIdentifiersAndIntegerSrcCode, updatedMultipleIdentifiersAndIntegerDestCode)
  }

  test("Added new free line") {
    testEditScript(addedNewFreeLineSrcCode, addedNewFreeLineDestCode)
  }

  test("Added new modifier") {
    testEditScript(addedNewModifierSrcCode, addedNewModifierDestCode)
  }

  test("Added modifier") {
    testEditScript(addedModifierSrcCode, addedModifierDestCode)
  }

  test("Added function") {
    testEditScript(addedFunctionSrcCode, addedFunctionDestCode)
  }

  test("Added new function modifier") {
    testEditScript(addedNewFunctionModifierSrcCode, addedNewFunctionModifierDestCode)
  }

  test("Swap function order 1") {
    testEditScript(swapFunctionOrder1SrcCode, swapFunctionOrder1DestCode)
  }

  test("Swap loops") {
    testEditScript(swapLoopsSrcCode, swapLoopsDestCode)
  }
}
