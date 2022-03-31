package inca.abstractTreesitterAPI

import inca.abstractTreesitterAPI.editscriptMappings.{ChildPrototype, EditScript, EditTag, SugaredEdit}
import inca.abstractTreesitterAPI.treesitterMappings.{TSDiffResult, TSNode}
import inca.abstractTreesitterAPI.util.{CLibrary, FILE}
import inca.codeExamples.JavaCodeExamples._
import inca.treesitterLanguage.JavaTreeSitter.{createDiffResult, freeResources, lang, litMap}
import inca.utils.JSONReader.getFieldEntries
import org.scalatest.funsuite.AnyFunSuite

class AbstractTreesitterAPITest extends AnyFunSuite {

  def correctChildrenFields(edit: SugaredEdit): Boolean = {
    edit.edit_tag match {
      case EditTag.ATTACH => true
      case EditTag.DETACH => true
      case EditTag.UNLOAD =>
        val nodeFields: Array[String] = getFieldEntries(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.sugar_edit.unload.tag))
        val unloadKids: Array[ChildPrototype] = {
          if (edit.sugar_edit.unload.kids.size == 0) {
            Array[ChildPrototype]()
          } else {
            val unloadChild: ChildPrototype = new ChildPrototype(edit.sugar_edit.unload.kids.content)
            unloadChild.toArray(edit.sugar_edit.unload.kids.size).asInstanceOf[Array[ChildPrototype]]
          }
        }
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
          val nodeFields: Array[String] = getFieldEntries(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.sugar_edit.load.tag))
          val loadChild: ChildPrototype = new ChildPrototype(edit.sugar_edit.load.kids.content)
          val loadKids: Array[ChildPrototype] = loadChild.toArray(edit.sugar_edit.load.kids.size).asInstanceOf[Array[ChildPrototype]]
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
          val nodeFields: Array[String] = getFieldEntries(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.sugar_edit.load_attach.tag))
          val loadAttachChild: ChildPrototype = new ChildPrototype(edit.sugar_edit.load_attach.kids.content)
          val loadAttachKids: Array[ChildPrototype] = loadAttachChild.toArray(edit.sugar_edit.load_attach.kids.size).asInstanceOf[Array[ChildPrototype]]
          loadAttachKids.forall { child =>
            if (child.is_field == 1) {
              nodeFields.contains(TreeSitterTruediffLibrary.lib.ts_language_field_name_for_id(lang, child.child_name.field_id))
            } else {
              true
            }
          }
        }
      case EditTag.DETACH_UNLOAD =>
        val nodeFields: Array[String] = getFieldEntries(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.sugar_edit.detach_unload.tag))
        val detachUnloadKids: Array[ChildPrototype] = {
          if (edit.sugar_edit.detach_unload.kids.size == 0) {
            Array[ChildPrototype]()
          } else {
            val detachUnloadChild: ChildPrototype = new ChildPrototype(edit.sugar_edit.detach_unload.kids.content)
            detachUnloadChild.toArray(edit.sugar_edit.detach_unload.kids.size).asInstanceOf[Array[ChildPrototype]]
          }
        }
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
    assert(editArray.forall { edit => correctChildrenFields(edit) })
  }

  def testEditScript(srcCode: String, destCode: String): Unit = {
    val diffResult: TSDiffResult = createDiffResult(srcCode, destCode)
    val rootNode: TSNode.ByValue = TreeSitterTruediffLibrary.lib.ts_tree_root_node(diffResult.constructed_tree)
    val file: FILE = CLibrary.lib.fopen("abstract-treesitter-truediff-API/src/main/treeGraph/constructedTreeGraph.txt", "w")
    TreeSitterTruediffLibrary.lib.ts_tree_diff_graph(rootNode, lang, file)
    CLibrary.lib.fclose(file)
    testFields(diffResult.edit_script)
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, diffResult.edit_script)
    freeResources(diffResult.constructed_tree, litMap, diffResult.edit_script)
  }

  test("Correct links and fields in annotation") {
    testEditScript(annotationSrcCode, annotationDestCode)
  }

  test("Correct links and fields in annotation_type_declaration") {
    testEditScript(annotationTypeDeclarationSrcCode, annotationTypeDeclarationDestCode)
  }

  test("Correct links and fields in annotation_type_element_declaration") {
    testEditScript(annotationTypeElementDeclarationSrcCode, annotationTypeElementDeclarationDestCode)
  }

  test("Correct links and fields in array_access") {
    testEditScript(arrayAccessSrcCode, arrayAccessDestCode)
  }

  test("Correct links and fields in array_creation_expression") {
    testEditScript(arrayCreationExpressionSrcCode, arrayCreationExpressionDestCode)
  }

  test("Correct links and fields in array_type") {
    testEditScript(arrayTypeSrcCode, arrayTypeDestCode)
  }

  test("Correct links and fields in assignment_expression") {
    testEditScript(assignmentExpressionSrcCode, assignmentExpressionDestCode)
  }

  test("Correct links and fields in binary_expression") {
    testEditScript(binaryExpressionSrcCode, binaryExpressionDestCode)
  }

  test("Correct links and fields in cast_expression") {
    testEditScript(castExpressionSrcCode, castExpressionDestCode)
  }

  test("Correct links and fields in catch_clause") {
    testEditScript(catchClauseSrcCode, catchClauseDestCode)
  }

  test("Correct links and fields in catch_formal_parameter") {
    testEditScript(catchFormalParameterSrcCode, catchFormalParameterDestCode)
  }

  test("Correct links and fields in class_declaration") {
    testEditScript(classDeclarationSrcCode, classDeclarationDestCode)
  }

  test("Correct links and fields in constant_declaration") {
    testEditScript(constantDeclarationSrcCode, constantDeclarationDestCode)
  }

  test("Correct links and fields in constructor_declaration") {
    testEditScript(constructorDeclarationSrcCode, constructorDeclarationDestCode)
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
