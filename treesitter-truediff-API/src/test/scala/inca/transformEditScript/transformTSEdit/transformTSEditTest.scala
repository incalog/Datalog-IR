package inca.transformEditScript.transformTSEdit

import inca.exampleEditScripts.JavaEditScripts._
import inca.treesitterAPI.TreeSitterTruediffLibrary
import inca.treesitterAPI.editscriptMappings.{EditTag, SugaredEdit}
import inca.treesitterAPI.treesitterMappings.{Subtree, TSDiffResult}
import inca.treesitterLanguage.JavaTreeSitter.{lang, litMap}
import org.scalatest.funsuite.AnyFunSuite
import transformEditScript.TransformConcreteToAbstract

class transformTSEditTest extends AnyFunSuite {

  def checkEditType(edit: SugaredEdit, transformedEdit: truechange.Edit): Boolean = {
    edit.edit_tag match {
      case EditTag.ATTACH =>
        transformedEdit.isInstanceOf[truechange.Attach]
      case EditTag.DETACH =>
        transformedEdit.isInstanceOf[truechange.Detach]
      case EditTag.UNLOAD =>
        transformedEdit.isInstanceOf[truechange.Unload]
      case EditTag.LOAD =>
        transformedEdit.isInstanceOf[truechange.Load]
      case EditTag.LOAD_ATTACH =>
        transformedEdit.isInstanceOf[truechange.LoadAttach]
      case EditTag.DETACH_UNLOAD =>
        transformedEdit.isInstanceOf[truechange.DetachUnload]
      case EditTag.UPDATE =>
        transformedEdit.isInstanceOf[truechange.Update]
      case _ =>
        throw new IllegalArgumentException("Unexpected type for edit.")
    }
  }

  def checkEditComponents(edit: SugaredEdit, transformedEdit: truechange.Edit): Boolean = ???

  def checkTransformedEdit(diffResult: TSDiffResult, srcCode: String, destCode: String): Unit = {
    val transformationObject: TransformConcreteToAbstract = new TransformConcreteToAbstract(lang)
    val rootSubtree: Subtree = TreeSitterTruediffLibrary.lib.ts_get_root_subtree(diffResult.constructed_tree)
    val filteredEdits: Seq[SugaredEdit] = transformationObject.abstractTSEditScript(diffResult.edit_script, rootSubtree)
    filteredEdits.foreach { edit =>
      val transformedEdit: truechange.Edit = transformationObject.transformTSEdit(edit, filteredEdits, srcCode, destCode, rootSubtree, litMap)
      println(transformedEdit)
      assert(transformedEdit.isInstanceOf[truechange.Edit])
      assert(checkEditType(edit, transformedEdit))
    }
  }

  test("no changes edits transformation") {
    checkTransformedEdit(noChangesEditScript, noChangesSrcCode, noChangesDestCode)
  }

  test("updated Integer edits transformation") {
    checkTransformedEdit(updatedIntegerEditScript, updatedIntegerSrcCode, updatedIntegerDestCode)
  }

  test("updated variable name edits transformation") {
    checkTransformedEdit(updatedVariableNameEditScript, updatedVariableNameSrcCode, updatedVariableNameDestCode)
  }

  test("updated identifier and Integer edits transformation") {
    checkTransformedEdit(updatedIdentifierAndIntegerEditScript, updatedIdentifierAndIntegerSrcCode, updatedIdentifierAndIntegerDestCode)
  }

  test("updated multiple identifiers and Integer edits transformation") {
    checkTransformedEdit(updatedMultipleIdentifiersAndIntegerEditScript, updatedMultipleIdentifiersAndIntegerSrcCode, updatedMultipleIdentifiersAndIntegerDestCode)
  }

  test("added new free line edits transformation") {
    checkTransformedEdit(addedNewFreeLineEditScript, addedNewFreeLineSrcCode, addedNewFreeLineDestCode)
  }

  test("added new modifier edits transformation") {
    checkTransformedEdit(addedNewModifierEditScript, addedNewModifierSrcCode, addedNewModifierDestCode)
  }

  test("added modifier edits transformation") {
    checkTransformedEdit(addedModifierEditScript, addedModifierSrcCode, addedModifierDestCode)
  }

  test("added function edits transformation") {
    checkTransformedEdit(addedFunctionEditScript, addedFunctionSrcCode, addedFunctionDestCode)
  }

  test("added new function modifier edits transformation") {
    checkTransformedEdit(addedNewFunctionModifierEditScript, addedNewFunctionModifierSrcCode, addedNewFunctionModifierDestCode)
  }

  test("swap function order 1 edits transformation") {
    checkTransformedEdit(swapFunctionOrder1EditScript, swapFunctionOrder1SrcCode, swapFunctionOrder1DestCode)
  }

  test("swap loops edits transformation") {
    checkTransformedEdit(swapLoopsEditScript, swapLoopsSrcCode, swapLoopsDestCode)
  }
}
