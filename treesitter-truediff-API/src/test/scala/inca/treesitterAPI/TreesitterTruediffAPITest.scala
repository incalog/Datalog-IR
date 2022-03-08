package inca.treesitterAPI

import inca.exampleEditScripts.JavaEditScripts._
import inca.treesitterAPI.editscriptMappings.EditScript
import inca.treesitterLanguage.JavaTreeSitter.{createDiffResult, lang}
import org.scalatest.funsuite.AnyFunSuite

class TreesitterTruediffAPITest extends AnyFunSuite {

  test("No changes") {
    val editScript: EditScript = createDiffResult(
      noChangesSrcCode, noChangesDestCode
    ).edit_script
    println("====== Sequential EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, editScript)
    assert(TreeSitterTruediffLibrary.lib.ts_edit_script_length(editScript) == 0)
  }

  test("Updated Integer") {
    val editScript: EditScript = createDiffResult(
      updatedIntegerSrcCode, updatedIntegerDestCode
    ).edit_script
    println("====== Sequential EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, editScript)
    assert(TreeSitterTruediffLibrary.lib.ts_edit_script_length(editScript) == 1)
  }

  test("Update variable name") {
    val editScript: EditScript = createDiffResult(
      updatedVariableNameSrcCode, updatedVariableNameDestCode
    ).edit_script
    println("====== Sequential EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, editScript)
    assert(TreeSitterTruediffLibrary.lib.ts_edit_script_length(editScript) == 1)
  }

  test("Updated identifier and Integer") {
    val editScript: EditScript = createDiffResult(
      updatedIdentifierAndIntegerSrcCode, updatedIdentifierAndIntegerDestCode
    ).edit_script
    println("====== Sequential EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, editScript)
    assert(TreeSitterTruediffLibrary.lib.ts_edit_script_length(editScript) == 2)
  }

  test("Updated multiple identifiers and Integer") {
    val editScript: EditScript = createDiffResult(
      updatedMultipleIdentifiersAndIntegerSrcCode, updatedMultipleIdentifiersAndIntegerDestCode
    ).edit_script
    println("====== Sequential EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, editScript)
    assert(TreeSitterTruediffLibrary.lib.ts_edit_script_length(editScript) == 3)
  }

  test("Added new free line") {
    val editScript: EditScript = createDiffResult(
      addedNewFreeLineSrcCode, addedNewFreeLineDestCode
    ).edit_script
    println("====== Sequential EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, editScript)
    assert(TreeSitterTruediffLibrary.lib.ts_edit_script_length(editScript) == 4)
  }

  test("Added new modifier") {
    val editScript: EditScript = createDiffResult(
      addedNewModifierSrcCode, addedNewModifierDestCode
    ).edit_script
    println("====== Sequential EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, editScript)
    assert(TreeSitterTruediffLibrary.lib.ts_edit_script_length(editScript) == 5)
  }

  test("Added modifier") {
    val editScript: EditScript = createDiffResult(
      addedModifierSrcCode, addedModifierDestCode
    ).edit_script
    println("====== Sequential EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, editScript)
    assert(TreeSitterTruediffLibrary.lib.ts_edit_script_length(editScript) == 8)
  }

  test("Added function") {
    val editScript: EditScript = createDiffResult(
      addedFunctionSrcCode, addedFunctionDestCode
    ).edit_script
    println("====== Sequential EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, editScript)
    assert(TreeSitterTruediffLibrary.lib.ts_edit_script_length(editScript) == 35)
  }

  test("Added new function modifier") {
    val editScript: EditScript = createDiffResult(
      addedNewFunctionModifierSrcCode, addedNewFunctionModifierDestCode
    ).edit_script
    println("====== Sequential EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, editScript)
    assert(TreeSitterTruediffLibrary.lib.ts_edit_script_length(editScript) == 7)
  }

  test("Swap function order 1") {
    val editScript: EditScript = createDiffResult(
      swapFunctionOrder1SrcCode, swapFunctionOrder1DestCode
    ).edit_script
    println("====== Sequential EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, editScript)
    assert(TreeSitterTruediffLibrary.lib.ts_edit_script_length(editScript) == 4)
  }

  test("Swap loops") {
    val editScript: EditScript = createDiffResult(
      swapLoopsSrcCode, swapLoopsDestCode
    ).edit_script
    println("====== Sequential EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, editScript)
    assert(TreeSitterTruediffLibrary.lib.ts_edit_script_length(editScript) == 6)
  }

}
