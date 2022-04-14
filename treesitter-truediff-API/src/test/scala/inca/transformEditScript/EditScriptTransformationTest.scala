package inca.transformEditScript

import inca.exampleEditScripts.JavaEditScripts._
import inca.treesitterAPI.TreeSitterTruediffLibrary
import inca.treesitterLanguage.JavaTreeSitter._
import transformEditScript.TransformConcreteToAbstract
import org.scalatest.funsuite.AnyFunSuite

class EditScriptTransformationTest extends AnyFunSuite {

  def newTransformationObject(): TransformConcreteToAbstract = new TransformConcreteToAbstract(lang)

  test("test") {

    val transformationObject: TransformConcreteToAbstract = newTransformationObject()
    val src: String =
      """
       public class Main {
          int temp(int x) {
            return x + 5
          }
      }
      """
      val dest: String =
        """
        public class Main {
          int temp (int x) {
            return x + (5 + 6)
          }
        }
        """
//    val transformedEditScript: truechange.EditScript = transformationObject.transformTSEditScript(
//      createDiffResult(src, dest).edit_script,
//      src,
//      dest,
//      createDiffResult(src, dest).constructed_tree,
//      litMap
//    )
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, createDiffResult(src, dest).edit_script)
  }

  test("No changes transformation") {
    val transformationObject: TransformConcreteToAbstract = newTransformationObject()
    val transformedEditScript: truechange.EditScript = transformationObject.transformTSEditScript(
      noChangesEditScript.edit_script,
      noChangesSrcCode,
      noChangesDestCode,
      noChangesEditScript.constructed_tree,
      litMap
    )
    println("====== Transformed EditScript ======")
    transformedEditScript.print()
    println("\n====== Old EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, noChangesEditScript.edit_script)
    assert(transformedEditScript.edits.isEmpty)
  }

  test("Updated Integer transformation") {
    val transformationObject: TransformConcreteToAbstract = newTransformationObject()
    val transformedEditScript: truechange.EditScript = transformationObject.transformTSEditScript(
      updatedIntegerEditScript.edit_script,
      updatedIntegerSrcCode,
      updatedIntegerDestCode,
      updatedIntegerEditScript.constructed_tree,
      litMap)
    println("====== Transformed EditScript ======")
    transformedEditScript.print()
    println("\n====== Old EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, updatedIntegerEditScript.edit_script)
    assert(transformedEditScript.edits.length == 1)
  }

  test("Updated variable name transformation") {
    val transformationObject: TransformConcreteToAbstract = newTransformationObject()
    val transformedEditScript: truechange.EditScript = transformationObject.transformTSEditScript(
      updatedVariableNameEditScript.edit_script,
      updatedVariableNameSrcCode,
      updatedVariableNameDestCode,
      updatedVariableNameEditScript.constructed_tree,
      litMap
    )
    println("====== Transformed EditScript ======")
    transformedEditScript.print()
    println("\n====== Old EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, updatedVariableNameEditScript.edit_script)
    assert(transformedEditScript.edits.length == 1)
  }

  test("Updated identifier and Integer transformation") {
    val transformationObject: TransformConcreteToAbstract = newTransformationObject()
    val transformedEditScript: truechange.EditScript = transformationObject.transformTSEditScript(
      updatedIdentifierAndIntegerEditScript.edit_script,
      updatedIdentifierAndIntegerSrcCode,
      updatedIdentifierAndIntegerDestCode,
      updatedIdentifierAndIntegerEditScript.constructed_tree,
      litMap
    )
    println("====== Transformed EditScript ======")
    transformedEditScript.print()
    println("\n====== Old EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, updatedIdentifierAndIntegerEditScript.edit_script)
    assert(transformedEditScript.edits.length == 2)
  }

  test("Updated multiple identifiers and Integer transformation") {
    val transformationObject: TransformConcreteToAbstract = newTransformationObject()
    val transformedEditScript: truechange.EditScript = transformationObject.transformTSEditScript(
      updatedMultipleIdentifiersAndIntegerEditScript.edit_script,
      updatedMultipleIdentifiersAndIntegerSrcCode,
      updatedMultipleIdentifiersAndIntegerDestCode,
      updatedMultipleIdentifiersAndIntegerEditScript.constructed_tree,
      litMap
    )
    println("====== Transformed EditScript ======")
    transformedEditScript.print()
    println("\n====== Old EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, updatedMultipleIdentifiersAndIntegerEditScript.edit_script)
    assert(transformedEditScript.edits.length == 3)
  }

  test("Added new free line transformation") {
    val transformationObject: TransformConcreteToAbstract = newTransformationObject()
    val transformedEditScript: truechange.EditScript = transformationObject.transformTSEditScript(
      addedNewFreeLineEditScript.edit_script,
      addedNewFreeLineSrcCode,
      addedNewFreeLineDestCode,
      addedNewFreeLineEditScript.constructed_tree,
      litMap
    )
    println("====== Transformed EditScript ======")
    transformedEditScript.print()
    println("\n====== Old EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, addedNewFreeLineEditScript.edit_script)
    assert(transformedEditScript.edits.isEmpty)
  }

  test("Added new modifier transformation") {
    val transformationObject: TransformConcreteToAbstract = newTransformationObject()
    val transformedEditScript: truechange.EditScript = transformationObject.transformTSEditScript(
      addedNewModifierEditScript.edit_script,
      addedNewModifierSrcCode,
      addedNewModifierDestCode,
      addedNewModifierEditScript.constructed_tree,
      litMap
    )
    println("====== Transformed EditScript ======")
    transformedEditScript.print()
    println("\n====== Old EditScript ======")
    TreeSitterTruediffLibrary.lib.print_edit_script(lang, addedNewModifierEditScript.edit_script)
    assert(transformedEditScript.edits.length == 3)
  }

}
