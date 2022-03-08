package inca.transformEditScript

import inca.exampleEditScripts.JavaEditScripts._
import inca.treesitterLanguage.JavaTreeSitter._
import transformEditScript.TransformConcreteToAbstract
import org.scalatest.funsuite.AnyFunSuite

class EditScriptTransformationTest extends AnyFunSuite {

  def newTransformationObject(): TransformConcreteToAbstract = new TransformConcreteToAbstract(lang)

  test("No changes transformation") {
    val transformationObject: TransformConcreteToAbstract = newTransformationObject()
    val transformedEditScript: truechange.EditScript = transformationObject.transformTSEditScript(
      noChangesEditScript.edit_script,
      noChangesSrcCode,
      noChangesDestCode,
      noChangesEditScript.constructed_tree,
      litMap
    )
    freeResources(noChangesEditScript.constructed_tree, litMap, noChangesEditScript.edit_script)
    transformedEditScript.print()
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
    freeResources(updatedIntegerEditScript.constructed_tree, litMap, updatedIntegerEditScript.edit_script)
    transformedEditScript.print()
    assert(transformedEditScript.edits.length == 1)
  }

}
