package inca.transformEditScript.findSubstitutes

import com.sun.jna.Pointer
import inca.exampleEditScripts.JavaEditScripts._
import inca.treesitterAPI.TreeSitterTruediffLibrary
import inca.treesitterAPI.editscriptMappings.{ChildPrototype, EditTag, SugaredEdit}
import inca.treesitterAPI.treesitterMappings.{Subtree, TSDiffResult, TSTree}
import inca.treesitterLanguage.JavaTreeSitter.lang
import org.scalatest.funsuite.AnyFunSuite
import transformEditScript.TransformConcreteToAbstract

class findSubstitutesTest extends AnyFunSuite {

  def checkChildValidity(childId: Pointer, constructedTree: Subtree, transformationObject: TransformConcreteToAbstract): Boolean = {
    val childTag = TreeSitterTruediffLibrary.lib.ts_get_node_tag(constructedTree, childId)
    transformationObject.isAbstract(childTag)
  }

  def checkSubstitutesHelper(edit: SugaredEdit, transformationObject: TransformConcreteToAbstract, constructedTree: Subtree): Unit = {
    val children: Seq[ChildPrototype] = edit.edit_tag match {
      case EditTag.UNLOAD =>
        val child = new ChildPrototype(edit.sugar_edit.unload.kids.content)
        child.toArray(edit.sugar_edit.unload.kids.size).asInstanceOf[Array[ChildPrototype]]
      case EditTag.DETACH_UNLOAD =>
        val child = new ChildPrototype(edit.sugar_edit.detach_unload.kids.content)
        child.toArray(edit.sugar_edit.detach_unload.kids.size).asInstanceOf[Array[ChildPrototype]]
      case EditTag.LOAD =>
        if (edit.sugar_edit.load.is_leaf == 0) {
          val child = new ChildPrototype(edit.sugar_edit.load.edit_data.node.kids.content)
          child.toArray(edit.sugar_edit.load.edit_data.node.kids.size).asInstanceOf[Array[ChildPrototype]]
        } else {
          Array[ChildPrototype]()
        }
      case EditTag.LOAD_ATTACH =>
        if (edit.sugar_edit.load_attach.is_leaf == 0) {
          val child = new ChildPrototype(edit.sugar_edit.load_attach.edit_data.node.kids.content)
          child.toArray(edit.sugar_edit.load_attach.edit_data.node.kids.size).asInstanceOf[Array[ChildPrototype]]
        } else {
          Array[ChildPrototype]()
        }
      case _ =>
        Array[ChildPrototype]()
    }
    val transformedChildren: Seq[ChildPrototype] = transformationObject.findSubstitutes(children, constructedTree)
    assert(transformedChildren.forall { child => child.isInstanceOf[ChildPrototype] })
    assert(transformedChildren.forall { child => child.child_id != Pointer.NULL })
    assert(transformedChildren.forall { child => checkChildValidity(child.child_id, constructedTree, transformationObject) })
  }

  def checkSubstitutes(diffResult: TSDiffResult): Unit = {
    val transformationObject: TransformConcreteToAbstract = new TransformConcreteToAbstract(lang)
    val rootSubtree: Subtree = TreeSitterTruediffLibrary.lib.ts_get_root_subtree(diffResult.constructed_tree)
    val editArray: Seq[SugaredEdit] = {
      if (diffResult.edit_script.edits.size == 0) {
        Array[SugaredEdit]()
      } else {
        diffResult.edit_script.edits.content.toArray(diffResult.edit_script.edits.size).asInstanceOf[Array[SugaredEdit]]
      }
    }
    editArray.foreach { edit =>
      checkSubstitutesHelper(edit, transformationObject, rootSubtree)
    }
  }

  test("no changes children substitution") {
    checkSubstitutes(noChangesEditScript)
  }

  test("updated Integer children substitution") {
    checkSubstitutes(updatedIntegerEditScript)
  }

  test("updated variable name children substitution") {
    checkSubstitutes(updatedVariableNameEditScript)
  }

  test("updated identifier and Integer children substitution") {
    checkSubstitutes(updatedIdentifierAndIntegerEditScript)
  }

  test("updated multiple identifiers and Integer children substitution") {
    checkSubstitutes(updatedMultipleIdentifiersAndIntegerEditScript)
  }

  test("added new free line children substitution") {
    checkSubstitutes(addedNewFreeLineEditScript)
  }

  test("added new modifier children substitution") {
    checkSubstitutes(addedNewModifierEditScript)
  }

  test("added modifier children substitution") {
    checkSubstitutes(addedModifierEditScript)
  }

  test("added function children substitution") {
    checkSubstitutes(addedFunctionEditScript)
  }

  test("added new function modifier children substitution") {
    checkSubstitutes(addedNewFunctionModifierEditScript)
  }

  test("swap function order 1 children substitution") {
    checkSubstitutes(swapFunctionOrder1EditScript)
  }

  test("swap loops children substitution") {
    checkSubstitutes(swapLoopsEditScript)
  }
}
