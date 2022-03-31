package inca.transformEditScript.filterEdits

import com.sun.jna.Pointer
import inca.exampleEditScripts.JavaEditScripts._
import inca.treesitterAPI.TreeSitterTruediffLibrary
import inca.treesitterAPI.editscriptMappings.{ChildPrototype, EditTag, SugaredEdit}
import inca.treesitterAPI.treesitterMappings.{Subtree, TSDiffResult}
import inca.treesitterLanguage.JavaTreeSitter.lang
import org.scalatest.funsuite.AnyFunSuite
import transformEditScript.TransformConcreteToAbstract

class filterEditsTest extends AnyFunSuite {

  def checkFilteredChildren(edit: SugaredEdit, constructedTree: Subtree, transformationObject: TransformConcreteToAbstract): Unit = {
    edit.edit_tag match {
      case EditTag.UNLOAD =>
        val children: Seq[ChildPrototype] = {
          if (edit.sugar_edit.unload.kids.size == 0) {
            Array[ChildPrototype]()
          } else {
            val child: ChildPrototype = new ChildPrototype(edit.sugar_edit.unload.kids.content)
            child.toArray(edit.sugar_edit.unload.kids.size).asInstanceOf[Array[ChildPrototype]]
          }
        }
        assert(children.forall { child => child.isInstanceOf[ChildPrototype] })
        assert(children.forall { child => child.child_id != Pointer.NULL })
        assert(children.forall { child => transformationObject.isAbstract(TreeSitterTruediffLibrary.lib.ts_get_node_tag(constructedTree, child.child_id)) })
      case EditTag.DETACH_UNLOAD =>
        val children: Seq[ChildPrototype] = {
          if (edit.sugar_edit.detach_unload.kids.size == 0) {
            Array[ChildPrototype]()
          } else {
            val child: ChildPrototype = new ChildPrototype(edit.sugar_edit.detach_unload.kids.content)
            child.toArray(edit.sugar_edit.detach_unload.kids.size).asInstanceOf[Array[ChildPrototype]]
          }
        }
        children.foreach { child => println(child) }
        assert(children.forall { child => child.isInstanceOf[ChildPrototype] })
        assert(children.forall { child => child.child_id != Pointer.NULL })
        assert(children.forall { child => transformationObject.isAbstract(TreeSitterTruediffLibrary.lib.ts_get_node_tag(constructedTree, child.child_id)) })
      case EditTag.LOAD =>
        val children: Seq[ChildPrototype] = {
          if (edit.sugar_edit.load.is_leaf == 0) {
            val tempChild = new ChildPrototype(edit.sugar_edit.load.edit_data.node.kids.content)
            tempChild.toArray(edit.sugar_edit.load.edit_data.node.kids.size).asInstanceOf[Array[ChildPrototype]]
          } else {
            Array[ChildPrototype]()
          }
        }
        children.foreach { child => println(child) }
        assert(children.forall { child => child.isInstanceOf[ChildPrototype] })
        assert(children.forall { child => child.child_id != Pointer.NULL })
        assert(children.forall { child => transformationObject.isAbstract(TreeSitterTruediffLibrary.lib.ts_get_node_tag(constructedTree, child.child_id)) })
      case EditTag.LOAD_ATTACH =>
        val children: Seq[ChildPrototype] = {
          if (edit.sugar_edit.load_attach.is_leaf == 0) {
            val tempChild = new ChildPrototype(edit.sugar_edit.load_attach.edit_data.node.kids.content)
            tempChild.toArray(edit.sugar_edit.load_attach.edit_data.node.kids.size).asInstanceOf[Array[ChildPrototype]]
          } else {
            Array[ChildPrototype]()
          }
        }
        children.foreach { child => println(child) }
        assert(children.forall { child => child.isInstanceOf[ChildPrototype] })
        assert(children.forall { child => child.child_id != Pointer.NULL })
        assert(children.forall { child => transformationObject.isAbstract(TreeSitterTruediffLibrary.lib.ts_get_node_tag(constructedTree, child.child_id)) })
      case _ =>
    }
  }

  def checkFilteredEdits(diffResult: TSDiffResult): Unit = {
    val transformationObject: TransformConcreteToAbstract = new TransformConcreteToAbstract(lang)
    val rootSubtree: Subtree = TreeSitterTruediffLibrary.lib.ts_get_root_subtree(diffResult.constructed_tree)
    val editArray: Seq[SugaredEdit] = {
      if (diffResult.edit_script.edits.size == 0) {
        Array[SugaredEdit]()
      } else {
        diffResult.edit_script.edits.content.toArray(diffResult.edit_script.edits.size).asInstanceOf[Array[SugaredEdit]]
      }
    }
    val filteredEdits: Seq[SugaredEdit] = transformationObject.filterEdits(editArray, rootSubtree, Map[String, Seq[ChildPrototype]]())
    filteredEdits.foreach { edit => transformationObject.isAbstract(edit.getTag) }
    filteredEdits.foreach { edit => checkFilteredChildren(edit, rootSubtree, transformationObject) }
  }

  test("no changes filter") {
    checkFilteredEdits(noChangesEditScript)
  }

  test("updated Integer filter") {
    checkFilteredEdits(updatedIntegerEditScript)
  }

  test("updated variable name filter") {
    checkFilteredEdits(updatedVariableNameEditScript)
  }

  test("updated identifier and Integer filter") {
    checkFilteredEdits(updatedIdentifierAndIntegerEditScript)
  }

  test("updated multiple identifiers and Integer filter") {
    checkFilteredEdits(updatedMultipleIdentifiersAndIntegerEditScript)
  }

  test("added new free line filter") {
    checkFilteredEdits(addedNewFreeLineEditScript)
  }

  test("added new modifier filter") {
    checkFilteredEdits(addedNewModifierEditScript)
  }

  test("added modifier filter") {
    checkFilteredEdits(addedModifierEditScript)
  }

  test("added function filter") {
    checkFilteredEdits(addedFunctionEditScript)
  }

  test("added new function modifier filter") {
    checkFilteredEdits(addedNewFunctionModifierEditScript)
  }

  test("swap function order 1 filter") {
    checkFilteredEdits(swapFunctionOrder1EditScript)
  }

  test("swap loops filter") {
    checkFilteredEdits(swapLoopsEditScript)
  }
}
