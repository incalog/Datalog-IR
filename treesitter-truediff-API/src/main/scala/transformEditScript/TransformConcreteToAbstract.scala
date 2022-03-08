package transformEditScript

import inca.treesitterAPI.TreeSitterTruediffLibrary
import inca.treesitterAPI.editscriptMappings.{ChildPrototype, EditScript, EditTag, SugaredEdit}
import inca.treesitterAPI.treesitterMappings.{TSLanguage, TSLiteralMap, TSTree}
import truechange.{Attach, Detach, DetachUnload, JVMURI, Load, LoadAttach, NamedLink, NamedTag, URI, Unload, Update}

import java.util.Objects

class TransformConcreteToAbstract(lang: TSLanguage) {

  private var registeredURIs: Map[String, URI] = Map()
  private val editIndices: Map[String, Int] = Map()
  private val abstractSymbolNames: List[String] = List(
    "identifier",
    "decimal_integer_literal",
    "hex_integer_literal",
    "octal_integer_literal",
    "binary_integer_literal",
    "decimal_floating_point_literal",
    "hex_floating_point_literal",
    "true",
    "false",
    "character_literal",
    "string_literal",
    "null_literal",
    "boolean_type",
    "void_type",
    "this",
    "super",
    "comment",
    "program",
    "expression",
    "cast_expression",
    "assignment_expression",
    "binary_expression",
    "instanceof_expression",
    "lambda_expression",
    "inferred_parameters",
    "ternary_expression",
    "unary_expression",
    "update_expression",
    "primary_expression",
    "array_creation_expression",
    "dimensions_expr",
    "paranthesized_expression",
    "class_literal",
    "object_creation_expression",
    "field_access",
    "array_access",
    "method_invocation",
    "argument_list",
    "method_reference",
    "type_arguments",
    "wildcard",
    "dimensions",
    "switch_expression",
    "switch_block",
    "switch_block_statement_group",
    "switch_rule",
    "switch_label",
    "statement",
    "block",
    "expression_statement",
    "labeled_statement",
    "assert_statement",
    "do_statement",
    "break_statement",
    "continue_statement",
    "return_statement",
    "yield_statement",
    "synchronized_statement",
    "throw_statement",
    "try_statement",
    "catch_clause",
    "catch_formal_parameter",
    "catch_type",
    "finally_clause",
    "try_with_resources_statement",
    "resource_specification",
    "resource",
    "if_statement",
    "while_statement",
    "for_statement",
    "enhanced_for_statement",
    "marker_annotation",
    "annotation",
    "annotation_argument_list",
    "element_value_pair",
    "element_value_array_initializer",
    "declaration",
    "module_declaration",
    "module_body",
    "module_directive",
    "requires_modifier",
    "package_declaration",
    "import_declaration",
    "asterisk",
    "enum_declaration",
    "enum_body",
    "enum_body_declarations",
    "enum_constant",
    "class_declaration",
    "modifiers",
    "type_parameters",
    "type_parameter",
    "type_bound",
    "superclass",
    "super_interfaces",
    "interface_type_list",
    "class_body",
    "static_initializer",
    "constructor_declaration",
    "constructor_body",
    "explicit_constructor_invocation",
    "scoped_identifier",
    "field_declaration",
    "record_declaration",
    "annotation_type_declaration",
    "annotation_type_body",
    "annotation_type_element_declaration",
    "interface_declaration",
    "extends_interfaces",
    "interface_body",
    "constant_declaration",
    "variable_declarator",
    "array_initializer",
    "annotated_type",
    "scoped_type_identifier",
    "generic_type",
    "array_type",
    "integral_type",
    "floating_point_type",
    "formal_parameters",
    "formal_parameter",
    "receiver_parameter",
    "spread_parameter",
    "throws",
    "local_variable_declaration",
    "method_declaration",
    "type_identifier"
  )

  def abstractTSEditScript(editScript: EditScript): Seq[SugaredEdit] = {
    val editArray: Seq[SugaredEdit] = {
      if (editScript.edits.size == 0) {
        Array[SugaredEdit]()
      } else {
        editScript.edits.content.toArray(editScript.edits.size).asInstanceOf[Array[SugaredEdit]]
      }
    }
    editArray.filter { edit =>
      abstractSymbolNames.contains(
        TreeSitterTruediffLibrary.lib.ts_language_symbol_name(
          lang, edit.getTag))
    }
  }

  def registerId(id: String): URI = {
    if (!registeredURIs.contains(id)) {
      val uri = new JVMURI
      registeredURIs += (id -> uri)
    }
    registeredURIs(id)
  }

  // Creates a map of edits with their index in the edit array (for easier child tracking during translation)
  def registerEditIndices(editArray: Seq[SugaredEdit]): Unit = {
    editArray.foreach { edit =>
      edit.edit_tag match {
        case EditTag.ATTACH => editIndices + (edit.getId -> editArray.indexOf(edit))
        case EditTag.UNLOAD =>
          val child = new ChildPrototype(edit.sugar_edit.unload.kids.content)
          val children: Array[ChildPrototype] = child.toArray(edit.sugar_edit.unload.kids.size).asInstanceOf[Array[ChildPrototype]]
          children.foreach { child => editIndices + (child.child_id.toString -> editArray.indexOf(child)) }
        case EditTag.LOAD =>
          if (edit.sugar_edit.load.is_leaf == 0) {
            val child = new ChildPrototype(edit.sugar_edit.load.edit_data.node.kids.content)
            val children: Array[ChildPrototype] = child.toArray(edit.sugar_edit.load.edit_data.node.kids.size).asInstanceOf[Array[ChildPrototype]]
            children.foreach { child => editIndices + (child.child_id.toString -> editArray.indexOf(child)) }
          }
        case EditTag.DETACH => editIndices + (edit.getId -> editArray.indexOf(edit))
        case EditTag.LOAD_ATTACH =>
          if (edit.sugar_edit.load_attach.is_leaf == 0) {
            val child = new ChildPrototype(edit.sugar_edit.load.edit_data.node.kids.content)
            val children: Array[ChildPrototype] = child.toArray(edit.sugar_edit.load.edit_data.node.kids.size).asInstanceOf[Array[ChildPrototype]]
            children.foreach { child => editIndices + (child.child_id.toString -> editArray.indexOf(child)) }
          }
        case EditTag.DETACH_UNLOAD =>
          val child = new ChildPrototype(edit.sugar_edit.detach_unload.kids.content)
          val children: Array[ChildPrototype] = child.toArray(edit.sugar_edit.detach_unload.kids.size).asInstanceOf[Array[ChildPrototype]]
          children.foreach { child => editIndices + (child.child_id.toString -> editArray.indexOf(child)) }
        case EditTag.UPDATE => editIndices + (edit.getId -> editArray.indexOf(edit))
        case _ => // nothing
      }
    }
  }
  
  // TS Edit -> TC Edit
  def transformTSEdit(edit: SugaredEdit, filteredEdits: Seq[SugaredEdit], srcCode: String, destCode: String, constructedTree: TSTree, litMap: TSLiteralMap): truechange.Edit = {
    edit.edit_tag match {
      case EditTag.ATTACH =>
        val newNodeId = registerId(edit.getId)
        // TODO: create function to translate tags
        val newTag = NamedTag(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.getTag))
        val newLink = NamedLink(edit.sugar_edit.attach.link.toString)
        val newParentId = registerId(filteredEdits(editIndices(edit.sugar_edit.attach.parent_id.toString)).getId)
        val newParentTag = NamedTag(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, filteredEdits(editIndices(edit.sugar_edit.attach.parent_id.toString)).getTag))

        Attach(newNodeId, newTag, newLink, newParentId, newParentTag)

      case EditTag.DETACH =>
        val newNodeId = registerId(edit.getId)
        val newTag = NamedTag(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.getTag))
        val newLink = NamedLink(edit.sugar_edit.detach.link.toString)
        val newParentId = registerId(filteredEdits(editIndices(edit.sugar_edit.detach.parent_id.toString)).getId)
        val newParentTag = NamedTag(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, filteredEdits(editIndices(edit.sugar_edit.attach.parent_id.toString)).getTag))

        Detach(newNodeId, newTag, newLink, newParentId, newParentTag)

      case EditTag.UNLOAD =>
        val newNodeId = registerId(edit.getId)
        val newTag = NamedTag(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.getTag))

        // Use parent id to get tags of children. Iterate over tags to determine kids and lits for unload edit.
        val newKids: Seq[(String, URI)] = Seq[(String, URI)]()
        val newLits: Seq[(String, Any)] = Seq[(String, Any)]()
        val newParentId = edit.sugar_edit.unload.id
        val tempChildren = new ChildPrototype(edit.sugar_edit.unload.kids.content)
        val children: Seq[ChildPrototype] = tempChildren.toArray(edit.sugar_edit.unload.kids.size).asInstanceOf[Seq[ChildPrototype]]
        val childrenTags = children.foldLeft(Seq[Short]())((tags, child) =>
          tags :+ TreeSitterTruediffLibrary.lib.ts_get_child_tag(
            constructedTree, newParentId, child.child_id))
        childrenTags.foreach(tag =>
          if (litMap.ts_literal_map_is_literal(tag) == 1) {
            // TODO: name of link as tag
            newLits :+ ("tag", TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, tag))
          } else {
            // TODO: get value of added kid in srcCode
            newKids :+ ("tag", null)
          })

        Unload(newNodeId, newTag, newKids, newLits)

      case EditTag.LOAD =>
        val newNodeId = registerId(edit.getId)
        val newTag = NamedTag(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.getTag))

        // Use parent id to get tags of children. Iterate over tags to determine kids and lits for unload edit.
        val newKids: Seq[(String, URI)] = Seq[(String, URI)]()
        val newLits: Seq[(String, Any)] = Seq[(String, Any)]()
        val newParentId = edit.sugar_edit.unload.id
        val tempChildren = new ChildPrototype(edit.sugar_edit.unload.kids.content)
        val children: Seq[ChildPrototype] = tempChildren.toArray(edit.sugar_edit.unload.kids.size).asInstanceOf[Seq[ChildPrototype]]
        val childrenTags = children.foldLeft(Seq[Short]())((tags, child) =>
          tags :+ TreeSitterTruediffLibrary.lib.ts_get_child_tag(
            constructedTree, newParentId, child.child_id))
        childrenTags.foreach(tag =>
          if (litMap.ts_literal_map_is_literal(tag) == 1) {
            // TODO: name of link as tag
            newLits :+ ("tag", TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, tag))
          } else {
            // TODO: get value of added kid in srcCode
            newKids :+ ("tag", null)
          })

        Load(newNodeId, newTag, newKids, newLits)

      case EditTag.LOAD_ATTACH =>
        val newNodeId = registerId(edit.getId)
        val newTag = NamedTag(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.getTag))
        val newLink = NamedLink(edit.sugar_edit.load_attach.link.toString)
        val newParentId = registerId(filteredEdits(editIndices(edit.sugar_edit.attach.parent_id.toString)).getId)
        val newParentTag = NamedTag(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, filteredEdits(editIndices(edit.sugar_edit.attach.parent_id.toString)).getTag))

        // Use parent id to get tags of children. Iterate over tags to determine kids and lits for unload edit.
        val newKids: Seq[(String, URI)] = Seq[(String, URI)]()
        val newLits: Seq[(String, Any)] = Seq[(String, Any)]()
        val ParentId = edit.sugar_edit.unload.id
        val tempChildren = new ChildPrototype(edit.sugar_edit.unload.kids.content)
        val children: Seq[ChildPrototype] = tempChildren.toArray(edit.sugar_edit.unload.kids.size).asInstanceOf[Seq[ChildPrototype]]
        val childrenTags = children.foldLeft(Seq[Short]())((tags, child) =>
          tags :+ TreeSitterTruediffLibrary.lib.ts_get_child_tag(
            constructedTree, ParentId, child.child_id))
        childrenTags.foreach(tag =>
          if (litMap.ts_literal_map_is_literal(tag) == 1) {
            // TODO: name of link as tag
            newLits :+ ("tag", TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, tag))
          } else {
            // TODO: get value of added kid in srcCode
            newKids :+ ("tag", null)
          })

        LoadAttach(newNodeId, newTag, newKids, newLits, newLink, newParentId, newParentTag)

      case EditTag.DETACH_UNLOAD =>
        val newNodeId = registerId(edit.getId)
        val newTag = NamedTag(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.getTag))
        val newLink = NamedLink(edit.sugar_edit.detach_unload.link.toString)
        val newParentId = registerId(filteredEdits(editIndices(edit.sugar_edit.attach.parent_id.toString)).getId)
        val newParentTag = NamedTag(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, filteredEdits(editIndices(edit.sugar_edit.attach.parent_id.toString)).getTag))

        // Use parent id to get tags of children. Iterate over tags to determine kids and lits for unload edit.
        val newKids: Seq[(String, URI)] = Seq[(String, URI)]()
        val newLits: Seq[(String, Any)] = Seq[(String, Any)]()
        val ParentId = edit.sugar_edit.unload.id
        val tempChildren = new ChildPrototype(edit.sugar_edit.unload.kids.content)
        val children: Seq[ChildPrototype] = tempChildren.toArray(edit.sugar_edit.unload.kids.size).asInstanceOf[Seq[ChildPrototype]]
        val childrenTags = children.foldLeft(Seq[Short]())((tags, child) =>
          tags :+ TreeSitterTruediffLibrary.lib.ts_get_child_tag(
            constructedTree, ParentId, child.child_id))
        childrenTags.foreach(tag =>
          if (litMap.ts_literal_map_is_literal(tag) == 1) {
            // TODO: name of link as tag
            newLits :+ ("tag", TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, tag))
          } else {
            // TODO: get value of added kid in srcCode
            newKids :+ ("tag", null)
          })

        DetachUnload(newNodeId, newTag, newKids, newLits, newLink, newParentId, newParentTag)

      case EditTag.UPDATE =>
        val newNodeId = registerId(edit.getId)
        val newTag = NamedTag(TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.getTag))

        val charArrayOld = Array.ofDim[Char](1)
        srcCode.getChars(edit.sugar_edit.update.old_start.bytes, edit.sugar_edit.update.old_start.bytes + edit.sugar_edit.update.old_size.bytes, charArrayOld, 0)
        val newOldLits = charArrayOld.foldLeft(Seq[(String, Any)]())((newArray, oldLit) => newArray :+ ("oldValue", oldLit))

        val charArrayNew = Array.ofDim[Char](1)
        destCode.getChars(edit.sugar_edit.update.new_start.bytes, edit.sugar_edit.update.new_start.bytes + edit.sugar_edit.update.new_size.bytes, charArrayNew, 0)
        val newLits = charArrayNew.foldLeft(Seq[(String, Any)]())((newArray, newLit) => newArray :+ ("newValue", newLit))

        Update(newNodeId, newTag, newOldLits, newLits)
    }
  }

  // TS EditScript -> TC EditScript
  def transformTSEditScript(editScript: EditScript, srcCode: String, destCode: String, constructedTree: TSTree, litMap: TSLiteralMap): truechange.EditScript = {
    val filteredEdits = abstractTSEditScript(editScript)
    registerEditIndices(filteredEdits)
    val truechangeEdits = filteredEdits.map { edit => transformTSEdit(edit, filteredEdits, srcCode, destCode, constructedTree, litMap) }
    truechange.EditScript(truechangeEdits)
  }

  // For debugging purposes
  def print_edits(editArray: Array[SugaredEdit]): Unit = {
    for (edit <- editArray) {
      edit.edit_tag match {
        case EditTag.ATTACH => if (is_root(edit.sugar_edit.attach.parent_id.toString, edit.sugar_edit.attach.parent_tag)) System.out.printf("[ATTACH | %s] To parent ROOT on link %d%n", edit.sugar_edit.attach.id.toString, edit.sugar_edit.attach.link)
        else System.out.printf("[ATTACH | %s] To parent %s of type \"%s\" on link %d%n", edit.sugar_edit.attach.id.toString, edit.sugar_edit.attach.parent_id.toString, TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.sugar_edit.attach.parent_tag), edit.sugar_edit.attach.link)

        case EditTag.DETACH => if (is_root(edit.sugar_edit.detach.parent_id.toString, edit.sugar_edit.detach.parent_tag)) System.out.printf("[DETACH | %s] Node of type \"%s\" from parent ROOT on link %d%n", edit.sugar_edit.detach.id.toString, TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.sugar_edit.detach.tag), edit.sugar_edit.detach.link)
        else System.out.printf("[DETACH | %s] Node of type \"%s\" from parent %s of type \"%s\" on link %d%n", edit.sugar_edit.detach.id.toString, TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.sugar_edit.detach.tag), edit.sugar_edit.detach.parent_id.toString, TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.sugar_edit.detach.parent_tag), edit.sugar_edit.detach.link)

        case EditTag.UNLOAD => System.out.printf("[UNLOAD | %s] Node of type \"%s\"", edit.sugar_edit.unload.id.toString, TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.sugar_edit.unload.tag))
          val prototype = new ChildPrototype(edit.sugar_edit.unload.kids.content)
          val prototypes = prototype.toArray(edit.sugar_edit.unload.kids.size).asInstanceOf[Array[ChildPrototype]]
          if (prototypes.length > 0) {
            System.out.printf(" and set its kids free [")
            for (i <- prototypes.indices) {
              if (i > 0) System.out.printf(", ")
              System.out.printf("%s", prototypes(i).child_id.toString)
            }
            System.out.printf("]")
          }
          System.out.printf("%n")

        case EditTag.LOAD => if (edit.sugar_edit.load.is_leaf == 1) System.out.printf("[LOAD | %s] Load new leaf of type \"%s\"\n", edit.sugar_edit.load.id.toString, TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.sugar_edit.load.tag))
        else {
          System.out.printf("[LOAD | %s] Load new subtree of type \"%s\" with kids [", edit.sugar_edit.load.id.toString, TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.sugar_edit.load.tag))
          val prototype = new ChildPrototype(edit.sugar_edit.load.edit_data.node.kids.content)
          val prototypes = prototype.toArray(edit.sugar_edit.load.edit_data.node.kids.size).asInstanceOf[Array[ChildPrototype]]
          for (i <- prototypes.indices) {
            if (i > 0) System.out.printf(", ")
            System.out.printf("%s", prototypes(i).child_id.toString)
          }
          System.out.printf("]%n")
        }

        case EditTag.LOAD_ATTACH => if (edit.sugar_edit.load_attach.is_leaf == 1) System.out.printf("[LOAD_ATTACH | %s] Load new leaf of type \"%s\" and attach to parent %s of type %s on link %d%n", edit.sugar_edit.load_attach.id.toString, TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.sugar_edit.load_attach.tag), edit.sugar_edit.load_attach.parent_id.toString, TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.sugar_edit.load_attach.parent_tag), edit.sugar_edit.load_attach.link)
        else {
          System.out.printf("[LOAD_ATTACH | %s] Load new subtree of type \"%s\" with kids [", edit.sugar_edit.load_attach.id.toString, TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.sugar_edit.load_attach.tag))
          val prototype = new ChildPrototype(edit.sugar_edit.load_attach.edit_data.node.kids.content)
          val prototypes = prototype.toArray(edit.sugar_edit.load_attach.edit_data.node.kids.size).asInstanceOf[Array[ChildPrototype]]
          for (i <- prototypes.indices) {
            if (i > 0) System.out.printf(", ")
            System.out.printf("%s", prototypes(i).child_id.toString)
          }
          if (is_root(edit.sugar_edit.load_attach.parent_id.toString, edit.sugar_edit.load_attach.parent_tag)) System.out.printf("] and attach to parent ROOT on link %d%n", edit.sugar_edit.load_attach.link)
          else System.out.printf("] and attach to parent %s of type \"%s\" on link %d%n", edit.sugar_edit.load_attach.parent_id.toString, TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.sugar_edit.load_attach.parent_tag), edit.sugar_edit.load_attach.link)
        }

        case EditTag.DETACH_UNLOAD => if (is_root(edit.sugar_edit.detach_unload.parent_id.toString, edit.sugar_edit.detach_unload.parent_tag)) System.out.printf("[DETACH_UNLOAD | %s] Node of type \"%s\" from parent ROOT on link %d", edit.sugar_edit.detach_unload.id.toString, TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.sugar_edit.detach_unload.tag), edit.sugar_edit.detach_unload.link)
        else System.out.printf("[DETACH_UNLOAD | %s] Node of type \"%s\" from parent %s of type \"%s\" on link %d", edit.sugar_edit.detach_unload.id.toString, TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.sugar_edit.detach_unload.tag), edit.sugar_edit.detach_unload.parent_id.toString, TreeSitterTruediffLibrary.lib.ts_language_symbol_name(lang, edit.sugar_edit.detach_unload.parent_tag), edit.sugar_edit.detach_unload.link)
          val prototype = new ChildPrototype(edit.sugar_edit.detach_unload.kids.content)
          val prototypes = prototype.toArray(edit.sugar_edit.detach_unload.kids.size).asInstanceOf[Array[ChildPrototype]]
          if (prototypes.length > 0) {
            System.out.printf(" and set its kids free [")
            for (i <- prototypes.indices) {
              if (i > 0) System.out.printf(", ")
              System.out.printf("%s", prototypes(i).child_id.toString)
            }
            System.out.printf("]")
          }
          System.out.printf("%n")

        case EditTag.UPDATE => System.out.printf("[UPDATE | %s] Old literal from %d (%d) => New literal from %d (%d)%n", edit.sugar_edit.update.id.toString, edit.sugar_edit.update.old_start.bytes, edit.sugar_edit.update.old_size.bytes, edit.sugar_edit.update.new_start.bytes, edit.sugar_edit.update.new_size.bytes)

        case EditTag.UPDATE_PADDING => System.out.printf("[UPDATE PADDING | %s] Update padding from %d to %d%n", edit.sugar_edit.update_padding.id.toString, edit.sugar_edit.update_padding.old_padding.bytes, edit.sugar_edit.update_padding.new_padding.bytes)

      }
    }
  }

  private def is_root(id: String, symbol: Int): Boolean = Objects.equals(id, "null") && symbol == Integer.MAX_VALUE
}
