package inca.treesitterLanguage

import inca.treesitterAPI.TreeSitterTruediffLibrary
import inca.treesitterAPI.editscriptMappings.EditScript
import inca.treesitterAPI.treesitterMappings.{TSDiffResult, TSLanguage, TSLiteralMap, TSParser, TSTree}

object JavaTreeSitter {

  val lang: TSLanguage = TreeSitterTruediffLibrary.java_lib.tree_sitter_java()

  val parser: TSParser = TreeSitterTruediffLibrary.lib.ts_parser_new()
  TreeSitterTruediffLibrary.lib.ts_parser_set_language(parser, lang)

  val litMap: TSLiteralMap = TreeSitterTruediffLibrary.lib.ts_literal_map_create(lang)
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(litMap, 1)   // identifier
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(litMap, 2)   // decimal_integer_literal
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(litMap, 3)   // hex_integer_literal
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(litMap, 4)   // octal_integer_literal
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(litMap, 5)   // binary_integer_literal
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(litMap, 6)   // decimal_floating_point_literal
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(litMap, 7)   // hex_floating_point_literal
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(litMap, 10)  // character_literal
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(litMap, 11)  // string_literal
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(litMap, 12)  // null_literal
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(litMap, 124) // comment

  def freeResources(constructedTree: TSTree, litMap: TSLiteralMap, editScript: EditScript): Unit = {
    TreeSitterTruediffLibrary.lib.ts_edit_script_delete(editScript)
    TreeSitterTruediffLibrary.lib.ts_diff_heap_delete(constructedTree)
    TreeSitterTruediffLibrary.lib.ts_tree_delete(constructedTree)
    TreeSitterTruediffLibrary.lib.ts_literal_map_destroy(litMap)
  }

  def cleanup(fstTree: TSTree, sndTree: TSTree, parser: TSParser): Unit = {
    TreeSitterTruediffLibrary.lib.ts_diff_heap_delete(fstTree)
    TreeSitterTruediffLibrary.lib.ts_diff_heap_delete(sndTree)
    TreeSitterTruediffLibrary.lib.ts_tree_delete(fstTree)
    TreeSitterTruediffLibrary.lib.ts_tree_delete(sndTree)
    TreeSitterTruediffLibrary.lib.ts_parser_delete(parser)
  }

  def createDiffResult(srcCode: String, destCode: String): TSDiffResult = {
    val fstTree: TSTree = TreeSitterTruediffLibrary.lib.ts_parser_parse_string(
      parser, null, srcCode, srcCode.length
    )
    val sndTree: TSTree = TreeSitterTruediffLibrary.lib.ts_parser_parse_string(
      parser, null, destCode, destCode.length
    )

    TreeSitterTruediffLibrary.lib.ts_diff_heap_initialize(fstTree, srcCode, litMap)
    TreeSitterTruediffLibrary.lib.ts_diff_heap_initialize(sndTree, destCode, litMap)

    TreeSitterTruediffLibrary.lib.ts_compare_to(fstTree, sndTree, srcCode, destCode, litMap)
  }

}
