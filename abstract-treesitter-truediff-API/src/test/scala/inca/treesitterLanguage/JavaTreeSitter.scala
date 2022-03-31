package inca.treesitterLanguage

import inca.abstractTreesitterAPI.TreeSitterTruediffLibrary
import inca.abstractTreesitterAPI.editscriptMappings.EditScript
import inca.abstractTreesitterAPI.treesitterMappings.{TSDiffResult, TSLanguage, TSLiteralMap, TSNode, TSParser, TSTree}

object JavaTreeSitter {

  val lang: TSLanguage = TreeSitterTruediffLibrary.java_lib.tree_sitter_java()

  val parser: TSParser = TreeSitterTruediffLibrary.lib.ts_parser_new()
  TreeSitterTruediffLibrary.lib.ts_parser_set_language(parser, lang)

  val litMap: TSLiteralMap = TreeSitterTruediffLibrary.lib.ts_literal_map_create(lang)
  // Add literals to litMap.
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

  // Add unnamed tokens to litMap to be included in EditScript.
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 14)  	// &
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 17)  	// +=
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 18)  	// -=
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 19)  	// *=
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 20)  	// /=
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 21)  	// &=
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 22)  	// |=
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 23)  	// ^=
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 24)  	// %=
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 25)  	// <<=
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 26)  	// >>=
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 27)  	// >>>=
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 28)  	// >
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 29)  	// <
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 30)  	// >=
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 31)  	// <=
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 32)  	// ==
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 33)  	// !=
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 34)  	// &&
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 35)  	// ||
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 36)  	// +
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 37)  	// -
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 38)  	// *
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 39)  	// /
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 40)  	// |
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 41)  	// ^
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 42)  	// %
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 43)  	// <<
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 44)  	// >>
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 45)  	// >>>
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 49)  	// ?
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 51)  	// !
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 52)  	// ~
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 53)  	// ++
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 54)  	// --
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 92)    // static
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 102)   // final
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 127) 	// expression
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 128) 	// cast_expression
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 129) 	// assignment_expression
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 130) 	// binary_expression
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 131) 	// instanceof_expression
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 132) 	// lambda_expression
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 133) 	// inferred_expression
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 134) 	// ternary_expression
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 135) 	// unary_expression
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 136) 	// update_expression
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 137) 	// primary_expression
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 138) 	// array_creation_expression
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 139) 	// dimensions_expression
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 140) 	// parenthesized_expression
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 141) 	// class_literal
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 142) 	// object_creation_expression
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 143) 	// field_access
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 144) 	// array_access
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 145) 	// method_invocation
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 146) 	// argument_list
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 147) 	// method_reference
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 148) 	// type_arguments
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 149) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 150) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 152) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 153) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 154) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 155) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 156) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 157) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 158) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 159) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 160) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 161) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 162) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 163) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 164) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 165) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 166) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 167) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 168) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 169) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 170) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 171) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 172) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 173) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 174) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 175) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 176) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 177) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 178) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 179) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 180) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 181) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 183) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 184) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 185) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 186) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 188) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 189) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 196) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 197) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 198) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 199) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 200) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 201) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 202) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 203) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 204) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 205) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 206) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 207) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 208) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 209) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 210) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 211) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 213) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 214) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 215) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 216) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 217) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 218) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 219) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 220) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 222) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 223) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 224) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 225) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 227) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 229) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 232) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 233) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 234) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 235) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 236) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 237) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 240) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 241) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 242) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 243) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 245) 	//
  TreeSitterTruediffLibrary.lib.ts_literal_map_add_unnamed_token(litMap, 279) 	//

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

    val diffResult: TSDiffResult = TreeSitterTruediffLibrary.lib.ts_compare_to(fstTree, sndTree, srcCode, destCode, litMap)
    val sndRoot: TSNode.ByValue = TreeSitterTruediffLibrary.lib.ts_tree_root_node(sndTree)
    val constructedRoot: TSNode.ByValue = TreeSitterTruediffLibrary.lib.ts_tree_root_node(diffResult.constructed_tree)
    assert(!TreeSitterTruediffLibrary.lib.ts_reconstruction_test(sndRoot, constructedRoot))
    cleanup(fstTree, sndTree, parser)

    diffResult
  }

}
