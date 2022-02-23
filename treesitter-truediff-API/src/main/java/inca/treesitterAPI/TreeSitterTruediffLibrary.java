package inca.treesitterAPI;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;
import inca.treesitterAPI.editscriptAPI.EditScript;
import inca.treesitterAPI.treesitterAPI.*;

public interface TreeSitterTruediffLibrary extends Library {

    boolean error = addLibPath();

    TreeSitterTruediffLibrary INSTANCE = error ? null : Native.load("libtree-sitter", TreeSitterTruediffLibrary.class);
    TreeSitterTruediffLibrary JAVA_INSTANCE = error ? null : Native.load("libtree-sitter-java", TreeSitterTruediffLibrary.class);

    TreeSitterTruediffLibrary lib = INSTANCE;
    TreeSitterTruediffLibrary java_lib = JAVA_INSTANCE;

    static boolean addLibPath() {
        if (Platform.isWindows()) {
            System.setProperty("jna.library.path", "src/main/resources/windows");
        } else if (Platform.isLinux()) {
            System.setProperty("jna.library.path", "src/main/resources/linux");
        } else if (Platform.isMac()) {
            System.setProperty("jna.library.path", "src/main/resources/darwin");
        } else {
            return true;
        }
        return false;
    }

    /**
     * Create a language build on the java grammar
     */

    TSLanguage tree_sitter_java();


    /********************/
    /* Section - Parser */
    /********************/

    /**
     * Create a new parser.
     */
    TSParser ts_parser_new();

    /**
     * Delete the parser, freeing all of the memory that it used.
     */
    void ts_parser_delete(TSParser parser);

    /**
     * Set the language that the parser should use for parsing.
     * <p>
     * Returns a boolean indicating whether or not the language was successfully
     * assigned. True means assignment succeeded. False means there was a version
     * mismatch: the language was generated with an incompatible version of the
     * Tree-sitter CLI. Check the language's version using `ts_language_version`
     * and compare it to this library's `TREE_SITTER_LANGUAGE_VERSION` and
     * `TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION` constants.
     */
    boolean ts_parser_set_language(TSParser self, TSLanguage language);


    /**
     * Use the parser to parse some source code stored in one contiguous buffer.
     * The first two parameters are the same as in the `ts_parser_parse` function
     * above. The second two parameters indicate the location of the buffer and its
     * length in bytes.
     */
    TSTree ts_parser_parse_string(TSParser self, TSTree old_tree, String string, int length);


    /******************/
    /* Section - Tree */
    /******************/

    /**
     * Get the root node of the syntax tree.
     */
    TSNode.ByValue ts_tree_root_node(TSTree self);

    /**
     * Delete the syntax tree, freeing all of the memory that it used.
     */
    void ts_tree_delete(TSTree self);


    /******************/
    /* Section - Node */
    /******************/

    /**
     * Get the node's type as a null-terminated string.
     */
    String ts_node_type(TSNode.ByValue node);

    /**
     * Get the node's type as a numerical id.
     */
    int ts_node_symbol(TSNode.ByValue node);

    /**
     * Get the node's start byte.
     */
    int ts_node_start_byte(TSNode.ByValue node);

    /**
     * Get the node's end byte.
     */
    int ts_node_end_byte(TSNode.ByValue node);

    /**
     * Check if the node is *named*. Named nodes correspond to named rules in the
     * grammar, whereas *anonymous* nodes correspond to string literals in the
     * grammar.
     */
    boolean ts_node_is_named(TSNode.ByValue node);

    /**
     * Get the node's child at the given index, where zero represents the first
     * child.
     */
    TSNode.ByValue ts_node_child(TSNode.ByValue node, int index);

    /**
     * Get the node's number of children.
     */
    int ts_node_child_count(TSNode.ByValue node);

    /**
     * Get the node's *named* child at the given index.
     * <p>
     * See also `ts_node_is_named`.
     */
    TSNode.ByValue ts_node_named_child(TSNode.ByValue node, int index);

    /**
     * Get the node's number of *named* children.
     * <p>
     * See also `ts_node_is_named`.
     */
    int ts_node_named_child_count(TSNode.ByValue node);

    /**
     * Get the node's next / previous *named* sibling.
     */
    TSNode.ByValue ts_node_next_named_sibling(TSNode.ByValue node);

    TSNode.ByValue ts_node_prev_named_sibling(TSNode.ByValue node);

    /**
     * Get an S-expression representing the node as a string.
     * <p>
     * This string is allocated with `malloc` and the caller is responsible for
     * freeing it using `free`.
     */
    String ts_node_string(TSNode.ByValue node);


    /************************/
    /* Section - TreeCursor */
    /************************/

    /**
     * Create a new tree cursor starting from the given node.
     * <p>
     * A tree cursor allows you to walk a syntax tree more efficiently than is
     * possible using the `API.TSNode` functions. It is a mutable object that is always
     * on a certain syntax node, and can be moved imperatively to different nodes.
     */
    TSTreeCursor.ByValue ts_tree_cursor_new(TSNode.ByValue node);

    /**
     * Delete a tree cursor, freeing all of the memory that it used.
     */
    void ts_tree_cursor_delete(TSTreeCursor.ByReference cursor);

    /**
     * Re-initialize a tree cursor to start at a different node.
     */
    void ts_tree_cursor_reset(TSTreeCursor.ByReference cursor, TSNode node);

    /**
     * Get the tree cursor's current node.
     */
    TSNode.ByValue ts_tree_cursor_current_node(TSTreeCursor.ByReference cursor);

    /**
     * Get the field name of the tree cursor's current node.
     * <p>
     * This returns `NULL` if the current node doesn't have a field.
     * See also `ts_node_child_by_field_name`.
     */
    String ts_tree_cursor_current_field_name(TSTreeCursor.ByReference cursor);

    /**
     * Get the field name of the tree cursor's current node.
     * <p>
     * This returns zero if the current node doesn't have a field.
     * See also `ts_node_child_by_field_id`, `ts_language_field_id_for_name`.
     */
    int ts_tree_cursor_current_field_id(TSTreeCursor.ByReference cursor);

    /**
     * Move the cursor to the parent of its current node.
     * <p>
     * This returns `true` if the cursor successfully moved, and returns `false`
     * if there was no parent node (the cursor was already on the root node).
     */
    boolean ts_tree_cursor_goto_parent(TSTreeCursor.ByReference cursor);

    /**
     * Move the cursor to the next sibling of its current node.
     * <p>
     * This returns `true` if the cursor successfully moved, and returns `false`
     * if there was no next sibling node.
     */
    boolean ts_tree_cursor_goto_next_sibling(TSTreeCursor.ByReference cursor);

    /**
     * Move the cursor to the first child of its current node.
     * <p>
     * This returns `true` if the cursor successfully moved, and returns `false`
     * if there were no children.
     */
    boolean ts_tree_cursor_goto_first_child(TSTreeCursor.ByReference cursor);

    /**
     * Move the cursor to the first child of its current node that extends beyond
     * the given byte offset.
     * <p>
     * This returns the index of the child node if one was found, and returns -1
     * if no such child was found.
     */
    long ts_tree_cursor_goto_first_child_for_byte(TSTreeCursor.ByReference cursor, int pos);

    TSTreeCursor.ByReference ts_tree_cursor_copy(TSTreeCursor.ByReference cursor);


    /**********************/
    /* Section - Language */
    /**********************/

    /**
     * Get the number of distinct node types in the language.
     */
    int ts_language_symbol_count(TSLanguage language);

    /**
     * Get a node type string for the given numerical id.
     */
    String ts_language_symbol_name(TSLanguage language, int symbol);


    /**
     * Check whether the given node type id belongs to named nodes, anonymous nodes,
     * or a hidden nodes.
     * <p>
     * See also `ts_node_is_named`. Hidden nodes are never returned from the API.
     */
    int ts_language_symbol_type(TSLanguage language, int symbol);


    /**********************/
    /* Section - Truediff */
    /**********************/

    /**
     * Creates and initializes new TSNodeDiffHeaps for this tree.
     * Memory is allocated, must be freed with ts_diff_heap_delete
     */
    void ts_diff_heap_initialize(TSTree tree, String str, TSLiteralMap lit_map);

    /**
     * Deletes all TSNodeDiffHeaps in this tree
     */
    void ts_diff_heap_delete(TSTree tree);

    /**
     * Compares two hashes
     */
    boolean ts_diff_heap_hash_eq(String fst_str, String snd_str);

    /**
     * Creates a new API.TSLiteralMap.
     * <p>
     * The API.TSLiteralMap is used to mark all literals (literal symbols) of a language.
     * Truediff includes literals in the calculation of the literal hash, while all
     * other nodes are just represented by their type in the structural hash.
     */
    TSLiteralMap ts_literal_map_create(TSLanguage lang);

    /**
     * Marks a specific symbol (represented by its id) as a literal.
     */
    void ts_literal_map_add_literal(TSLiteralMap lit_map, short id);

    /**
     * Deletes an API.TSLiteralMap.
     */
    void ts_literal_map_destroy(TSLiteralMap lit_map);

    /**
     * Compares two Trees and computes their Treesitter_API.EditScript_API.EditScript
     * <p>
     * The two trees must be initialized!
     */
    TSDiffResult.ByValue ts_compare_to(TSTree fst_tree, TSTree snd_tree, String fst_str, String snd_str, TSLiteralMap lit_map);

    /**
     * Compares two Trees and computes their Treesitter_API.EditScript_API.EditScript and generates an assignment graph
     * <p>
     * The two trees must be initialized!
     */
    TSDiffResult.ByValue ts_compare_to_print_graph(TSTree fst_tree, TSTree snd_tree, String fst_str, String snd_str, TSLiteralMap lit_map, Pointer file);

    void ts_tree_diff_graph(TSNode.ByValue fst_node, TSNode.ByValue snd_node, TSLanguage lang, Pointer file);

    void ts_edit_script_delete(EditScript edit_script);

    void print_edit_script(TSLanguage lang, EditScript edit_script);

    void print_minimized_edit_script(TSLanguage lang, EditScript edit_script);

    int ts_edit_script_length(EditScript edit_script);

    void print_struct_sizes();

    /**
     * The following TreeCursor functions should be private but are
     * provided to the api for debugging purposes
     */

    boolean ts_diff_tree_cursor_goto_parent(TSTreeCursor tree_cursor);

    boolean ts_diff_tree_cursor_goto_next_sibling(TSTreeCursor tree_cursor);

    boolean ts_diff_tree_cursor_goto_first_child(TSTreeCursor tree_cursor);

    boolean ts_reconstruction_test(TSNode.ByValue fst_node, TSNode.ByValue snd_node);

    boolean ts_incremental_parse_test(TSNode.ByValue fst_node, TSNode.ByValue snd_node);
}