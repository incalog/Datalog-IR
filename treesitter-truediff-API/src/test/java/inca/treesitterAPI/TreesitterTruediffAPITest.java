package inca.treesitterAPI;

import inca.treesitterAPI.editscriptAPI.EditScript;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;

public class TreesitterTruediffAPITest {

    public static final TSLanguage lang = TreeSitterTruediffLibrary.java_lib.tree_sitter_java();

    public static class TreeContainer {

        private final TSTree first;
        private final TSTree second;

        public TreeContainer(TSTree fst_tree, TSTree snd_tree) {
            this.first = fst_tree;
            this.second = snd_tree;
        }

        public TSTree getFirst() {
            return first;
        }

        public TSTree getSecond() {
            return second;
        }
    }

    public static class TreesitterTruediffJava {

        public final TSParser parser;
        public final TSLiteralMap lit_map;

        public TreesitterTruediffJava() {

            // Create parser.
            parser = TreeSitterTruediffLibrary.lib.ts_parser_new();

            // Set language of parser.
            TreeSitterTruediffLibrary.lib.ts_parser_set_language(parser, lang);

            // Create map with literals used in tests.
            lit_map = TreeSitterTruediffLibrary.lib.ts_literal_map_create(lang);
            TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 1);
            TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 2);
            TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 3);
            TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 4);
            TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 5);
            TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 6);
            TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 7);
            TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 10);
            TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 11);
            TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 12);
            TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 122);
        }

        public void freeResources() {
            TreeSitterTruediffLibrary.lib.ts_parser_delete(parser);
            TreeSitterTruediffLibrary.lib.ts_literal_map_destroy(lit_map);
        }
    }

    public void cleanup(TSTree fst_tree, TSTree snd_tree, TSTree constructed_tree, TreesitterTruediffJava java_parser, EditScript edit_script) {
        TreeSitterTruediffLibrary.lib.ts_edit_script_delete(edit_script);
        TreeSitterTruediffLibrary.lib.ts_diff_heap_delete(fst_tree);
        TreeSitterTruediffLibrary.lib.ts_diff_heap_delete(snd_tree);
        TreeSitterTruediffLibrary.lib.ts_diff_heap_delete(constructed_tree);
        TreeSitterTruediffLibrary.lib.ts_tree_delete(fst_tree);
        TreeSitterTruediffLibrary.lib.ts_tree_delete(snd_tree);
        TreeSitterTruediffLibrary.lib.ts_tree_delete(constructed_tree);
        java_parser.freeResources();
    }

    public TreeContainer createTSTrees(String source, String dest, TSParser parser, TSLiteralMap lit_map) {
        TSTree tree1 = TreeSitterTruediffLibrary.lib.ts_parser_parse_string(
                parser,
                null,
                source,
                source.length()
        );
        TSTree tree2 = TreeSitterTruediffLibrary.lib.ts_parser_parse_string(
                parser,
                null,
                dest,
                dest.length()
        );

        TreeSitterTruediffLibrary.lib.ts_diff_heap_initialize(tree1, source, lit_map);
        TreeSitterTruediffLibrary.lib.ts_diff_heap_initialize(tree2, dest, lit_map);

        return new TreeContainer(tree1, tree2);
    }

    public void testEditscript(TreesitterTruediffJava java_parser, String source, String dest) {
        TreeContainer trees = createTSTrees(source, dest, java_parser.parser, java_parser.lit_map);
        TSNode.ByValue source_root2 = TreeSitterTruediffLibrary.lib.ts_tree_root_node(trees.getFirst());
        TSDiffResult results = TreeSitterTruediffLibrary.lib.ts_compare_to(trees.getFirst(), trees.getSecond(), source, dest, java_parser.lit_map);

        System.out.println("==== Sequential Editscript ====\n");
        TreeSitterTruediffLibrary.lib.print_edit_script(lang, results.edit_script);

        TSNode.ByValue constructed_root = TreeSitterTruediffLibrary.lib.ts_tree_root_node(results.constructed_tree);
        Assertions.assertEquals(35, TreeSitterTruediffLibrary.lib.ts_edit_script_length(results.edit_script));
//        Assertions.assertFalse(TreeSitterTruediffLibrary.lib.ts_reconstruction_test(source_root2, constructed_root));

        cleanup(trees.getFirst(), trees.getSecond(), results.constructed_tree, java_parser, results.edit_script);
    }

    @Test
    @DisplayName("No change")
    void noChanges() {
        TreesitterTruediffJava java_parser = new TreesitterTruediffJava();
        String source = "public class Main {" +
                        "}";
        String dest =   "public class Main {" +
                        "   void myMethod() {" +
                        "       System.out.println(\" I have been executed!\");" +
                        "   }" +
                        "}";
        testEditscript(java_parser, source, dest);
    }


    // Mainly used for debugging things
    public static void main(String[] args) {

        TSParser parser = TreeSitterTruediffLibrary.lib.ts_parser_new();
        TreeSitterTruediffLibrary.lib.ts_parser_set_language(parser, lang);

        TSLiteralMap lit_map = TreeSitterTruediffLibrary.lib.ts_literal_map_create(lang);
        TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 1);
        TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 2);
        TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 3);
        TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 4);
        TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 5);
        TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 6);
        TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 7);
        TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 10);
        TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 11);
        TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 12);
        TreeSitterTruediffLibrary.lib.ts_literal_map_add_literal(lit_map, (short) 122);

        String source = "public class Main {" +
                        "}";
        String dest =   "public class Main {" +
                        "   void myMethod() {" +
                        "       System.out.println(\" I have been executed!\");" +
                        "   }" +
                        "}";

        TSTree tree1 = TreeSitterTruediffLibrary.lib.ts_parser_parse_string(
                parser,
                null,
                source,
                source.length()
        );
        TSTree tree2 = TreeSitterTruediffLibrary.lib.ts_parser_parse_string(
                parser,
                null,
                dest,
                dest.length()
        );

        TreeSitterTruediffLibrary.lib.ts_diff_heap_initialize(tree1, source, lit_map);
        TreeSitterTruediffLibrary.lib.ts_diff_heap_initialize(tree2, dest, lit_map);

        TSDiffResult diff_result = TreeSitterTruediffLibrary.lib.ts_compare_to(tree1, tree2, source, dest, lit_map);
        System.out.println(diff_result.edit_script.edits.content);
        System.out.println(diff_result.edit_script.edits.content.getPointer().dump(0, 56));
        System.out.println(diff_result.edit_script.edits.content.getPointer().dump(57, 52));
        System.out.println(diff_result.edit_script.edits.content.getPointer().dump(109, 48));
        System.out.println(diff_result.edit_script.edits.content.getPointer().dump(157, 64));
        System.out.println(diff_result.edit_script.edits.content.getPointer().dump(221, 48));
        System.out.println(diff_result.edit_script.edits.content.getPointer().dump(269, 64));
        System.out.println(diff_result.edit_script.edits.content.getPointer().dump(333, 48));
//        final SugaredEdit[] edit_array = (SugaredEdit[])diff_result.edit_script.edits.content.toArray(diff_result.edit_script.edits.size);
//        System.out.println("Hopefully it worked: ");
//        for (SugaredEdit edits : edit_array) {
//            System.out.println("\t" + "Tag: " + edits.edit_tag);
//        }
//
//        TreeSitterTruediffLibrary.lib.print_edit_script(lang, diff_result.edit_script);

        TreeSitterTruediffLibrary.lib.ts_edit_script_delete(diff_result.edit_script);
        TreeSitterTruediffLibrary.lib.ts_diff_heap_delete(tree1);
        TreeSitterTruediffLibrary.lib.ts_diff_heap_delete(tree2);
        TreeSitterTruediffLibrary.lib.ts_tree_delete(tree1);
        TreeSitterTruediffLibrary.lib.ts_tree_delete(tree2);
    }
}