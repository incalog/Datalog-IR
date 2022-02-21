package inca.treesitterAPI;

import inca.treesitterAPI.editscriptAPI.*;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;

public class TreesitterTruediffAPITest {

    public static final TSLanguage lang = TreeSitterTruediffLibrary.java_lib.tree_sitter_java();

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

    public static void cleanup(TSTree fst_tree, TSTree snd_tree, TSTree constructed_tree, TreesitterTruediffJava java_parser, EditScript edit_script) {
        TreeSitterTruediffLibrary.lib.ts_edit_script_delete(edit_script);
        TreeSitterTruediffLibrary.lib.ts_diff_heap_delete(fst_tree);
        TreeSitterTruediffLibrary.lib.ts_diff_heap_delete(snd_tree);
        TreeSitterTruediffLibrary.lib.ts_diff_heap_delete(constructed_tree);
        TreeSitterTruediffLibrary.lib.ts_tree_delete(fst_tree);
        TreeSitterTruediffLibrary.lib.ts_tree_delete(snd_tree);
        TreeSitterTruediffLibrary.lib.ts_tree_delete(constructed_tree);
        java_parser.freeResources();
    }

    public void testEditscript(TreesitterTruediffJava java_parser, int expected_edits_count, String source, String dest) {

        TSTree tree1 = TreeSitterTruediffLibrary.lib.ts_parser_parse_string(
                java_parser.parser,
                null,
                source,
                source.length()
        );
        TSTree tree2 = TreeSitterTruediffLibrary.lib.ts_parser_parse_string(
                java_parser.parser,
                null,
                dest,
                dest.length()
        );

        TreeSitterTruediffLibrary.lib.ts_diff_heap_initialize(tree1, source, java_parser.lit_map);
        TreeSitterTruediffLibrary.lib.ts_diff_heap_initialize(tree2, dest, java_parser.lit_map);
        TSNode.ByValue source_root2 = TreeSitterTruediffLibrary.lib.ts_tree_root_node(tree1);
        TSDiffResult results = TreeSitterTruediffLibrary.lib.ts_compare_to(tree1, tree2, source, dest, java_parser.lit_map);

        System.out.println("==== Sequential Editscript ====\n");
        TreeSitterTruediffLibrary.lib.print_edit_script(lang, results.edit_script);

        TSNode.ByValue constructed_root = TreeSitterTruediffLibrary.lib.ts_tree_root_node(results.constructed_tree);
        Assertions.assertEquals(expected_edits_count, TreeSitterTruediffLibrary.lib.ts_edit_script_length(results.edit_script));
//        Assertions.assertFalse(TreeSitterTruediffLibrary.lib.ts_reconstruction_test(source_root2, constructed_root));

        cleanup(tree1, tree2, results.constructed_tree, java_parser, results.edit_script);
    }

    @Test
    @DisplayName("No changes")
    void noChanges() {
        TreesitterTruediffJava java_parser = new TreesitterTruediffJava();
        String source = """
                        public class Main {
                           int x = 5;
                        }
                        """;
        String dest =   """
                        public class Main {
                           int x = 5;
                        }
                        """;
        testEditscript(java_parser, 0, source, dest);
    }

    @Test
    @DisplayName("Updated Integer")
    void updatedInteger() {
        TreesitterTruediffJava java_parser = new TreesitterTruediffJava();
        String source = """
                        public class Main {
                           int x = 5;
                        }
                        """;
        String dest =   """
                        public class Main {
                           int x = 6;
                        }
                        """;
        testEditscript(java_parser, 1, source, dest);
    }

    @Test
    @DisplayName("Updated variable name")
    void updatedVariableName() {
        TreesitterTruediffJava java_parser = new TreesitterTruediffJava();
        String source = """
                        public class Main {
                            int x = 5;
                        }
                        """;
        String dest =   """
                        public class Main {
                            int y = 5;
                        }
                        """;
        testEditscript(java_parser, 1, source, dest);
    }

    @Test
    @DisplayName("Updated identifier and Integer")
    void updatedIdentifierAndInteger() {
        TreesitterTruediffJava java_parser = new TreesitterTruediffJava();
        String source = """
                        public class Main {
                            int x = 5;
                        }
                        """;
        String dest =   """
                        public class Main {
                            int y = 6;
                        }
                        """;
        testEditscript(java_parser, 2, source, dest);
    }

    @Test
    @DisplayName("Updated multiple identifiers and Integer")
    void updatedMultipleIdentifiersAndInteger() {
        TreesitterTruediffJava java_parser = new TreesitterTruediffJava();
        String source = """
                        public class Main {
                            int x = 5;
                        }
                        """;
        String dest =   """
                        public class MainClass {
                            int y = 6;
                        }
                        """;
        testEditscript(java_parser, 3, source, dest);
    }

    @Test
    @DisplayName("Added new free line")
    void addedNewFreeLine() {
        TreesitterTruediffJava java_parser = new TreesitterTruediffJava();
        String source = """
                        public class Main {
                            int x = 5;
                        }
                        """;
        String dest =   """
                        public class Main {
                            
                            int x = 5;
                        }
                        """;
        testEditscript(java_parser, 4, source, dest);
    }

    @Test
    @DisplayName("Added new modifier")
    void addedNewModifier() {
        TreesitterTruediffJava java_parser = new TreesitterTruediffJava();
        String source = """
                        class Main {
                            int x = 5;
                        }
                        """;
        String dest =   """
                        public class Main {
                            int x = 5;
                        }
                        """;
        testEditscript(java_parser, 5, source, dest);
    }

    @Test
    @DisplayName("Added modifier")
    void addedModifier() {
        TreesitterTruediffJava java_parser = new TreesitterTruediffJava();
        String source = """
                        public class Main {
                            int = 5;
                        }
                        """;
        String dest =   """
                        public final class Main {
                            int x = 5;
                        }
                        """;
        testEditscript(java_parser, 8, source, dest); // differs from Janik's tests
    }

    @Test
    @DisplayName("Added function")
    void addedFunction() {
        TreesitterTruediffJava java_parser = new TreesitterTruediffJava();
        String source = """
                        public class Main {
                        }
                        """;
        String dest =   """
                        public class Main {
                           void myMethod() {
                               System.out.println("I have been executed!");
                           }
                        }
                        """;
        testEditscript(java_parser, 35, source, dest);
    }

    @Test
    @DisplayName("Added new function modifier")
    void addedNewFunctionModifier() {
        TreesitterTruediffJava java_parser = new TreesitterTruediffJava();
        String source = """
                        public class Main {
                            void myMethod() {
                                System.out.println("I just got executed!");
                            }
                        }
                        """;
        String dest =   """
                        public class Main {
                            public void myMethod() {
                                System.out.println("I just got executed!");
                            }
                        }
                        """;
        testEditscript(java_parser, 7, source, dest);
    }

    @Test
    @DisplayName("Swap function order one")
    void swapFunctionOrderOne() {
        TreesitterTruediffJava java_parser = new TreesitterTruediffJava();
        String source = """
                        public class Main {
                            boolean myMethod() {
                                return false;
                            }
                            boolean myOtherMethod() {
                                return true;
                            }
                        }
                        """;
        String dest =   """
                        public class Main {
                            boolean myOtherMethod() {
                                return true;
                            }
                            boolean myMethod() {
                                return false;
                            }
                        }
                        """;
        testEditscript(java_parser, 4, source, dest);
    }

    @Test
    @DisplayName("Swap loops")
    void swapLoops() {
        TreesitterTruediffJava java_parser = new TreesitterTruediffJava();
        String source = """
                        public class Main {
                            void myMethod() {
                                for (int i = 0; i < 5; i++) {
                                    System.out.println(i);
                                }
                            }
                            void MyOtherMethod() {
                                for (int i = 4; i >= 0; i--) {
                                    System.out.println(i);
                                }
                            }
                        }
                        """;
        String dest = """
                        public class Main {
                            void myMethod() {
                                for (int i = 4; i >= 0; i--) {
                                    System.out.println(i);
                                }
                            }
                            void MyOtherMethod() {
                                for (int i = 0; i < 5; i++) {
                                    System.out.println(i);
                                }
                            }
                        }
                        """;
        testEditscript(java_parser, 6, source, dest);
    }


    // Mainly used for debugging things
    public static void main(String[] args) {

        TreesitterTruediffJava java_test = new TreesitterTruediffJava();

        String source = "public class Main {" +
                        "}";
        String dest =   "public class Main {" +
                        "   void myMethod() {" +
                        "       System.out.println(\" I have been executed!\");" +
                        "   }" +
                        "}";

        TSTree tree1 = TreeSitterTruediffLibrary.lib.ts_parser_parse_string(
                java_test.parser,
                null,
                source,
                source.length()
        );
        TSTree tree2 = TreeSitterTruediffLibrary.lib.ts_parser_parse_string(
                java_test.parser,
                null,
                dest,
                dest.length()
        );

        TreeSitterTruediffLibrary.lib.ts_diff_heap_initialize(tree1, source, java_test.lit_map);
        TreeSitterTruediffLibrary.lib.ts_diff_heap_initialize(tree2, dest, java_test.lit_map);

        TSDiffResult.ByValue diff_result = TreeSitterTruediffLibrary.lib.ts_compare_to(tree1, tree2, source, dest, java_test.lit_map);

        final SugaredEdit[] edit_array = (SugaredEdit[]) diff_result.edit_script.edits.content.toArray(diff_result.edit_script.edits.size);
//        for (int i = 0; i < diff_result.edit_script.edits.size; i++) {
//            System.out.println(edit_array[i].toString(true));
//        }

        final ChildPrototypeArray test_children = edit_array[0].sugar_edit.detach_unload.kids;
        final ChildPrototype child = new ChildPrototype(test_children.content);
        final ChildPrototype[] children = (ChildPrototype[]) child.toArray(2);
        for (int i = 0; i < 2; i++) {
            System.out.println(children[i].child_id);
        }

        TreeSitterTruediffLibrary.lib.print_edit_script(lang, diff_result.edit_script);

        cleanup(tree1, tree2, diff_result.constructed_tree, java_test, diff_result.edit_script);
    }
}