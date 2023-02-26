package language;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.tree.IElementType;
import language.psi.FunIncATypes;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static com.intellij.openapi.editor.colors.TextAttributesKey.createTextAttributesKey;

public class FunIncASyntaxHighlighter extends SyntaxHighlighterBase {

    // Types of Highlights in Inca
    public static final TextAttributesKey RESERVED_ID =
            createTextAttributesKey("INCA_RESERVED_ID", DefaultLanguageHighlighterColors.KEYWORD);

    public static final TextAttributesKey RESERVED_OP
            = TextAttributesKey.createTextAttributesKey("INCA_RESERVED_OP", DefaultLanguageHighlighterColors.PREDEFINED_SYMBOL);

    public static final TextAttributesKey COMMA
            = TextAttributesKey.createTextAttributesKey("INCA_COMMA", DefaultLanguageHighlighterColors.COMMA);

    public static final TextAttributesKey BRACKETS
            = TextAttributesKey.createTextAttributesKey("INCA_BRACKETS", DefaultLanguageHighlighterColors.BRACKETS);

    public static final TextAttributesKey PARENTHESES
            = TextAttributesKey.createTextAttributesKey("INCA_PARENTHESES", DefaultLanguageHighlighterColors.PARENTHESES);

    public static final TextAttributesKey BRACES
            = TextAttributesKey.createTextAttributesKey("INCA_BRACES", DefaultLanguageHighlighterColors.BRACES);

    public static final TextAttributesKey COMMENT
            = TextAttributesKey.createTextAttributesKey("INCA_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT);

    public static final TextAttributesKey INTEGER
            = TextAttributesKey.createTextAttributesKey("INCA_INTEGER", DefaultLanguageHighlighterColors.NUMBER);

    public static final TextAttributesKey DOUBLE
            = TextAttributesKey.createTextAttributesKey("INCA_DOUBLE", DefaultLanguageHighlighterColors.NUMBER);

    public static final TextAttributesKey LONG
            = TextAttributesKey.createTextAttributesKey("INCA_DOUBLE", DefaultLanguageHighlighterColors.NUMBER);

    public static final TextAttributesKey STRING
            = TextAttributesKey.createTextAttributesKey("INCA_STRING", DefaultLanguageHighlighterColors.STRING);

    public static final TextAttributesKey OPSYM
            = TextAttributesKey.createTextAttributesKey("INCA_OPSYM", DefaultLanguageHighlighterColors.OPERATION_SIGN);

    public static final TextAttributesKey SCALATERM
            = TextAttributesKey.createTextAttributesKey("INCA_SCALATERM", DefaultLanguageHighlighterColors.FUNCTION_CALL);

    public static final TextAttributesKey MAINANNOTATION
            = TextAttributesKey.createTextAttributesKey("INCA_MAIN_ANNOTATION", DefaultLanguageHighlighterColors.METADATA);

    public static final TextAttributesKey PARAMETER
            = TextAttributesKey.createTextAttributesKey("INCA_PARAMETER", DefaultLanguageHighlighterColors.PARAMETER);

    public static final TextAttributesKey FUNCALL
            = TextAttributesKey.createTextAttributesKey("INCA_FUNCTION_CALL", DefaultLanguageHighlighterColors.FUNCTION_CALL);

    /**
     * Helper to point multiple token types to a single color.
     */
    private static final Map<IElementType, TextAttributesKey> keys;

    private static void keysPutEach(Iterable<IElementType> tokenTypes, TextAttributesKey value) {
        for (IElementType tokenType : tokenTypes) {
            keys.put(tokenType, value);
        }
    }

    static {
        keys = new HashMap<>(0);

        keysPutEach(
                Arrays.asList(
                        FunIncATypes.ANNOTATION_MAIN,
                        FunIncATypes.BOOLEAN_FALSE,
                        FunIncATypes.BOOLEAN_TRUE,
                        FunIncATypes.CAST,
                        FunIncATypes.KEYWORD_CASE,
                        FunIncATypes.KEYWORD_DATA,
                        FunIncATypes.KEYWORD_DEF,
                        FunIncATypes.KEYWORD_ELSE,
                        FunIncATypes.KEYWORD_FAIL,
                        FunIncATypes.KEYWORD_IF,
                        FunIncATypes.KEYWORD_IMPORT,
                        FunIncATypes.KEYWORD_IN,
                        FunIncATypes.KEYWORD_LET,
                        FunIncATypes.KEYWORD_MATCH,
                        FunIncATypes.KEYWORD_MODULE,
                        FunIncATypes.KEYWORD_NONE,
                        FunIncATypes.KEYWORD_OPTION,
                        FunIncATypes.KEYWORD_SET,
                        FunIncATypes.KEYWORD_SOME,
                        FunIncATypes.TYPE_ANY,
                        FunIncATypes.TYPE_NOTHING,
                        FunIncATypes.TYPE_UNIT,
                        FunIncATypes.VISIBILITY_PRIVATE
                ),
                RESERVED_ID
        );

        keysPutEach(
                Arrays.asList(
                        FunIncATypes.ARROW,
                        FunIncATypes.COLON,
                        FunIncATypes.DOT
                ),
                RESERVED_OP
        );

        keysPutEach(Arrays.asList(FunIncATypes.COMMA), COMMA);

        keysPutEach(Arrays.asList(FunIncATypes.SQUARE_BRACKET_OPEN, FunIncATypes.SQUARE_BRACKET_CLOSE), BRACKETS);

        keysPutEach(Arrays.asList(FunIncATypes.PARENS_OPEN, FunIncATypes.PARENS_CLOSE), PARENTHESES);

        keysPutEach(Arrays.asList(FunIncATypes.BRACES_OPEN, FunIncATypes.BRACES_CLOSE), BRACES);

        keysPutEach(Arrays.asList(FunIncATypes.COMMENT), COMMENT);

        keysPutEach(Arrays.asList(FunIncATypes.INTEGER, FunIncATypes.LONG), INTEGER);

        keysPutEach(Arrays.asList(FunIncATypes.DOUBLE), DOUBLE);

        keysPutEach(Arrays.asList(FunIncATypes.QUOTATION_MARK, FunIncATypes.STRING), STRING);

        keysPutEach(Arrays.asList(FunIncATypes.KEYWORD_FOLD), FUNCALL);

        keysPutEach(
                Arrays.asList(
                        FunIncATypes.AND,
                        FunIncATypes.EQUIVALENCE,
                        FunIncATypes.GEQ,
                        FunIncATypes.GT,
                        FunIncATypes.LEQ,
                        FunIncATypes.LT,
                        FunIncATypes.MINUS,
                        FunIncATypes.MODULO,
                        FunIncATypes.NEGATION,
                        FunIncATypes.NON_EQUIVALENCE,
                        FunIncATypes.OR,
                        FunIncATypes.PLUS,
                        FunIncATypes.SET_INTERSECTION,
                        FunIncATypes.SET_UNION,
                        FunIncATypes.SLASH,
                        FunIncATypes.STAR
                ),
                OPSYM);

        keysPutEach(Arrays.asList(FunIncATypes.BACK_TICK, FunIncATypes.SCALATERM), SCALATERM);

        keysPutEach(Arrays.asList(FunIncATypes.ANNOTATION_MAIN), MAINANNOTATION);
    }


    @Override
    public @NotNull Lexer getHighlightingLexer() {
        return new FunIncALexerAdapter();
    }

    @Override
    public TextAttributesKey @NotNull [] getTokenHighlights(IElementType tokenType) {
        return pack(keys.get(tokenType), EMPTY);
    }
}
