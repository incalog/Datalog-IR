package language;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.HighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import language.psi.FuncIncaTypes;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static com.intellij.openapi.editor.colors.TextAttributesKey.createTextAttributesKey;

public class FuncIncaSyntaxHighlighter extends SyntaxHighlighterBase {

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

    public static final TextAttributesKey STRING
            = TextAttributesKey.createTextAttributesKey("HS_STRING", DefaultLanguageHighlighterColors.STRING);

    public static final TextAttributesKey OPSYM
            = TextAttributesKey.createTextAttributesKey("INCA_OPSYM", DefaultLanguageHighlighterColors.OPERATION_SIGN);

    public static final TextAttributesKey SCALATERM
            = TextAttributesKey.createTextAttributesKey("INCA_SCALATERM", DefaultLanguageHighlighterColors.FUNCTION_CALL);

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
                        FuncIncaTypes.ANNOTATION_MAIN,
                        FuncIncaTypes.BOOLEAN_FALSE,
                        FuncIncaTypes.BOOLEAN_TRUE,
                        FuncIncaTypes.CAST,
                        FuncIncaTypes.KEYWORD_CASE,
                        FuncIncaTypes.KEYWORD_DATA,
                        FuncIncaTypes.KEYWORD_DEF,
                        FuncIncaTypes.KEYWORD_ELSE,
                        FuncIncaTypes.KEYWORD_FAIL,
                        FuncIncaTypes.KEYWORD_FOLD,
                        FuncIncaTypes.KEYWORD_IF,
                        FuncIncaTypes.KEYWORD_IMPORT,
                        FuncIncaTypes.KEYWORD_IN,
                        FuncIncaTypes.KEYWORD_LET,
                        FuncIncaTypes.KEYWORD_MATCH,
                        FuncIncaTypes.KEYWORD_MODULE,
                        FuncIncaTypes.KEYWORD_NONE,
                        FuncIncaTypes.KEYWORD_OPTION,
                        FuncIncaTypes.KEYWORD_SET,
                        FuncIncaTypes.KEYWORD_SOME,
                        FuncIncaTypes.TYPE_ANY,
                        FuncIncaTypes.TYPE_NOTHING,
                        FuncIncaTypes.TYPE_UNIT,
                        FuncIncaTypes.VISIBILITY_PRIVATE
                ),
                RESERVED_ID
        );

        keysPutEach(
                Arrays.asList(
                        FuncIncaTypes.ARROW,
                        FuncIncaTypes.COLON,
                        FuncIncaTypes.DOT
                ),
                RESERVED_OP
        );

        keysPutEach(Arrays.asList(FuncIncaTypes.COMMA), COMMA);

        keysPutEach(Arrays.asList(FuncIncaTypes.SQUARE_BRACKET_OPEN, FuncIncaTypes.SQUARE_BRACKET_CLOSE), BRACKETS);

        keysPutEach(Arrays.asList(FuncIncaTypes.PARENS_OPEN, FuncIncaTypes.PARENS_CLOSE), PARENTHESES);

        keysPutEach(Arrays.asList(FuncIncaTypes.BRACES_OPEN, FuncIncaTypes.BRACES_CLOSE), BRACES);

        keysPutEach(Arrays.asList(FuncIncaTypes.COMMENT), COMMENT);

        keysPutEach(Arrays.asList(FuncIncaTypes.NUMBER), INTEGER);

        keysPutEach(Arrays.asList(FuncIncaTypes.DOUBLE), DOUBLE);

        keysPutEach(Arrays.asList(FuncIncaTypes.QUOTATION_MARK, FuncIncaTypes.STRING), STRING);

        keysPutEach(
                Arrays.asList(
                        FuncIncaTypes.AND,
                        FuncIncaTypes.EQUIVALENCE,
                        FuncIncaTypes.GEQ,
                        FuncIncaTypes.GT,
                        FuncIncaTypes.LEQ,
                        FuncIncaTypes.LT,
                        FuncIncaTypes.MINUS,
                        FuncIncaTypes.MODULO,
                        FuncIncaTypes.NEGATION,
                        FuncIncaTypes.NON_EQUIVALENCE,
                        FuncIncaTypes.OR,
                        FuncIncaTypes.PLUS,
                        FuncIncaTypes.SET_INTERSECTION,
                        FuncIncaTypes.SET_UNION,
                        FuncIncaTypes.SLASH,
                        FuncIncaTypes.STAR
                ),
                OPSYM);

        keysPutEach(Arrays.asList(FuncIncaTypes.BACK_TICK, FuncIncaTypes.SCALA_TERM), SCALATERM);
    }


    @Override
    public @NotNull Lexer getHighlightingLexer() {
        return new FuncIncaLexerAdapter();
    }

    @Override
    public TextAttributesKey @NotNull [] getTokenHighlights(IElementType tokenType) {
        return pack(keys.get(tokenType), EMPTY);
    }
}
