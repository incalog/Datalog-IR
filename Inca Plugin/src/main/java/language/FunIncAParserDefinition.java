package language;

import com.intellij.lang.ASTNode;
import com.intellij.lang.ParserDefinition;
import com.intellij.lang.PsiParser;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IFileElementType;
import com.intellij.psi.tree.TokenSet;
import language.parser.FunIncAParser;
import language.psi.FunIncATypes;
import language.psi.FunIncAFile;
import org.jetbrains.annotations.NotNull;

public class FunIncAParserDefinition implements ParserDefinition{

    public static final TokenSet WHITE_SPACES = TokenSet.create(TokenType.WHITE_SPACE);
    public static final TokenSet COMMENTS = TokenSet.create(FunIncATypes.COMMENT);

    public static final IFileElementType FILE = new IFileElementType(FunIncALanguage.INSTANCE);

    @Override
    public @NotNull Lexer createLexer(Project project) {
        return new FunIncALexerAdapter();
    }

    @Override
    public @NotNull PsiParser createParser(Project project) {
        return new FunIncAParser();
    }

    @Override
    public @NotNull IFileElementType getFileNodeType() {
        return FILE;
    }

    @Override
    public @NotNull TokenSet getCommentTokens() {
        return COMMENTS;
    }

    @Override
    public @NotNull TokenSet getWhitespaceTokens() {
        return WHITE_SPACES;
    }

    @Override
    public @NotNull TokenSet getStringLiteralElements() {
        return TokenSet.EMPTY;
    }

    @Override
    public @NotNull PsiElement createElement(ASTNode node) {
        return FunIncATypes.Factory.createElement(node);
    }

    @Override
    public @NotNull PsiFile createFile(FileViewProvider viewProvider) {
        return new FunIncAFile(viewProvider);
    }

    @Override
    public SpaceRequirements spaceExistenceTypeBetweenTokens(ASTNode left, ASTNode right) {
        return SpaceRequirements.MAY;
    }


}
