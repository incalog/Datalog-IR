package language;

import com.intellij.lexer.FlexAdapter;

class FuncIncaLexerAdapter extends FlexAdapter {
    public FuncIncaLexerAdapter() {
        super(new _FuncIncaLexer(null));
    }
}
