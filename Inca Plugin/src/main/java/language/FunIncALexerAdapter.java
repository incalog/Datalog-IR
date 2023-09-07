package language;

import com.intellij.lexer.FlexAdapter;

class FunIncALexerAdapter extends FlexAdapter {
    public FunIncALexerAdapter() {
        super(new _FunIncALexer(null));
    }
}
