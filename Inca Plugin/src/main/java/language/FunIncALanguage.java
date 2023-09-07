package language;

import com.intellij.lang.Language;

public class FunIncALanguage extends Language {

    public static final FunIncALanguage INSTANCE = new FunIncALanguage();

    private FunIncALanguage() {
        super("Functional IncA");
    }

}
