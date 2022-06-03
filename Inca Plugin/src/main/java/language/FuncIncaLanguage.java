package language;

import com.intellij.lang.Language;

public class FuncIncaLanguage extends Language {

    public static final FuncIncaLanguage INSTANCE = new FuncIncaLanguage();

    private FuncIncaLanguage() {
        super("Functional IncA");
    }

}
