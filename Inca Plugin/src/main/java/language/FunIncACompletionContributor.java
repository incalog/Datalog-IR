package language;

import com.intellij.codeInsight.completion.CompletionContributor;

public class FunIncACompletionContributor extends CompletionContributor {
    private static String[] keywords = new String[]{"if", "else", "let", "in", "match", "case", "fail", "Option",
            "None", "Some", "Set", "fold", "module", "import", "not", "data", "def"};

    public static String[] getKeywords(){
        return keywords.clone();
    }

    // finish named elements and referneces first
    // Q&A for implementation in CompletionContributor class file
}
