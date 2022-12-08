package language;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.psi.PsiElement;
import language.psi.*;
import language.types.FuncIncaTypechecker;
import org.jetbrains.annotations.NotNull;

public class FuncIncaAnnotator implements Annotator {
    @Override
    public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {

        FuncIncaTypechecker.checkTypes(element, holder);
        
    }

}
