package language;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.PsiReferenceProvider;
import com.intellij.util.ProcessingContext;
import org.jetbrains.annotations.NotNull;

public class FuncIncaReferenceProvider extends PsiReferenceProvider {
    @Override
    public PsiReference @NotNull [] getReferencesByElement(@NotNull PsiElement element, @NotNull ProcessingContext context){
        if (!element.getLanguage().is(FuncIncaLanguage.INSTANCE)) {
            return PsiReference.EMPTY_ARRAY;
        }
        if (element instanceof PsiElement) {
            PsiElement se = (PsiElement) element;
            return new PsiReference[]{new FuncIncaReference(se, se.getTextRange())};
        }
        return PsiReference.EMPTY_ARRAY;
    }
}
