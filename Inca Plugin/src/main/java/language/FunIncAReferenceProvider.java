package language;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.PsiReferenceProvider;
import com.intellij.util.ProcessingContext;
import org.jetbrains.annotations.NotNull;

public class FunIncAReferenceProvider extends PsiReferenceProvider {
    @Override
    public PsiReference @NotNull [] getReferencesByElement(@NotNull PsiElement element, @NotNull ProcessingContext context){
        if (!element.getLanguage().is(FunIncALanguage.INSTANCE)) {
            return PsiReference.EMPTY_ARRAY;
        }
        return new PsiReference[]{new FunIncAReference(element, element.getTextRange())};
        // return PsiReference.EMPTY_ARRAY;
    }
}
