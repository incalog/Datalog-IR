package language;

import com.intellij.patterns.PlatformPatterns;
import com.intellij.patterns.PsiElementPattern;
import com.intellij.psi.*;
import org.jetbrains.annotations.NotNull;

public class FuncIncaReferenceContributor extends PsiReferenceContributor {

    @Override
    public void registerReferenceProviders(@NotNull PsiReferenceRegistrar registrar) {
        PsiElementPattern.Capture<PsiNamedElement> variableCapture =
                PlatformPatterns.psiElement(PsiNamedElement.class).withLanguage(FuncIncaLanguage.INSTANCE);
        registrar.registerReferenceProvider(variableCapture, new FuncIncaReferenceProvider());
    }
}
