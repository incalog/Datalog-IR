package language;

import com.intellij.patterns.PlatformPatterns;
import com.intellij.patterns.PsiElementPattern;
import com.intellij.psi.*;
import org.jetbrains.annotations.NotNull;

public class FunIncAReferenceContributor extends PsiReferenceContributor {

    @Override
    public void registerReferenceProviders(@NotNull PsiReferenceRegistrar registrar) {
        PsiElementPattern.Capture<PsiNamedElement> variableCapture =
                PlatformPatterns.psiElement(PsiNamedElement.class).withLanguage(FunIncALanguage.INSTANCE);
        registrar.registerReferenceProvider(variableCapture, new FunIncAReferenceProvider());
    }

/*

    SIMPLE_PREFIX_STR = "simple"
    SIMPLE_SEPERATOR = ":"

    @Override
    public void registerReferenceProvider(@NotNull PsiReferenceRegistrar registrar){
        registrar.registerReferenceProvider(PlatformPatterns.psiElement(PsiLiteralExpression.class),
                new PsiReferenceProvider() {
                    @Override
                    public PsiReference @NotNull [] getReferencesByElement(@NotNull PsiElement element,
                                                                           @NotNull ProcessingContext context) {
                        PsiLiteralExpression literalExpression = (PsiLiteralExpression) element;
                        String value = literalExpression.getValue() instanceof String ?
                                (String) literalExpression.getValue() : null;
                        if ((value != null && value.startsWith(SIMPLE_PREFIX_STR + SIMPLE_SEPARATOR_STR))) {
                            TextRange property = new TextRange(SIMPLE_PREFIX_STR.length() + SIMPLE_SEPARATOR_STR.length() + 1,
                                    value.length() + 1);
                            return new PsiReference[]{new SimpleReference(element, property)};
                        }
                        return PsiReference.EMPTY_ARRAY;
                    }
                });
    }
    
 */
}
