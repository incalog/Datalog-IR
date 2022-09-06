package language;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class FuncIncaReference extends PsiReferenceBase<PsiNamedElement> implements PsiPolyVariantReference {

    private String name;
    public static final ResolveResult[] EMPTY_RESOLVE_RESULT = new ResolveResult[0];

    public FuncIncaReference(@NotNull PsiNamedElement element, TextRange textRange) {
        super(element, textRange);
        name = element.getName();
    }

    @Override
    public ResolveResult @NotNull [] multiResolve(boolean incompleteCode) {
        return new ResolveResult[0];
    }

    @Override
    public @Nullable PsiElement resolve() {
        ResolveResult[] resolveResults = multiResolve(false);
        if(resolveResults.length == 1)
            return resolveResults[0].getElement();
        else
            return null;
    }
}
