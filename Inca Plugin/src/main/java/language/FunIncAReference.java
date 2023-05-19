package language;

import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import language.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class FunIncAReference extends PsiReferenceBase<PsiElement> implements PsiPolyVariantReference {

    private final String name;
    public static final ResolveResult[] EMPTY_RESOLVE_RESULT = new ResolveResult[0];

    public FunIncAReference(@NotNull PsiElement element, TextRange textRange) {
        super(element, new TextRange(0, textRange.getLength()));
        name = element.getText().substring(0, textRange.getLength());
    }

    @Override
    // for example for overloading a method
    public ResolveResult @NotNull [] multiResolve(boolean incompleteCode) {
        if(!(myElement instanceof FunIncATypeNameRef || myElement instanceof FunIncAVarRefExp || myElement instanceof FunIncAConstructorRef))
            return EMPTY_RESOLVE_RESULT;
        Project project = myElement.getProject();
        final List<PsiNamedElement> namedElements = FunIncAUtil.findDefinitionNode(project, name, myElement);
        List<ResolveResult> res = new ArrayList<ResolveResult>();
        for(PsiNamedElement element: namedElements){
            res.add(new PsiElementResolveResult(element));
        }
        return res.toArray(new ResolveResult[res.size()]);
    }

    @Override
    public @Nullable PsiElement resolve() {
        ResolveResult[] resolveResults = multiResolve(false);
        if(resolveResults.length == 1)
            return resolveResults[0].getElement();
        else
            return null;
    }

    // this method is provided by the interface PsiReference and will help with basic code completion
    public Object @NotNull [] getVariants(){
        if (PsiTreeUtil.getParentOfType(myElement, FunIncAExp.class) == null) {
            return new Object[]{};
        }
        final PsiFile file = myElement.getContainingFile();
        List<PsiNamedElement> namedElements = FunIncAUtil.findDefinitionNode((FunIncAFile)file, null, null);
        List<LookupElement> variants = new ArrayList<>();
        for(final PsiNamedElement namedElement : namedElements){
            final String name = namedElement.getName();
            if (name == null) { continue; }
            variants.add(LookupElementBuilder.create(name));
        }
        return variants.toArray();
    }
}
