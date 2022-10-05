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

public class FuncIncaReference extends PsiReferenceBase<PsiNamedElement> implements PsiPolyVariantReference {

    private String name;
    public static final ResolveResult[] EMPTY_RESOLVE_RESULT = new ResolveResult[0];

    public FuncIncaReference(@NotNull PsiNamedElement element, TextRange textRange) {
        super(element, textRange);
        name = element.getName();
    }

    @Override
    // for example for overloading a method
    public ResolveResult @NotNull [] multiResolve(boolean incompleteCode) {
        Project project = myElement.getProject();
        final List<PsiNamedElement> namedElements = FuncIncaUtil.findDefinitionNode(project, name, myElement);
        List<ResolveResult> res = new ArrayList<>();
        for(PsiNamedElement element: namedElements){
            res.add(new PsiElementResolveResult(element));
        }
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

    public Object[] getVariants(){
        // If we are not in an expression, don't provide reference completion.
        if (PsiTreeUtil.getParentOfType(myElement, FuncIncaExp.class) == null) {
            return new Object[]{};
        }
        final PsiFile file = myElement.getContainingFile();
        List<PsiNamedElement> namedElements = FuncIncaUtil.findDefinitionNode((FuncIncaFile)file, null, null);
        List<LookupElement> variants = new ArrayList<>();
        for(final PsiNamedElement namedElement : namedElements){
            final String name = namedElement.getName();
            if (name == null) { continue; }
            final PsiFile psiFile = namedElement.getContainingFile();
            final String type;
            // TODO type
            type = "";
            variants.add(LookupElementBuilder.create(name).withIcon(FuncIncaIcons.FILE).withTypeText(type));
        }
        return variants.toArray();
    }
}
