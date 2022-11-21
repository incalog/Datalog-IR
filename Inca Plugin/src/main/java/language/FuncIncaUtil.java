package language;

import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.search.FileTypeIndex;
import language.psi.*;
import com.intellij.openapi.project.Project;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

/**
 * General util class. Provides methods for finding named nodes in the Psi tree.
 */
public class FuncIncaUtil {
    /*
    * finds all Psi Definition nodes named "name" in the whole project. For name = null find all definitions.*/
    public static List<PsiNamedElement> findDefinitionNode(@NotNull Project project, @Nullable String name, @NotNull PsiElement e){
        List<PsiNamedElement> res = new ArrayList<>();
        final PsiFile psiFile = e.getContainingFile().getOriginalFile();
        // this would be the place for getting the imported modules
        // following for loop gets definition from every file in the directory, expand for supported import
        Collection<VirtualFile> virtualFiles = FileTypeIndex.getFiles(FuncIncaFileType.INSTANCE, GlobalSearchScope.projectScope(project));
        for (VirtualFile virtualFile : virtualFiles){
            FuncIncaFile f = (FuncIncaFile) PsiManager.getInstance(project).findFile(virtualFile);
            res.addAll(findDefinitionNode(f, name, e));
        }
        return res;
    }

    /*
    * finds all Psi Definition nodes named "name" in one file*/
    public static List<PsiNamedElement> findDefinitionNode(@Nullable FuncIncaFile file, @Nullable String name, @Nullable PsiElement e){
        List<PsiNamedElement> res = new ArrayList<>();
        if(file == null)
            return res;
        final FuncIncaComprehensionExp setParent = PsiTreeUtil.getParentOfType(e, FuncIncaComprehensionExp.class);
        final Boolean setComprehension = setParent != null;
        // We only want to look for classes that match the element e we are resolving
        final Class<? extends PsiNamedElement> elementClass;
        if(e instanceof FuncIncaConsId)
            elementClass = FuncIncaDataConstructor.class;
        else if(setComprehension)
            elementClass = FuncIncaNamedElement.class;
        else
            elementClass = FuncIncaDecl.class;

        Collection<PsiNamedElement> namedElements = PsiTreeUtil.findChildrenOfType(file, elementClass);

        if(setComprehension && name != null){
            // finding candidates for resolving references
            List<PsiNamedElement> resCandidates = new ArrayList<>();
            for (PsiNamedElement namedElement : namedElements) {
                if(name.equals(namedElement.getName()) && namedElement != e){ // excludes e from resolved candidates
                    if(namedElement instanceof FuncIncaVar){
                        if(PsiTreeUtil.getParentOfType(namedElement, FuncIncaComprehensionExp.class) == setParent &&
                                PsiTreeUtil.getParentOfType(namedElement, FuncIncaMemberExp.class) != null) {
                            resCandidates.add(namedElement); // declarations with FuncIncaVar in rhs can only be member-expressions
                        }
                    }
                    if(namedElement instanceof FuncIncaDecl){ // possible let-expressions in rhs are excluded
                        if(e.getTextRange().getStartOffset() > namedElement.getTextRange().getStartOffset())
                            resCandidates.add(namedElement);
                    }
                }
            }

            if(resCandidates.size() == 1) { // there is only one declaration of e, return the declaration
                  // TODO check typing
                  res.add(resCandidates.get(0));
            }
            else { // there are two or more declarations of e. The ones outside the set comprehension shadow declarations in set
                for(PsiNamedElement node : resCandidates){
                    if(node instanceof FuncIncaDecl){
                        //TODO check typing
                        res.add(node);
                        continue;
                    } // we dont need to add the declarations via member-expression.
                    // if there is 1 declaration via member expression, there must be at least 1 via let-expression.
                    // if there are 2 or more declarations via member expression, these declarations cannot be resolved
                    // and are therfore invalid
                }
            }


        } else { // if e is not in a set comprehension proceed as normal
            PsiNamedElement funDefParentOfNamedElement;
            PsiNamedElement funDefParentOfE = PsiTreeUtil.getParentOfType(e, FuncIncaFunDef.class);

            for(PsiNamedElement namedElement: namedElements){
                if(name == null){
                    res.add(namedElement);
                    continue;
                }
                funDefParentOfNamedElement =
                        PsiTreeUtil.getParentOfType(namedElement, FuncIncaFunDef.class);
                if(name.equals(namedElement.getName())){
                    if (namedElement instanceof FuncIncaVarId) {// Decl in Let-exp
                        if (namedElement.getTextRange().getStartOffset() < e.getTextRange().getStartOffset() &&
                                funDefParentOfNamedElement == funDefParentOfE) {
                            res.add(namedElement);
                            // TODO type checking
                        }
                    } else if (namedElement instanceof FuncIncaConsPatternId){
                        if (PsiTreeUtil.getParentOfType(namedElement, FuncIncaMatchCase.class) ==
                                PsiTreeUtil.getParentOfType(e, FuncIncaMatchCase.class)){
                            // TODO type checking
                            res.add(namedElement);
                        }
                    } else if (namedElement instanceof FuncIncaParam){
                        if (funDefParentOfNamedElement == funDefParentOfE){
                            res.add(namedElement);
                            // TODO type checking
                        }
                    } else if (namedElement instanceof FuncIncaParamType){
                        if (funDefParentOfNamedElement == funDefParentOfE){
                            res.add(namedElement);
                            // TODO type checking
                        }
                    } else {
                    // TODO type checking
                    res.add(namedElement);
                    }
                }
            }
        }
        // TODO check typing
        return res;

    }


}
