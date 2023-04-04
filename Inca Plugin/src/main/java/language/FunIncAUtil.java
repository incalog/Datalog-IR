package language;

import com.intellij.psi.*;
import language.psi.*;
import com.intellij.openapi.project.Project;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

/**
 * General util class. Provides methods for finding named nodes in the Psi tree.
 */
public class FunIncAUtil {
    /*
     * finds all Psi Definition nodes named "name" in the whole project. For name = null find all definitions.*/
    public static List<PsiNamedElement> findDefinitionNode(@NotNull Project project, @Nullable String name, @NotNull PsiElement e) {
        List<PsiNamedElement> res = new ArrayList<>();
        final PsiFile psiFile = e.getContainingFile().getOriginalFile();
        // this would be the place for getting the imported modules
        // following for loop gets every file in the directory, expand for supported import
        /*
        Collection<VirtualFile> virtualFiles = FileTypeIndex.getFiles(FunIncAFileType.INSTANCE, GlobalSearchScope.projectScope(project));
        for (VirtualFile virtualFile : virtualFiles) {
            FunIncAFile f = (FunIncAFile) PsiManager.getInstance(project).findFile(virtualFile);
            res.addAll(findDefinitionNode(f, name, e));
        }*/
        res.addAll(findDefinitionNode((FunIncAFile) psiFile, name, e));
        return res;
    }

    // TODO fix set comprehensions
    /*
     * finds all Psi Definition nodes named "name" in one file
     * if name = null return all found definition nodes
     * */
    public static List<PsiNamedElement> findDefinitionNode(@Nullable FunIncAFile file, @Nullable String name, @Nullable PsiElement e) {
        List<PsiNamedElement> res = new ArrayList<>();
        if (file == null)
            return new ArrayList<PsiNamedElement>();
        final FunIncASetComprehensionExp setParent = PsiTreeUtil.getParentOfType(e, FunIncASetComprehensionExp.class);
        final boolean isSetComprehension = setParent != null;
        // We only want to look for classes that match the element e we are resolving
        final Class<? extends PsiNamedElement> elementClass;
        if (e instanceof FunIncATypeNameRef) // if e is type constructor of TypeNameRef, only DataDef are important
            elementClass = FunIncADataDef.class;
        else if (e instanceof FunIncAConstructorRef) { // if e is a pattern in a match case, only constructor definitions are important
            elementClass = FunIncADataConstructorDef.class;
        } else if (isSetComprehension) // in a set comprehension everything can be a definition of e
            elementClass = FunIncANamedElement.class;
        else
            elementClass = FunIncADecl.class;

        Collection<PsiNamedElement> namedElements = PsiTreeUtil.findChildrenOfType(file, elementClass);

        if (isSetComprehension && name != null) {
            // finding candidates for resolving references
            List<PsiNamedElement> resCandidates = new ArrayList<>();
            for (PsiNamedElement namedElement : namedElements) {
                if (name.equals(namedElement.getName())) {
                    if (namedElement instanceof FunIncAVarRefExp) {
                        if (PsiTreeUtil.getParentOfType(namedElement, FunIncASetComprehensionExp.class) == setParent && // TODO named element set comprehension is ancestor to setParent bc bei verschachtelten setcompr können innere auf äußere zugreifen
                                PsiTreeUtil.getParentOfType(namedElement, FunIncASetMemberExp.class) != null) { // TODO aber nur die bereits definierten?
                            // declarations with FuncIncaVar in predicates can only be member-expressions
                            resCandidates.add(namedElement);
                        }
                    }
                    if (namedElement instanceof FunIncADecl) { // every other possible variable definition
                        FunIncAFunDef funParentNamedElement = PsiTreeUtil.getParentOfType(namedElement, FunIncAFunDef.class);
                        FunIncAFunDef funParentE = PsiTreeUtil.getParentOfType(e, FunIncAFunDef.class);
                        if (funParentNamedElement == funParentE) { // declaration of e must be within the same function definition
                            if (namedElement instanceof FunIncAVarDef) { // declaration in let expressions
                                if (e.getTextRange().getStartOffset() > namedElement.getTextRange().getStartOffset()) // possible let-expressions in rhs are excluded
                                    resCandidates.add(namedElement);
                            } else if (namedElement instanceof FunIncAParamDef) {
                                resCandidates.add(namedElement);
                            } else if (namedElement instanceof FunIncAPatternVarDef) {
                                FunIncAMatchCase matchCaseParent = PsiTreeUtil.getParentOfType(
                                        namedElement, FunIncAMatchCase.class);
                                if (PsiTreeUtil.isAncestor(matchCaseParent, e, true))
                                    resCandidates.add(namedElement);
                            } else if (namedElement instanceof FunIncADataConstructorDef) {
                                resCandidates.add(namedElement);
                            }
                        } else if (namedElement instanceof FunIncAFunDef) {
                            if (PsiTreeUtil.getParentOfType(e, FunIncACallExp.class) != null) // only resolve to FunDef if e is part of a function call
                                resCandidates.add(namedElement);
                        } else if (namedElement instanceof FunIncADataDef) {
                            resCandidates.add(namedElement);
                        }

                    }
                }
            }

            if (resCandidates.size() > 0) { // there may be two or more declarations of e or declarations inside
                // of the set-comprehension-expression
                // the ones outside the set comprehension shadow declarations in the set-comprehension-exp
                for (PsiNamedElement node : resCandidates) {
                    if (node instanceof FunIncADecl) {
                        res.add(node);
                    } // we dont need to add the declarations via member-expression.
                    // if there is 1 declaration via member expression, there must be at least 1 via let-expression.
                    // if there are 2 or more declarations via member expression, these declarations cannot be resolved
                    // and are therefore invalid
                }
                if (res.isEmpty()) {
                    // since res is empty, none of the found definitions are outside the set-comprehension-expression,
                    // so these definitions must be inside the set-comprehension, inside member-expressions.
                    // if there are more than one definition via member-expression, i.e.
                    // {(n1,n2) | (n2,n3) in set1, (n1,n2) in set2}, then the first definition of n2 has to be the resolved
                    // element, since the variable n2 is first introduced in "(n2,n3) in set1" and n2 in "(n1,n2)"
                    // references the first n2.
                    PsiNamedElement firstDef = resCandidates.get(0);
                    for (PsiNamedElement candidate : resCandidates) {
                        if (candidate.getTextRange().getStartOffset() < firstDef.getTextRange().getStartOffset())
                            firstDef = candidate;
                    }
                    if (firstDef != e)
                        res.add(firstDef);
                    else { //
                        FunIncASetMemberExp memberParent = PsiTreeUtil.getParentOfType(e, FunIncASetMemberExp.class);
                        if (resCandidates.size() > 1 &&
                                PsiTreeUtil.isAncestor(memberParent, setParent, true)) {
                            resCandidates.remove(e);
                            firstDef = resCandidates.get(0);
                            for (PsiNamedElement candidate : resCandidates) {
                                if (candidate.getTextRange().getStartOffset() < firstDef.getTextRange().getStartOffset())
                                    firstDef = candidate;
                            }
                            res.add(firstDef);
                        }
                    }
                }
            }


        } else { // if e is not in a set comprehension proceed as normal and check all found named elements
            for (PsiNamedElement namedElement : namedElements) {

                if (name == null) {
                    res.add(namedElement);
                    continue;
                }

                PsiNamedElement funDefParentOfNamedElement;
                PsiNamedElement funDefParentOfE = PsiTreeUtil.getParentOfType(e, FunIncAFunDef.class);
                funDefParentOfNamedElement =
                        PsiTreeUtil.getParentOfType(namedElement, FunIncAFunDef.class);
                boolean isFunCall = false;
                boolean isCallingFunction = false;
                FunIncACallExp funCall = PsiTreeUtil.getParentOfType(e, FunIncACallExp.class);
                if (funCall != null) { // is e part of a function call?
                    isFunCall = true;
                    PsiElement fun = funCall.getFirstChild();
                    if (PsiTreeUtil.isAncestor(fun, e, false)) {
                        isCallingFunction = true;
                    }
                }
                if (PsiTreeUtil.getParentOfType(e, FunIncAFoldExp.class) != null)
                    isFunCall = true;

                if (name.equals(namedElement.getName())) {
                    if (namedElement instanceof FunIncAVarDef) { // declaration is in let-exp
                        if (PsiTreeUtil.isAncestor(namedElement.getParent(), e, true) &&
                                funDefParentOfNamedElement == funDefParentOfE) {
                            res.add(namedElement);
                        }
                    } else if (namedElement instanceof FunIncAPatternVarDef) { // declaration is in pattern match case
                        FunIncAMatchCase matchCaseParent =
                                PsiTreeUtil.getParentOfType(namedElement, FunIncAMatchCase.class);
                        if (PsiTreeUtil.isAncestor(matchCaseParent, e, false)) {
                            res.add(namedElement);
                        }
                    } else if (namedElement instanceof FunIncAParamDef) { // declaration is a parameter
                        FunIncAParamDef paramDef = (FunIncAParamDef) namedElement;
                        if (isCallingFunction) {
                            if (paramDef.getType().getFunType() != null) {
                                if (funDefParentOfNamedElement == funDefParentOfE) {
                                    res.add(namedElement);
                                }
                            }
                        } else if (funDefParentOfNamedElement == funDefParentOfE) {
                            res.add(namedElement);
                        }
                    } else if (namedElement instanceof FunIncATypeVarDef && !isFunCall) { // declaration is a type variable
                        if (funDefParentOfNamedElement == funDefParentOfE) {
                            res.add(namedElement);
                        }
                    } else if (namedElement instanceof FunIncAFunDef && isFunCall){ // declaration is a function definition
                        res.add(namedElement);
                    } else if (namedElement instanceof FunIncADataDef) { // declaration is a type name
                        res.add(namedElement);
                    } else if (namedElement instanceof FunIncADataConstructorDef) { // declaration is a constructor
                        res.add(namedElement);
                    }
                }
            }
        }
        return res;
    }

}
