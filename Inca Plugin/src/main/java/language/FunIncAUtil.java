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
        }
        */
        res.addAll(findDefinitionNode((FunIncAFile) psiFile, name, e));
        return res;
    }

    /*
     * finds all Psi Definition nodes named "name" in one file
     * if name = null return all found definition nodes
     * */
    public static List<PsiNamedElement> findDefinitionNode(@Nullable FunIncAFile file, @Nullable String name, @Nullable PsiElement e) {
        List<PsiNamedElement> res = new ArrayList<>();
        if (file == null)
            return new ArrayList<>();
        final FunIncASetComprehensionExp setParent = PsiTreeUtil.getParentOfType(e, FunIncASetComprehensionExp.class);
        final boolean isSetComprehension = setParent != null;
        // We only want to look for classes that match the element e we are resolving
        final Class<? extends PsiNamedElement> elementClass;
        if (e instanceof FunIncAConstructorRef) { // if e is a pattern in a match case, only constructor definitions are important
            elementClass = FunIncADataConstructorDef.class;
        } else if (isSetComprehension) // in a set comprehension everything can be a definition of e
            elementClass = FunIncANamedElement.class;
        else
            elementClass = FunIncADecl.class;

        Collection<PsiNamedElement> namedElements = PsiTreeUtil.findChildrenOfType(file, elementClass);

        if (isSetComprehension && name != null) { // TODO own function for readability
            // finding candidates for resolving references
            List<PsiNamedElement> resCandidates = new ArrayList<>();
            for (PsiNamedElement namedElement : namedElements) {
                if (name.equals(namedElement.getName())) {
                    if (namedElement instanceof FunIncAVarRefExp) {
                        FunIncASetComprehensionExp namedElementSetCompParent =
                                PsiTreeUtil.getParentOfType(namedElement, FunIncASetComprehensionExp.class);
                        FunIncASetMemberExp namedElementSetMemberParent =
                                PsiTreeUtil.getParentOfType(namedElement, FunIncASetMemberExp.class);
                        FunIncACallExp namedElementCallParent =
                                PsiTreeUtil.getParentOfType(namedElement, FunIncACallExp.class);
                        boolean isDefinition = false;// namedElement is only considered a definition if it is within a SetMemberExp within a SetComprehensionExp
                        if (namedElementSetCompParent != null && namedElementSetMemberParent != null)
                            isDefinition = PsiTreeUtil.isAncestor(namedElementSetCompParent, namedElementSetMemberParent, false);
                        boolean isPartOfCallExp = false; // excludes VarRefExp in CallExp within nested set comprehensions from being considered definitions
                        if (namedElementCallParent != null)
                            isPartOfCallExp = PsiTreeUtil.isAncestor(namedElementSetCompParent, namedElementCallParent, false);
                        if (namedElementSetCompParent == setParent
                                && namedElementSetMemberParent != null
                                && isDefinition
                                && !isPartOfCallExp) {
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
                            } else if (namedElement instanceof FunIncATypeVarDef
                                    && e instanceof FunIncATypeNameRef) {
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
                // the set-comprehension-expression.
                // the definitions outside the set comprehension shadow declarations in the set-comprehension-exp.
                // outside of SetCompExp only FunIncADecl can be valid definitions
                for (PsiNamedElement candidate : resCandidates) {
                    if (candidate instanceof FunIncADecl) {
                        res.add(candidate);
                    }
                }
                if (res.isEmpty()) {
                    // since res is empty, none of the found definitions are outside the set-comprehension-expression,
                    // so these definitions must be inside the set-comprehension, inside member-expressions.
                    // if there are more than one definition via member-expression, i.e.
                    // {(n1,n2) | (n2,n3) in set1, (n1,n2) in set2}, then the first definition of n2 has to be the resolved
                    // element, since the variable n2 is first introduced in "(n2,n3) in set1" and n2 in "(n1,n2)"
                    // references the first n2.
                    PsiNamedElement firstDef = resCandidates.get(0);
                    if (PsiTreeUtil.isAncestor(setParent.getExpList().get(0), firstDef, false)) {
                        if (resCandidates.size() > 1) {
                            resCandidates.remove(0);
                            firstDef = resCandidates.get(0);
                        } else { // otherwise there is no valid definition of e
                            return res;
                        }
                    }
                    for (PsiNamedElement candidate : resCandidates) {
                        if (candidate.getTextRange().getStartOffset() < firstDef.getTextRange().getStartOffset())
                            firstDef = candidate;
                    }
                    if (firstDef != e)
                        res.add(firstDef);
                    else {
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

                if (name.equals(namedElement.getName())) {

                    PsiNamedElement funParentNamedElement =
                            PsiTreeUtil.getParentOfType(namedElement, FunIncAFunDef.class);
                    PsiNamedElement funParentE = PsiTreeUtil.getParentOfType(e, FunIncAFunDef.class);
                    boolean isFunCall = false; // flag, that indicates if e is a part of a FunCallExp (function or argument)
                    boolean isCalledFunction = false; // flag, that indicates if e is the function called in the expression
                    FunIncACallExp funCall = PsiTreeUtil.getParentOfType(e, FunIncACallExp.class);
                    if (funCall != null) { // is e part of a function call?
                        isFunCall = true;
                        PsiElement fun = funCall.getFirstChild();
                        if (PsiTreeUtil.isAncestor(fun, e, false)) {
                            // is e part of the first child of the FunCallExp, first child being the called function
                            isCalledFunction = true;
                        }
                    }
                    if (PsiTreeUtil.getParentOfType(e, FunIncAFoldExp.class) != null)
                        isFunCall = true;

                    if (namedElement instanceof FunIncAVarDef) { // declaration is in let-exp
                        if (PsiTreeUtil.isAncestor(namedElement.getParent(), e, true) &&
                                funParentNamedElement == funParentE) {
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
                        if (isCalledFunction) {
                            if (paramDef.getType().getFunType() != null) {
                                if (funParentNamedElement == funParentE) {
                                    res.add(namedElement);
                                }
                            }
                        } else if (funParentNamedElement == funParentE) {
                            res.add(namedElement);
                        }
                    } else if (namedElement instanceof FunIncATypeVarDef // declaration is a type variable
                            && e instanceof FunIncATypeNameRef) { // can only be referenced by type name refs
                        FunIncADataDef dataDefParentE = PsiTreeUtil.getParentOfType(e, FunIncADataDef.class);
                        FunIncADataDef dataDefParentNamedElement =
                                PsiTreeUtil.getParentOfType(namedElement, FunIncADataDef.class);
                        if (dataDefParentE != null && dataDefParentE == dataDefParentNamedElement) {
                            // TypeVarDef in DataDef
                            res.add(namedElement);
                        } else if (funParentE != null && funParentNamedElement == funParentE) {
                            // TypeVarDef in FunDef
                            res.add(namedElement);
                        }
                    } else if (namedElement instanceof FunIncAFunDef && isFunCall){ // declaration is a function definition
                        res.add(namedElement);
                    } else if (namedElement instanceof FunIncADataDef // declaration is a data definition
                            && e instanceof FunIncATypeNameRef) {
                        res.add(namedElement);
                    } else if (namedElement instanceof FunIncADataConstructorDef) { // declaration is a constructor
                        if (e instanceof FunIncAConstructorRef || isFunCall)
                            res.add(namedElement);
                    }
                }
            }
        }
        return res;
    }

}
