package language;

import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.psi.*;
import language.psi.*;
import com.intellij.openapi.project.Project;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

/**
 * General util class. Provides methods for finding Named Elements in the Psi tree.
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

        // resolving a reference from the inside of a set comprehension
        if (isSetComprehension && name != null) {
            // finding candidates for resolving references
            List<PsiNamedElement> resCandidates = new ArrayList<>();
            for (PsiNamedElement namedElement : namedElements) {
                if (name.equals(namedElement.getName())) {
                    // possible variable definitions outside of the SetComprehension
                    if (namedElement instanceof FunIncADecl) {
                        if (isDefinitionNode(namedElement, e))
                            resCandidates.add(namedElement);
                    } else if (namedElement instanceof FunIncAVarRefExp) {
                        // possible variable definitions inside the SetComprehensionExp
                        FunIncASetComprehensionExp namedElementSetCompParent =
                                PsiTreeUtil.getParentOfType(namedElement, FunIncASetComprehensionExp.class);
                        FunIncASetMemberExp namedElementSetMemberParent =
                                PsiTreeUtil.getParentOfType(namedElement, FunIncASetMemberExp.class);
                        FunIncACallExp namedElementCallParent =
                                PsiTreeUtil.getParentOfType(namedElement, FunIncACallExp.class);
                        boolean isDefinition = false;// namedElement is only considered a definition if it is within a SetMemberExp within a SetComprehensionExp
                        if (namedElementSetCompParent != null && namedElementSetMemberParent != null)
                            isDefinition =
                                    PsiTreeUtil.isAncestor(namedElementSetCompParent, namedElementSetMemberParent, false);
                        boolean isPartOfCallExp = false; // excludes VarRefExp in CallExp within set comprehensions from being considered definitions
                        if (namedElementCallParent != null)
                            isPartOfCallExp = PsiTreeUtil.isAncestor(namedElementSetMemberParent, namedElementCallParent, false);
                        if (namedElementSetCompParent == setParent
                                && namedElementSetMemberParent != null
                                && isDefinition
                                && !isPartOfCallExp) {
                        // declarations with FuncIncaVar in predicates can only be member-expressions
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
                    for (PsiNamedElement candidate : resCandidates) {
                        if (candidate.getTextRange().getStartOffset() < firstDef.getTextRange().getStartOffset())
                            firstDef = candidate;
                    }
                    if (firstDef != e)
                        res.add(firstDef);
                } else { // res was not empty
                    // only definitions outside of set comprehension
                    // shadowing when more than 1 definition was found
                    if (res.size() > 1) {
                        res = shadowing(res, e);
                    }
                }
            }


        } else { // if e is not in a set comprehension proceed as normal and check all found named elements
            for (PsiNamedElement namedElement : namedElements) {
                if (name == null)
                    res.add(namedElement);
                else if (name.equals(namedElement.getName())) {
                    if (isDefinitionNode(namedElement, e))
                        res.add(namedElement);
                }
            }
            // shadowing
            res = shadowing(res, e);
        }
        return res;
    }

    private static boolean isDefinitionNode(PsiNamedElement namedElement, PsiElement e) {

        if (namedElement instanceof FunIncAVarDef) { // declaration is in let-exp
            if (PsiTreeUtil.isAncestor(namedElement.getParent(), e, true)) {
                return true;
            }
        } else if (namedElement instanceof FunIncAPatternVarDef) { // declaration is in pattern match case
            FunIncAMatchCase matchCaseParent =
                    PsiTreeUtil.getParentOfType(namedElement, FunIncAMatchCase.class);
            if (PsiTreeUtil.isAncestor(matchCaseParent, e, false)) {
                return true;
            }
        } else if (namedElement instanceof FunIncAParamDef) { // declaration is a parameter
            FunIncAFunDef funParentNamedElement =
                    PsiTreeUtil.getParentOfType(namedElement, FunIncAFunDef.class);
            FunIncAFunDef funParentE =
                    PsiTreeUtil.getParentOfType(e, FunIncAFunDef.class);
            if (funParentNamedElement == funParentE) {
                return true;
            }
        } else if (namedElement instanceof FunIncATypeVarDef // declaration is a type variable
                && e instanceof FunIncATypeNameRef) { // can only be referenced by type name refs
            FunIncADataDef dataDefParentE =
                    PsiTreeUtil.getParentOfType(e, FunIncADataDef.class);
            FunIncADataDef dataDefParentNamedElement =
                    PsiTreeUtil.getParentOfType(namedElement, FunIncADataDef.class);
            FunIncAFunDef funParentNamedElement =
                    PsiTreeUtil.getParentOfType(namedElement, FunIncAFunDef.class);
            FunIncAFunDef funParentE =
                    PsiTreeUtil.getParentOfType(e, FunIncAFunDef.class);
            if (dataDefParentE != null && dataDefParentE == dataDefParentNamedElement) {
                // TypeVarDef in DataDef
                return true;
            } else if (funParentE != null && funParentNamedElement == funParentE) {
                // TypeVarDef in FunDef
                return true;
            }
        } else if (namedElement instanceof FunIncAFunDef){ // declaration is a function definition
            return true;
        } else if (namedElement instanceof FunIncADataDef // declaration is a data definition
                && e instanceof FunIncATypeNameRef) {
            return true;
        } else if (namedElement instanceof FunIncADataConstructorDef) { // declaration is a constructor
            return true;
        }
        // if these cases are not true, then namedElement cannot be a definition of e
        return false;
    }

    private static List<PsiNamedElement> shadowing(List<PsiNamedElement> definitionNodes, PsiElement e) {
        if (e instanceof FunIncATypeNameRef) {
            List<PsiNamedElement> dataDef = new ArrayList<>();
            List<PsiNamedElement> typeVar = new ArrayList<>();
            for (PsiNamedElement node : definitionNodes)
                if (node instanceof FunIncADataDef)
                    dataDef.add(node);
                else if (node instanceof FunIncATypeVarDef)
                    typeVar.add(node);
            // data definitions shadow type variables with the same name
            if (!dataDef.isEmpty())
                return dataDef;
            else
                return typeVar;
        } else if (e instanceof FunIncAVarRefExp) {
            List<PsiNamedElement> varDef = new ArrayList<>();
            List<PsiNamedElement> param = new ArrayList<>();
            List<PsiNamedElement> funcs = new ArrayList<>();
            List<PsiNamedElement> patternVar = new ArrayList<>();
            for (PsiNamedElement node : definitionNodes)
                if (node instanceof FunIncAVarDef)
                    varDef.add(node);
                else if (node instanceof FunIncAParamDef)
                    param.add(node);
                else if (node instanceof FunIncAFunDef || node instanceof FunIncADataConstructorDef)
                    funcs.add(node);
                else if (node instanceof FunIncAPatternVarDef)
                    patternVar.add(node);

            if (!varDef.isEmpty() && patternVar.isEmpty()) {
                int n = varDef.size();
                if (n == 1) {
                    if (!param.isEmpty())
                        FunIncAAnnotator.newAnnotation(HighlightSeverity.ERROR,
                                "Shadows previous definition of " + e.getText(),
                                varDef.get(0));
                    return varDef;
                } else {
                    FunIncAAnnotator.newAnnotation(HighlightSeverity.ERROR,
                            "Shadows previous definition of " + e.getText(),
                            varDef.get(n-1));
                    return Collections.singletonList(varDef.get(n-1));
                }
            }
            else if (varDef.isEmpty() && !patternVar.isEmpty()) {
                int n = patternVar.size();
                if (n == 1)
                    return patternVar;
                else {
                    FunIncAAnnotator.newAnnotation(HighlightSeverity.ERROR,
                            "Shadows previous definition of " + e.getText(),
                            patternVar.get(n-1));
                    return Collections.singletonList(patternVar.get(n-1));
                }
            }
            else if (!varDef.isEmpty() && !patternVar.isEmpty()) {
                int n = varDef.size();
                int m = patternVar.size();
                PsiNamedElement lastVarDef = varDef.get(n-1);
                PsiNamedElement lastPatVar = patternVar.get(m-1);
                PsiNamedElement lastDef;
                if (lastPatVar.getTextRange().getStartOffset() < lastVarDef.getTextRange().getStartOffset())
                    lastDef = lastVarDef;
                else
                    lastDef = lastPatVar;
                FunIncAAnnotator.newAnnotation(HighlightSeverity.ERROR,
                        "Shadows previous definition of " + e.getText(),
                        lastDef);
                return Arrays.asList(lastDef);
            }
            else if (!param.isEmpty())
                return param;
            else
                return funcs;
        } else if (e instanceof FunIncAConstructorRef) {
            List<PsiNamedElement> cons = new ArrayList<>();
            for (PsiNamedElement node : definitionNodes)
                if (node instanceof FunIncADataConstructorDef)
                    cons.add(node);
            return cons;
        }
        return new ArrayList<>();
    }

}
