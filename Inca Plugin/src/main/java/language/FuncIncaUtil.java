package language;

import com.google.common.collect.Lists;
import language.psi.*;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementResolveResult;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

/**
 * General util class. Provides methods for finding named nodes in the Psi tree.
 */
public class FuncIncaUtil {

    /**
    * finds the definition of functions with name in a module
    */
    public static List<FuncIncaFunDef> findFunDefs(@NotNull Project project, @Nullable String name){
        List<FuncIncaFunDef> res = new ArrayList<>();
        return res;
    }



}
