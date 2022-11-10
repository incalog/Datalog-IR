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
        // We only want to look for classes that match the element we are resolving
        final Class<? extends PsiNamedElement> elementClass;
        if(e instanceof FuncIncaConsId)
            elementClass = FuncIncaDataConstructor.class;
        else
            elementClass = FuncIncaNamedElement.class;

        Collection<PsiNamedElement> namedElements = PsiTreeUtil.findChildrenOfType(file, elementClass);
        for(PsiNamedElement namedElement: namedElements){
            if(name == null || name.equals(namedElement.getName()))
                res.add(namedElement);
        }
        return res;
    }


}
