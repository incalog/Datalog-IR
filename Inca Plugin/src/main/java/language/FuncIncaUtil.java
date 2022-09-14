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
    public static List<PsiNamedElement> findDefinitionNode(@NotNull Project project, @Nullable String name, @NotNull PsiNamedElement e){
        List<PsiNamedElement> res = new ArrayList<>();
        final PsiFile psiFile = e.getContainingFile().getOriginalFile();
        // this would be the place for getting the imported modules

        Collection<VirtualFile> virtualFiles = FileTypeIndex.getFiles(FuncIncaFileType.INSTANCE, GlobalSearchScope.projectScope(project));
        for (VirtualFile virtualFile : virtualFiles){
            FuncIncaFile f = (FuncIncaFile) PsiManager.getInstance(project).findFile(virtualFile);
            final boolean returnAllReferences = (name == null);
            final boolean inLocalModule = (f != null && f.equals(psiFile));
            res.addAll(findDefinitionNode(f, name, e));
        }
        return res;
    }

    /*
    * finds all Psi Definition nodes named "name" in one file*/
    public static List<PsiNamedElement> findDefinitionNode(@Nullable FuncIncaFile file, @Nullable String name, @Nullable PsiNamedElement e){
        List<PsiNamedElement> res = new ArrayList<>();
        if(file == null)
            return res;
        // We only want to look for classes that match the element we are resolving
        final Class<? extends PsiNamedElement> elementClass;
        if(e instanceof FuncIncaTypeName || e instanceof FuncIncaDataDef)
            elementClass = FuncIncaDataDef.class;
        else if(e instanceof FuncIncaConstructorPattern || e instanceof FuncIncaDataConstructor)
            elementClass = FuncIncaDataConstructor.class;
        else
            elementClass = FuncIncaDecl.class;

        Collection<PsiNamedElement> namedElements = PsiTreeUtil.findChildrenOfType(file, elementClass);
        for(PsiNamedElement namedElement: namedElements){
            if(name == null || name.equals(namedElement.getName()))
                res.add(namedElement);
        }
        return res;
    }


}
