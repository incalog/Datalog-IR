package language;

import com.google.common.collect.Lists;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.search.FileTypeIndex;
import language.psi.*;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.project.Project;
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
    * finds the definition of Named Elements with name in a project
    */
    public static List<PsiNamedElement> findFunDefs(@NotNull Project project, @Nullable FuncIncaFile f, @Nullable String name){
        List<PsiNamedElement> res = new ArrayList<>();
        Collection<VirtualFile> virtualFiles = FileTypeIndex.getFiles(FuncIncaFileType.INSTANCE, GlobalSearchScope.projectScope(project));
        return res;
    }


}
