package language.types;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;

public class FuncIncaTypechecker {

    public static void checkTypes(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
        // TODO
    }
    
    public Boolean isSubTypeOf(FuncIncaType subTy, FuncIncaType superTy) {
        return isSupertypeOf(superTy, subTy);    
    }
    
    public static Boolean isSupertypeOf(FuncIncaType superTy, FuncIncaType subTy){
        if (superTy == null || subTy == null)
            return true;
        return true;
    }

}
