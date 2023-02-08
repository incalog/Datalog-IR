package language;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.psi.PsiElement;
import language.psi.*;
import language.types.FuncIncaType;
import language.types.FuncIncaTypeUtil;
import language.types.FuncIncaTypechecker;
import org.jetbrains.annotations.NotNull;

public class FuncIncaAnnotator implements Annotator {
    @Override
    public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {

        if (element instanceof FuncIncaFunDef) {
            FuncIncaExp body = ((FuncIncaFunDef) element).getExp();
            FuncIncaType resolvedType = FuncIncaTypechecker.typecheckCore(body, holder);
            FuncIncaTypeAnnotation expected = ((FuncIncaFunDef) element).getTypeAnnotation();
            FuncIncaType expectedType = FuncIncaTypeUtil.psiToFuncIncaType(expected);
            if (!FuncIncaTypechecker.subtype(resolvedType, expectedType)) {
                holder.newAnnotation(HighlightSeverity.ERROR, "Expected return Type " + expectedType +
                        " but got " + resolvedType)
                        .range(expected)
                        .create();
            }
        }

        if (element instanceof FuncIncaDataDef) {
            holder.newAnnotation(HighlightSeverity.INFORMATION, "Typecheck not yet implemented")
                    .range(element)
                    .create();
        }

        FuncIncaType elType = FuncIncaTypechecker.typecheckCore(element, holder);

    }

}
