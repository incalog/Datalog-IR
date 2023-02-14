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

import java.util.List;

import static language.types.TypeContext.bindVar;
import static language.types.TypeContext.scopedTypeContext;

public class FuncIncaAnnotator implements Annotator {
    @Override
    public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {

        if (element instanceof FuncIncaFunDef) {
            FuncIncaParamList paramList = ((FuncIncaFunDef) element).getParamList();
            FuncIncaExp body = ((FuncIncaFunDef) element).getExp();
            FuncIncaTypeAnnotation expected = ((FuncIncaFunDef) element).getTypeAnnotation();
            FuncIncaType expectedType = FuncIncaTypeUtil.psiToFuncIncaType(expected);
            final FuncIncaType[] resolvedType = new FuncIncaType[1];
            if (paramList != null) {
                List<FuncIncaParam> params = paramList.getParamList();
                scopedTypeContext(new Runnable() {
                    @Override
                    public void run() {
                        for (FuncIncaParam param : params) {
                            String name = param.getId().getText();
                            FuncIncaType type = FuncIncaTypeUtil.psiToFuncIncaType(param.getTypeAnnotation());
                            bindVar(name, param, type, holder);
                        }
                        resolvedType[0] = FuncIncaTypechecker.typecheck(body, holder);
                    }
                });
            } else { // no parameters found
                resolvedType[0] = FuncIncaTypechecker.typecheck(body, holder);
            }
            if (!FuncIncaTypechecker.subtype(resolvedType[0], expectedType)) {
                holder.newAnnotation(HighlightSeverity.ERROR, "Expected return type " + expectedType +
                        ", but got " + resolvedType[0])
                        .range(expected)
                        .create();
            }
        }

        if (element instanceof FuncIncaDataDef) {
            holder.newAnnotation(HighlightSeverity.INFORMATION, "Typecheck not yet implemented")
                    .range(element)
                    .create();
        }

    }

}
