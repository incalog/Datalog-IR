package language;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.psi.PsiElement;
import language.psi.*;
import language.typing.FunIncATypeUtil;
import language.typing.FunIncATypechecker;
import language.typing.PsiToTypeConverter;
import language.typing.types.Type;
import org.jetbrains.annotations.NotNull;

import java.util.List;

// import static language.types.TypeContext.*;

public class FunIncAAnnotator implements Annotator {
    @Override
    public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {

        if (element instanceof FunIncAFunDef) {
            FunIncAFunDef funDef = ((FunIncAFunDef) element);
            FunIncAExp body = funDef.getExp();
            FunIncAType expected = funDef.getType();
            Type expectedType = PsiToTypeConverter.convert(expected, holder);
            if (body == null) { // do not annotate if function has no body
                return;
            }
            Type resolvedType = FunIncATypechecker.typecheck(body, holder);
            if (expected == null) {
                holder.newAnnotation(HighlightSeverity.ERROR, "Missing return type")
                        .range(funDef)
                        .create();
            } else if (!FunIncATypeUtil.subtype(resolvedType, expectedType)) {
                holder.newAnnotation(HighlightSeverity.ERROR, "Expected return type " + expectedType +
                        ", but got " + resolvedType)
                        .range(expected)
                        .create();
            }
        }

        if (element instanceof FunIncADataDef) {
            FunIncADataDef dataDef = (FunIncADataDef) element;
            // check if parametric types are correctly used
            holder.newAnnotation(HighlightSeverity.INFORMATION, "Typecheck not yet implemented")
                    .range(element)
                    .create();
        }

    }

}
