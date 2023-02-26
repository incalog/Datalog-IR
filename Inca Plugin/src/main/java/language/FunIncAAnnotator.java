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
            Type expectedType = PsiToTypeConverter.convert(expected);
            //bindFun(funDef); // TODO move to another point
            if (body == null) { // do not annotate if function has no body
                return;
            }
            final Type[] resolvedType = new Type[1];
            resolvedType[0] = FunIncATypechecker.typecheck(body, holder);
//            TypeContext.scopedTypeContext(new Runnable() {
//                @Override
//                public void run() {
//                    for (FunIncAParamDef param : funDef.getParamDefList()) {
//                        String name = param.getId().getText();
//                        Type type = PsiToTypeConverter.convert(param.getType());
//                        // TypeContext.bindVar(name, param, type, holder);
//                    }
//                    resolvedType[0] = FunIncATypechecker.typecheck(body, holder);
//                }
//            });
            if (expected == null) {
                holder.newAnnotation(HighlightSeverity.ERROR, "Missing return type")
                        .range(funDef)
                        .create();
            } else if (!FunIncATypeUtil.subtype(resolvedType[0], expectedType)) {
                holder.newAnnotation(HighlightSeverity.ERROR, "Expected return type " + expectedType +
                        ", but got " + resolvedType[0])
                        .range(expected)
                        .create();
            }
        }

        if (element instanceof FunIncADataDef) {
            FunIncADataDef dataDef = (FunIncADataDef) element;
            // bindData(dataDef);
            holder.newAnnotation(HighlightSeverity.INFORMATION, "Typecheck not yet implemented")
                    .range(element)
                    .create();
        }

    }

}
