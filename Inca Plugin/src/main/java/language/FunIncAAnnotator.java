package language;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import language.psi.*;
import language.typing.FunIncATypeUtil;
import language.typing.FunIncATypechecker;
import language.typing.PsiToTypeConverter;
import language.typing.types.Type;
import org.jetbrains.annotations.NotNull;

import java.util.*;

public class FunIncAAnnotator implements Annotator {

    private static Map<TextRange, Set<String>> annotationMap;

    private static AnnotationHolder annotationHolder;

    public static void newAnnotation(HighlightSeverity hs, String message, PsiElement e) {
        newAnnotation(hs, message, e.getTextRange());
    }

    public static void newAnnotation(HighlightSeverity hs, String message, TextRange textRange) {
        if (annotationHolder != null && annotationMap != null) {
            Set<String> annos = annotationMap.get(textRange);
            if (annos == null) { // no annotations yet for that specific text range
                annotationMap.put(textRange, new HashSet<>(Collections.singletonList(message)));
                annotationHolder.newAnnotation(hs, message).range(textRange).create(); // annotation is displayed
            } else { // this text range already displays annotations
                if (!annos.contains(message)) {
                    annos.add(message);
                    annotationHolder.newAnnotation(hs, message).range(textRange).create(); // annotation is displayed
                }
            }
        }
    }

    @Override
    public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
        annotationMap = new HashMap<>();
        annotationHolder = holder;

        if (element instanceof FunIncAFunDef) {
            FunIncAFunDef funDef = ((FunIncAFunDef) element);
            PsiElement root = funDef.getContainingFile();
/*            List<PsiElement> rootChildren = List.of(root.getChildren());
            for (PsiElement child : rootChildren) {
                if (child != funDef && child instanceof FunIncAFunDef
                        && funDef.getName().equals(((FunIncAFunDef) child).getName())) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                                    "Duplicate name " + funDef.getName())
                            .range(funDef.getId())
                            .create();
                }
            }
            */
            FunIncAExp body = funDef.getExp();
            FunIncAType expected = funDef.getType();
            if (expected != null)
                FunIncATypechecker.validateType(expected);
            Type expectedType = PsiToTypeConverter.convert(expected);
            for (FunIncAParamDef paramDef : funDef.getParamDefList()) // annotate all parameter types
                if (paramDef.getType() != null)
                    FunIncATypechecker.validateType(paramDef.getType());

            if (body == null) { // do not annotate if function has no body
                return;
            }
            Type resolvedType = FunIncATypechecker.typecheckExp(body);
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
            List<FunIncADataConstructorDef> constructors = dataDef.getDataConstructorDefList();
/*            PsiElement root = dataDef.getContainingFile();
            List<PsiElement> rootChildren = List.of(root.getChildren());
            for (PsiElement child : rootChildren) {
                if (child instanceof FunIncAFunDef) {
                    for (FunIncADataConstructorDef cons : constructors) {
                        if (cons.getName().equals(((FunIncAFunDef) child).getName())) {
                            holder.newAnnotation(HighlightSeverity.ERROR,
                                            "Duplicate name " + dataDef.getName())
                                    .range(cons.getId())
                                    .create();
                        }
                    }
                }
                if (child != dataDef && child instanceof FunIncADataDef
                        && dataDef.getName().equals(((FunIncADataDef) child).getName())) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                                    "Duplicate name " + dataDef.getName())
                            .range(dataDef.getId())
                            .create();
                }
            }
            */
            for (FunIncADataConstructorDef cons : constructors) {
                if (cons.getTypeList() != null)
                    FunIncATypechecker.validateTypes(cons.getTypeList());
            }
        }

    }

}
