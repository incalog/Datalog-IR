package language;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.psi.PsiElement;
import language.psi.*;
import org.jetbrains.annotations.NotNull;

public class FuncIncaAnnotator implements Annotator {
    @Override
    public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
        element.accept(new FuncIncaVisitor(){
            @Override
            public void visitAtomicType(@NotNull FuncIncaAtomicType o) {
                super.visitAtomicType(o);
                setHighlighting(o, holder, FuncIncaSyntaxHighlighter.PARAMETER);
            }

            @Override
            public void visitParamType(@NotNull FuncIncaParamType o) {
                super.visitParamType(o);
                setHighlighting(o, holder, FuncIncaSyntaxHighlighter.PARAMETER);
            }

            @Override
            public void visitParam(@NotNull FuncIncaParam o) {
                super.visitParam(o);
                setHighlighting(o.getFirstChild(), holder, FuncIncaSyntaxHighlighter.PARAMETER);
            }

            @Override
            public void visitTypeName(@NotNull FuncIncaTypeName o) {
                super.visitTypeName(o);
                setHighlighting(o, holder, FuncIncaSyntaxHighlighter.PARAMETER);
            }

            @Override
            public void visitLambdaVars(@NotNull FuncIncaLambdaVars o) {
                super.visitLambdaVars(o);
                setHighlighting(o, holder, FuncIncaSyntaxHighlighter.PARAMETER);
            }

            @Override
            public void visitCallExp(@NotNull FuncIncaCallExp o) {
                super.visitCallExp(o);
                if(o.getFirstChild().getNode().getElementType() == FuncIncaTypes.VAR)
                    setHighlighting(o.getFirstChild(), holder, FuncIncaSyntaxHighlighter.FUNCALL);
            }
        });
    }

    private static void setHighlighting(@NotNull PsiElement element, @NotNull AnnotationHolder holder, @NotNull TextAttributesKey key) {
        holder.createInfoAnnotation(element, null).setEnforcedTextAttributes(
                EditorColorsManager.getInstance().getGlobalScheme().getAttributes(key));
    }

}
