package language.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import language.psi.FuncIncaNamedElement;
import org.jetbrains.annotations.NotNull;

public abstract class FuncIncaNamedElementImpl extends ASTWrapperPsiElement implements FuncIncaNamedElement {

    public FuncIncaNamedElementImpl(@NotNull ASTNode node) {
        super(node);
    }
}
