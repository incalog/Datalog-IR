package language.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import language.psi.FunIncaNamedElement;
import org.jetbrains.annotations.NotNull;

public abstract class FunIncANamedVariableImpl extends ASTWrapperPsiElement implements FunIncaNamedElement {
    public FunIncANamedVariableImpl(@NotNull ASTNode node) {
        super(node);
    }
}
