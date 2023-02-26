package language.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import language.psi.FunIncaNamedElement;
import org.jetbrains.annotations.NotNull;

public abstract class FunIncADeclImpl extends ASTWrapperPsiElement implements FunIncaNamedElement {
    public FunIncADeclImpl(@NotNull ASTNode node) {
        super(node);
    }
}
