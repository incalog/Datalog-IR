package language.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import language.psi.FunIncANamedElement;
import org.jetbrains.annotations.NotNull;

public abstract class FunIncANamedElementImpl extends ASTWrapperPsiElement implements FunIncANamedElement {

    public FunIncANamedElementImpl(@NotNull ASTNode node) {
        super(node);
    }
}
