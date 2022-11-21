package language.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import language.psi.FuncIncaNamedElement;
import org.jetbrains.annotations.NotNull;

public abstract class FuncIncaNamedVariableImpl extends ASTWrapperPsiElement implements FuncIncaNamedElement { 
    public FuncIncaNamedVariableImpl(@NotNull ASTNode node) {
        super(node);
    }
}
