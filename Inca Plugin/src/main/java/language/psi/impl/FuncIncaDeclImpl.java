package language.psi.impl;

import com.intellij.lang.ASTNode;
import language.psi.FuncIncaNamedElement;
import org.jetbrains.annotations.NotNull;

public abstract class FuncIncaDeclImpl extends FuncIncaNamedElementImpl implements FuncIncaNamedElement {
    public FuncIncaDeclImpl(@NotNull ASTNode node) {
        super(node);
    }
}
