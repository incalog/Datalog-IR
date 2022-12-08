package language.types.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import language.types.FuncIncaType;
import org.jetbrains.annotations.NotNull;

public abstract class FuncIncaTypeImpl extends ASTWrapperPsiElement implements FuncIncaType {
    private String type;

    public FuncIncaTypeImpl(@NotNull ASTNode node) {
        super(node);
        determineType();
    }

    public void determineType(){
        // TODO: looks for type
    }

    public String getType(){
        return type;
    }
}
