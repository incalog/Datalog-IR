// This is a generated file. Not intended for manual editing.
package language.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static language.psi.FunIncATypes.*;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import language.psi.*;

public class FunIncAMultipleBindingsImpl extends ASTWrapperPsiElement implements FunIncAMultipleBindings {

  public FunIncAMultipleBindingsImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull FunIncAVisitor visitor) {
    visitor.visitMultipleBindings(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FunIncAVisitor) accept((FunIncAVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<FunIncAExp> getExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FunIncAExp.class);
  }

  @Override
  @NotNull
  public List<FunIncAType> getTypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FunIncAType.class);
  }

  @Override
  @NotNull
  public List<FunIncAVarDef> getVarDefList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FunIncAVarDef.class);
  }

}
