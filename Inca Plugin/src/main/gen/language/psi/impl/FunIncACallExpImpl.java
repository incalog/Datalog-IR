// This is a generated file. Not intended for manual editing.
package language.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static language.psi.FunIncATypes.*;
import language.psi.*;

public class FunIncACallExpImpl extends FunIncAExpImpl implements FunIncACallExp {

  public FunIncACallExpImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull FunIncAVisitor visitor) {
    visitor.visitCallExp(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FunIncAVisitor) accept((FunIncAVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<FunIncACallExpList> getCallExpListList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FunIncACallExpList.class);
  }

  @Override
  @NotNull
  public FunIncAExp getExp() {
    return findNotNullChildByClass(FunIncAExp.class);
  }

  @Override
  @NotNull
  public List<FunIncAType> getTypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FunIncAType.class);
  }

}
