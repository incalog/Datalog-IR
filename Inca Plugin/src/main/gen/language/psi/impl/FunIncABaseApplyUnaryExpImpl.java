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

public class FunIncABaseApplyUnaryExpImpl extends FunIncAExpImpl implements FunIncABaseApplyUnaryExp {

  public FunIncABaseApplyUnaryExpImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull FunIncAVisitor visitor) {
    visitor.visitBaseApplyUnaryExp(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FunIncAVisitor) accept((FunIncAVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public FunIncAExp getExp() {
    return findChildByClass(FunIncAExp.class);
  }

  @Override
  @NotNull
  public FunIncAUnaryOp getUnaryOp() {
    return findNotNullChildByClass(FunIncAUnaryOp.class);
  }

}
