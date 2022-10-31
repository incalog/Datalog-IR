// This is a generated file. Not intended for manual editing.
package language.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static language.psi.FuncIncaTypes.*;
import language.psi.*;

public class FuncIncaBaseApplyUnaryExpImpl extends FuncIncaExpImpl implements FuncIncaBaseApplyUnaryExp {

  public FuncIncaBaseApplyUnaryExpImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull FuncIncaVisitor visitor) {
    visitor.visitBaseApplyUnaryExp(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FuncIncaVisitor) accept((FuncIncaVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public FuncIncaExp getExp() {
    return findChildByClass(FuncIncaExp.class);
  }

  @Override
  @NotNull
  public FuncIncaUnaryOp getUnaryOp() {
    return findNotNullChildByClass(FuncIncaUnaryOp.class);
  }

}
