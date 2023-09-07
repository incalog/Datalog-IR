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

public class FunIncAFunTypeImpl extends ASTWrapperPsiElement implements FunIncAFunType {

  public FunIncAFunTypeImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull FunIncAVisitor visitor) {
    visitor.visitFunType(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FunIncAVisitor) accept((FunIncAVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public FunIncAAtomicType getAtomicType() {
    return findNotNullChildByClass(FunIncAAtomicType.class);
  }

  @Override
  @Nullable
  public FunIncAType getType() {
    return findChildByClass(FunIncAType.class);
  }

}
