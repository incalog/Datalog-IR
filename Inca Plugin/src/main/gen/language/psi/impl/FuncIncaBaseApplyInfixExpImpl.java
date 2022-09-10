// This is a generated file. Not intended for manual editing.
package language.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static language.psi.FuncIncaTypes.*;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import language.psi.*;

public class FuncIncaBaseApplyInfixExpImpl extends ASTWrapperPsiElement implements FuncIncaBaseApplyInfixExp {

  public FuncIncaBaseApplyInfixExpImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull FuncIncaVisitor visitor) {
    visitor.visitBaseApplyInfixExp(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FuncIncaVisitor) accept((FuncIncaVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public FuncIncaInfixExp getInfixExp() {
    return findChildByClass(FuncIncaInfixExp.class);
  }

  @Override
  @NotNull
  public FuncIncaOp getOp() {
    return findNotNullChildByClass(FuncIncaOp.class);
  }

  @Override
  @NotNull
  public FuncIncaSubinfixExp getSubinfixExp() {
    return findNotNullChildByClass(FuncIncaSubinfixExp.class);
  }

}
