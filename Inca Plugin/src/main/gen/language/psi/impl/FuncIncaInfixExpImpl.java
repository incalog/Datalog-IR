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

public class FuncIncaInfixExpImpl extends ASTWrapperPsiElement implements FuncIncaInfixExp {

  public FuncIncaInfixExpImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull FuncIncaVisitor visitor) {
    visitor.visitInfixExp(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FuncIncaVisitor) accept((FuncIncaVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public FuncIncaBaseApplyInfixExp getBaseApplyInfixExp() {
    return findChildByClass(FuncIncaBaseApplyInfixExp.class);
  }

  @Override
  @Nullable
  public FuncIncaBaseApplyMethodExp getBaseApplyMethodExp() {
    return findChildByClass(FuncIncaBaseApplyMethodExp.class);
  }

  @Override
  @Nullable
  public FuncIncaCastExp getCastExp() {
    return findChildByClass(FuncIncaCastExp.class);
  }

  @Override
  @Nullable
  public FuncIncaMatchExp getMatchExp() {
    return findChildByClass(FuncIncaMatchExp.class);
  }

  @Override
  @Nullable
  public FuncIncaSubinfixExp getSubinfixExp() {
    return findChildByClass(FuncIncaSubinfixExp.class);
  }

}
