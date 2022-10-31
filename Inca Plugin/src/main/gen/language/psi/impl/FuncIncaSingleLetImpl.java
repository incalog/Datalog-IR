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

public class FuncIncaSingleLetImpl extends ASTWrapperPsiElement implements FuncIncaSingleLet {

  public FuncIncaSingleLetImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull FuncIncaVisitor visitor) {
    visitor.visitSingleLet(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FuncIncaVisitor) accept((FuncIncaVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<FuncIncaExp> getExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaExp.class);
  }

  @Override
  @Nullable
  public FuncIncaTypeAnnotation getTypeAnnotation() {
    return findChildByClass(FuncIncaTypeAnnotation.class);
  }

  @Override
  @NotNull
  public FuncIncaVarId getVarId() {
    return findNotNullChildByClass(FuncIncaVarId.class);
  }

}
