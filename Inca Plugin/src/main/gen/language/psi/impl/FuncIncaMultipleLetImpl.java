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

public class FuncIncaMultipleLetImpl extends ASTWrapperPsiElement implements FuncIncaMultipleLet {

  public FuncIncaMultipleLetImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull FuncIncaVisitor visitor) {
    visitor.visitMultipleLet(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FuncIncaVisitor) accept((FuncIncaVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public FuncIncaExp getExp() {
    return findNotNullChildByClass(FuncIncaExp.class);
  }

  @Override
  @NotNull
  public FuncIncaInfixExp getInfixExp() {
    return findNotNullChildByClass(FuncIncaInfixExp.class);
  }

  @Override
  @NotNull
  public List<FuncIncaTypeAnnotation> getTypeAnnotationList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaTypeAnnotation.class);
  }

  @Override
  @NotNull
  public List<FuncIncaVarId> getVarIdList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaVarId.class);
  }

}
