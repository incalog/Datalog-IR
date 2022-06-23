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

public class FuncIncaFunDefImpl extends ASTWrapperPsiElement implements FuncIncaFunDef {

  public FuncIncaFunDefImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull FuncIncaVisitor visitor) {
    visitor.visitFunDef(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FuncIncaVisitor) accept((FuncIncaVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<FuncIncaAnnotation> getAnnotationList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaAnnotation.class);
  }

  @Override
  @Nullable
  public FuncIncaAtomicType getAtomicType() {
    return findChildByClass(FuncIncaAtomicType.class);
  }

  @Override
  @NotNull
  public FuncIncaExp getExp() {
    return findNotNullChildByClass(FuncIncaExp.class);
  }

  @Override
  @Nullable
  public FuncIncaFunType getFunType() {
    return findChildByClass(FuncIncaFunType.class);
  }

  @Override
  @NotNull
  public FuncIncaId getId() {
    return findNotNullChildByClass(FuncIncaId.class);
  }

  @Override
  @NotNull
  public List<FuncIncaParam> getParamList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaParam.class);
  }

  @Override
  @Nullable
  public FuncIncaParamTypes getParamTypes() {
    return findChildByClass(FuncIncaParamTypes.class);
  }

  @Override
  @Nullable
  public FuncIncaVisibility getVisibility() {
    return findChildByClass(FuncIncaVisibility.class);
  }

}
