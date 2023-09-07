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

public class FunIncAFunDefImpl extends FunIncADeclImpl implements FunIncAFunDef {

  public FunIncAFunDefImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull FunIncAVisitor visitor) {
    visitor.visitFunDef(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FunIncAVisitor) accept((FunIncAVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<FunIncAAnnotation> getAnnotationList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FunIncAAnnotation.class);
  }

  @Override
  @Nullable
  public FunIncAExp getExp() {
    return findChildByClass(FunIncAExp.class);
  }

  @Override
  @NotNull
  public List<FunIncAParamDef> getParamDefList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FunIncAParamDef.class);
  }

  @Override
  @Nullable
  public FunIncAType getType() {
    return findChildByClass(FunIncAType.class);
  }

  @Override
  @NotNull
  public List<FunIncATypeVarDef> getTypeVarDefList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FunIncATypeVarDef.class);
  }

  @Override
  @Nullable
  public FunIncAVisibility getVisibility() {
    return findChildByClass(FunIncAVisibility.class);
  }

  @Override
  @Nullable
  public PsiElement getId() {
    return findChildByType(ID);
  }

  @Override
  public String getName() {
    return FunIncAPsiImplUtil.getName(this);
  }

  @Override
  public PsiElement setName(String newName) {
    return FunIncAPsiImplUtil.setName(this, newName);
  }

  @Override
  public PsiElement getNameIdentifier() {
    return FunIncAPsiImplUtil.getNameIdentifier(this);
  }

}
