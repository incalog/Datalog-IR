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
import com.intellij.psi.PsiReference;

public class FuncIncaVarIdImpl extends FuncIncaNamedElementImpl implements FuncIncaVarId {

  public FuncIncaVarIdImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull FuncIncaVisitor visitor) {
    visitor.visitVarId(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FuncIncaVisitor) accept((FuncIncaVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public PsiElement getId() {
    return findNotNullChildByType(ID);
  }

  @Override
  public String getName() {
    return FuncIncaPsiImplUtil.getName(this);
  }

  @Override
  public PsiElement setName(String newName) {
    return FuncIncaPsiImplUtil.setName(this, newName);
  }

  @Override
  public PsiElement getNameIdentifier() {
    return FuncIncaPsiImplUtil.getNameIdentifier(this);
  }

  @Override
  public PsiReference getReference() {
    return FuncIncaPsiImplUtil.getReference(this);
  }

}
