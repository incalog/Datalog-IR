// This is a generated file. Not intended for manual editing.
package language.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static language.psi.FuncIncaTypes.*;
import language.psi.FuncIncaNamedElementImpl;
import language.psi.*;

public class FuncIncaParamImpl extends FuncIncaNamedElementImpl implements FuncIncaParam {

  public FuncIncaParamImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull FuncIncaVisitor visitor) {
    visitor.visitParam(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FuncIncaVisitor) accept((FuncIncaVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public FuncIncaAtomicType getAtomicType() {
    return findChildByClass(FuncIncaAtomicType.class);
  }

  @Override
  @Nullable
  public FuncIncaFunType getFunType() {
    return findChildByClass(FuncIncaFunType.class);
  }

  @Override
  @NotNull
  public PsiElement getId() {
    return findNotNullChildByType(ID);
  }

}
