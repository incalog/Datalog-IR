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

public class FuncIncaLambdaVarsImpl extends ASTWrapperPsiElement implements FuncIncaLambdaVars {

  public FuncIncaLambdaVarsImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull FuncIncaVisitor visitor) {
    visitor.visitLambdaVars(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FuncIncaVisitor) accept((FuncIncaVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<FuncIncaAtomicType> getAtomicTypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaAtomicType.class);
  }

  @Override
  @NotNull
  public List<FuncIncaFunType> getFunTypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaFunType.class);
  }

  @Override
  @NotNull
  public List<FuncIncaId> getIdList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaId.class);
  }

}
