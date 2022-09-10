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

public class FuncIncaSubinfixExpImpl extends ASTWrapperPsiElement implements FuncIncaSubinfixExp {

  public FuncIncaSubinfixExpImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull FuncIncaVisitor visitor) {
    visitor.visitSubinfixExp(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FuncIncaVisitor) accept((FuncIncaVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public FuncIncaAtomicExp getAtomicExp() {
    return findChildByClass(FuncIncaAtomicExp.class);
  }

  @Override
  @Nullable
  public FuncIncaCallExp getCallExp() {
    return findChildByClass(FuncIncaCallExp.class);
  }

  @Override
  @Nullable
  public FuncIncaLambdaExp getLambdaExp() {
    return findChildByClass(FuncIncaLambdaExp.class);
  }

}
