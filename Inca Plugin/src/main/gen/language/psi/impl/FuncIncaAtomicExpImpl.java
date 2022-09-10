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

public class FuncIncaAtomicExpImpl extends ASTWrapperPsiElement implements FuncIncaAtomicExp {

  public FuncIncaAtomicExpImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull FuncIncaVisitor visitor) {
    visitor.visitAtomicExp(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FuncIncaVisitor) accept((FuncIncaVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public FuncIncaBaseApplyExp getBaseApplyExp() {
    return findChildByClass(FuncIncaBaseApplyExp.class);
  }

  @Override
  @Nullable
  public FuncIncaBaseApplyUnaryExp getBaseApplyUnaryExp() {
    return findChildByClass(FuncIncaBaseApplyUnaryExp.class);
  }

  @Override
  @Nullable
  public FuncIncaBaseLitExp getBaseLitExp() {
    return findChildByClass(FuncIncaBaseLitExp.class);
  }

  @Override
  @Nullable
  public FuncIncaComprehensionExp getComprehensionExp() {
    return findChildByClass(FuncIncaComprehensionExp.class);
  }

  @Override
  @Nullable
  public FuncIncaConstSetExp getConstSetExp() {
    return findChildByClass(FuncIncaConstSetExp.class);
  }

  @Override
  @Nullable
  public FuncIncaFoldExp getFoldExp() {
    return findChildByClass(FuncIncaFoldExp.class);
  }

  @Override
  @Nullable
  public FuncIncaOptionExp getOptionExp() {
    return findChildByClass(FuncIncaOptionExp.class);
  }

  @Override
  @Nullable
  public FuncIncaParensExp getParensExp() {
    return findChildByClass(FuncIncaParensExp.class);
  }

  @Override
  @Nullable
  public FuncIncaTupleExp getTupleExp() {
    return findChildByClass(FuncIncaTupleExp.class);
  }

  @Override
  @Nullable
  public FuncIncaVar getVar() {
    return findChildByClass(FuncIncaVar.class);
  }

}
