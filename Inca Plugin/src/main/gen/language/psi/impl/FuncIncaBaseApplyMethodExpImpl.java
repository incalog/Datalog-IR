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

public class FuncIncaBaseApplyMethodExpImpl extends ASTWrapperPsiElement implements FuncIncaBaseApplyMethodExp {

  public FuncIncaBaseApplyMethodExpImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull FuncIncaVisitor visitor) {
    visitor.visitBaseApplyMethodExp(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FuncIncaVisitor) accept((FuncIncaVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<FuncIncaBaseApplyExp> getBaseApplyExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaBaseApplyExp.class);
  }

  @Override
  @NotNull
  public List<FuncIncaBaseApplyInfixExp> getBaseApplyInfixExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaBaseApplyInfixExp.class);
  }

  @Override
  @NotNull
  public List<FuncIncaBaseApplyMethodExp> getBaseApplyMethodExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaBaseApplyMethodExp.class);
  }

  @Override
  @NotNull
  public List<FuncIncaBaseApplyUnaryExp> getBaseApplyUnaryExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaBaseApplyUnaryExp.class);
  }

  @Override
  @NotNull
  public List<FuncIncaBooleanLit> getBooleanLitList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaBooleanLit.class);
  }

  @Override
  @NotNull
  public List<FuncIncaCallExp> getCallExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaCallExp.class);
  }

  @Override
  @NotNull
  public List<FuncIncaCastExp> getCastExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaCastExp.class);
  }

  @Override
  @NotNull
  public List<FuncIncaComprehensionExp> getComprehensionExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaComprehensionExp.class);
  }

  @Override
  @NotNull
  public List<FuncIncaConstSetExp> getConstSetExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaConstSetExp.class);
  }

  @Override
  @NotNull
  public List<FuncIncaFoldExp> getFoldExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaFoldExp.class);
  }

  @Override
  @NotNull
  public FuncIncaId getId() {
    return findNotNullChildByClass(FuncIncaId.class);
  }

  @Override
  @NotNull
  public List<FuncIncaLambdaExp> getLambdaExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaLambdaExp.class);
  }

  @Override
  @NotNull
  public List<FuncIncaMatchExp> getMatchExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaMatchExp.class);
  }

  @Override
  @NotNull
  public List<FuncIncaNumericLit> getNumericLitList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaNumericLit.class);
  }

  @Override
  @NotNull
  public List<FuncIncaOptionExp> getOptionExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaOptionExp.class);
  }

  @Override
  @NotNull
  public List<FuncIncaParensExp> getParensExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaParensExp.class);
  }

  @Override
  @NotNull
  public List<FuncIncaStringLit> getStringLitList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaStringLit.class);
  }

  @Override
  @NotNull
  public List<FuncIncaTupleExp> getTupleExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaTupleExp.class);
  }

  @Override
  @NotNull
  public List<FuncIncaVar> getVarList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaVar.class);
  }

}
