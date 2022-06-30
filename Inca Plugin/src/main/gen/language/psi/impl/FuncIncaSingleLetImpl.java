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
  @Nullable
  public FuncIncaAtomicType getAtomicType() {
    return findChildByClass(FuncIncaAtomicType.class);
  }

  @Override
  @Nullable
  public FuncIncaBaseApplyExp getBaseApplyExp() {
    return findChildByClass(FuncIncaBaseApplyExp.class);
  }

  @Override
  @Nullable
  public FuncIncaBaseApplyInfixExp getBaseApplyInfixExp() {
    return findChildByClass(FuncIncaBaseApplyInfixExp.class);
  }

  @Override
  @Nullable
  public FuncIncaBaseApplyMethodExp getBaseApplyMethodExp() {
    return findChildByClass(FuncIncaBaseApplyMethodExp.class);
  }

  @Override
  @Nullable
  public FuncIncaBaseApplyUnaryExp getBaseApplyUnaryExp() {
    return findChildByClass(FuncIncaBaseApplyUnaryExp.class);
  }

  @Override
  @Nullable
  public FuncIncaBooleanLit getBooleanLit() {
    return findChildByClass(FuncIncaBooleanLit.class);
  }

  @Override
  @Nullable
  public FuncIncaCallExp getCallExp() {
    return findChildByClass(FuncIncaCallExp.class);
  }

  @Override
  @Nullable
  public FuncIncaCastExp getCastExp() {
    return findChildByClass(FuncIncaCastExp.class);
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
  @NotNull
  public FuncIncaExp getExp() {
    return findNotNullChildByClass(FuncIncaExp.class);
  }

  @Override
  @Nullable
  public FuncIncaFoldExp getFoldExp() {
    return findChildByClass(FuncIncaFoldExp.class);
  }

  @Override
  @Nullable
  public FuncIncaFunType getFunType() {
    return findChildByClass(FuncIncaFunType.class);
  }

  @Override
  @Nullable
  public FuncIncaLambdaExp getLambdaExp() {
    return findChildByClass(FuncIncaLambdaExp.class);
  }

  @Override
  @Nullable
  public FuncIncaMatchExp getMatchExp() {
    return findChildByClass(FuncIncaMatchExp.class);
  }

  @Override
  @Nullable
  public FuncIncaNumericLit getNumericLit() {
    return findChildByClass(FuncIncaNumericLit.class);
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
  public FuncIncaStringLit getStringLit() {
    return findChildByClass(FuncIncaStringLit.class);
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

  @Override
  @NotNull
  public PsiElement getId() {
    return findNotNullChildByType(ID);
  }

  @Override
  @Nullable
  public PsiElement getScalaTerm() {
    return findChildByType(SCALA_TERM);
  }

}
