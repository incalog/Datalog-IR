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

public class FuncIncaAtomicTypeImpl extends ASTWrapperPsiElement implements FuncIncaAtomicType {

  public FuncIncaAtomicTypeImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull FuncIncaVisitor visitor) {
    visitor.visitAtomicType(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FuncIncaVisitor) accept((FuncIncaVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public FuncIncaConstr getConstr() {
    return findChildByClass(FuncIncaConstr.class);
  }

  @Override
  @Nullable
  public FuncIncaData getData() {
    return findChildByClass(FuncIncaData.class);
  }

  @Override
  @Nullable
  public FuncIncaOption getOption() {
    return findChildByClass(FuncIncaOption.class);
  }

  @Override
  @Nullable
  public FuncIncaSet getSet() {
    return findChildByClass(FuncIncaSet.class);
  }

  @Override
  @Nullable
  public FuncIncaTuple getTuple() {
    return findChildByClass(FuncIncaTuple.class);
  }

  @Override
  @Nullable
  public PsiElement getScalaTerm() {
    return findChildByType(SCALA_TERM);
  }

}
