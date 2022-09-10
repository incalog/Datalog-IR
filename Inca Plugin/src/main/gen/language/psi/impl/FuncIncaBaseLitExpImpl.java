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

public class FuncIncaBaseLitExpImpl extends ASTWrapperPsiElement implements FuncIncaBaseLitExp {

  public FuncIncaBaseLitExpImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull FuncIncaVisitor visitor) {
    visitor.visitBaseLitExp(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FuncIncaVisitor) accept((FuncIncaVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public FuncIncaBooleanLit getBooleanLit() {
    return findChildByClass(FuncIncaBooleanLit.class);
  }

  @Override
  @Nullable
  public FuncIncaNumericLit getNumericLit() {
    return findChildByClass(FuncIncaNumericLit.class);
  }

  @Override
  @Nullable
  public FuncIncaStringLit getStringLit() {
    return findChildByClass(FuncIncaStringLit.class);
  }

  @Override
  @Nullable
  public PsiElement getScalaTerm() {
    return findChildByType(SCALA_TERM);
  }

}
