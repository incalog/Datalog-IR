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

public class FunIncALiteralExpImpl extends FunIncAExpImpl implements FunIncALiteralExp {

  public FunIncALiteralExpImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull FunIncAVisitor visitor) {
    visitor.visitLiteralExp(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FunIncAVisitor) accept((FunIncAVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public FunIncABooleanLit getBooleanLit() {
    return findChildByClass(FunIncABooleanLit.class);
  }

  @Override
  @Nullable
  public FunIncADoubleLit getDoubleLit() {
    return findChildByClass(FunIncADoubleLit.class);
  }

  @Override
  @Nullable
  public FunIncAIntLit getIntLit() {
    return findChildByClass(FunIncAIntLit.class);
  }

  @Override
  @Nullable
  public FunIncALongLit getLongLit() {
    return findChildByClass(FunIncALongLit.class);
  }

  @Override
  @Nullable
  public FunIncAScalaLit getScalaLit() {
    return findChildByClass(FunIncAScalaLit.class);
  }

  @Override
  @Nullable
  public FunIncAStringLit getStringLit() {
    return findChildByClass(FunIncAStringLit.class);
  }

}
