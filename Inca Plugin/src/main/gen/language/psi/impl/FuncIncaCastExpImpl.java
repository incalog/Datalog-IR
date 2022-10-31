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

public class FuncIncaCastExpImpl extends FuncIncaExpImpl implements FuncIncaCastExp {

  public FuncIncaCastExpImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull FuncIncaVisitor visitor) {
    visitor.visitCastExp(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FuncIncaVisitor) accept((FuncIncaVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public FuncIncaExp getExp() {
    return findNotNullChildByClass(FuncIncaExp.class);
  }

  @Override
  @Nullable
  public FuncIncaTypeName getTypeName() {
    return findChildByClass(FuncIncaTypeName.class);
  }

}
