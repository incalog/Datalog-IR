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

public class FuncIncaConstructorPatternImpl extends FuncIncaNamedElementImpl implements FuncIncaConstructorPattern {

  public FuncIncaConstructorPatternImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull FuncIncaVisitor visitor) {
    visitor.visitConstructorPattern(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FuncIncaVisitor) accept((FuncIncaVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<FuncIncaConsPatternId> getConsPatternIdList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, FuncIncaConsPatternId.class);
  }

  @Override
  @Nullable
  public FuncIncaParamTypes getParamTypes() {
    return findChildByClass(FuncIncaParamTypes.class);
  }

  @Override
  @NotNull
  public PsiElement getId() {
    return findNotNullChildByType(ID);
  }

  @Override
  public String getName() {
    return FuncIncaPsiImplUtil.getName(this);
  }

  @Override
  public PsiElement setName(String newName) {
    return FuncIncaPsiImplUtil.setName(this, newName);
  }

  @Override
  public PsiElement getNameIdentifier() {
    return FuncIncaPsiImplUtil.getNameIdentifier(this);
  }

}
