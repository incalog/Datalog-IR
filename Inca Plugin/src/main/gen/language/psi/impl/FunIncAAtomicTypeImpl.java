// This is a generated file. Not intended for manual editing.
package language.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static language.psi.FunIncATypes.*;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import language.psi.*;

public class FunIncAAtomicTypeImpl extends ASTWrapperPsiElement implements FunIncAAtomicType {

  public FunIncAAtomicTypeImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull FunIncAVisitor visitor) {
    visitor.visitAtomicType(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof FunIncAVisitor) accept((FunIncAVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public FunIncAConstructorType getConstructorType() {
    return findChildByClass(FunIncAConstructorType.class);
  }

  @Override
  @Nullable
  public FunIncAPrimitiveType getPrimitiveType() {
    return findChildByClass(FunIncAPrimitiveType.class);
  }

  @Override
  @Nullable
  public FunIncAScalaType getScalaType() {
    return findChildByClass(FunIncAScalaType.class);
  }

  @Override
  @Nullable
  public FunIncASetType getSetType() {
    return findChildByClass(FunIncASetType.class);
  }

  @Override
  @Nullable
  public FunIncATupleType getTupleType() {
    return findChildByClass(FunIncATupleType.class);
  }

  @Override
  @Nullable
  public FunIncATypeNameRef getTypeNameRef() {
    return findChildByClass(FunIncATypeNameRef.class);
  }

}
