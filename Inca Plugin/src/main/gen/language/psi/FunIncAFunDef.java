// This is a generated file. Not intended for manual editing.
package language.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface FunIncAFunDef extends FunIncADecl {

  @NotNull
  List<FunIncAAnnotation> getAnnotationList();

  @Nullable
  FunIncAExp getExp();

  @NotNull
  List<FunIncAParamDef> getParamDefList();

  @Nullable
  FunIncAType getType();

  @NotNull
  List<FunIncATypeVarDef> getTypeVarDefList();

  @Nullable
  FunIncAVisibility getVisibility();

  @Nullable
  PsiElement getId();

  String getName();

  PsiElement setName(String newName);

  PsiElement getNameIdentifier();

}
