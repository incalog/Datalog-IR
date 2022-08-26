// This is a generated file. Not intended for manual editing.
package language.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface FuncIncaFunDef extends PsiElement {

  @NotNull
  List<FuncIncaAnnotation> getAnnotationList();

  @Nullable
  FuncIncaAtomicType getAtomicType();

  @Nullable
  FuncIncaExp getExp();

  @Nullable
  FuncIncaFunType getFunType();

  @NotNull
  List<FuncIncaParam> getParamList();

  @Nullable
  FuncIncaParamTypes getParamTypes();

  @Nullable
  FuncIncaVisibility getVisibility();

  @Nullable
  PsiElement getId();

  String getName();

  PsiElement setName(String newName);

  PsiElement getNameIdentifier();

}
