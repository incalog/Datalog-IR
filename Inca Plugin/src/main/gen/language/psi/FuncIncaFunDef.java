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

  @NotNull
  FuncIncaExp getExp();

  @Nullable
  FuncIncaFunType getFunType();

  @NotNull
  FuncIncaId getId();

  @NotNull
  List<FuncIncaParam> getParamList();

  @Nullable
  FuncIncaParamTypes getParamTypes();

  @Nullable
  FuncIncaVisibility getVisibility();

}
