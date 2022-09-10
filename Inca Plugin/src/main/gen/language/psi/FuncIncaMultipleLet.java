// This is a generated file. Not intended for manual editing.
package language.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface FuncIncaMultipleLet extends PsiElement {

  @NotNull
  FuncIncaExp getExp();

  @NotNull
  FuncIncaInfixExp getInfixExp();

  @NotNull
  List<FuncIncaTypeAnnotation> getTypeAnnotationList();

  @NotNull
  List<FuncIncaVarId> getVarIdList();

}
