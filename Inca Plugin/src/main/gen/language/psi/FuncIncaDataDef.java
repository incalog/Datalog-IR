// This is a generated file. Not intended for manual editing.
package language.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface FuncIncaDataDef extends FuncIncaNamedElement {

  @NotNull
  List<FuncIncaAnnotation> getAnnotationList();

  @NotNull
  List<FuncIncaDataConstructor> getDataConstructorList();

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
