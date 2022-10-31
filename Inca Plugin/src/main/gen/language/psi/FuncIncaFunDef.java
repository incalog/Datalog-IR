// This is a generated file. Not intended for manual editing.
package language.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;

public interface FuncIncaFunDef extends FuncIncaNamedElement, FuncIncaDecl {

  @NotNull
  List<FuncIncaAnnotation> getAnnotationList();

  @Nullable
  FuncIncaExp getExp();

  @Nullable
  FuncIncaParamList getParamList();

  @Nullable
  FuncIncaParamTypes getParamTypes();

  @Nullable
  FuncIncaTypeAnnotation getTypeAnnotation();

  @Nullable
  FuncIncaVisibility getVisibility();

  @Nullable
  PsiElement getId();

  String getName();

  PsiElement setName(String newName);

  PsiElement getNameIdentifier();

  PsiReference getReference();

}
