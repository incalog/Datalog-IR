// This is a generated file. Not intended for manual editing.
package language.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface FuncIncaDataConstructor extends FuncIncaNamedElement {

  @NotNull
  List<FuncIncaAtomicType> getAtomicTypeList();

  @NotNull
  List<FuncIncaFunType> getFunTypeList();

  @NotNull
  PsiElement getId();

  String getName();

  PsiElement setName(String newName);

  PsiElement getNameIdentifier();

}
