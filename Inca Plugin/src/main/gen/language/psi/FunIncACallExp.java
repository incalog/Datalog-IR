// This is a generated file. Not intended for manual editing.
package language.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface FunIncACallExp extends FunIncAExp {

  @NotNull
  List<FunIncACallExpList> getCallExpListList();

  @NotNull
  FunIncAExp getExp();

  @NotNull
  List<FunIncAType> getTypeList();

}
