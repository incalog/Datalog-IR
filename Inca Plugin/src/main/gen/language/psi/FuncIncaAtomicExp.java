// This is a generated file. Not intended for manual editing.
package language.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface FuncIncaAtomicExp extends PsiElement {

  @Nullable
  FuncIncaBaseApplyExp getBaseApplyExp();

  @Nullable
  FuncIncaBaseApplyUnaryExp getBaseApplyUnaryExp();

  @Nullable
  FuncIncaBaseLitExp getBaseLitExp();

  @Nullable
  FuncIncaComprehensionExp getComprehensionExp();

  @Nullable
  FuncIncaConstSetExp getConstSetExp();

  @Nullable
  FuncIncaFoldExp getFoldExp();

  @Nullable
  FuncIncaOptionExp getOptionExp();

  @Nullable
  FuncIncaParensExp getParensExp();

  @Nullable
  FuncIncaTupleExp getTupleExp();

  @Nullable
  FuncIncaVar getVar();

}
