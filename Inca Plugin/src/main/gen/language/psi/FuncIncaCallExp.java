// This is a generated file. Not intended for manual editing.
package language.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface FuncIncaCallExp extends PsiElement {

  @Nullable
  FuncIncaBaseApplyExp getBaseApplyExp();

  @Nullable
  FuncIncaBaseApplyUnaryExp getBaseApplyUnaryExp();

  @Nullable
  FuncIncaBooleanLit getBooleanLit();

  @Nullable
  FuncIncaComprehensionExp getComprehensionExp();

  @Nullable
  FuncIncaConstSetExp getConstSetExp();

  @NotNull
  List<FuncIncaExp> getExpList();

  @Nullable
  FuncIncaFoldExp getFoldExp();

  @Nullable
  FuncIncaNumericLit getNumericLit();

  @Nullable
  FuncIncaOptionExp getOptionExp();

  @Nullable
  FuncIncaParamTypes getParamTypes();

  @Nullable
  FuncIncaParensExp getParensExp();

  @Nullable
  FuncIncaStringLit getStringLit();

  @Nullable
  FuncIncaTupleExp getTupleExp();

  @Nullable
  FuncIncaVar getVar();

  @Nullable
  PsiElement getScalaTerm();

}
