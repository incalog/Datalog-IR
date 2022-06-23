// This is a generated file. Not intended for manual editing.
package language.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface FuncIncaComprehensionExp extends PsiElement {

  @Nullable
  FuncIncaBaseApplyExp getBaseApplyExp();

  @Nullable
  FuncIncaBaseApplyUnaryExp getBaseApplyUnaryExp();

  @Nullable
  FuncIncaBooleanLit getBooleanLit();

  @Nullable
  FuncIncaCallExp getCallExp();

  @Nullable
  FuncIncaComprehensionExp getComprehensionExp();

  @Nullable
  FuncIncaConstSetExp getConstSetExp();

  @NotNull
  List<FuncIncaExp> getExpList();

  @Nullable
  FuncIncaFoldExp getFoldExp();

  @Nullable
  FuncIncaLambdaExp getLambdaExp();

  @Nullable
  FuncIncaNumericLit getNumericLit();

  @Nullable
  FuncIncaOptionExp getOptionExp();

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
