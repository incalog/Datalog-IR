// This is a generated file. Not intended for manual editing.
package language.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface FuncIncaMatchExp extends PsiElement {

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

  @Nullable
  FuncIncaFoldExp getFoldExp();

  @Nullable
  FuncIncaLambdaExp getLambdaExp();

  @NotNull
  List<FuncIncaMatchCase> getMatchCaseList();

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
