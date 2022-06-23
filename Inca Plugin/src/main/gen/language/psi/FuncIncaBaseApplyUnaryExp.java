// This is a generated file. Not intended for manual editing.
package language.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface FuncIncaBaseApplyUnaryExp extends PsiElement {

  @Nullable
  FuncIncaBaseApplyExp getBaseApplyExp();

  @Nullable
  FuncIncaBaseApplyInfixExp getBaseApplyInfixExp();

  @Nullable
  FuncIncaBaseApplyMethodExp getBaseApplyMethodExp();

  @Nullable
  FuncIncaBaseApplyUnaryExp getBaseApplyUnaryExp();

  @Nullable
  FuncIncaBooleanLit getBooleanLit();

  @Nullable
  FuncIncaCallExp getCallExp();

  @Nullable
  FuncIncaCastExp getCastExp();

  @Nullable
  FuncIncaComprehensionExp getComprehensionExp();

  @Nullable
  FuncIncaConstSetExp getConstSetExp();

  @Nullable
  FuncIncaFoldExp getFoldExp();

  @Nullable
  FuncIncaLambdaExp getLambdaExp();

  @Nullable
  FuncIncaMatchExp getMatchExp();

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
