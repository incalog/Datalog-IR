// This is a generated file. Not intended for manual editing.
package language.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface FuncIncaMemberExp extends PsiElement {

  @NotNull
  List<FuncIncaBaseApplyExp> getBaseApplyExpList();

  @Nullable
  FuncIncaBaseApplyInfixExp getBaseApplyInfixExp();

  @Nullable
  FuncIncaBaseApplyMethodExp getBaseApplyMethodExp();

  @NotNull
  List<FuncIncaBaseApplyUnaryExp> getBaseApplyUnaryExpList();

  @NotNull
  List<FuncIncaBooleanLit> getBooleanLitList();

  @Nullable
  FuncIncaCallExp getCallExp();

  @Nullable
  FuncIncaCastExp getCastExp();

  @NotNull
  List<FuncIncaComprehensionExp> getComprehensionExpList();

  @NotNull
  List<FuncIncaConstSetExp> getConstSetExpList();

  @NotNull
  List<FuncIncaFoldExp> getFoldExpList();

  @Nullable
  FuncIncaLambdaExp getLambdaExp();

  @Nullable
  FuncIncaMatchExp getMatchExp();

  @NotNull
  List<FuncIncaNumericLit> getNumericLitList();

  @NotNull
  List<FuncIncaOptionExp> getOptionExpList();

  @NotNull
  List<FuncIncaParensExp> getParensExpList();

  @NotNull
  List<FuncIncaStringLit> getStringLitList();

  @NotNull
  List<FuncIncaTupleExp> getTupleExpList();

  @NotNull
  List<FuncIncaVar> getVarList();

}
