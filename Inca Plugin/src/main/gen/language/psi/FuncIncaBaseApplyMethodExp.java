// This is a generated file. Not intended for manual editing.
package language.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface FuncIncaBaseApplyMethodExp extends PsiElement {

  @NotNull
  List<FuncIncaBaseApplyExp> getBaseApplyExpList();

  @NotNull
  List<FuncIncaBaseApplyInfixExp> getBaseApplyInfixExpList();

  @NotNull
  List<FuncIncaBaseApplyMethodExp> getBaseApplyMethodExpList();

  @NotNull
  List<FuncIncaBaseApplyUnaryExp> getBaseApplyUnaryExpList();

  @NotNull
  List<FuncIncaBooleanLit> getBooleanLitList();

  @NotNull
  List<FuncIncaCallExp> getCallExpList();

  @NotNull
  List<FuncIncaCastExp> getCastExpList();

  @NotNull
  List<FuncIncaComprehensionExp> getComprehensionExpList();

  @NotNull
  List<FuncIncaConstSetExp> getConstSetExpList();

  @NotNull
  List<FuncIncaFoldExp> getFoldExpList();

  @NotNull
  FuncIncaId getId();

  @NotNull
  List<FuncIncaLambdaExp> getLambdaExpList();

  @NotNull
  List<FuncIncaMatchExp> getMatchExpList();

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
