// This is a generated file. Not intended for manual editing.
package language.psi;

import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiElement;

public class FuncIncaVisitor extends PsiElementVisitor {

  public void visitAnnotation(@NotNull FuncIncaAnnotation o) {
    visitPsiElement(o);
  }

  public void visitAtomicExp(@NotNull FuncIncaAtomicExp o) {
    visitExp(o);
  }

  public void visitAtomicType(@NotNull FuncIncaAtomicType o) {
    visitPsiElement(o);
  }

  public void visitBaseApplyExp(@NotNull FuncIncaBaseApplyExp o) {
    visitExp(o);
  }

  public void visitBaseApplyInfixExp(@NotNull FuncIncaBaseApplyInfixExp o) {
    visitExp(o);
  }

  public void visitBaseApplyMethodExp(@NotNull FuncIncaBaseApplyMethodExp o) {
    visitExp(o);
  }

  public void visitBaseApplyUnaryExp(@NotNull FuncIncaBaseApplyUnaryExp o) {
    visitExp(o);
  }

  public void visitBaseLitExp(@NotNull FuncIncaBaseLitExp o) {
    visitExp(o);
  }

  public void visitBooleanLit(@NotNull FuncIncaBooleanLit o) {
    visitPsiElement(o);
  }

  public void visitBooleanType(@NotNull FuncIncaBooleanType o) {
    visitPsiElement(o);
  }

  public void visitCallExp(@NotNull FuncIncaCallExp o) {
    visitExp(o);
  }

  public void visitCastExp(@NotNull FuncIncaCastExp o) {
    visitExp(o);
  }

  public void visitComprehensionExp(@NotNull FuncIncaComprehensionExp o) {
    visitExp(o);
  }

  public void visitConsId(@NotNull FuncIncaConsId o) {
    visitPsiElement(o);
  }

  public void visitConsPatternId(@NotNull FuncIncaConsPatternId o) {
    visitDecl(o);
  }

  public void visitConstSetExp(@NotNull FuncIncaConstSetExp o) {
    visitExp(o);
  }

  public void visitConstr(@NotNull FuncIncaConstr o) {
    visitPsiElement(o);
  }

  public void visitConstructorPattern(@NotNull FuncIncaConstructorPattern o) {
    visitPsiElement(o);
  }

  public void visitDataConstructor(@NotNull FuncIncaDataConstructor o) {
    visitDecl(o);
  }

  public void visitDataDef(@NotNull FuncIncaDataDef o) {
    visitDecl(o);
  }

  public void visitDoubleLit(@NotNull FuncIncaDoubleLit o) {
    visitPsiElement(o);
  }

  public void visitDoubleType(@NotNull FuncIncaDoubleType o) {
    visitPsiElement(o);
  }

  public void visitExp(@NotNull FuncIncaExp o) {
    visitPsiElement(o);
  }

  public void visitFoldExp(@NotNull FuncIncaFoldExp o) {
    visitExp(o);
  }

  public void visitFunDef(@NotNull FuncIncaFunDef o) {
    visitDecl(o);
  }

  public void visitFunType(@NotNull FuncIncaFunType o) {
    visitPsiElement(o);
  }

  public void visitIfExp(@NotNull FuncIncaIfExp o) {
    visitExp(o);
  }

  public void visitImport(@NotNull FuncIncaImport o) {
    visitDecl(o);
  }

  public void visitInfixExp(@NotNull FuncIncaInfixExp o) {
    visitExp(o);
  }

  public void visitIntegerLit(@NotNull FuncIncaIntegerLit o) {
    visitPsiElement(o);
  }

  public void visitIntegerType(@NotNull FuncIncaIntegerType o) {
    visitPsiElement(o);
  }

  public void visitLambdaExp(@NotNull FuncIncaLambdaExp o) {
    visitExp(o);
  }

  public void visitLetExp(@NotNull FuncIncaLetExp o) {
    visitExp(o);
  }

  public void visitLongLit(@NotNull FuncIncaLongLit o) {
    visitPsiElement(o);
  }

  public void visitLongType(@NotNull FuncIncaLongType o) {
    visitPsiElement(o);
  }

  public void visitMatchCase(@NotNull FuncIncaMatchCase o) {
    visitPsiElement(o);
  }

  public void visitMatchExp(@NotNull FuncIncaMatchExp o) {
    visitExp(o);
  }

  public void visitMemberExp(@NotNull FuncIncaMemberExp o) {
    visitExp(o);
  }

  public void visitMultipleLet(@NotNull FuncIncaMultipleLet o) {
    visitPsiElement(o);
  }

  public void visitOp(@NotNull FuncIncaOp o) {
    visitPsiElement(o);
  }

  public void visitOption(@NotNull FuncIncaOption o) {
    visitPsiElement(o);
  }

  public void visitOptionExp(@NotNull FuncIncaOptionExp o) {
    visitExp(o);
  }

  public void visitOptionPattern(@NotNull FuncIncaOptionPattern o) {
    visitPsiElement(o);
  }

  public void visitParam(@NotNull FuncIncaParam o) {
    visitDecl(o);
  }

  public void visitParamList(@NotNull FuncIncaParamList o) {
    visitPsiElement(o);
  }

  public void visitParamType(@NotNull FuncIncaParamType o) {
    visitDecl(o);
  }

  public void visitParamTypes(@NotNull FuncIncaParamTypes o) {
    visitPsiElement(o);
  }

  public void visitParensExp(@NotNull FuncIncaParensExp o) {
    visitExp(o);
  }

  public void visitPattern(@NotNull FuncIncaPattern o) {
    visitPsiElement(o);
  }

  public void visitPrimitiveType(@NotNull FuncIncaPrimitiveType o) {
    visitPsiElement(o);
  }

  public void visitSet(@NotNull FuncIncaSet o) {
    visitPsiElement(o);
  }

  public void visitSingleLet(@NotNull FuncIncaSingleLet o) {
    visitPsiElement(o);
  }

  public void visitStringLit(@NotNull FuncIncaStringLit o) {
    visitPsiElement(o);
  }

  public void visitStringType(@NotNull FuncIncaStringType o) {
    visitPsiElement(o);
  }

  public void visitSubinfixExp(@NotNull FuncIncaSubinfixExp o) {
    visitExp(o);
  }

  public void visitTuple(@NotNull FuncIncaTuple o) {
    visitPsiElement(o);
  }

  public void visitTupleExp(@NotNull FuncIncaTupleExp o) {
    visitExp(o);
  }

  public void visitTypeAnnotation(@NotNull FuncIncaTypeAnnotation o) {
    visitPsiElement(o);
  }

  public void visitTypeName(@NotNull FuncIncaTypeName o) {
    visitPsiElement(o);
  }

  public void visitUnaryOp(@NotNull FuncIncaUnaryOp o) {
    visitPsiElement(o);
  }

  public void visitVar(@NotNull FuncIncaVar o) {
    visitNamedVariable(o);
  }

  public void visitVarId(@NotNull FuncIncaVarId o) {
    visitDecl(o);
  }

  public void visitVisibility(@NotNull FuncIncaVisibility o) {
    visitPsiElement(o);
  }

  public void visitDecl(@NotNull FuncIncaDecl o) {
    visitPsiElement(o);
  }

  public void visitNamedVariable(@NotNull FuncIncaNamedVariable o) {
    visitPsiElement(o);
  }

  public void visitPsiElement(@NotNull PsiElement o) {
    visitElement(o);
  }

}
