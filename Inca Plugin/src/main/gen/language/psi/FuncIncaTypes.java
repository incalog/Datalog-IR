// This is a generated file. Not intended for manual editing.
package language.psi;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.PsiElement;
import com.intellij.lang.ASTNode;
import language.psi.impl.*;

public interface FuncIncaTypes {

  IElementType ANNOTATION = new FuncIncaElementType("ANNOTATION");
  IElementType ATOMIC_EXP = new FuncIncaElementType("ATOMIC_EXP");
  IElementType ATOMIC_TYPE = new FuncIncaElementType("ATOMIC_TYPE");
  IElementType BASE_APPLY_EXP = new FuncIncaElementType("BASE_APPLY_EXP");
  IElementType BASE_APPLY_INFIX_EXP = new FuncIncaElementType("BASE_APPLY_INFIX_EXP");
  IElementType BASE_APPLY_METHOD_EXP = new FuncIncaElementType("BASE_APPLY_METHOD_EXP");
  IElementType BASE_APPLY_UNARY_EXP = new FuncIncaElementType("BASE_APPLY_UNARY_EXP");
  IElementType BASE_LIT_EXP = new FuncIncaElementType("BASE_LIT_EXP");
  IElementType BOOLEAN_LIT = new FuncIncaElementType("BOOLEAN_LIT");
  IElementType CALL_EXP = new FuncIncaElementType("CALL_EXP");
  IElementType CAST_EXP = new FuncIncaElementType("CAST_EXP");
  IElementType COMPREHENSION_EXP = new FuncIncaElementType("COMPREHENSION_EXP");
  IElementType CONSTR = new FuncIncaElementType("CONSTR");
  IElementType CONSTRUCTOR_PATTERN = new FuncIncaElementType("CONSTRUCTOR_PATTERN");
  IElementType CONST_SET_EXP = new FuncIncaElementType("CONST_SET_EXP");
  IElementType CONS_ID = new FuncIncaElementType("CONS_ID");
  IElementType CONS_PATTERN_ID = new FuncIncaElementType("CONS_PATTERN_ID");
  IElementType DATA_CONSTRUCTOR = new FuncIncaElementType("DATA_CONSTRUCTOR");
  IElementType DATA_DEF = new FuncIncaElementType("DATA_DEF");
  IElementType EXP = new FuncIncaElementType("EXP");
  IElementType FOLD_EXP = new FuncIncaElementType("FOLD_EXP");
  IElementType FUN_DEF = new FuncIncaElementType("FUN_DEF");
  IElementType FUN_TYPE = new FuncIncaElementType("FUN_TYPE");
  IElementType IF_EXP = new FuncIncaElementType("IF_EXP");
  IElementType IMPORT = new FuncIncaElementType("IMPORT");
  IElementType INFIX_EXP = new FuncIncaElementType("INFIX_EXP");
  IElementType LAMBDA_EXP = new FuncIncaElementType("LAMBDA_EXP");
  IElementType LET_EXP = new FuncIncaElementType("LET_EXP");
  IElementType MATCH_CASE = new FuncIncaElementType("MATCH_CASE");
  IElementType MATCH_EXP = new FuncIncaElementType("MATCH_EXP");
  IElementType MEMBER_EXP = new FuncIncaElementType("MEMBER_EXP");
  IElementType MULTIPLE_LET = new FuncIncaElementType("MULTIPLE_LET");
  IElementType NUMERIC_LIT = new FuncIncaElementType("NUMERIC_LIT");
  IElementType OP = new FuncIncaElementType("OP");
  IElementType OPTION = new FuncIncaElementType("OPTION");
  IElementType OPTION_EXP = new FuncIncaElementType("OPTION_EXP");
  IElementType OPTION_PATTERN = new FuncIncaElementType("OPTION_PATTERN");
  IElementType PARAM = new FuncIncaElementType("PARAM");
  IElementType PARAM_LIST = new FuncIncaElementType("PARAM_LIST");
  IElementType PARAM_TYPE = new FuncIncaElementType("PARAM_TYPE");
  IElementType PARAM_TYPES = new FuncIncaElementType("PARAM_TYPES");
  IElementType PARENS_EXP = new FuncIncaElementType("PARENS_EXP");
  IElementType PATTERN = new FuncIncaElementType("PATTERN");
  IElementType SET = new FuncIncaElementType("SET");
  IElementType SINGLE_LET = new FuncIncaElementType("SINGLE_LET");
  IElementType STRING_LIT = new FuncIncaElementType("STRING_LIT");
  IElementType SUBINFIX_EXP = new FuncIncaElementType("SUBINFIX_EXP");
  IElementType TUPLE = new FuncIncaElementType("TUPLE");
  IElementType TUPLE_EXP = new FuncIncaElementType("TUPLE_EXP");
  IElementType TYPE_ANNOTATION = new FuncIncaElementType("TYPE_ANNOTATION");
  IElementType TYPE_NAME = new FuncIncaElementType("TYPE_NAME");
  IElementType UNARY_OP = new FuncIncaElementType("UNARY_OP");
  IElementType VAR = new FuncIncaElementType("VAR");
  IElementType VAR_ID = new FuncIncaElementType("VAR_ID");
  IElementType VISIBILITY = new FuncIncaElementType("VISIBILITY");

  IElementType AND = new FuncIncaTokenType("&&");
  IElementType ANNOTATION_MAIN = new FuncIncaTokenType("@main");
  IElementType ARROW = new FuncIncaTokenType("=>");
  IElementType BACK_TICK = new FuncIncaTokenType("`");
  IElementType BAR = new FuncIncaTokenType("|");
  IElementType BOOLEAN_FALSE = new FuncIncaTokenType("false");
  IElementType BOOLEAN_TRUE = new FuncIncaTokenType("true");
  IElementType BRACES_CLOSE = new FuncIncaTokenType("}");
  IElementType BRACES_OPEN = new FuncIncaTokenType("{");
  IElementType CAST = new FuncIncaTokenType("as");
  IElementType COLON = new FuncIncaTokenType(":");
  IElementType COMMA = new FuncIncaTokenType(",");
  IElementType COMMENT = new FuncIncaTokenType("comment");
  IElementType DOT = new FuncIncaTokenType(".");
  IElementType DOUBLE = new FuncIncaTokenType("double");
  IElementType EQUAL_SIGN = new FuncIncaTokenType("=");
  IElementType EQUIVALENCE = new FuncIncaTokenType("==");
  IElementType GEQ = new FuncIncaTokenType(">=");
  IElementType GT = new FuncIncaTokenType(">");
  IElementType ID = new FuncIncaTokenType("id");
  IElementType INTEGER = new FuncIncaTokenType("integer");
  IElementType KEYWORD_CASE = new FuncIncaTokenType("case");
  IElementType KEYWORD_DATA = new FuncIncaTokenType("data");
  IElementType KEYWORD_DEF = new FuncIncaTokenType("def");
  IElementType KEYWORD_ELSE = new FuncIncaTokenType("else");
  IElementType KEYWORD_FAIL = new FuncIncaTokenType("fail");
  IElementType KEYWORD_FOLD = new FuncIncaTokenType("fold");
  IElementType KEYWORD_IF = new FuncIncaTokenType("if");
  IElementType KEYWORD_IMPORT = new FuncIncaTokenType("import");
  IElementType KEYWORD_IN = new FuncIncaTokenType("in");
  IElementType KEYWORD_LET = new FuncIncaTokenType("let");
  IElementType KEYWORD_MATCH = new FuncIncaTokenType("match");
  IElementType KEYWORD_MODULE = new FuncIncaTokenType("module");
  IElementType KEYWORD_NONE = new FuncIncaTokenType("None");
  IElementType KEYWORD_NOT = new FuncIncaTokenType("not");
  IElementType KEYWORD_OPTION = new FuncIncaTokenType("Option");
  IElementType KEYWORD_SET = new FuncIncaTokenType("Set");
  IElementType KEYWORD_SOME = new FuncIncaTokenType("Some");
  IElementType LEQ = new FuncIncaTokenType("<=");
  IElementType LONG = new FuncIncaTokenType("long");
  IElementType LT = new FuncIncaTokenType("<");
  IElementType MINUS = new FuncIncaTokenType("-");
  IElementType MODULO = new FuncIncaTokenType("%");
  IElementType NEGATION = new FuncIncaTokenType("!");
  IElementType NON_EQUIVALENCE = new FuncIncaTokenType("!=");
  IElementType OR = new FuncIncaTokenType("||");
  IElementType PARENS_CLOSE = new FuncIncaTokenType(")");
  IElementType PARENS_OPEN = new FuncIncaTokenType("(");
  IElementType PLUS = new FuncIncaTokenType("+");
  IElementType QUOTATION_MARK = new FuncIncaTokenType("\"");
  IElementType SCALA_TERM = new FuncIncaTokenType("scala_term");
  IElementType SET_INTERSECTION = new FuncIncaTokenType("&");
  IElementType SET_UNION = new FuncIncaTokenType("++");
  IElementType SLASH = new FuncIncaTokenType("/");
  IElementType SQUARE_BRACKET_CLOSE = new FuncIncaTokenType("]");
  IElementType SQUARE_BRACKET_OPEN = new FuncIncaTokenType("[");
  IElementType STAR = new FuncIncaTokenType("*");
  IElementType STRING = new FuncIncaTokenType("string");
  IElementType TYPE_ANY = new FuncIncaTokenType("Any");
  IElementType TYPE_NOTHING = new FuncIncaTokenType("Nothing");
  IElementType TYPE_UNIT = new FuncIncaTokenType("Unit");
  IElementType VISIBILITY_PRIVATE = new FuncIncaTokenType("private");

  class Factory {
    public static PsiElement createElement(ASTNode node) {
      IElementType type = node.getElementType();
      if (type == ANNOTATION) {
        return new FuncIncaAnnotationImpl(node);
      }
      else if (type == ATOMIC_EXP) {
        return new FuncIncaAtomicExpImpl(node);
      }
      else if (type == ATOMIC_TYPE) {
        return new FuncIncaAtomicTypeImpl(node);
      }
      else if (type == BASE_APPLY_EXP) {
        return new FuncIncaBaseApplyExpImpl(node);
      }
      else if (type == BASE_APPLY_INFIX_EXP) {
        return new FuncIncaBaseApplyInfixExpImpl(node);
      }
      else if (type == BASE_APPLY_METHOD_EXP) {
        return new FuncIncaBaseApplyMethodExpImpl(node);
      }
      else if (type == BASE_APPLY_UNARY_EXP) {
        return new FuncIncaBaseApplyUnaryExpImpl(node);
      }
      else if (type == BASE_LIT_EXP) {
        return new FuncIncaBaseLitExpImpl(node);
      }
      else if (type == BOOLEAN_LIT) {
        return new FuncIncaBooleanLitImpl(node);
      }
      else if (type == CALL_EXP) {
        return new FuncIncaCallExpImpl(node);
      }
      else if (type == CAST_EXP) {
        return new FuncIncaCastExpImpl(node);
      }
      else if (type == COMPREHENSION_EXP) {
        return new FuncIncaComprehensionExpImpl(node);
      }
      else if (type == CONSTR) {
        return new FuncIncaConstrImpl(node);
      }
      else if (type == CONSTRUCTOR_PATTERN) {
        return new FuncIncaConstructorPatternImpl(node);
      }
      else if (type == CONST_SET_EXP) {
        return new FuncIncaConstSetExpImpl(node);
      }
      else if (type == CONS_ID) {
        return new FuncIncaConsIdImpl(node);
      }
      else if (type == CONS_PATTERN_ID) {
        return new FuncIncaConsPatternIdImpl(node);
      }
      else if (type == DATA_CONSTRUCTOR) {
        return new FuncIncaDataConstructorImpl(node);
      }
      else if (type == DATA_DEF) {
        return new FuncIncaDataDefImpl(node);
      }
      else if (type == FOLD_EXP) {
        return new FuncIncaFoldExpImpl(node);
      }
      else if (type == FUN_DEF) {
        return new FuncIncaFunDefImpl(node);
      }
      else if (type == FUN_TYPE) {
        return new FuncIncaFunTypeImpl(node);
      }
      else if (type == IF_EXP) {
        return new FuncIncaIfExpImpl(node);
      }
      else if (type == IMPORT) {
        return new FuncIncaImportImpl(node);
      }
      else if (type == LAMBDA_EXP) {
        return new FuncIncaLambdaExpImpl(node);
      }
      else if (type == LET_EXP) {
        return new FuncIncaLetExpImpl(node);
      }
      else if (type == MATCH_CASE) {
        return new FuncIncaMatchCaseImpl(node);
      }
      else if (type == MATCH_EXP) {
        return new FuncIncaMatchExpImpl(node);
      }
      else if (type == MEMBER_EXP) {
        return new FuncIncaMemberExpImpl(node);
      }
      else if (type == MULTIPLE_LET) {
        return new FuncIncaMultipleLetImpl(node);
      }
      else if (type == NUMERIC_LIT) {
        return new FuncIncaNumericLitImpl(node);
      }
      else if (type == OP) {
        return new FuncIncaOpImpl(node);
      }
      else if (type == OPTION) {
        return new FuncIncaOptionImpl(node);
      }
      else if (type == OPTION_EXP) {
        return new FuncIncaOptionExpImpl(node);
      }
      else if (type == OPTION_PATTERN) {
        return new FuncIncaOptionPatternImpl(node);
      }
      else if (type == PARAM) {
        return new FuncIncaParamImpl(node);
      }
      else if (type == PARAM_LIST) {
        return new FuncIncaParamListImpl(node);
      }
      else if (type == PARAM_TYPE) {
        return new FuncIncaParamTypeImpl(node);
      }
      else if (type == PARAM_TYPES) {
        return new FuncIncaParamTypesImpl(node);
      }
      else if (type == PARENS_EXP) {
        return new FuncIncaParensExpImpl(node);
      }
      else if (type == PATTERN) {
        return new FuncIncaPatternImpl(node);
      }
      else if (type == SET) {
        return new FuncIncaSetImpl(node);
      }
      else if (type == SINGLE_LET) {
        return new FuncIncaSingleLetImpl(node);
      }
      else if (type == STRING_LIT) {
        return new FuncIncaStringLitImpl(node);
      }
      else if (type == TUPLE) {
        return new FuncIncaTupleImpl(node);
      }
      else if (type == TUPLE_EXP) {
        return new FuncIncaTupleExpImpl(node);
      }
      else if (type == TYPE_ANNOTATION) {
        return new FuncIncaTypeAnnotationImpl(node);
      }
      else if (type == TYPE_NAME) {
        return new FuncIncaTypeNameImpl(node);
      }
      else if (type == UNARY_OP) {
        return new FuncIncaUnaryOpImpl(node);
      }
      else if (type == VAR) {
        return new FuncIncaVarImpl(node);
      }
      else if (type == VAR_ID) {
        return new FuncIncaVarIdImpl(node);
      }
      else if (type == VISIBILITY) {
        return new FuncIncaVisibilityImpl(node);
      }
      throw new AssertionError("Unknown element type: " + type);
    }
  }
}
