// This is a generated file. Not intended for manual editing.
package language.psi;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.PsiElement;
import com.intellij.lang.ASTNode;
import language.psi.impl.*;

public interface FunIncATypes {

  IElementType ANNOTATION = new FunIncAElementType("ANNOTATION");
  IElementType ATOMIC_EXP = new FunIncAElementType("ATOMIC_EXP");
  IElementType ATOMIC_TYPE = new FunIncAElementType("ATOMIC_TYPE");
  IElementType BASE_APPLY_EXP = new FunIncAElementType("BASE_APPLY_EXP");
  IElementType BASE_APPLY_INFIX_EXP = new FunIncAElementType("BASE_APPLY_INFIX_EXP");
  IElementType BASE_APPLY_METHOD_EXP = new FunIncAElementType("BASE_APPLY_METHOD_EXP");
  IElementType BASE_APPLY_UNARY_EXP = new FunIncAElementType("BASE_APPLY_UNARY_EXP");
  IElementType BINARY_OP = new FunIncAElementType("BINARY_OP");
  IElementType BOOLEAN_LIT = new FunIncAElementType("BOOLEAN_LIT");
  IElementType CALL_EXP = new FunIncAElementType("CALL_EXP");
  IElementType CALL_EXP_LIST = new FunIncAElementType("CALL_EXP_LIST");
  IElementType CAST_EXP = new FunIncAElementType("CAST_EXP");
  IElementType CONSTRUCTOR_PAT = new FunIncAElementType("CONSTRUCTOR_PAT");
  IElementType CONSTRUCTOR_REF = new FunIncAElementType("CONSTRUCTOR_REF");
  IElementType CONSTRUCTOR_TYPE = new FunIncAElementType("CONSTRUCTOR_TYPE");
  IElementType CONST_SET_EXP = new FunIncAElementType("CONST_SET_EXP");
  IElementType DATA_CONSTRUCTOR_DEF = new FunIncAElementType("DATA_CONSTRUCTOR_DEF");
  IElementType DATA_DEF = new FunIncAElementType("DATA_DEF");
  IElementType DOUBLE_LIT = new FunIncAElementType("DOUBLE_LIT");
  IElementType EXP = new FunIncAElementType("EXP");
  IElementType FOLD_EXP = new FunIncAElementType("FOLD_EXP");
  IElementType FUN_DEF = new FunIncAElementType("FUN_DEF");
  IElementType FUN_TYPE = new FunIncAElementType("FUN_TYPE");
  IElementType IF_EXP = new FunIncAElementType("IF_EXP");
  IElementType IMPORT = new FunIncAElementType("IMPORT");
  IElementType INFIX_EXP = new FunIncAElementType("INFIX_EXP");
  IElementType INT_LIT = new FunIncAElementType("INT_LIT");
  IElementType LAMBDA_EXP = new FunIncAElementType("LAMBDA_EXP");
  IElementType LET_EXP = new FunIncAElementType("LET_EXP");
  IElementType LITERAL_EXP = new FunIncAElementType("LITERAL_EXP");
  IElementType LONG_LIT = new FunIncAElementType("LONG_LIT");
  IElementType MATCH_CASE = new FunIncAElementType("MATCH_CASE");
  IElementType MATCH_EXP = new FunIncAElementType("MATCH_EXP");
  IElementType MULTIPLE_BINDINGS = new FunIncAElementType("MULTIPLE_BINDINGS");
  IElementType PARAM_DEF = new FunIncAElementType("PARAM_DEF");
  IElementType PARENTHESIS_EXP = new FunIncAElementType("PARENTHESIS_EXP");
  IElementType PAT = new FunIncAElementType("PAT");
  IElementType PATTERN_VAR_DEF = new FunIncAElementType("PATTERN_VAR_DEF");
  IElementType PRIMITIVE_TYPE = new FunIncAElementType("PRIMITIVE_TYPE");
  IElementType SCALA_LIT = new FunIncAElementType("SCALA_LIT");
  IElementType SCALA_TYPE = new FunIncAElementType("SCALA_TYPE");
  IElementType SET_COMPREHENSION_EXP = new FunIncAElementType("SET_COMPREHENSION_EXP");
  IElementType SET_MEMBER_EXP = new FunIncAElementType("SET_MEMBER_EXP");
  IElementType SET_TYPE = new FunIncAElementType("SET_TYPE");
  IElementType SINGLE_BINDING = new FunIncAElementType("SINGLE_BINDING");
  IElementType STRING_LIT = new FunIncAElementType("STRING_LIT");
  IElementType SUBINFIX_EXP = new FunIncAElementType("SUBINFIX_EXP");
  IElementType TUPLE_EXP = new FunIncAElementType("TUPLE_EXP");
  IElementType TUPLE_TYPE = new FunIncAElementType("TUPLE_TYPE");
  IElementType TYPE = new FunIncAElementType("TYPE");
  IElementType TYPE_NAME_REF = new FunIncAElementType("TYPE_NAME_REF");
  IElementType TYPE_VAR_DEF = new FunIncAElementType("TYPE_VAR_DEF");
  IElementType UNARY_OP = new FunIncAElementType("UNARY_OP");
  IElementType VAR_DEF = new FunIncAElementType("VAR_DEF");
  IElementType VAR_REF_EXP = new FunIncAElementType("VAR_REF_EXP");
  IElementType VISIBILITY = new FunIncAElementType("VISIBILITY");

  IElementType AND = new FunIncATokenType("&&");
  IElementType ANNOTATION_MAIN = new FunIncATokenType("@main");
  IElementType ARROW = new FunIncATokenType("=>");
  IElementType BACK_TICK = new FunIncATokenType("`");
  IElementType BAR = new FunIncATokenType("|");
  IElementType BOOLEAN_FALSE = new FunIncATokenType("false");
  IElementType BOOLEAN_TRUE = new FunIncATokenType("true");
  IElementType BRACES_CLOSE = new FunIncATokenType("}");
  IElementType BRACES_OPEN = new FunIncATokenType("{");
  IElementType CAST = new FunIncATokenType("as");
  IElementType COLON = new FunIncATokenType(":");
  IElementType COMMA = new FunIncATokenType(",");
  IElementType COMMENT = new FunIncATokenType("comment");
  IElementType DOT = new FunIncATokenType(".");
  IElementType DOUBLE = new FunIncATokenType("double");
  IElementType EQUAL_SIGN = new FunIncATokenType("=");
  IElementType EQUIVALENCE = new FunIncATokenType("==");
  IElementType GEQ = new FunIncATokenType(">=");
  IElementType GT = new FunIncATokenType(">");
  IElementType ID = new FunIncATokenType("id");
  IElementType INTEGER = new FunIncATokenType("integer");
  IElementType KEYWORD_CASE = new FunIncATokenType("case");
  IElementType KEYWORD_DATA = new FunIncATokenType("data");
  IElementType KEYWORD_DEF = new FunIncATokenType("def");
  IElementType KEYWORD_ELSE = new FunIncATokenType("else");
  IElementType KEYWORD_FAIL = new FunIncATokenType("fail");
  IElementType KEYWORD_FOLD = new FunIncATokenType("fold");
  IElementType KEYWORD_IF = new FunIncATokenType("if");
  IElementType KEYWORD_IMPORT = new FunIncATokenType("import");
  IElementType KEYWORD_IN = new FunIncATokenType("in");
  IElementType KEYWORD_LET = new FunIncATokenType("let");
  IElementType KEYWORD_MATCH = new FunIncATokenType("match");
  IElementType KEYWORD_MODULE = new FunIncATokenType("module");
  IElementType KEYWORD_NONE = new FunIncATokenType("None");
  IElementType KEYWORD_NOT = new FunIncATokenType("not");
  IElementType KEYWORD_OPTION = new FunIncATokenType("Option");
  IElementType KEYWORD_SET = new FunIncATokenType("Set");
  IElementType KEYWORD_SOME = new FunIncATokenType("Some");
  IElementType LEQ = new FunIncATokenType("<=");
  IElementType LONG = new FunIncATokenType("long");
  IElementType LT = new FunIncATokenType("<");
  IElementType MINUS = new FunIncATokenType("-");
  IElementType MODULO = new FunIncATokenType("%");
  IElementType NEGATION = new FunIncATokenType("!");
  IElementType NON_EQUIVALENCE = new FunIncATokenType("!=");
  IElementType OR = new FunIncATokenType("||");
  IElementType PARENS_CLOSE = new FunIncATokenType(")");
  IElementType PARENS_OPEN = new FunIncATokenType("(");
  IElementType PLUS = new FunIncATokenType("+");
  IElementType QUOTATION_MARK = new FunIncATokenType("\"");
  IElementType SCALATERM = new FunIncATokenType("scalaterm");
  IElementType SET_INTERSECTION = new FunIncATokenType("&");
  IElementType SET_UNION = new FunIncATokenType("++");
  IElementType SLASH = new FunIncATokenType("/");
  IElementType SQUARE_BRACKET_CLOSE = new FunIncATokenType("]");
  IElementType SQUARE_BRACKET_OPEN = new FunIncATokenType("[");
  IElementType STAR = new FunIncATokenType("*");
  IElementType STRING = new FunIncATokenType("string");
  IElementType TYPE_ANY = new FunIncATokenType("Any");
  IElementType TYPE_BOOLEAN = new FunIncATokenType("Boolean");
  IElementType TYPE_DOUBLE = new FunIncATokenType("Double");
  IElementType TYPE_INT = new FunIncATokenType("Int");
  IElementType TYPE_LONG = new FunIncATokenType("Long");
  IElementType TYPE_NOTHING = new FunIncATokenType("Nothing");
  IElementType TYPE_STRING = new FunIncATokenType("String");
  IElementType TYPE_UNIT = new FunIncATokenType("Unit");
  IElementType VISIBILITY_PRIVATE = new FunIncATokenType("private");

  class Factory {
    public static PsiElement createElement(ASTNode node) {
      IElementType type = node.getElementType();
      if (type == ANNOTATION) {
        return new FunIncAAnnotationImpl(node);
      }
      else if (type == ATOMIC_TYPE) {
        return new FunIncAAtomicTypeImpl(node);
      }
      else if (type == BASE_APPLY_EXP) {
        return new FunIncABaseApplyExpImpl(node);
      }
      else if (type == BASE_APPLY_INFIX_EXP) {
        return new FunIncABaseApplyInfixExpImpl(node);
      }
      else if (type == BASE_APPLY_METHOD_EXP) {
        return new FunIncABaseApplyMethodExpImpl(node);
      }
      else if (type == BASE_APPLY_UNARY_EXP) {
        return new FunIncABaseApplyUnaryExpImpl(node);
      }
      else if (type == BINARY_OP) {
        return new FunIncABinaryOpImpl(node);
      }
      else if (type == BOOLEAN_LIT) {
        return new FunIncABooleanLitImpl(node);
      }
      else if (type == CALL_EXP) {
        return new FunIncACallExpImpl(node);
      }
      else if (type == CALL_EXP_LIST) {
        return new FunIncACallExpListImpl(node);
      }
      else if (type == CAST_EXP) {
        return new FunIncACastExpImpl(node);
      }
      else if (type == CONSTRUCTOR_PAT) {
        return new FunIncAConstructorPatImpl(node);
      }
      else if (type == CONSTRUCTOR_REF) {
        return new FunIncAConstructorRefImpl(node);
      }
      else if (type == CONSTRUCTOR_TYPE) {
        return new FunIncAConstructorTypeImpl(node);
      }
      else if (type == CONST_SET_EXP) {
        return new FunIncAConstSetExpImpl(node);
      }
      else if (type == DATA_CONSTRUCTOR_DEF) {
        return new FunIncADataConstructorDefImpl(node);
      }
      else if (type == DATA_DEF) {
        return new FunIncADataDefImpl(node);
      }
      else if (type == DOUBLE_LIT) {
        return new FunIncADoubleLitImpl(node);
      }
      else if (type == FOLD_EXP) {
        return new FunIncAFoldExpImpl(node);
      }
      else if (type == FUN_DEF) {
        return new FunIncAFunDefImpl(node);
      }
      else if (type == FUN_TYPE) {
        return new FunIncAFunTypeImpl(node);
      }
      else if (type == IF_EXP) {
        return new FunIncAIfExpImpl(node);
      }
      else if (type == IMPORT) {
        return new FunIncAImportImpl(node);
      }
      else if (type == INT_LIT) {
        return new FunIncAIntLitImpl(node);
      }
      else if (type == LAMBDA_EXP) {
        return new FunIncALambdaExpImpl(node);
      }
      else if (type == LET_EXP) {
        return new FunIncALetExpImpl(node);
      }
      else if (type == LITERAL_EXP) {
        return new FunIncALiteralExpImpl(node);
      }
      else if (type == LONG_LIT) {
        return new FunIncALongLitImpl(node);
      }
      else if (type == MATCH_CASE) {
        return new FunIncAMatchCaseImpl(node);
      }
      else if (type == MATCH_EXP) {
        return new FunIncAMatchExpImpl(node);
      }
      else if (type == MULTIPLE_BINDINGS) {
        return new FunIncAMultipleBindingsImpl(node);
      }
      else if (type == PARAM_DEF) {
        return new FunIncAParamDefImpl(node);
      }
      else if (type == PARENTHESIS_EXP) {
        return new FunIncAParenthesisExpImpl(node);
      }
      else if (type == PAT) {
        return new FunIncAPatImpl(node);
      }
      else if (type == PATTERN_VAR_DEF) {
        return new FunIncAPatternVarDefImpl(node);
      }
      else if (type == PRIMITIVE_TYPE) {
        return new FunIncAPrimitiveTypeImpl(node);
      }
      else if (type == SCALA_LIT) {
        return new FunIncAScalaLitImpl(node);
      }
      else if (type == SCALA_TYPE) {
        return new FunIncAScalaTypeImpl(node);
      }
      else if (type == SET_COMPREHENSION_EXP) {
        return new FunIncASetComprehensionExpImpl(node);
      }
      else if (type == SET_MEMBER_EXP) {
        return new FunIncASetMemberExpImpl(node);
      }
      else if (type == SET_TYPE) {
        return new FunIncASetTypeImpl(node);
      }
      else if (type == SINGLE_BINDING) {
        return new FunIncASingleBindingImpl(node);
      }
      else if (type == STRING_LIT) {
        return new FunIncAStringLitImpl(node);
      }
      else if (type == TUPLE_EXP) {
        return new FunIncATupleExpImpl(node);
      }
      else if (type == TUPLE_TYPE) {
        return new FunIncATupleTypeImpl(node);
      }
      else if (type == TYPE) {
        return new FunIncATypeImpl(node);
      }
      else if (type == TYPE_NAME_REF) {
        return new FunIncATypeNameRefImpl(node);
      }
      else if (type == TYPE_VAR_DEF) {
        return new FunIncATypeVarDefImpl(node);
      }
      else if (type == UNARY_OP) {
        return new FunIncAUnaryOpImpl(node);
      }
      else if (type == VAR_DEF) {
        return new FunIncAVarDefImpl(node);
      }
      else if (type == VAR_REF_EXP) {
        return new FunIncAVarRefExpImpl(node);
      }
      else if (type == VISIBILITY) {
        return new FunIncAVisibilityImpl(node);
      }
      throw new AssertionError("Unknown element type: " + type);
    }
  }
}
