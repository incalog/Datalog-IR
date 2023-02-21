// This is a generated file. Not intended for manual editing.
package language.parser;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilder.Marker;
import static language.psi.FuncIncaTypes.*;
import static com.intellij.lang.parser.GeneratedParserUtilBase.*;
import com.intellij.psi.tree.IElementType;
import com.intellij.lang.ASTNode;
import com.intellij.psi.tree.TokenSet;
import com.intellij.lang.PsiParser;
import com.intellij.lang.LightPsiParser;

@SuppressWarnings({"SimplifiableIfStatement", "UnusedAssignment"})
public class FuncIncaParser implements PsiParser, LightPsiParser {

  public ASTNode parse(IElementType t, PsiBuilder b) {
    parseLight(t, b);
    return b.getTreeBuilt();
  }

  public void parseLight(IElementType t, PsiBuilder b) {
    boolean r;
    b = adapt_builder_(t, b, this, EXTENDS_SETS_);
    Marker m = enter_section_(b, 0, _COLLAPSE_, null);
    r = parse_root_(t, b);
    exit_section_(b, 0, m, t, r, true, TRUE_CONDITION);
  }

  protected boolean parse_root_(IElementType t, PsiBuilder b) {
    return parse_root_(t, b, 0);
  }

  static boolean parse_root_(IElementType t, PsiBuilder b, int l) {
    return module(b, l + 1);
  }

  public static final TokenSet[] EXTENDS_SETS_ = new TokenSet[] {
    create_token_set_(ATOMIC_EXP, BASE_APPLY_EXP, BASE_APPLY_INFIX_EXP, BASE_APPLY_METHOD_EXP,
      BASE_APPLY_UNARY_EXP, BASE_LIT_EXP, CALL_EXP, CAST_EXP,
      COMPREHENSION_EXP, CONST_SET_EXP, EXP, FOLD_EXP,
      IF_EXP, INFIX_EXP, LAMBDA_EXP, LET_EXP,
      MATCH_EXP, MEMBER_EXP, OPTION_EXP, PARENS_EXP,
      SUBINFIX_EXP, TUPLE_EXP, VAR),
  };

  /* ********************************************************** */
  // '@main'
  public static boolean annotation(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "annotation")) return false;
    if (!nextTokenIs(b, ANNOTATION_MAIN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ANNOTATION_MAIN);
    exit_section_(b, m, ANNOTATION, r);
    return r;
  }

  /* ********************************************************** */
  // parens_exp | option_exp | comprehension_exp | const_set_exp | tuple_exp | fold_exp
  //                | base_apply_exp | base_lit_exp | var | base_apply_unary_exp
  public static boolean atomic_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atomic_exp")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, ATOMIC_EXP, "<atomic exp>");
    r = parens_exp(b, l + 1);
    if (!r) r = option_exp(b, l + 1);
    if (!r) r = comprehension_exp(b, l + 1);
    if (!r) r = const_set_exp(b, l + 1);
    if (!r) r = tuple_exp(b, l + 1);
    if (!r) r = fold_exp(b, l + 1);
    if (!r) r = base_apply_exp(b, l + 1);
    if (!r) r = base_lit_exp(b, l + 1);
    if (!r) r = var(b, l + 1);
    if (!r) r = base_apply_unary_exp(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // tuple | 'Any' | 'Nothing' | 'Unit' | primitive_type | set | constr | scala_term |type_name
  public static boolean atomic_type(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atomic_type")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, ATOMIC_TYPE, "<atomic type>");
    r = tuple(b, l + 1);
    if (!r) r = consumeToken(b, TYPE_ANY);
    if (!r) r = consumeToken(b, TYPE_NOTHING);
    if (!r) r = consumeToken(b, TYPE_UNIT);
    if (!r) r = primitive_type(b, l + 1);
    if (!r) r = set(b, l + 1);
    if (!r) r = constr(b, l + 1);
    if (!r) r = consumeToken(b, SCALA_TERM);
    if (!r) r = type_name(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // scala_term '(' (exp (',' exp)*)? ')'
  public static boolean base_apply_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "base_apply_exp")) return false;
    if (!nextTokenIs(b, SCALA_TERM)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, SCALA_TERM, PARENS_OPEN);
    r = r && base_apply_exp_2(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, BASE_APPLY_EXP, r);
    return r;
  }

  // (exp (',' exp)*)?
  private static boolean base_apply_exp_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "base_apply_exp_2")) return false;
    base_apply_exp_2_0(b, l + 1);
    return true;
  }

  // exp (',' exp)*
  private static boolean base_apply_exp_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "base_apply_exp_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = exp(b, l + 1);
    r = r && base_apply_exp_2_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' exp)*
  private static boolean base_apply_exp_2_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "base_apply_exp_2_0_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!base_apply_exp_2_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "base_apply_exp_2_0_1", c)) break;
    }
    return true;
  }

  // ',' exp
  private static boolean base_apply_exp_2_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "base_apply_exp_2_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // subinfix_exp op infix_exp
  public static boolean base_apply_infix_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "base_apply_infix_exp")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, BASE_APPLY_INFIX_EXP, "<base apply infix exp>");
    r = subinfix_exp(b, l + 1);
    r = r && op(b, l + 1);
    p = r; // pin = op
    r = r && infix_exp(b, l + 1);
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // subinfix_exp '.' scala_term ( '(' (infix_exp (',' infix_exp)*)? ')' )?
  public static boolean base_apply_method_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "base_apply_method_exp")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, BASE_APPLY_METHOD_EXP, "<base apply method exp>");
    r = subinfix_exp(b, l + 1);
    r = r && consumeTokens(b, 2, DOT, SCALA_TERM);
    p = r; // pin = 3
    r = r && base_apply_method_exp_3(b, l + 1);
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // ( '(' (infix_exp (',' infix_exp)*)? ')' )?
  private static boolean base_apply_method_exp_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "base_apply_method_exp_3")) return false;
    base_apply_method_exp_3_0(b, l + 1);
    return true;
  }

  // '(' (infix_exp (',' infix_exp)*)? ')'
  private static boolean base_apply_method_exp_3_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "base_apply_method_exp_3_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PARENS_OPEN);
    r = r && base_apply_method_exp_3_0_1(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, null, r);
    return r;
  }

  // (infix_exp (',' infix_exp)*)?
  private static boolean base_apply_method_exp_3_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "base_apply_method_exp_3_0_1")) return false;
    base_apply_method_exp_3_0_1_0(b, l + 1);
    return true;
  }

  // infix_exp (',' infix_exp)*
  private static boolean base_apply_method_exp_3_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "base_apply_method_exp_3_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = infix_exp(b, l + 1);
    r = r && base_apply_method_exp_3_0_1_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' infix_exp)*
  private static boolean base_apply_method_exp_3_0_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "base_apply_method_exp_3_0_1_0_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!base_apply_method_exp_3_0_1_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "base_apply_method_exp_3_0_1_0_1", c)) break;
    }
    return true;
  }

  // ',' infix_exp
  private static boolean base_apply_method_exp_3_0_1_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "base_apply_method_exp_3_0_1_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && infix_exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // unary_op infix_exp
  public static boolean base_apply_unary_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "base_apply_unary_exp")) return false;
    if (!nextTokenIs(b, "<base apply unary exp>", MINUS, NEGATION)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, BASE_APPLY_UNARY_EXP, "<base apply unary exp>");
    r = unary_op(b, l + 1);
    p = r; // pin = 1
    r = r && infix_exp(b, l + 1);
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // int_lit | long_lit | double_lit | boolean_lit | string_lit | scala_term
  public static boolean base_lit_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "base_lit_exp")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, BASE_LIT_EXP, "<base lit exp>");
    r = int_lit(b, l + 1);
    if (!r) r = long_lit(b, l + 1);
    if (!r) r = double_lit(b, l + 1);
    if (!r) r = boolean_lit(b, l + 1);
    if (!r) r = string_lit(b, l + 1);
    if (!r) r = consumeToken(b, SCALA_TERM);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // 'true' | 'false'
  public static boolean boolean_lit(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "boolean_lit")) return false;
    if (!nextTokenIs(b, "<boolean lit>", BOOLEAN_FALSE, BOOLEAN_TRUE)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, BOOLEAN_LIT, "<boolean lit>");
    r = consumeToken(b, BOOLEAN_TRUE);
    if (!r) r = consumeToken(b, BOOLEAN_FALSE);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // atomic_exp type_variables? ('(' (exp (',' exp)*)? ')')+
  public static boolean call_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "call_exp")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, CALL_EXP, "<call exp>");
    r = atomic_exp(b, l + 1);
    r = r && call_exp_1(b, l + 1);
    r = r && call_exp_2(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // type_variables?
  private static boolean call_exp_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "call_exp_1")) return false;
    type_variables(b, l + 1);
    return true;
  }

  // ('(' (exp (',' exp)*)? ')')+
  private static boolean call_exp_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "call_exp_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = call_exp_2_0(b, l + 1);
    while (r) {
      int c = current_position_(b);
      if (!call_exp_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "call_exp_2", c)) break;
    }
    exit_section_(b, m, null, r);
    return r;
  }

  // '(' (exp (',' exp)*)? ')'
  private static boolean call_exp_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "call_exp_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PARENS_OPEN);
    r = r && call_exp_2_0_1(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, null, r);
    return r;
  }

  // (exp (',' exp)*)?
  private static boolean call_exp_2_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "call_exp_2_0_1")) return false;
    call_exp_2_0_1_0(b, l + 1);
    return true;
  }

  // exp (',' exp)*
  private static boolean call_exp_2_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "call_exp_2_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = exp(b, l + 1);
    r = r && call_exp_2_0_1_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' exp)*
  private static boolean call_exp_2_0_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "call_exp_2_0_1_0_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!call_exp_2_0_1_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "call_exp_2_0_1_0_1", c)) break;
    }
    return true;
  }

  // ',' exp
  private static boolean call_exp_2_0_1_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "call_exp_2_0_1_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // subinfix_exp '.' 'as' '[' type_name ']'
  public static boolean cast_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cast_exp")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, CAST_EXP, "<cast exp>");
    r = subinfix_exp(b, l + 1);
    r = r && consumeTokens(b, 2, DOT, CAST, SQUARE_BRACKET_OPEN);
    p = r; // pin = 3
    r = r && report_error_(b, type_name(b, l + 1));
    r = p && consumeToken(b, SQUARE_BRACKET_CLOSE) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '{' subinfix_exp '|' exp (',' exp)* '}'
  public static boolean comprehension_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "comprehension_exp")) return false;
    if (!nextTokenIs(b, BRACES_OPEN)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, COMPREHENSION_EXP, null);
    r = consumeToken(b, BRACES_OPEN);
    r = r && subinfix_exp(b, l + 1);
    r = r && consumeToken(b, BAR);
    p = r; // pin = 3
    r = r && report_error_(b, exp(b, l + 1));
    r = p && report_error_(b, comprehension_exp_4(b, l + 1)) && r;
    r = p && consumeToken(b, BRACES_CLOSE) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // (',' exp)*
  private static boolean comprehension_exp_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "comprehension_exp_4")) return false;
    while (true) {
      int c = current_position_(b);
      if (!comprehension_exp_4_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "comprehension_exp_4", c)) break;
    }
    return true;
  }

  // ',' exp
  private static boolean comprehension_exp_4_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "comprehension_exp_4_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // id
  public static boolean cons_id(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cons_id")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ID);
    exit_section_(b, m, CONS_ID, r);
    return r;
  }

  /* ********************************************************** */
  // id
  public static boolean cons_pattern_id(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cons_pattern_id")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ID);
    exit_section_(b, m, CONS_PATTERN_ID, r);
    return r;
  }

  /* ********************************************************** */
  // '{' (exp (',' exp)*)? '}'
  public static boolean const_set_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "const_set_exp")) return false;
    if (!nextTokenIs(b, BRACES_OPEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, BRACES_OPEN);
    r = r && const_set_exp_1(b, l + 1);
    r = r && consumeToken(b, BRACES_CLOSE);
    exit_section_(b, m, CONST_SET_EXP, r);
    return r;
  }

  // (exp (',' exp)*)?
  private static boolean const_set_exp_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "const_set_exp_1")) return false;
    const_set_exp_1_0(b, l + 1);
    return true;
  }

  // exp (',' exp)*
  private static boolean const_set_exp_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "const_set_exp_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = exp(b, l + 1);
    r = r && const_set_exp_1_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' exp)*
  private static boolean const_set_exp_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "const_set_exp_1_0_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!const_set_exp_1_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "const_set_exp_1_0_1", c)) break;
    }
    return true;
  }

  // ',' exp
  private static boolean const_set_exp_1_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "const_set_exp_1_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // var '[' type_annotation (',' type_annotation)* ']'
  public static boolean constr(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constr")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = var(b, l + 1);
    r = r && consumeToken(b, SQUARE_BRACKET_OPEN);
    r = r && type_annotation(b, l + 1);
    r = r && constr_3(b, l + 1);
    r = r && consumeToken(b, SQUARE_BRACKET_CLOSE);
    exit_section_(b, m, CONSTR, r);
    return r;
  }

  // (',' type_annotation)*
  private static boolean constr_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constr_3")) return false;
    while (true) {
      int c = current_position_(b);
      if (!constr_3_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "constr_3", c)) break;
    }
    return true;
  }

  // ',' type_annotation
  private static boolean constr_3_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constr_3_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && type_annotation(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // cons_id type_variables? '(' (cons_pattern_id (',' cons_pattern_id)*)? ')'
  public static boolean constructor_pattern(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constructor_pattern")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = cons_id(b, l + 1);
    r = r && constructor_pattern_1(b, l + 1);
    r = r && consumeToken(b, PARENS_OPEN);
    r = r && constructor_pattern_3(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, CONSTRUCTOR_PATTERN, r);
    return r;
  }

  // type_variables?
  private static boolean constructor_pattern_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constructor_pattern_1")) return false;
    type_variables(b, l + 1);
    return true;
  }

  // (cons_pattern_id (',' cons_pattern_id)*)?
  private static boolean constructor_pattern_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constructor_pattern_3")) return false;
    constructor_pattern_3_0(b, l + 1);
    return true;
  }

  // cons_pattern_id (',' cons_pattern_id)*
  private static boolean constructor_pattern_3_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constructor_pattern_3_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = cons_pattern_id(b, l + 1);
    r = r && constructor_pattern_3_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' cons_pattern_id)*
  private static boolean constructor_pattern_3_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constructor_pattern_3_0_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!constructor_pattern_3_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "constructor_pattern_3_0_1", c)) break;
    }
    return true;
  }

  // ',' cons_pattern_id
  private static boolean constructor_pattern_3_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "constructor_pattern_3_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && cons_pattern_id(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // id type_variables? '(' (type_annotation (',' type_annotation)*)? ')'
  public static boolean data_constructor(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "data_constructor")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ID);
    r = r && data_constructor_1(b, l + 1);
    r = r && consumeToken(b, PARENS_OPEN);
    r = r && data_constructor_3(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, DATA_CONSTRUCTOR, r);
    return r;
  }

  // type_variables?
  private static boolean data_constructor_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "data_constructor_1")) return false;
    type_variables(b, l + 1);
    return true;
  }

  // (type_annotation (',' type_annotation)*)?
  private static boolean data_constructor_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "data_constructor_3")) return false;
    data_constructor_3_0(b, l + 1);
    return true;
  }

  // type_annotation (',' type_annotation)*
  private static boolean data_constructor_3_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "data_constructor_3_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = type_annotation(b, l + 1);
    r = r && data_constructor_3_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' type_annotation)*
  private static boolean data_constructor_3_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "data_constructor_3_0_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!data_constructor_3_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "data_constructor_3_0_1", c)) break;
    }
    return true;
  }

  // ',' type_annotation
  private static boolean data_constructor_3_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "data_constructor_3_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && type_annotation(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // annotation* visibility? 'data' id type_variables? '=' data_constructor ('|' data_constructor)*
  public static boolean data_def(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "data_def")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, DATA_DEF, "<data def>");
    r = data_def_0(b, l + 1);
    r = r && data_def_1(b, l + 1);
    r = r && consumeTokens(b, 1, KEYWORD_DATA, ID);
    p = r; // pin = 3
    r = r && report_error_(b, data_def_4(b, l + 1));
    r = p && report_error_(b, consumeToken(b, EQUAL_SIGN)) && r;
    r = p && report_error_(b, data_constructor(b, l + 1)) && r;
    r = p && data_def_7(b, l + 1) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // annotation*
  private static boolean data_def_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "data_def_0")) return false;
    while (true) {
      int c = current_position_(b);
      if (!annotation(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "data_def_0", c)) break;
    }
    return true;
  }

  // visibility?
  private static boolean data_def_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "data_def_1")) return false;
    visibility(b, l + 1);
    return true;
  }

  // type_variables?
  private static boolean data_def_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "data_def_4")) return false;
    type_variables(b, l + 1);
    return true;
  }

  // ('|' data_constructor)*
  private static boolean data_def_7(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "data_def_7")) return false;
    while (true) {
      int c = current_position_(b);
      if (!data_def_7_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "data_def_7", c)) break;
    }
    return true;
  }

  // '|' data_constructor
  private static boolean data_def_7_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "data_def_7_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, BAR);
    r = r && data_constructor(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // double
  public static boolean double_lit(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "double_lit")) return false;
    if (!nextTokenIs(b, DOUBLE)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, DOUBLE);
    exit_section_(b, m, DOUBLE_LIT, r);
    return r;
  }

  /* ********************************************************** */
  // if_exp | let_exp | member_exp | infix_exp
  public static boolean exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exp")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, EXP, "<exp>");
    r = if_exp(b, l + 1);
    if (!r) r = let_exp(b, l + 1);
    if (!r) r = member_exp(b, l + 1);
    if (!r) r = infix_exp(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // 'fold' ('[' type_annotation ']')? '(' exp ',' exp ',' exp ')'
  public static boolean fold_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fold_exp")) return false;
    if (!nextTokenIs(b, KEYWORD_FOLD)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, FOLD_EXP, null);
    r = consumeToken(b, KEYWORD_FOLD);
    p = r; // pin = 1
    r = r && report_error_(b, fold_exp_1(b, l + 1));
    r = p && report_error_(b, consumeToken(b, PARENS_OPEN)) && r;
    r = p && report_error_(b, exp(b, l + 1)) && r;
    r = p && report_error_(b, consumeToken(b, COMMA)) && r;
    r = p && report_error_(b, exp(b, l + 1)) && r;
    r = p && report_error_(b, consumeToken(b, COMMA)) && r;
    r = p && report_error_(b, exp(b, l + 1)) && r;
    r = p && consumeToken(b, PARENS_CLOSE) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // ('[' type_annotation ']')?
  private static boolean fold_exp_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fold_exp_1")) return false;
    fold_exp_1_0(b, l + 1);
    return true;
  }

  // '[' type_annotation ']'
  private static boolean fold_exp_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fold_exp_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, SQUARE_BRACKET_OPEN);
    r = r && type_annotation(b, l + 1);
    r = r && consumeToken(b, SQUARE_BRACKET_CLOSE);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // annotation* visibility? 'def' id type_variables? ('(' param_list ')')? ':' type_annotation '=' exp
  public static boolean fun_def(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_def")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, FUN_DEF, "<fun def>");
    r = fun_def_0(b, l + 1);
    r = r && fun_def_1(b, l + 1);
    r = r && consumeTokens(b, 1, KEYWORD_DEF, ID);
    p = r; // pin = 3
    r = r && report_error_(b, fun_def_4(b, l + 1));
    r = p && report_error_(b, fun_def_5(b, l + 1)) && r;
    r = p && report_error_(b, consumeToken(b, COLON)) && r;
    r = p && report_error_(b, type_annotation(b, l + 1)) && r;
    r = p && report_error_(b, consumeToken(b, EQUAL_SIGN)) && r;
    r = p && exp(b, l + 1) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // annotation*
  private static boolean fun_def_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_def_0")) return false;
    while (true) {
      int c = current_position_(b);
      if (!annotation(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "fun_def_0", c)) break;
    }
    return true;
  }

  // visibility?
  private static boolean fun_def_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_def_1")) return false;
    visibility(b, l + 1);
    return true;
  }

  // type_variables?
  private static boolean fun_def_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_def_4")) return false;
    type_variables(b, l + 1);
    return true;
  }

  // ('(' param_list ')')?
  private static boolean fun_def_5(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_def_5")) return false;
    fun_def_5_0(b, l + 1);
    return true;
  }

  // '(' param_list ')'
  private static boolean fun_def_5_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_def_5_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PARENS_OPEN);
    r = r && param_list(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // atomic_type '=>' type_annotation
  public static boolean fun_type(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_type")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, FUN_TYPE, "<fun type>");
    r = atomic_type(b, l + 1);
    r = r && consumeToken(b, ARROW);
    p = r; // pin = 2
    r = r && type_annotation(b, l + 1);
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // 'if' '(' exp ')' exp 'else' exp
  public static boolean if_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "if_exp")) return false;
    if (!nextTokenIs(b, KEYWORD_IF)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, IF_EXP, null);
    r = consumeTokens(b, 1, KEYWORD_IF, PARENS_OPEN);
    p = r; // pin = 1
    r = r && report_error_(b, exp(b, l + 1));
    r = p && report_error_(b, consumeToken(b, PARENS_CLOSE)) && r;
    r = p && report_error_(b, exp(b, l + 1)) && r;
    r = p && report_error_(b, consumeToken(b, KEYWORD_ELSE)) && r;
    r = p && exp(b, l + 1) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // 'import' id
  public static boolean import_$(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "import_$")) return false;
    if (!nextTokenIs(b, KEYWORD_IMPORT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, IMPORT, null);
    r = consumeTokens(b, 1, KEYWORD_IMPORT, ID);
    p = r; // pin = 1
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // cast_exp | base_apply_method_exp | base_apply_infix_exp | match_exp | subinfix_exp
  public static boolean infix_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "infix_exp")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, INFIX_EXP, "<infix exp>");
    r = cast_exp(b, l + 1);
    if (!r) r = base_apply_method_exp(b, l + 1);
    if (!r) r = base_apply_infix_exp(b, l + 1);
    if (!r) r = match_exp(b, l + 1);
    if (!r) r = subinfix_exp(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // integer
  public static boolean int_lit(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "int_lit")) return false;
    if (!nextTokenIs(b, INTEGER)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, INTEGER);
    exit_section_(b, m, INT_LIT, r);
    return r;
  }

  /* ********************************************************** */
  // '(' param_list ')' '=>' exp
  public static boolean lambda_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lambda_exp")) return false;
    if (!nextTokenIs(b, PARENS_OPEN)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, LAMBDA_EXP, null);
    r = consumeToken(b, PARENS_OPEN);
    r = r && param_list(b, l + 1);
    r = r && consumeTokens(b, 2, PARENS_CLOSE, ARROW);
    p = r; // pin = 4
    r = r && exp(b, l + 1);
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // 'let' (single_let | multiple_let)
  public static boolean let_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "let_exp")) return false;
    if (!nextTokenIs(b, KEYWORD_LET)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, LET_EXP, null);
    r = consumeToken(b, KEYWORD_LET);
    p = r; // pin = 1
    r = r && let_exp_1(b, l + 1);
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // single_let | multiple_let
  private static boolean let_exp_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "let_exp_1")) return false;
    boolean r;
    r = single_let(b, l + 1);
    if (!r) r = multiple_let(b, l + 1);
    return r;
  }

  /* ********************************************************** */
  // long
  public static boolean long_lit(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "long_lit")) return false;
    if (!nextTokenIs(b, LONG)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LONG);
    exit_section_(b, m, LONG_LIT, r);
    return r;
  }

  /* ********************************************************** */
  // 'case' pattern '=>' exp
  public static boolean match_case(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "match_case")) return false;
    if (!nextTokenIs(b, KEYWORD_CASE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, MATCH_CASE, null);
    r = consumeToken(b, KEYWORD_CASE);
    p = r; // pin = 1
    r = r && report_error_(b, pattern(b, l + 1));
    r = p && report_error_(b, consumeToken(b, ARROW)) && r;
    r = p && exp(b, l + 1) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // subinfix_exp 'match' '{' match_case* '}'
  public static boolean match_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "match_exp")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, MATCH_EXP, "<match exp>");
    r = subinfix_exp(b, l + 1);
    r = r && consumeTokens(b, 1, KEYWORD_MATCH, BRACES_OPEN);
    p = r; // pin = 2
    r = r && report_error_(b, match_exp_3(b, l + 1));
    r = p && consumeToken(b, BRACES_CLOSE) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // match_case*
  private static boolean match_exp_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "match_exp_3")) return false;
    while (true) {
      int c = current_position_(b);
      if (!match_case(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "match_exp_3", c)) break;
    }
    return true;
  }

  /* ********************************************************** */
  // atomic_exp 'not'? 'in' infix_exp
  public static boolean member_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "member_exp")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, MEMBER_EXP, "<member exp>");
    r = atomic_exp(b, l + 1);
    r = r && member_exp_1(b, l + 1);
    r = r && consumeToken(b, KEYWORD_IN);
    p = r; // pin = 3
    r = r && infix_exp(b, l + 1);
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // 'not'?
  private static boolean member_exp_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "member_exp_1")) return false;
    consumeToken(b, KEYWORD_NOT);
    return true;
  }

  /* ********************************************************** */
  // 'module' id import* module_content*
  static boolean module(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "module")) return false;
    if (!nextTokenIs(b, KEYWORD_MODULE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_);
    r = consumeTokens(b, 1, KEYWORD_MODULE, ID);
    p = r; // pin = 1
    r = r && report_error_(b, module_2(b, l + 1));
    r = p && module_3(b, l + 1) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // import*
  private static boolean module_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "module_2")) return false;
    while (true) {
      int c = current_position_(b);
      if (!import_$(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "module_2", c)) break;
    }
    return true;
  }

  // module_content*
  private static boolean module_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "module_3")) return false;
    while (true) {
      int c = current_position_(b);
      if (!module_content(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "module_3", c)) break;
    }
    return true;
  }

  /* ********************************************************** */
  // fun_def | data_def
  static boolean module_content(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "module_content")) return false;
    boolean r;
    r = fun_def(b, l + 1);
    if (!r) r = data_def(b, l + 1);
    return r;
  }

  /* ********************************************************** */
  // '(' var_id (':' type_annotation)? (',' var_id (':' type_annotation)?)+ ')' '=' infix_exp 'in' exp
  public static boolean multiple_let(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "multiple_let")) return false;
    if (!nextTokenIs(b, PARENS_OPEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PARENS_OPEN);
    r = r && var_id(b, l + 1);
    r = r && multiple_let_2(b, l + 1);
    r = r && multiple_let_3(b, l + 1);
    r = r && consumeTokens(b, 0, PARENS_CLOSE, EQUAL_SIGN);
    r = r && infix_exp(b, l + 1);
    r = r && consumeToken(b, KEYWORD_IN);
    r = r && exp(b, l + 1);
    exit_section_(b, m, MULTIPLE_LET, r);
    return r;
  }

  // (':' type_annotation)?
  private static boolean multiple_let_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "multiple_let_2")) return false;
    multiple_let_2_0(b, l + 1);
    return true;
  }

  // ':' type_annotation
  private static boolean multiple_let_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "multiple_let_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COLON);
    r = r && type_annotation(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' var_id (':' type_annotation)?)+
  private static boolean multiple_let_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "multiple_let_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = multiple_let_3_0(b, l + 1);
    while (r) {
      int c = current_position_(b);
      if (!multiple_let_3_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "multiple_let_3", c)) break;
    }
    exit_section_(b, m, null, r);
    return r;
  }

  // ',' var_id (':' type_annotation)?
  private static boolean multiple_let_3_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "multiple_let_3_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && var_id(b, l + 1);
    r = r && multiple_let_3_0_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (':' type_annotation)?
  private static boolean multiple_let_3_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "multiple_let_3_0_2")) return false;
    multiple_let_3_0_2_0(b, l + 1);
    return true;
  }

  // ':' type_annotation
  private static boolean multiple_let_3_0_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "multiple_let_3_0_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COLON);
    r = r && type_annotation(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '+' | '-' | '*' | '/' | '%' // arithmetic
  //         | '&&' | '||' | '<' | '>' | '==' | '!=' | '<=' | '>=' // logic
  //         | '++' | '&'
  public static boolean op(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "op")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, OP, "<op>");
    r = consumeToken(b, PLUS);
    if (!r) r = consumeToken(b, MINUS);
    if (!r) r = consumeToken(b, STAR);
    if (!r) r = consumeToken(b, SLASH);
    if (!r) r = consumeToken(b, MODULO);
    if (!r) r = consumeToken(b, AND);
    if (!r) r = consumeToken(b, OR);
    if (!r) r = consumeToken(b, LT);
    if (!r) r = consumeToken(b, GT);
    if (!r) r = consumeToken(b, EQUIVALENCE);
    if (!r) r = consumeToken(b, NON_EQUIVALENCE);
    if (!r) r = consumeToken(b, LEQ);
    if (!r) r = consumeToken(b, GEQ);
    if (!r) r = consumeToken(b, SET_UNION);
    if (!r) r = consumeToken(b, SET_INTERSECTION);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // 'None' | 'Some' '(' (exp (',' exp)*)? ')'
  public static boolean option_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "option_exp")) return false;
    if (!nextTokenIs(b, "<option exp>", KEYWORD_NONE, KEYWORD_SOME)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, OPTION_EXP, "<option exp>");
    r = consumeToken(b, KEYWORD_NONE);
    if (!r) r = option_exp_1(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // 'Some' '(' (exp (',' exp)*)? ')'
  private static boolean option_exp_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "option_exp_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, KEYWORD_SOME, PARENS_OPEN);
    r = r && option_exp_1_2(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, null, r);
    return r;
  }

  // (exp (',' exp)*)?
  private static boolean option_exp_1_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "option_exp_1_2")) return false;
    option_exp_1_2_0(b, l + 1);
    return true;
  }

  // exp (',' exp)*
  private static boolean option_exp_1_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "option_exp_1_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = exp(b, l + 1);
    r = r && option_exp_1_2_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' exp)*
  private static boolean option_exp_1_2_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "option_exp_1_2_0_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!option_exp_1_2_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "option_exp_1_2_0_1", c)) break;
    }
    return true;
  }

  // ',' exp
  private static boolean option_exp_1_2_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "option_exp_1_2_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // 'None' | 'Some' '(' (id (',' id)*)? ')'
  public static boolean option_pattern(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "option_pattern")) return false;
    if (!nextTokenIs(b, "<option pattern>", KEYWORD_NONE, KEYWORD_SOME)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, OPTION_PATTERN, "<option pattern>");
    r = consumeToken(b, KEYWORD_NONE);
    if (!r) r = option_pattern_1(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // 'Some' '(' (id (',' id)*)? ')'
  private static boolean option_pattern_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "option_pattern_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, KEYWORD_SOME, PARENS_OPEN);
    r = r && option_pattern_1_2(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, null, r);
    return r;
  }

  // (id (',' id)*)?
  private static boolean option_pattern_1_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "option_pattern_1_2")) return false;
    option_pattern_1_2_0(b, l + 1);
    return true;
  }

  // id (',' id)*
  private static boolean option_pattern_1_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "option_pattern_1_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ID);
    r = r && option_pattern_1_2_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' id)*
  private static boolean option_pattern_1_2_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "option_pattern_1_2_0_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!option_pattern_1_2_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "option_pattern_1_2_0_1", c)) break;
    }
    return true;
  }

  // ',' id
  private static boolean option_pattern_1_2_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "option_pattern_1_2_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, COMMA, ID);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // id ':' type_annotation
  public static boolean param(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "param")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, ID, COLON);
    r = r && type_annotation(b, l + 1);
    exit_section_(b, m, PARAM, r);
    return r;
  }

  /* ********************************************************** */
  // (param (',' param)*)?
  public static boolean param_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "param_list")) return false;
    Marker m = enter_section_(b, l, _NONE_, PARAM_LIST, "<param list>");
    param_list_0(b, l + 1);
    exit_section_(b, l, m, true, false, null);
    return true;
  }

  // param (',' param)*
  private static boolean param_list_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "param_list_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = param(b, l + 1);
    r = r && param_list_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' param)*
  private static boolean param_list_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "param_list_0_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!param_list_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "param_list_0_1", c)) break;
    }
    return true;
  }

  // ',' param
  private static boolean param_list_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "param_list_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && param(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '(' exp ')'
  public static boolean parens_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parens_exp")) return false;
    if (!nextTokenIs(b, PARENS_OPEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PARENS_OPEN);
    r = r && exp(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, PARENS_EXP, r);
    return r;
  }

  /* ********************************************************** */
  // option_pattern | constructor_pattern
  public static boolean pattern(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pattern")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, PATTERN, "<pattern>");
    r = option_pattern(b, l + 1);
    if (!r) r = constructor_pattern(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // type_int | type_long | type_double | type_boolean | type_string
  public static boolean primitive_type(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "primitive_type")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, PRIMITIVE_TYPE, "<primitive type>");
    r = consumeToken(b, TYPE_INT);
    if (!r) r = consumeToken(b, TYPE_LONG);
    if (!r) r = consumeToken(b, TYPE_DOUBLE);
    if (!r) r = consumeToken(b, TYPE_BOOLEAN);
    if (!r) r = consumeToken(b, TYPE_STRING);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // 'Set' '[' type_annotation ']'
  public static boolean set(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "set")) return false;
    if (!nextTokenIs(b, KEYWORD_SET)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, SET, null);
    r = consumeTokens(b, 1, KEYWORD_SET, SQUARE_BRACKET_OPEN);
    p = r; // pin = 1
    r = r && report_error_(b, type_annotation(b, l + 1));
    r = p && consumeToken(b, SQUARE_BRACKET_CLOSE) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // var_id (':' type_annotation)? '=' infix_exp 'in' exp
  public static boolean single_let(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "single_let")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = var_id(b, l + 1);
    r = r && single_let_1(b, l + 1);
    r = r && consumeToken(b, EQUAL_SIGN);
    r = r && infix_exp(b, l + 1);
    r = r && consumeToken(b, KEYWORD_IN);
    r = r && exp(b, l + 1);
    exit_section_(b, m, SINGLE_LET, r);
    return r;
  }

  // (':' type_annotation)?
  private static boolean single_let_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "single_let_1")) return false;
    single_let_1_0(b, l + 1);
    return true;
  }

  // ':' type_annotation
  private static boolean single_let_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "single_let_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COLON);
    r = r && type_annotation(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // string
  public static boolean string_lit(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "string_lit")) return false;
    if (!nextTokenIs(b, STRING)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, STRING);
    exit_section_(b, m, STRING_LIT, r);
    return r;
  }

  /* ********************************************************** */
  // call_exp | lambda_exp | atomic_exp
  public static boolean subinfix_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "subinfix_exp")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, SUBINFIX_EXP, "<subinfix exp>");
    r = call_exp(b, l + 1);
    if (!r) r = lambda_exp(b, l + 1);
    if (!r) r = atomic_exp(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // '(' (type_annotation (',' type_annotation)*)? ')'
  public static boolean tuple(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tuple")) return false;
    if (!nextTokenIs(b, PARENS_OPEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PARENS_OPEN);
    r = r && tuple_1(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, TUPLE, r);
    return r;
  }

  // (type_annotation (',' type_annotation)*)?
  private static boolean tuple_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tuple_1")) return false;
    tuple_1_0(b, l + 1);
    return true;
  }

  // type_annotation (',' type_annotation)*
  private static boolean tuple_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tuple_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = type_annotation(b, l + 1);
    r = r && tuple_1_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' type_annotation)*
  private static boolean tuple_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tuple_1_0_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!tuple_1_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "tuple_1_0_1", c)) break;
    }
    return true;
  }

  // ',' type_annotation
  private static boolean tuple_1_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tuple_1_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && type_annotation(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '(' exp (',' exp)+ ')'
  public static boolean tuple_exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tuple_exp")) return false;
    if (!nextTokenIs(b, PARENS_OPEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PARENS_OPEN);
    r = r && exp(b, l + 1);
    r = r && tuple_exp_2(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, TUPLE_EXP, r);
    return r;
  }

  // (',' exp)+
  private static boolean tuple_exp_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tuple_exp_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = tuple_exp_2_0(b, l + 1);
    while (r) {
      int c = current_position_(b);
      if (!tuple_exp_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "tuple_exp_2", c)) break;
    }
    exit_section_(b, m, null, r);
    return r;
  }

  // ',' exp
  private static boolean tuple_exp_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tuple_exp_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // fun_type | atomic_type
  public static boolean type_annotation(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_annotation")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, TYPE_ANNOTATION, "<type annotation>");
    r = fun_type(b, l + 1);
    if (!r) r = atomic_type(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // id
  public static boolean type_name(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_name")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ID);
    exit_section_(b, m, TYPE_NAME, r);
    return r;
  }

  /* ********************************************************** */
  // id
  public static boolean type_variable(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_variable")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ID);
    exit_section_(b, m, TYPE_VARIABLE, r);
    return r;
  }

  /* ********************************************************** */
  // '[' type_variable (',' type_variable)* ']'
  public static boolean type_variables(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_variables")) return false;
    if (!nextTokenIs(b, SQUARE_BRACKET_OPEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, SQUARE_BRACKET_OPEN);
    r = r && type_variable(b, l + 1);
    r = r && type_variables_2(b, l + 1);
    r = r && consumeToken(b, SQUARE_BRACKET_CLOSE);
    exit_section_(b, m, TYPE_VARIABLES, r);
    return r;
  }

  // (',' type_variable)*
  private static boolean type_variables_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_variables_2")) return false;
    while (true) {
      int c = current_position_(b);
      if (!type_variables_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "type_variables_2", c)) break;
    }
    return true;
  }

  // ',' type_variable
  private static boolean type_variables_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_variables_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && type_variable(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '-' | '!'
  public static boolean unary_op(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "unary_op")) return false;
    if (!nextTokenIs(b, "<unary op>", MINUS, NEGATION)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, UNARY_OP, "<unary op>");
    r = consumeToken(b, MINUS);
    if (!r) r = consumeToken(b, NEGATION);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // id
  public static boolean var(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "var")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ID);
    exit_section_(b, m, VAR, r);
    return r;
  }

  /* ********************************************************** */
  // id
  public static boolean var_id(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "var_id")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ID);
    exit_section_(b, m, VAR_ID, r);
    return r;
  }

  /* ********************************************************** */
  // 'private'
  public static boolean visibility(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "visibility")) return false;
    if (!nextTokenIs(b, VISIBILITY_PRIVATE)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, VISIBILITY_PRIVATE);
    exit_section_(b, m, VISIBILITY, r);
    return r;
  }

}
