// This is a generated file. Not intended for manual editing.
package language.parser;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilder.Marker;
import static language.psi.FunIncATypes.*;
import static language.parser.FunIncAParserUtil.*;
import com.intellij.psi.tree.IElementType;
import com.intellij.lang.ASTNode;
import com.intellij.psi.tree.TokenSet;
import com.intellij.lang.PsiParser;
import com.intellij.lang.LightPsiParser;

@SuppressWarnings({"SimplifiableIfStatement", "UnusedAssignment"})
public class FunIncAParser implements PsiParser, LightPsiParser {

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
    return Module(b, l + 1);
  }

  public static final TokenSet[] EXTENDS_SETS_ = new TokenSet[] {
    create_token_set_(ATOMIC_EXP, BASE_APPLY_EXP, BASE_APPLY_INFIX_EXP, BASE_APPLY_METHOD_EXP,
      BASE_APPLY_UNARY_EXP, CALL_EXP, CAST_EXP, CONST_SET_EXP,
      EXP, FOLD_EXP, IF_EXP, INFIX_EXP,
      LAMBDA_EXP, LET_EXP, LITERAL_EXP, MATCH_EXP,
      PARENTHESIS_EXP, SET_COMPREHENSION_EXP, SET_MEMBER_EXP, SUBINFIX_EXP,
      TUPLE_EXP, VAR_REF_EXP),
  };

  /* ********************************************************** */
  // '@main'
  public static boolean Annotation(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "Annotation")) return false;
    if (!nextTokenIs(b, ANNOTATION_MAIN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ANNOTATION_MAIN);
    exit_section_(b, m, ANNOTATION, r);
    return r;
  }

  /* ********************************************************** */
  // ParenthesisExp | SetComprehensionExp | ConstSetExp | TupleExp | FoldExp
  //                | BaseApplyExp | LiteralExp | VarRefExp | BaseApplyUnaryExp
  public static boolean AtomicExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "AtomicExp")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, ATOMIC_EXP, "<atomic exp>");
    r = ParenthesisExp(b, l + 1);
    if (!r) r = SetComprehensionExp(b, l + 1);
    if (!r) r = ConstSetExp(b, l + 1);
    if (!r) r = TupleExp(b, l + 1);
    if (!r) r = FoldExp(b, l + 1);
    if (!r) r = BaseApplyExp(b, l + 1);
    if (!r) r = LiteralExp(b, l + 1);
    if (!r) r = VarRefExp(b, l + 1);
    if (!r) r = BaseApplyUnaryExp(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // TupleType | 'Any' | 'Nothing' | 'Unit' | PrimitiveType | SetType | ConstructorType | ScalaType | TypeNameRef
  public static boolean AtomicType(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "AtomicType")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, ATOMIC_TYPE, "<atomic type>");
    r = TupleType(b, l + 1);
    if (!r) r = consumeToken(b, TYPE_ANY);
    if (!r) r = consumeToken(b, TYPE_NOTHING);
    if (!r) r = consumeToken(b, TYPE_UNIT);
    if (!r) r = PrimitiveType(b, l + 1);
    if (!r) r = SetType(b, l + 1);
    if (!r) r = ConstructorType(b, l + 1);
    if (!r) r = ScalaType(b, l + 1);
    if (!r) r = TypeNameRef(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // scalaterm '(' (Exp (',' Exp)*)? ')'
  public static boolean BaseApplyExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "BaseApplyExp")) return false;
    if (!nextTokenIs(b, SCALATERM)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, SCALATERM, PARENS_OPEN);
    r = r && BaseApplyExp_2(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, BASE_APPLY_EXP, r);
    return r;
  }

  // (Exp (',' Exp)*)?
  private static boolean BaseApplyExp_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "BaseApplyExp_2")) return false;
    BaseApplyExp_2_0(b, l + 1);
    return true;
  }

  // Exp (',' Exp)*
  private static boolean BaseApplyExp_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "BaseApplyExp_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = Exp(b, l + 1);
    r = r && BaseApplyExp_2_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' Exp)*
  private static boolean BaseApplyExp_2_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "BaseApplyExp_2_0_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!BaseApplyExp_2_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "BaseApplyExp_2_0_1", c)) break;
    }
    return true;
  }

  // ',' Exp
  private static boolean BaseApplyExp_2_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "BaseApplyExp_2_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && Exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // SubinfixExp BinaryOp InfixExp
  public static boolean BaseApplyInfixExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "BaseApplyInfixExp")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, BASE_APPLY_INFIX_EXP, "<base apply infix exp>");
    r = SubinfixExp(b, l + 1);
    r = r && BinaryOp(b, l + 1);
    p = r; // pin = BinaryOp
    r = r && InfixExp(b, l + 1);
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // SubinfixExp '.' scalaterm ( '(' (InfixExp (',' InfixExp)*)? ')' )?
  public static boolean BaseApplyMethodExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "BaseApplyMethodExp")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, BASE_APPLY_METHOD_EXP, "<base apply method exp>");
    r = SubinfixExp(b, l + 1);
    r = r && consumeTokens(b, 2, DOT, SCALATERM);
    p = r; // pin = 3
    r = r && BaseApplyMethodExp_3(b, l + 1);
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // ( '(' (InfixExp (',' InfixExp)*)? ')' )?
  private static boolean BaseApplyMethodExp_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "BaseApplyMethodExp_3")) return false;
    BaseApplyMethodExp_3_0(b, l + 1);
    return true;
  }

  // '(' (InfixExp (',' InfixExp)*)? ')'
  private static boolean BaseApplyMethodExp_3_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "BaseApplyMethodExp_3_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PARENS_OPEN);
    r = r && BaseApplyMethodExp_3_0_1(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, null, r);
    return r;
  }

  // (InfixExp (',' InfixExp)*)?
  private static boolean BaseApplyMethodExp_3_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "BaseApplyMethodExp_3_0_1")) return false;
    BaseApplyMethodExp_3_0_1_0(b, l + 1);
    return true;
  }

  // InfixExp (',' InfixExp)*
  private static boolean BaseApplyMethodExp_3_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "BaseApplyMethodExp_3_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = InfixExp(b, l + 1);
    r = r && BaseApplyMethodExp_3_0_1_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' InfixExp)*
  private static boolean BaseApplyMethodExp_3_0_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "BaseApplyMethodExp_3_0_1_0_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!BaseApplyMethodExp_3_0_1_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "BaseApplyMethodExp_3_0_1_0_1", c)) break;
    }
    return true;
  }

  // ',' InfixExp
  private static boolean BaseApplyMethodExp_3_0_1_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "BaseApplyMethodExp_3_0_1_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && InfixExp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // UnaryOp InfixExp
  public static boolean BaseApplyUnaryExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "BaseApplyUnaryExp")) return false;
    if (!nextTokenIs(b, "<base apply unary exp>", MINUS, NEGATION)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, BASE_APPLY_UNARY_EXP, "<base apply unary exp>");
    r = UnaryOp(b, l + 1);
    p = r; // pin = 1
    r = r && InfixExp(b, l + 1);
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '+' | '-' | '*' | '/' | '%' // arithmetic
  //         | '&&' | '||' | '<' | '>' | '==' | '!=' | '<=' | '>=' // logic
  //         | '++' | '&'
  public static boolean BinaryOp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "BinaryOp")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, BINARY_OP, "<binary op>");
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
  // 'true' | 'false'
  public static boolean BooleanLit(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "BooleanLit")) return false;
    if (!nextTokenIs(b, "<boolean lit>", BOOLEAN_FALSE, BOOLEAN_TRUE)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, BOOLEAN_LIT, "<boolean lit>");
    r = consumeToken(b, BOOLEAN_TRUE);
    if (!r) r = consumeToken(b, BOOLEAN_FALSE);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // AtomicExp ('[' TypeList ']')? ('(' CallExpList ')')+
  public static boolean CallExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "CallExp")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, CALL_EXP, "<call exp>");
    r = AtomicExp(b, l + 1);
    r = r && CallExp_1(b, l + 1);
    r = r && CallExp_2(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // ('[' TypeList ']')?
  private static boolean CallExp_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "CallExp_1")) return false;
    CallExp_1_0(b, l + 1);
    return true;
  }

  // '[' TypeList ']'
  private static boolean CallExp_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "CallExp_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, SQUARE_BRACKET_OPEN);
    r = r && TypeList(b, l + 1);
    r = r && consumeToken(b, SQUARE_BRACKET_CLOSE);
    exit_section_(b, m, null, r);
    return r;
  }

  // ('(' CallExpList ')')+
  private static boolean CallExp_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "CallExp_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = CallExp_2_0(b, l + 1);
    while (r) {
      int c = current_position_(b);
      if (!CallExp_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "CallExp_2", c)) break;
    }
    exit_section_(b, m, null, r);
    return r;
  }

  // '(' CallExpList ')'
  private static boolean CallExp_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "CallExp_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PARENS_OPEN);
    r = r && CallExpList(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // (Exp (',' Exp)*)?
  public static boolean CallExpList(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "CallExpList")) return false;
    Marker m = enter_section_(b, l, _NONE_, CALL_EXP_LIST, "<call exp list>");
    CallExpList_0(b, l + 1);
    exit_section_(b, l, m, true, false, null);
    return true;
  }

  // Exp (',' Exp)*
  private static boolean CallExpList_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "CallExpList_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = Exp(b, l + 1);
    r = r && CallExpList_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' Exp)*
  private static boolean CallExpList_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "CallExpList_0_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!CallExpList_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "CallExpList_0_1", c)) break;
    }
    return true;
  }

  // ',' Exp
  private static boolean CallExpList_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "CallExpList_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && Exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // SubinfixExp '.' 'as' '[' TypeNameRef ']'
  public static boolean CastExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "CastExp")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, CAST_EXP, "<cast exp>");
    r = SubinfixExp(b, l + 1);
    r = r && consumeTokens(b, 2, DOT, CAST, SQUARE_BRACKET_OPEN);
    p = r; // pin = 3
    r = r && report_error_(b, TypeNameRef(b, l + 1));
    r = p && consumeToken(b, SQUARE_BRACKET_CLOSE) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '{' (Exp (',' Exp)*)? '}'
  public static boolean ConstSetExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ConstSetExp")) return false;
    if (!nextTokenIs(b, BRACES_OPEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, BRACES_OPEN);
    r = r && ConstSetExp_1(b, l + 1);
    r = r && consumeToken(b, BRACES_CLOSE);
    exit_section_(b, m, CONST_SET_EXP, r);
    return r;
  }

  // (Exp (',' Exp)*)?
  private static boolean ConstSetExp_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ConstSetExp_1")) return false;
    ConstSetExp_1_0(b, l + 1);
    return true;
  }

  // Exp (',' Exp)*
  private static boolean ConstSetExp_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ConstSetExp_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = Exp(b, l + 1);
    r = r && ConstSetExp_1_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' Exp)*
  private static boolean ConstSetExp_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ConstSetExp_1_0_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!ConstSetExp_1_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "ConstSetExp_1_0_1", c)) break;
    }
    return true;
  }

  // ',' Exp
  private static boolean ConstSetExp_1_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ConstSetExp_1_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && Exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // ConstructorRef '(' (PatternVarDef (',' PatternVarDef)*)? ')'
  public static boolean ConstructorPat(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ConstructorPat")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = ConstructorRef(b, l + 1);
    r = r && consumeToken(b, PARENS_OPEN);
    r = r && ConstructorPat_2(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, CONSTRUCTOR_PAT, r);
    return r;
  }

  // (PatternVarDef (',' PatternVarDef)*)?
  private static boolean ConstructorPat_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ConstructorPat_2")) return false;
    ConstructorPat_2_0(b, l + 1);
    return true;
  }

  // PatternVarDef (',' PatternVarDef)*
  private static boolean ConstructorPat_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ConstructorPat_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = PatternVarDef(b, l + 1);
    r = r && ConstructorPat_2_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' PatternVarDef)*
  private static boolean ConstructorPat_2_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ConstructorPat_2_0_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!ConstructorPat_2_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "ConstructorPat_2_0_1", c)) break;
    }
    return true;
  }

  // ',' PatternVarDef
  private static boolean ConstructorPat_2_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ConstructorPat_2_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && PatternVarDef(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // id
  public static boolean ConstructorRef(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ConstructorRef")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ID);
    exit_section_(b, m, CONSTRUCTOR_REF, r);
    return r;
  }

  /* ********************************************************** */
  // TypeNameRef '[' TypeList ']'
  public static boolean ConstructorType(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ConstructorType")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = TypeNameRef(b, l + 1);
    r = r && consumeToken(b, SQUARE_BRACKET_OPEN);
    r = r && TypeList(b, l + 1);
    r = r && consumeToken(b, SQUARE_BRACKET_CLOSE);
    exit_section_(b, m, CONSTRUCTOR_TYPE, r);
    return r;
  }

  /* ********************************************************** */
  // id '(' (Type (',' Type)*)? ')'
  public static boolean DataConstructorDef(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "DataConstructorDef")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, ID, PARENS_OPEN);
    r = r && DataConstructorDef_2(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, DATA_CONSTRUCTOR_DEF, r);
    return r;
  }

  // (Type (',' Type)*)?
  private static boolean DataConstructorDef_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "DataConstructorDef_2")) return false;
    DataConstructorDef_2_0(b, l + 1);
    return true;
  }

  // Type (',' Type)*
  private static boolean DataConstructorDef_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "DataConstructorDef_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = Type(b, l + 1);
    r = r && DataConstructorDef_2_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' Type)*
  private static boolean DataConstructorDef_2_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "DataConstructorDef_2_0_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!DataConstructorDef_2_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "DataConstructorDef_2_0_1", c)) break;
    }
    return true;
  }

  // ',' Type
  private static boolean DataConstructorDef_2_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "DataConstructorDef_2_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && Type(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // Annotation* Visibility? 'data' id TypeVarDefList? '=' DataConstructorDef ('|' DataConstructorDef)*
  public static boolean DataDef(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "DataDef")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, DATA_DEF, "<data def>");
    r = DataDef_0(b, l + 1);
    r = r && DataDef_1(b, l + 1);
    r = r && consumeTokens(b, 1, KEYWORD_DATA, ID);
    p = r; // pin = 3
    r = r && report_error_(b, DataDef_4(b, l + 1));
    r = p && report_error_(b, consumeToken(b, EQUAL_SIGN)) && r;
    r = p && report_error_(b, DataConstructorDef(b, l + 1)) && r;
    r = p && DataDef_7(b, l + 1) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // Annotation*
  private static boolean DataDef_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "DataDef_0")) return false;
    while (true) {
      int c = current_position_(b);
      if (!Annotation(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "DataDef_0", c)) break;
    }
    return true;
  }

  // Visibility?
  private static boolean DataDef_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "DataDef_1")) return false;
    Visibility(b, l + 1);
    return true;
  }

  // TypeVarDefList?
  private static boolean DataDef_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "DataDef_4")) return false;
    TypeVarDefList(b, l + 1);
    return true;
  }

  // ('|' DataConstructorDef)*
  private static boolean DataDef_7(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "DataDef_7")) return false;
    while (true) {
      int c = current_position_(b);
      if (!DataDef_7_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "DataDef_7", c)) break;
    }
    return true;
  }

  // '|' DataConstructorDef
  private static boolean DataDef_7_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "DataDef_7_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, BAR);
    r = r && DataConstructorDef(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // double
  public static boolean DoubleLit(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "DoubleLit")) return false;
    if (!nextTokenIs(b, DOUBLE)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, DOUBLE);
    exit_section_(b, m, DOUBLE_LIT, r);
    return r;
  }

  /* ********************************************************** */
  // IfExp | LetExp | SetMemberExp | InfixExp
  public static boolean Exp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "Exp")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, EXP, "<exp>");
    r = IfExp(b, l + 1);
    if (!r) r = LetExp(b, l + 1);
    if (!r) r = SetMemberExp(b, l + 1);
    if (!r) r = InfixExp(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // 'fold' ('[' Type ']')? '(' Exp ',' Exp ',' Exp ')'
  public static boolean FoldExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "FoldExp")) return false;
    if (!nextTokenIs(b, KEYWORD_FOLD)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, FOLD_EXP, null);
    r = consumeToken(b, KEYWORD_FOLD);
    p = r; // pin = 1
    r = r && report_error_(b, FoldExp_1(b, l + 1));
    r = p && report_error_(b, consumeToken(b, PARENS_OPEN)) && r;
    r = p && report_error_(b, Exp(b, l + 1)) && r;
    r = p && report_error_(b, consumeToken(b, COMMA)) && r;
    r = p && report_error_(b, Exp(b, l + 1)) && r;
    r = p && report_error_(b, consumeToken(b, COMMA)) && r;
    r = p && report_error_(b, Exp(b, l + 1)) && r;
    r = p && consumeToken(b, PARENS_CLOSE) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // ('[' Type ']')?
  private static boolean FoldExp_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "FoldExp_1")) return false;
    FoldExp_1_0(b, l + 1);
    return true;
  }

  // '[' Type ']'
  private static boolean FoldExp_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "FoldExp_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, SQUARE_BRACKET_OPEN);
    r = r && Type(b, l + 1);
    r = r && consumeToken(b, SQUARE_BRACKET_CLOSE);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // Annotation* Visibility? 'def' id TypeVarDefList? ('(' ParamList ')')? ':' Type '=' Exp
  public static boolean FunDef(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "FunDef")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, FUN_DEF, "<fun def>");
    r = FunDef_0(b, l + 1);
    r = r && FunDef_1(b, l + 1);
    r = r && consumeTokens(b, 1, KEYWORD_DEF, ID);
    p = r; // pin = 3
    r = r && report_error_(b, FunDef_4(b, l + 1));
    r = p && report_error_(b, FunDef_5(b, l + 1)) && r;
    r = p && report_error_(b, consumeToken(b, COLON)) && r;
    r = p && report_error_(b, Type(b, l + 1)) && r;
    r = p && report_error_(b, consumeToken(b, EQUAL_SIGN)) && r;
    r = p && Exp(b, l + 1) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // Annotation*
  private static boolean FunDef_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "FunDef_0")) return false;
    while (true) {
      int c = current_position_(b);
      if (!Annotation(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "FunDef_0", c)) break;
    }
    return true;
  }

  // Visibility?
  private static boolean FunDef_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "FunDef_1")) return false;
    Visibility(b, l + 1);
    return true;
  }

  // TypeVarDefList?
  private static boolean FunDef_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "FunDef_4")) return false;
    TypeVarDefList(b, l + 1);
    return true;
  }

  // ('(' ParamList ')')?
  private static boolean FunDef_5(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "FunDef_5")) return false;
    FunDef_5_0(b, l + 1);
    return true;
  }

  // '(' ParamList ')'
  private static boolean FunDef_5_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "FunDef_5_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PARENS_OPEN);
    r = r && ParamList(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // AtomicType '=>' Type
  public static boolean FunType(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "FunType")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, FUN_TYPE, "<fun type>");
    r = AtomicType(b, l + 1);
    r = r && consumeToken(b, ARROW);
    p = r; // pin = 2
    r = r && Type(b, l + 1);
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // 'if' '(' Exp ')' Exp 'else' Exp
  public static boolean IfExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "IfExp")) return false;
    if (!nextTokenIs(b, KEYWORD_IF)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, IF_EXP, null);
    r = consumeTokens(b, 1, KEYWORD_IF, PARENS_OPEN);
    p = r; // pin = 1
    r = r && report_error_(b, Exp(b, l + 1));
    r = p && report_error_(b, consumeToken(b, PARENS_CLOSE)) && r;
    r = p && report_error_(b, Exp(b, l + 1)) && r;
    r = p && report_error_(b, consumeToken(b, KEYWORD_ELSE)) && r;
    r = p && Exp(b, l + 1) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // 'import' id
  public static boolean Import(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "Import")) return false;
    if (!nextTokenIs(b, KEYWORD_IMPORT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, IMPORT, null);
    r = consumeTokens(b, 1, KEYWORD_IMPORT, ID);
    p = r; // pin = 1
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // CastExp | BaseApplyMethodExp | BaseApplyInfixExp | MatchExp | SubinfixExp
  public static boolean InfixExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "InfixExp")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, INFIX_EXP, "<infix exp>");
    r = CastExp(b, l + 1);
    if (!r) r = BaseApplyMethodExp(b, l + 1);
    if (!r) r = BaseApplyInfixExp(b, l + 1);
    if (!r) r = MatchExp(b, l + 1);
    if (!r) r = SubinfixExp(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // integer
  public static boolean IntLit(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "IntLit")) return false;
    if (!nextTokenIs(b, INTEGER)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, INTEGER);
    exit_section_(b, m, INT_LIT, r);
    return r;
  }

  /* ********************************************************** */
  // '(' ParamList ')' '=>' Exp
  public static boolean LambdaExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "LambdaExp")) return false;
    if (!nextTokenIs(b, PARENS_OPEN)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, LAMBDA_EXP, null);
    r = consumeToken(b, PARENS_OPEN);
    r = r && ParamList(b, l + 1);
    r = r && consumeTokens(b, 2, PARENS_CLOSE, ARROW);
    p = r; // pin = 4
    r = r && Exp(b, l + 1);
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // 'let' (SingleBinding | MultipleBindings)
  public static boolean LetExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "LetExp")) return false;
    if (!nextTokenIs(b, KEYWORD_LET)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, LET_EXP, null);
    r = consumeToken(b, KEYWORD_LET);
    p = r; // pin = 1
    r = r && LetExp_1(b, l + 1);
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // SingleBinding | MultipleBindings
  private static boolean LetExp_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "LetExp_1")) return false;
    boolean r;
    r = SingleBinding(b, l + 1);
    if (!r) r = MultipleBindings(b, l + 1);
    return r;
  }

  /* ********************************************************** */
  // IntLit | LongLit | DoubleLit | BooleanLit | StringLit | ScalaLit
  public static boolean LiteralExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "LiteralExp")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, LITERAL_EXP, "<literal exp>");
    r = IntLit(b, l + 1);
    if (!r) r = LongLit(b, l + 1);
    if (!r) r = DoubleLit(b, l + 1);
    if (!r) r = BooleanLit(b, l + 1);
    if (!r) r = StringLit(b, l + 1);
    if (!r) r = ScalaLit(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // long
  public static boolean LongLit(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "LongLit")) return false;
    if (!nextTokenIs(b, LONG)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LONG);
    exit_section_(b, m, LONG_LIT, r);
    return r;
  }

  /* ********************************************************** */
  // 'case' Pat '=>' Exp
  public static boolean MatchCase(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "MatchCase")) return false;
    if (!nextTokenIs(b, KEYWORD_CASE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, MATCH_CASE, null);
    r = consumeToken(b, KEYWORD_CASE);
    p = r; // pin = 1
    r = r && report_error_(b, Pat(b, l + 1));
    r = p && report_error_(b, consumeToken(b, ARROW)) && r;
    r = p && Exp(b, l + 1) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // SubinfixExp 'match' '{' MatchCase* '}'
  public static boolean MatchExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "MatchExp")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, MATCH_EXP, "<match exp>");
    r = SubinfixExp(b, l + 1);
    r = r && consumeTokens(b, 1, KEYWORD_MATCH, BRACES_OPEN);
    p = r; // pin = 2
    r = r && report_error_(b, MatchExp_3(b, l + 1));
    r = p && consumeToken(b, BRACES_CLOSE) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // MatchCase*
  private static boolean MatchExp_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "MatchExp_3")) return false;
    while (true) {
      int c = current_position_(b);
      if (!MatchCase(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "MatchExp_3", c)) break;
    }
    return true;
  }

  /* ********************************************************** */
  // 'module' id Import* ModuleContent*
  static boolean Module(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "Module")) return false;
    if (!nextTokenIs(b, KEYWORD_MODULE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_);
    r = consumeTokens(b, 1, KEYWORD_MODULE, ID);
    p = r; // pin = 1
    r = r && report_error_(b, Module_2(b, l + 1));
    r = p && Module_3(b, l + 1) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // Import*
  private static boolean Module_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "Module_2")) return false;
    while (true) {
      int c = current_position_(b);
      if (!Import(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "Module_2", c)) break;
    }
    return true;
  }

  // ModuleContent*
  private static boolean Module_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "Module_3")) return false;
    while (true) {
      int c = current_position_(b);
      if (!ModuleContent(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "Module_3", c)) break;
    }
    return true;
  }

  /* ********************************************************** */
  // FunDef | DataDef
  static boolean ModuleContent(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ModuleContent")) return false;
    boolean r;
    r = FunDef(b, l + 1);
    if (!r) r = DataDef(b, l + 1);
    return r;
  }

  /* ********************************************************** */
  // '(' VarDef  (',' VarDef )+ ')' (':' Type)? '=' InfixExp 'in' Exp
  public static boolean MultipleBindings(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "MultipleBindings")) return false;
    if (!nextTokenIs(b, PARENS_OPEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PARENS_OPEN);
    r = r && VarDef(b, l + 1);
    r = r && MultipleBindings_2(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    r = r && MultipleBindings_4(b, l + 1);
    r = r && consumeToken(b, EQUAL_SIGN);
    r = r && InfixExp(b, l + 1);
    r = r && consumeToken(b, KEYWORD_IN);
    r = r && Exp(b, l + 1);
    exit_section_(b, m, MULTIPLE_BINDINGS, r);
    return r;
  }

  // (',' VarDef )+
  private static boolean MultipleBindings_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "MultipleBindings_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = MultipleBindings_2_0(b, l + 1);
    while (r) {
      int c = current_position_(b);
      if (!MultipleBindings_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "MultipleBindings_2", c)) break;
    }
    exit_section_(b, m, null, r);
    return r;
  }

  // ',' VarDef
  private static boolean MultipleBindings_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "MultipleBindings_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && VarDef(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (':' Type)?
  private static boolean MultipleBindings_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "MultipleBindings_4")) return false;
    MultipleBindings_4_0(b, l + 1);
    return true;
  }

  // ':' Type
  private static boolean MultipleBindings_4_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "MultipleBindings_4_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COLON);
    r = r && Type(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // id ':' Type
  public static boolean ParamDef(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ParamDef")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, ID, COLON);
    r = r && Type(b, l + 1);
    exit_section_(b, m, PARAM_DEF, r);
    return r;
  }

  /* ********************************************************** */
  // (ParamDef (',' ParamDef)*)?
  static boolean ParamList(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ParamList")) return false;
    ParamList_0(b, l + 1);
    return true;
  }

  // ParamDef (',' ParamDef)*
  private static boolean ParamList_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ParamList_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = ParamDef(b, l + 1);
    r = r && ParamList_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' ParamDef)*
  private static boolean ParamList_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ParamList_0_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!ParamList_0_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "ParamList_0_1", c)) break;
    }
    return true;
  }

  // ',' ParamDef
  private static boolean ParamList_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ParamList_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && ParamDef(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '(' Exp ')'
  public static boolean ParenthesisExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ParenthesisExp")) return false;
    if (!nextTokenIs(b, PARENS_OPEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PARENS_OPEN);
    r = r && Exp(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, PARENTHESIS_EXP, r);
    return r;
  }

  /* ********************************************************** */
  // ConstructorPat
  public static boolean Pat(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "Pat")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = ConstructorPat(b, l + 1);
    exit_section_(b, m, PAT, r);
    return r;
  }

  /* ********************************************************** */
  // id
  public static boolean PatternVarDef(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "PatternVarDef")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ID);
    exit_section_(b, m, PATTERN_VAR_DEF, r);
    return r;
  }

  /* ********************************************************** */
  // type_int | type_long | type_double | type_boolean | type_string
  public static boolean PrimitiveType(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "PrimitiveType")) return false;
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
  // scalaterm
  public static boolean ScalaLit(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ScalaLit")) return false;
    if (!nextTokenIs(b, SCALATERM)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, SCALATERM);
    exit_section_(b, m, SCALA_LIT, r);
    return r;
  }

  /* ********************************************************** */
  // scalaterm
  public static boolean ScalaType(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ScalaType")) return false;
    if (!nextTokenIs(b, SCALATERM)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, SCALATERM);
    exit_section_(b, m, SCALA_TYPE, r);
    return r;
  }

  /* ********************************************************** */
  // '{' SubinfixExp '|' Exp (',' Exp)* '}'
  public static boolean SetComprehensionExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "SetComprehensionExp")) return false;
    if (!nextTokenIs(b, BRACES_OPEN)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, SET_COMPREHENSION_EXP, null);
    r = consumeToken(b, BRACES_OPEN);
    r = r && SubinfixExp(b, l + 1);
    r = r && consumeToken(b, BAR);
    p = r; // pin = 3
    r = r && report_error_(b, Exp(b, l + 1));
    r = p && report_error_(b, SetComprehensionExp_4(b, l + 1)) && r;
    r = p && consumeToken(b, BRACES_CLOSE) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // (',' Exp)*
  private static boolean SetComprehensionExp_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "SetComprehensionExp_4")) return false;
    while (true) {
      int c = current_position_(b);
      if (!SetComprehensionExp_4_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "SetComprehensionExp_4", c)) break;
    }
    return true;
  }

  // ',' Exp
  private static boolean SetComprehensionExp_4_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "SetComprehensionExp_4_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && Exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // AtomicExp 'not'? 'in' InfixExp
  public static boolean SetMemberExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "SetMemberExp")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, SET_MEMBER_EXP, "<set member exp>");
    r = AtomicExp(b, l + 1);
    r = r && SetMemberExp_1(b, l + 1);
    r = r && consumeToken(b, KEYWORD_IN);
    p = r; // pin = 3
    r = r && InfixExp(b, l + 1);
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // 'not'?
  private static boolean SetMemberExp_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "SetMemberExp_1")) return false;
    consumeToken(b, KEYWORD_NOT);
    return true;
  }

  /* ********************************************************** */
  // 'Set' '[' Type ']'
  public static boolean SetType(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "SetType")) return false;
    if (!nextTokenIs(b, KEYWORD_SET)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, SET_TYPE, null);
    r = consumeTokens(b, 1, KEYWORD_SET, SQUARE_BRACKET_OPEN);
    p = r; // pin = 1
    r = r && report_error_(b, Type(b, l + 1));
    r = p && consumeToken(b, SQUARE_BRACKET_CLOSE) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // VarDef (':' Type)? '=' InfixExp 'in' Exp
  public static boolean SingleBinding(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "SingleBinding")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = VarDef(b, l + 1);
    r = r && SingleBinding_1(b, l + 1);
    r = r && consumeToken(b, EQUAL_SIGN);
    r = r && InfixExp(b, l + 1);
    r = r && consumeToken(b, KEYWORD_IN);
    r = r && Exp(b, l + 1);
    exit_section_(b, m, SINGLE_BINDING, r);
    return r;
  }

  // (':' Type)?
  private static boolean SingleBinding_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "SingleBinding_1")) return false;
    SingleBinding_1_0(b, l + 1);
    return true;
  }

  // ':' Type
  private static boolean SingleBinding_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "SingleBinding_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COLON);
    r = r && Type(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // string
  public static boolean StringLit(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "StringLit")) return false;
    if (!nextTokenIs(b, STRING)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, STRING);
    exit_section_(b, m, STRING_LIT, r);
    return r;
  }

  /* ********************************************************** */
  // CallExp | LambdaExp | AtomicExp
  public static boolean SubinfixExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "SubinfixExp")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, SUBINFIX_EXP, "<subinfix exp>");
    r = CallExp(b, l + 1);
    if (!r) r = LambdaExp(b, l + 1);
    if (!r) r = AtomicExp(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // '(' Exp (',' Exp)+ ')'
  public static boolean TupleExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "TupleExp")) return false;
    if (!nextTokenIs(b, PARENS_OPEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PARENS_OPEN);
    r = r && Exp(b, l + 1);
    r = r && TupleExp_2(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, TUPLE_EXP, r);
    return r;
  }

  // (',' Exp)+
  private static boolean TupleExp_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "TupleExp_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = TupleExp_2_0(b, l + 1);
    while (r) {
      int c = current_position_(b);
      if (!TupleExp_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "TupleExp_2", c)) break;
    }
    exit_section_(b, m, null, r);
    return r;
  }

  // ',' Exp
  private static boolean TupleExp_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "TupleExp_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && Exp(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '(' TypeList? ')'
  public static boolean TupleType(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "TupleType")) return false;
    if (!nextTokenIs(b, PARENS_OPEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, PARENS_OPEN);
    r = r && TupleType_1(b, l + 1);
    r = r && consumeToken(b, PARENS_CLOSE);
    exit_section_(b, m, TUPLE_TYPE, r);
    return r;
  }

  // TypeList?
  private static boolean TupleType_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "TupleType_1")) return false;
    TypeList(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // FunType | AtomicType
  public static boolean Type(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "Type")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, TYPE, "<type>");
    r = FunType(b, l + 1);
    if (!r) r = AtomicType(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // Type (',' Type)*
  static boolean TypeList(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "TypeList")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = Type(b, l + 1);
    r = r && TypeList_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' Type)*
  private static boolean TypeList_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "TypeList_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!TypeList_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "TypeList_1", c)) break;
    }
    return true;
  }

  // ',' Type
  private static boolean TypeList_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "TypeList_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && Type(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // id
  public static boolean TypeNameRef(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "TypeNameRef")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ID);
    exit_section_(b, m, TYPE_NAME_REF, r);
    return r;
  }

  /* ********************************************************** */
  // id
  public static boolean TypeVarDef(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "TypeVarDef")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ID);
    exit_section_(b, m, TYPE_VAR_DEF, r);
    return r;
  }

  /* ********************************************************** */
  // '[' TypeVarDef (',' TypeVarDef)* ']'
  static boolean TypeVarDefList(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "TypeVarDefList")) return false;
    if (!nextTokenIs(b, SQUARE_BRACKET_OPEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, SQUARE_BRACKET_OPEN);
    r = r && TypeVarDef(b, l + 1);
    r = r && TypeVarDefList_2(b, l + 1);
    r = r && consumeToken(b, SQUARE_BRACKET_CLOSE);
    exit_section_(b, m, null, r);
    return r;
  }

  // (',' TypeVarDef)*
  private static boolean TypeVarDefList_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "TypeVarDefList_2")) return false;
    while (true) {
      int c = current_position_(b);
      if (!TypeVarDefList_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "TypeVarDefList_2", c)) break;
    }
    return true;
  }

  // ',' TypeVarDef
  private static boolean TypeVarDefList_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "TypeVarDefList_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, COMMA);
    r = r && TypeVarDef(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '-' | '!'
  public static boolean UnaryOp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "UnaryOp")) return false;
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
  public static boolean VarDef(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "VarDef")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ID);
    exit_section_(b, m, VAR_DEF, r);
    return r;
  }

  /* ********************************************************** */
  // id
  public static boolean VarRefExp(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "VarRefExp")) return false;
    if (!nextTokenIs(b, ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ID);
    exit_section_(b, m, VAR_REF_EXP, r);
    return r;
  }

  /* ********************************************************** */
  // 'private'
  public static boolean Visibility(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "Visibility")) return false;
    if (!nextTokenIs(b, VISIBILITY_PRIVATE)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, VISIBILITY_PRIVATE);
    exit_section_(b, m, VISIBILITY, r);
    return r;
  }

}
