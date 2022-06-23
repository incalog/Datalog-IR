package language;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;

import static com.intellij.psi.TokenType.BAD_CHARACTER;
import static com.intellij.psi.TokenType.WHITE_SPACE;
import static language.psi.FuncIncaTypes.*;

%%

%{
  public _FuncIncaLexer() {
    this((java.io.Reader)null);
  }
%}

%public
%class _FuncIncaLexer
%implements FlexLexer
%function advance
%type IElementType
%unicode

EOL=\R
WHITE_SPACE=\s+

SPACE=[ \t\n\x0B\f\r]+
VALID_ID=[a-zA-Z_][a-zA-Z_0-9]*
STRING=\"([^\"\\]|\\.)*\"
SCALA_TERM=`.[^`]*`
NUMBER=-?[0-9]+
DOUBLE=-?[0-9]+(\.[0-9]+)?
LINE_COMMENT="//".*
BLOCK_COMMENT="/"\*(.|\n)*\*"/"

%%
<YYINITIAL> {
  {WHITE_SPACE}        { return WHITE_SPACE; }

  "if"                 { return KEYWORD_IF; }
  "else"               { return KEYWORD_ELSE; }
  "let"                { return KEYWORD_LET; }
  "in"                 { return KEYWORD_IN; }
  "match"              { return KEYWORD_MATCH; }
  "case"               { return KEYWORD_CASE; }
  "fail"               { return KEYWORD_FAIL; }
  "Option"             { return KEYWORD_OPTION; }
  "None"               { return KEYWORD_NONE; }
  "Some"               { return KEYWORD_SOME; }
  "Set"                { return KEYWORD_SET; }
  "fold"               { return KEYWORD_FOLD; }
  "module"             { return KEYWORD_MODULE; }
  "import"             { return KEYWORD_IMPORT; }
  "not"                { return KEYWORD_NOT; }
  "data"               { return KEYWORD_DATA; }
  "def"                { return KEYWORD_DEF; }

  {SPACE}              { return SPACE; }
  {VALID_ID}           { return VALID_ID; }
  {STRING}             { return STRING; }
  {SCALA_TERM}         { return SCALA_TERM; }
  {NUMBER}             { return NUMBER; }
  {DOUBLE}             { return DOUBLE; }
  {LINE_COMMENT}       { return LINE_COMMENT; }
  {BLOCK_COMMENT}      { return BLOCK_COMMENT; }

}

[^] { return BAD_CHARACTER; }
