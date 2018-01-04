%{ /* -*- C++ -*- */
# include <cerrno>
# include <climits>
# include <cstdlib>
# include <string>
# include "calc++-driver.hh"
# include "calc++-parser.hh"

// Work around an incompatibility in flex (at least versions
// 2.5.31 through 2.5.33): it generates code that does
// not conform to C89.  See Debian bug 333231
// <http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=333231>.
# undef yywrap
# define yywrap() 1

// The location of the current token.
static yy::location loc;
%}
%option noyywrap nounput batch debug noinput
int   [0-9]+
blank [ \t]

%{
  // Code run each time a pattern is matched.
  # define YY_USER_ACTION  loc.columns (yyleng);
%}


%e  1019
%p  2807
%n  371
%k  284
%a  1213
%o  1117

O   [0-7]
D   [0-9]
NZ  [1-9]
L   [a-zA-Z_]
A   [a-zA-Z_0-9]
H   [a-fA-F0-9]
HP  (0[xX])
E   ([Ee][+-]?{D}+)
P   ([Pp][+-]?{D}+)
FS  (f|F|l|L)
IS  (((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?))
CP  (u|U|L)
SP  (u8|u|U|L)
ES  (\\(['"\?\\abfnrtv]|[0-7]{1,3}|x[a-fA-F0-9]+))
WS  [ \t\v\n\f]

%{
#include <stdio.h>
#include <string>
#include "grammar.hxx"

extern void yyerror(const char *);  /* prints grammar violation message */

extern int sym_type(const char *);  /* returns type from symbol table */

#define sym_type(identifier) IDENTIFIER /* with no symbol table, fake it */

static void comment(void);
static int check_type(void);
%}

%%

%{
  // Code run each time yylex is called.
  loc.step ();
%}

{blank}+   loc.step ();
[\n]+      loc.lines (yyleng); loc.step ();
"-"      return yy::calcxx_parser::make_MINUS(loc);
"+"      return yy::calcxx_parser::make_PLUS(loc);
"*"      return yy::calcxx_parser::make_STAR(loc);
"/"      return yy::calcxx_parser::make_SLASH(loc);
"("      return yy::calcxx_parser::make_LPAREN(loc);
")"      return yy::calcxx_parser::make_RPAREN(loc);
":="     return yy::calcxx_parser::make_ASSIGN(loc);


{int}      {
  errno = 0;
  long n = strtol (yytext, NULL, 10);
  if (! (INT_MIN <= n && n <= INT_MAX && errno != ERANGE))
    driver.error (loc, "integer is out of range");
  return yy::calcxx_parser::make_NUMBER(n, loc);
}

<<EOF>>    return yy::calcxx_parser::make_END(loc);


"/*"                                    { comment(); }
"//".*                                    { /* consume //-comment */ }

"auto"					{ return yy::calcxx_parser::make_AUTO(loc); }
"break"					{ return yy::calcxx_parser::make_BREAK(loc); }
"case"					{ return yy::calcxx_parser::make_CASE(loc); }
"char"					{ return yy::calcxx_parser::make_CHAR(loc); }
"const"					{ return yy::calcxx_parser::make_CONST(loc); }
"continue"				{ return yy::calcxx_parser::make_CONTINUE(loc); }
"default"				{ return yy::calcxx_parser::make_DEFAULT(loc); }
"do"					{ return yy::calcxx_parser::make_DO(loc); }
"double"				{ return yy::calcxx_parser::make_DOUBLE(loc); }
"else"					{ return yy::calcxx_parser::make_ELSE(loc); }
"enum"					{ return yy::calcxx_parser::make_ENUM(loc); }
"extern"				{ return yy::calcxx_parser::make_EXTERN(loc); }
"float"					{ return yy::calcxx_parser::make_FLOAT(loc); }
"for"					{ return yy::calcxx_parser::make_FOR(loc); }
"goto"					{ return yy::calcxx_parser::make_GOTO(loc); }
"if"					{ return yy::calcxx_parser::make_IF(loc); }
"inline"				{ return yy::calcxx_parser::make_INLINE(loc); }
"int"					{ return yy::calcxx_parser::make_INT(loc); }
"long"					{ return yy::calcxx_parser::make_LONG(loc); }
"register"				{ return yy::calcxx_parser::make_REGISTER(loc); }
"restrict"				{ return yy::calcxx_parser::make_RESTRICT(loc); }
"return"				{ return(RETURN); }
"short"					{ return yy::calcxx_parser::make_SHORT(loc); }
"signed"				{ return yy::calcxx_parser::make_SIGNED(loc); }
"sizeof"				{ return yy::calcxx_parser::make_SIZEOF(loc); }
"static"				{ return yy::calcxx_parser::make_STATIC(loc); }
"struct"				{ return yy::calcxx_parser::make_STRUCT(loc); }
"switch"				{ return yy::calcxx_parser::make_SWITCH(loc); }
"typedef"				{ return yy::calcxx_parser::make_TYPEDEF(loc); }
"union"					{ return yy::calcxx_parser::make_UNION(loc); }
"unsigned"				{ return yy::calcxx_parser::make_UNSIGNED(loc); }
"void"					{ return yy::calcxx_parser::make_VOID(loc); }
"volatile"				{ return yy::calcxx_parser::make_VOLATILE(loc); }
"while"					{ return yy::calcxx_parser::make_WHILE(loc); }
"_Alignas"                              { return ALIGNAS; }
"_Alignof"                              { return ALIGNOF; }
"_Atomic"                               { return ATOMIC; }
"_Bool"                                 { return BOOL; }
"_Complex"                              { return COMPLEX; }
"_Generic"                              { return GENERIC; }
"_Imaginary"                            { return IMAGINARY; }
"_Noreturn"                             { return NORETURN; }
"_Static_assert"                        { return STATIC_ASSERT; }
"_Thread_local"                         { return THREAD_LOCAL; }
"__func__"                              { return FUNC_NAME; }

{L}{A}*					{ return check_type(); }

{HP}{H}+{IS}?				{ return I_CONSTANT; }
{NZ}{D}*{IS}?				{ return I_CONSTANT; }
"0"{O}*{IS}?				{ return I_CONSTANT; }
{CP}?"'"([^'\\\n]|{ES})+"'"		{ return I_CONSTANT; }

{D}+{E}{FS}?				{ return F_CONSTANT; }
{D}*"."{D}+{E}?{FS}?			{ return F_CONSTANT; }
{D}+"."{E}?{FS}?			{ return F_CONSTANT; }
{HP}{H}+{P}{FS}?			{ return F_CONSTANT; }
{HP}{H}*"."{H}+{P}{FS}?			{ return F_CONSTANT; }
{HP}{H}+"."{P}{FS}?			{ return F_CONSTANT; }

({SP}?\"([^"\\\n]|{ES})*\"{WS}*)+	{ return STRING_LITERAL; }

"..."					{ return ELLIPSIS; }
">>="					{ return RIGHT_ASSIGN; }
"<<="					{ return LEFT_ASSIGN; }
"+="					{ return ADD_ASSIGN; }
"-="					{ return SUB_ASSIGN; }
"*="					{ return MUL_ASSIGN; }
"/="					{ return DIV_ASSIGN; }
"%="					{ return MOD_ASSIGN; }
"&="					{ return AND_ASSIGN; }
"^="					{ return XOR_ASSIGN; }
"|="					{ return OR_ASSIGN; }
">>"					{ return RIGHT_OP; }
"<<"					{ return LEFT_OP; }
"++"					{ return INC_OP; }
"--"					{ return DEC_OP; }
"->"					{ return PTR_OP; }
"&&"					{ return AND_OP; }
"||"					{ return OR_OP; }
"<="					{ return LE_OP; }
">="					{ return GE_OP; }
"=="					{ return EQ_OP; }
"!="					{ return NE_OP; }
";"					{ return ';'; }
("{"|"<%")				{ return '{'; }
("}"|"%>")				{ return '}'; }
","					{ return ','; }
":"					{ return ':'; }
"="					{ return '='; }
"("					{ return '('; }
")"					{ return ')'; }
("["|"<:")				{ return '['; }
("]"|":>")				{ return ']'; }
"."					{ return '.'; }
"&"					{ return '&'; }
"!"					{ return '!'; }
"~"					{ return '~'; }
"-"					{ return '-'; }
"+"					{ return '+'; }
"*"					{ return '*'; }
"/"					{ return '/'; }
"%"					{ return '%'; }
"<"					{ return '<'; }
">"					{ return '>'; }
"^"					{ return '^'; }
"|"					{ return '|'; }
"?"					{ return '?'; }

{WS}+					{ /* whitespace separates tokens */ }
.					{ /* discard bad characters */ }


%%

void
calcxx_driver::scan_begin ()
{
  yy_flex_debug = trace_scanning;
  if (file.empty () || file == "-")
    yyin = stdin;
  else if (!(yyin = fopen (file.c_str (), "r")))
    {
      error ("cannot open " + file + ": " + strerror(errno));
      exit (EXIT_FAILURE);
    }
}



void
calcxx_driver::scan_end ()
{
  fclose (yyin);
}

