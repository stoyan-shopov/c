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
%option noyywrap nounput batch debug
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

extern int sym_type(const char *);  /* returns type from symbol table */

#define sym_type(identifier) yy::calcxx_parser::token::TOK_IDENTIFIER /* with no symbol table, fake it */

static yy::calcxx_parser::symbol_type check_type(void);
%}

%%

%{
  // Code run each time yylex is called.
  loc.step ();
%}

{blank}+   loc.step ();
[\n]+      loc.lines (yyleng); loc.step ();
<<EOF>>    return yy::calcxx_parser::make_END(loc);


"/*"        {
            int c;

            for ( ; ; )
                {
                while ( (c = yyinput()) != '*' &&
                        c != EOF )
		{
			/* eat up text of comment */
			loc.columns(1);
			if (c == '\n')
			{
				loc.lines(1);
			}
			loc.step();
		}

                if ( c == '*' )
                    {
			    loc.columns(1);
                    while ( (c = yyinput()) != EOF )
		    {
			    loc.columns(1);
			    if (c == '\n')
			    {
				    loc.lines(1);
			    }
			    loc.step();
			    if (c != '*')
				    break;
		    }
                    if ( c == '/' )
                        break;    /* found the end */
                    }

                if ( c == EOF )
                    {
                    driver.error( "EOF in comment" );
                    break;
                    }
                }
            }


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
"return"				{ return yy::calcxx_parser::make_RETURN(loc); }
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
"_Alignas"                              { return yy::calcxx_parser::make_ALIGNAS(loc); }
"_Alignof"                              { return yy::calcxx_parser::make_ALIGNOF(loc); }
"_Atomic"                               { return yy::calcxx_parser::make_ATOMIC(loc); }
"_Bool"                                 { return yy::calcxx_parser::make_BOOL(loc); }
"_Complex"                              { return yy::calcxx_parser::make_COMPLEX(loc); }
"_Generic"                              { return yy::calcxx_parser::make_GENERIC(loc); }
"_Imaginary"                            { return yy::calcxx_parser::make_IMAGINARY(loc); }
"_Noreturn"                             { return yy::calcxx_parser::make_NORETURN(loc); }
"_Static_assert"                        { return yy::calcxx_parser::make_STATIC_ASSERT(loc); }
"_Thread_local"                         { return yy::calcxx_parser::make_THREAD_LOCAL(loc); }
"__func__"                              { return yy::calcxx_parser::make_FUNC_NAME(loc); }

{L}{A}*					{ return check_type(); }

{HP}{H}+{IS}?				{ std::cout << "hex integer constant detected:" << yytext << std::endl; return yy::calcxx_parser::make_I_CONSTANT(-1, loc); }
{NZ}{D}*{IS}?				{ return yy::calcxx_parser::make_I_CONSTANT(-2, loc); }
"0"{O}*{IS}?				{ return yy::calcxx_parser::make_I_CONSTANT(-3, loc); }
{CP}?"'"([^'\\\n]|{ES})+"'"		{ return yy::calcxx_parser::make_I_CONSTANT(-4, loc); }

{D}+{E}{FS}?				{ return yy::calcxx_parser::make_F_CONSTANT(loc); }
{D}*"."{D}+{E}?{FS}?			{ return yy::calcxx_parser::make_F_CONSTANT(loc); }
{D}+"."{E}?{FS}?			{ return yy::calcxx_parser::make_F_CONSTANT(loc); }
{HP}{H}+{P}{FS}?			{ return yy::calcxx_parser::make_F_CONSTANT(loc); }
{HP}{H}*"."{H}+{P}{FS}?			{ return yy::calcxx_parser::make_F_CONSTANT(loc); }
{HP}{H}+"."{P}{FS}?			{ return yy::calcxx_parser::make_F_CONSTANT(loc); }

({SP}?\"([^"\\\n]|{ES})*\"{WS}*)+	{ return yy::calcxx_parser::make_STRING_LITERAL(loc); }

"..."					{ return yy::calcxx_parser::make_ELLIPSIS(loc); }
">>="					{ return yy::calcxx_parser::make_RIGHT_ASSIGN(loc); }
"<<="					{ return yy::calcxx_parser::make_LEFT_ASSIGN(loc); }
"+="					{ return yy::calcxx_parser::make_ADD_ASSIGN(loc); }
"-="					{ return yy::calcxx_parser::make_SUB_ASSIGN(loc); }
"*="					{ return yy::calcxx_parser::make_MUL_ASSIGN(loc); }
"/="					{ return yy::calcxx_parser::make_DIV_ASSIGN(loc); }
"%="					{ return yy::calcxx_parser::make_MOD_ASSIGN(loc); }
"&="					{ return yy::calcxx_parser::make_AND_ASSIGN(loc); }
"^="					{ return yy::calcxx_parser::make_XOR_ASSIGN(loc); }
"|="					{ return yy::calcxx_parser::make_OR_ASSIGN(loc); }
">>"					{ return yy::calcxx_parser::make_RIGHT_OP(loc); }
"<<"					{ return yy::calcxx_parser::make_LEFT_OP(loc); }
"++"					{ return yy::calcxx_parser::make_INC_OP(loc); }
"--"					{ return yy::calcxx_parser::make_DEC_OP(loc); }
"->"					{ return yy::calcxx_parser::make_PTR_OP(loc); }
"&&"					{ return yy::calcxx_parser::make_AND_OP(loc); }
"||"					{ return yy::calcxx_parser::make_OR_OP(loc); }
"<="					{ return yy::calcxx_parser::make_LE_OP(loc); }
">="					{ return yy::calcxx_parser::make_GE_OP(loc); }
"=="					{ return yy::calcxx_parser::make_EQ_OP(loc); }
"!="					{ return yy::calcxx_parser::make_NE_OP(loc); }
";"					{ return yy::calcxx_parser::make_SEMICOLON(loc); }
("{"|"<%")				{ return yy::calcxx_parser::make_LCURLY_BRACE(loc); }
("}"|"%>")				{ return yy::calcxx_parser::make_RCURLY_BRACE(loc); }
","					{ return yy::calcxx_parser::make_COMMA(loc); }
":"					{ return yy::calcxx_parser::make_COLON(loc); }
"="					{ return yy::calcxx_parser::make_ASSIGN(loc); }
"("					{ return yy::calcxx_parser::make_LPAREN(loc); }
")"					{ return yy::calcxx_parser::make_RPAREN(loc); }
("["|"<:")				{ return yy::calcxx_parser::make_LSQUARE_BRACKET(loc); }
("]"|":>")				{ return yy::calcxx_parser::make_RSQUARE_BRACKET(loc); }
"."					{ return yy::calcxx_parser::make_DOT(loc); }
"&"					{ return yy::calcxx_parser::make_AMPERSAND(loc); }
"!"					{ return yy::calcxx_parser::make_EXCLAMATION_MARK(loc); }
"~"					{ return yy::calcxx_parser::make_TILDE(loc); }
"-"					{ return yy::calcxx_parser::make_MINUS(loc); }
"+"					{ return yy::calcxx_parser::make_PLUS(loc); }
"*"					{ return yy::calcxx_parser::make_STAR(loc); }
"/"					{ return yy::calcxx_parser::make_SLASH(loc); }
"%"					{ return yy::calcxx_parser::make_PERCENTAGE_SIGN(loc); }
"<"					{ return yy::calcxx_parser::make_LESS_THAN_SIGN(loc); }
">"					{ return yy::calcxx_parser::make_GREATER_THAN_SIGN(loc); }
"^"					{ return yy::calcxx_parser::make_CARET(loc); }
"|"					{ return yy::calcxx_parser::make_PIPE(loc); }
"?"					{ return yy::calcxx_parser::make_QUESTION_MARK(loc); }

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


static yy::calcxx_parser::symbol_type check_type(void)
{
	switch (sym_type(yytext))
	{
		case yy::calcxx_parser::token::TOK_TYPEDEF_NAME:                /* previously defined */
			return yy::calcxx_parser::make_TYPEDEF_NAME(loc);
		case yy::calcxx_parser::token::TOK_ENUMERATION_CONSTANT:        /* previously defined */
			return yy::calcxx_parser::make_ENUMERATION_CONSTANT(loc);
		default:                          /* includes undefined */
			return yy::calcxx_parser::make_IDENTIFIER(std::string("id\"") + yytext + "\"", loc);
	}
}
