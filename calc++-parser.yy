%skeleton "lalr1.cc" /* -*- C++ -*- */
%require "3.0.4"
%defines
%define parser_class_name {calcxx_parser}
%define api.token.constructor
%define api.value.type variant
%define parse.assert
%code requires
{
# include <string>
# include <sstream>
class calcxx_driver;
}
// The parsing context.
%param { calcxx_driver& driver }
%locations
%initial-action
{
  // Initialize the initial location.
  @$.begin.filename = @$.end.filename = &driver.file;
};
%define parse.trace
%define parse.error verbose
%code
{
# include "calc++-driver.hh"
}
%define api.token.prefix {TOK_}
%token
	END  0  "end of file"
	ASSIGN  ":="
	MINUS   "-"
	PLUS    "+"
	STAR    "*"
	SLASH   "/"
	LPAREN  "("
	RPAREN  ")"
	SEMICOLON	";"
	LCURLY_BRACE	"{"
	RCURLY_BRACE	"}"
	COMMA	","
	COLON	":"
	LSQUARE_BRACKET	"["
	RSQUARE_BRACKET	"]"
	DOT	"."
	AMPERSAND	"&"
	EXCLAMATION_MARK	"!"
	TILDE	"~"
	PERCENTAGE_SIGN	"%"
	LESS_THAN_SIGN	"<"
	GREATER_THAN_SIGN	">"
	CARET	"^"
	PIPE	"|"
	QUESTION_MARK	"?"

;
%token <std::string> IDENTIFIER "identifier"
%token <std::string> TYPEDEF_NAME "typedef_name"
%token <signed long long> I_CONSTANT "integer_constant"
%token  F_CONSTANT
%token  STRING_LITERAL
%token  FUNC_NAME
%token  SIZEOF
%token	PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token	AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token	SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token	XOR_ASSIGN OR_ASSIGN
%token	ENUMERATION_CONSTANT

%token	TYPEDEF EXTERN STATIC AUTO REGISTER INLINE
%token	CONST RESTRICT VOLATILE
%token	BOOL CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE VOID
%token	COMPLEX IMAGINARY 
%token	STRUCT UNION ENUM ELLIPSIS

%token	CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%token	ALIGNAS ALIGNOF ATOMIC GENERIC NORETURN STATIC_ASSERT THREAD_LOCAL

%type <std::string> constant
%type <std::string> string
%type <std::string> primary_expression
%type <std::string> postfix_expression
%type <std::string> unary_operator
%type <std::string> unary_expression
%type <std::string> type_name
%type <std::string> cast_expression
%type <std::string> multiplicative_expression
%type <std::string> additive_expression
%type <std::string> shift_expression
%type <std::string> relational_expression
%type <std::string> equality_expression
%type <std::string> and_expression
%type <std::string> exclusive_or_expression
%type <std::string> inclusive_or_expression
%type <std::string> logical_and_expression
%type <std::string> logical_or_expression
%type <std::string> conditional_expression
%type <std::string> assignment_operator
%type <std::string> assignment_expression
%type <std::string> expression

%type <std::string> type_qualifier
%type <std::string> type_specifier
%type <std::string> atomic_type_specifier
%type <std::string> struct_or_union
%type <std::string> struct_or_union_specifier
%type <std::string> specifier_qualifier_list
%type <std::string> storage_class_specifier
%type <std::string> function_specifier
%type <std::string> declaration_specifiers
%type <std::string> alignment_specifier
%type <std::string> struct_declaration_list
%type <std::string> direct_declarator
%type <std::string> declarator
%type <std::string> pointer
%type <std::string> type_qualifier_list
%type <std::string> identifier_list
%type <std::string> struct_declaration
%type <std::string> struct_declarator
%type <std::string> struct_declarator_list
%type <std::string> init_declarator_list
%type <std::string> init_declarator

%printer { yyoutput << $$; } <*>;
%%
%start translation_unit;

primary_expression
	: IDENTIFIER		{ $$ = $1; }
	| constant		{ $$ = $1; }
	| string		{ $$ = $1; }
	| "(" expression ")"	{ $$ = "val{ " + $2 + " }"; }
	| generic_selection	{ $$ = "<<<generic-selection>>>"; }
	;

constant
	: I_CONSTANT		/* includes character_constant */		{ std::stringstream ss(" "); ss << $1; $$ = ss.str(); }
	| F_CONSTANT								{ $$ = "-2"; }
	| ENUMERATION_CONSTANT	/* after it has been defined as such */		{ $$ = "-3"; }
	;

enumeration_constant		/* before it has been defined as such */
	: IDENTIFIER
	;

string
	: STRING_LITERAL	{ $$ = "<<<string-literal>>>"; }
	| FUNC_NAME		{ $$ = "<<<func-name>>>"; }
	;

generic_selection
	: GENERIC "(" assignment_expression "," generic_assoc_list ")"
	;

generic_assoc_list
	: generic_association
	| generic_assoc_list "," generic_association
	;

generic_association
	: type_name ":" assignment_expression
	| DEFAULT ":" assignment_expression
	;

postfix_expression
	: primary_expression					{ $$ = $1; }
	| postfix_expression "[" expression "]"			{ $$ = $1 + " " + $3 + " " + "[]" ; }
	| postfix_expression "(" ")"				{ $$ = $1 + " " + "()"; }
	| postfix_expression "(" argument_expression_list ")"	{ $$ = $1 + " " + "(<<<function-call-with-arguments>>>)"; }
	| postfix_expression "." IDENTIFIER			{ $$ = $1 + " " + $3 + " " + ".member-access"; }
	| postfix_expression PTR_OP IDENTIFIER			{ $$ = $1 + " " + $3 + " " + "->member-access"; }
	| postfix_expression INC_OP				{ $$ = "lval{ " + $1 + " }" + " " + "postfix++"; }
	| postfix_expression DEC_OP				{ $$ = "lval{ " + $1 + " }" + " " + "postfix--"; }
	| "(" type_name ")" "{" initializer_list "}"		{ $$ = "<<<compound-literal>>>"; }
	| "(" type_name ")" "{" initializer_list "," "}"	{ $$ = "<<<compound-literal>>>"; }
	;

argument_expression_list
	: assignment_expression
	| argument_expression_list "," assignment_expression
	;

unary_expression
	: postfix_expression			{ $$ = $1; }
	| INC_OP unary_expression		{ $$ = "lval{ " + $2 + " }" + " " + "prefix++"; }
	| DEC_OP unary_expression		{ $$ = "lval{ " + $2 + " }" + " " + "prefix--"; }
	| unary_operator cast_expression	{ $$ = "val{ " + $2 + " }" + " " + $1; }
	| SIZEOF unary_expression		{ $$ = "type{ " + $2 + " }" + " " + "sizeof"; }
	| SIZEOF "(" type_name ")"		{ $$ = "type{ " + $3 + " }" + " " + "sizeof"; }
	| ALIGNOF "(" type_name ")"		{ $$ = "type{ " + $3 + " }" + " " + "alignof"; }
	;

unary_operator
	: "&"	{ $$ = "unary-&"; }
	| "*"	{ $$ = "unary-*"; }
	| "+"	{ $$ = "unary-+"; }
	| "-"	{ $$ = "unary--"; }
	| "~"	{ $$ = "unary-~"; }
	| "!"	{ $$ = "unary-!"; }
	;

cast_expression
	: unary_expression			{ $$ = $1; }
	| "(" type_name ")" cast_expression	{ $$ = "val{ " + $4 + " }" + " " + " type{ " + $2 + " }" + " " + "type-cast"; }
	;

multiplicative_expression
	: cast_expression					{ $$ = $1; }
	| multiplicative_expression "*" cast_expression		{ $$ = $1 + " " + $3 + " " + "*"; }
	| multiplicative_expression "/" cast_expression		{ $$ = $1 + " " + $3 + " " + "/"; }
	| multiplicative_expression "%" cast_expression		{ $$ = $1 + " " + $3 + " " + "%"; }
	;

additive_expression
	: multiplicative_expression				{ $$ = $1; }
	| additive_expression "+" multiplicative_expression	{ $$ = $1 + " " + $3 + " " + "+"; }
	| additive_expression "-" multiplicative_expression	{ $$ = $1 + " " + $3 + " " + "-"; }
	;

shift_expression
	: additive_expression					{ $$ = $1; }
	| shift_expression LEFT_OP additive_expression		{ $$ = $1 + " " + $3 + " " + "lshift"; }
	| shift_expression RIGHT_OP additive_expression		{ $$ = $1 + " " + $3 + " " + "rshift"; }
	;

relational_expression
	: shift_expression				{ $$ = $1; }
	| relational_expression "<" shift_expression	{ $$ = $1 + " " + $3 + " " + "<"; }
	| relational_expression ">" shift_expression	{ $$ = $1 + " " + $3 + " " + ">"; }
	| relational_expression LE_OP shift_expression	{ $$ = $1 + " " + $3 + " " + "<="; }
	| relational_expression GE_OP shift_expression	{ $$ = $1 + " " + $3 + " " + ">="; }
	;

equality_expression
	: relational_expression					{ $$ = $1; }
	| equality_expression EQ_OP relational_expression	{ $$ = $1 + " " + $3 + " " + "=="; }
	| equality_expression NE_OP relational_expression	{ $$ = $1 + " " + $3 + " " + "!="; }
	;

and_expression
	: equality_expression				{ $$ = $1; }
	| and_expression "&" equality_expression	{ $$ = $1 + " " + $3 + " " + "&"; }
	;

exclusive_or_expression
	: and_expression				{ $$ = $1; }
	| exclusive_or_expression "^" and_expression	{ $$ = $1 + " " + $3 + " " + "^"; }
	;

inclusive_or_expression
	: exclusive_or_expression				{ $$ = $1; }
	| inclusive_or_expression "|" exclusive_or_expression	{ $$ = $1 + " " + $3 + " " + "|"; }
	;

logical_and_expression
	: inclusive_or_expression					{ $$ = $1; }
	| logical_and_expression AND_OP inclusive_or_expression		{ $$ = $1 + " " + $3 + " " + "&&"; }
	;

logical_or_expression
	: logical_and_expression				{ $$ = $1; }
	| logical_or_expression OR_OP logical_and_expression	{ $$ = $1 + " " + $3 + " " + "||"; }
	;

conditional_expression
	: logical_or_expression							{ $$ = $1; }
	| logical_or_expression "?" expression ":" conditional_expression	{ $$ = $1 + " " + "[if]" + " " + $3 + " " + "[else]" + " " + $5 + " " + "[then]"; }
	;

assignment_expression
	: conditional_expression					{ $$ = $1; }
	| unary_expression assignment_operator assignment_expression	{ $$ = "lval{ " + $1 + " }" + " " + "val{ " + $3 + " }" + " " + $2; }
	;

assignment_operator
	: "="			{ $$ = "store!"; }
	| MUL_ASSIGN		{ $$ = "*-store!"; }
	| DIV_ASSIGN		{ $$ = "/-store!"; }
	| MOD_ASSIGN		{ $$ = "mod-store!"; }
	| ADD_ASSIGN		{ $$ = "+-store!"; }
	| SUB_ASSIGN		{ $$ = "minus-store!"; }
	| LEFT_ASSIGN		{ $$ = "lshift-store!"; }
	| RIGHT_ASSIGN		{ $$ = "rshift-store!"; }
	| AND_ASSIGN		{ $$ = "and-store!"; }
	| XOR_ASSIGN		{ $$ = "xor-store!"; }
	| OR_ASSIGN		{ $$ = "or-store!"; }
	;

expression
	: assignment_expression				{ $$ = $1; }
	| expression "," assignment_expression		{ $$ = $1 + " " + ",,,,,,,," + " " + $3; }
	;

constant_expression
	: conditional_expression	/* with constraints */
	;

declaration
	: declaration_specifiers ";"				{ std::cout << "declaration detected: " << $1 << " " << "declaration-end" << std::endl; }
	| declaration_specifiers init_declarator_list ";"	{ std::cout << "declaration detected: " << $2 << " " << $1 << " " << "define-variables" << " " << "declaration-end" << std::endl; }
	| static_assert_declaration
	;

declaration_specifiers
	: storage_class_specifier declaration_specifiers	{ $$ = $2 + " " + $1; }
	| storage_class_specifier				{ $$ = $1; }
	| type_specifier declaration_specifiers			{ $$ = $2 + " " + $1; }
	| type_specifier					{ $$ = $1; }
	| type_qualifier declaration_specifiers			{ $$ = $2 + " " + $1; }
	| type_qualifier					{ $$ = $1; }
	| function_specifier declaration_specifiers		{ $$ = $2 + " " + $1; }
	| function_specifier					{ $$ = $1; }
	| alignment_specifier declaration_specifiers		{ $$ = $2 + " " + $1; }
	| alignment_specifier					{ $$ = $1; }
	;

init_declarator_list
	: init_declarator				{ $$ = $1; }
	| init_declarator_list "," init_declarator	{ $$ = $1 + " " + $3; }
	;

init_declarator
	: declarator "=" initializer	{ $$ = $1 + " " + "init{ " + "..." + " }"; }
	| declarator			{ $$ = $1; }
	;

storage_class_specifier
	: TYPEDEF	/* identifiers must be flagged as TYPEDEF_NAME */	{ $$ = ">>>typedef"; }
	| EXTERN	{ $$ = ">>>extern"; }
	| STATIC	{ $$ = ">>>static"; }
	| THREAD_LOCAL	{ $$ = ">>>thread-local"; }
	| AUTO		{ $$ = ">>>auto"; }
	| REGISTER	{ $$ = ">>>register"; }
	;

type_specifier
	: VOID				{ $$ = ">>void"; }
	| CHAR				{ $$ = ">>char"; }
	| SHORT				{ $$ = ">>short"; }
	| INT				{ $$ = ">>int"; }
	| LONG				{ $$ = ">>long"; }
	| FLOAT				{ $$ = ">>float"; }
	| DOUBLE			{ $$ = ">>double"; }
	| SIGNED			{ $$ = ">>signed"; }
	| UNSIGNED			{ $$ = ">>unsigned"; }
	| BOOL				{ $$ = ">>bool"; }
	| COMPLEX			{ $$ = ">>complex"; }
	| IMAGINARY	  	/* non-mandated extension */	{ $$ = ">>imaginary"; }
	| atomic_type_specifier		{ $$ = $1; }
	| struct_or_union_specifier	{ $$ = $1; }
	| enum_specifier
	| TYPEDEF_NAME		/* after it has been defined as such */	{ $$ = $1; }
	;

struct_or_union_specifier
	: struct_or_union "{" struct_declaration_list "}"		{ $$ = std::string("aggregate{ ") + $3 + " }aggregate-end" + " " + $1; }
	| struct_or_union IDENTIFIER "{" struct_declaration_list "}"	{ $$ = std::string("aggregate{ ") + $4 + " }aggregate-end" + " " + $2 + " " + $1; }
	| struct_or_union IDENTIFIER					{ $$ = $2 + " " + $1; }
	;

struct_or_union
	: STRUCT	{ $$ = ">struct"; }
	| UNION		{ $$ = ">union"; }
	;

struct_declaration_list
	: struct_declaration				{ $$ = $1; }
	| struct_declaration_list struct_declaration	{ $$ = $1 + " " + $2; }
	;

struct_declaration
	: specifier_qualifier_list ";"	/* for anonymous struct/union */	{ $$ = $1 + " " + ">anonymous-aggregate"; }
	| specifier_qualifier_list struct_declarator_list ";"			{ $$ = std::string() + $1 + " " + "struct-declarator-list{ " + $2 + /* " " + $1 + " " +*/ " }struct-declarator-list-end"; }
	| static_assert_declaration
	;

specifier_qualifier_list
	: type_specifier specifier_qualifier_list	{ $$ = $2 + " " + $1; }
	| type_specifier				{ $$ = $1; }
	| type_qualifier specifier_qualifier_list	{ $$ = $2 + " " + $1; }
	| type_qualifier				{ $$ = $1; }
	;

struct_declarator_list
	: struct_declarator				{ $$ = $1; }
	| struct_declarator_list "," struct_declarator	{ $$ = $1 + " " + $3; }
	;

struct_declarator
	: ":" constant_expression
	| declarator ":" constant_expression
	| declarator				{ $$ = $1; }
	;

enum_specifier
	: ENUM "{" enumerator_list "}"
	| ENUM "{" enumerator_list "," "}"
	| ENUM IDENTIFIER "{" enumerator_list "}"
	| ENUM IDENTIFIER "{" enumerator_list "," "}"
	| ENUM IDENTIFIER
	;

enumerator_list
	: enumerator
	| enumerator_list "," enumerator
	;

enumerator	/* identifiers must be flagged as ENUMERATION_CONSTANT */
	: enumeration_constant "=" constant_expression
	| enumeration_constant
	;

atomic_type_specifier
	: ATOMIC "(" type_name ")"	{ $$ = "type{ " + $3 + " }" + " " + ">>atomic"; }
	;

type_qualifier
	: CONST		{ $$ = ">const"; }
	| RESTRICT	{ $$ = ">restrict"; }
	| VOLATILE	{ $$ = ">volatile"; }
	| ATOMIC	{ $$ = ">atomic"; }
	;

function_specifier
	: INLINE	{ $$ = ">>>>inline;"; }
	| NORETURN	{ $$ = ">>>>noreturn;"; }
	;

alignment_specifier
	: ALIGNAS "(" type_name ")"			{ $$ = "<<<alignment-specifier>>>"; }
	| ALIGNAS "(" constant_expression ")"		{ $$ = "<<<alignment-specifier>>>"; }
	;

declarator
	: pointer direct_declarator	{ $$ = $2 + " " + $1; }
	| direct_declarator		{ $$ = $1; }
	;

direct_declarator
	: IDENTIFIER				{ $$ = $1; }
	| "(" declarator ")"			{ $$ = /* std::string("decl{ ") + */$2/* + " }decl-end"*/; }
	| direct_declarator "[" "]"		{ $$ = std::string() + $1 + " " + ">array[]"; }
	| direct_declarator "[" "*" "]"
	| direct_declarator "[" STATIC type_qualifier_list assignment_expression "]"
	| direct_declarator "[" STATIC assignment_expression "]"
	| direct_declarator "[" type_qualifier_list "*" "]"
	| direct_declarator "[" type_qualifier_list STATIC assignment_expression "]"
	| direct_declarator "[" type_qualifier_list assignment_expression "]"
	| direct_declarator "[" type_qualifier_list "]"
	| direct_declarator "[" assignment_expression "]"	{ $$ = $1 + " " + ">array{ " + $3 + " }array-end"; }
	| direct_declarator "(" parameter_type_list ")"
	| direct_declarator "(" ")"				{ $$ = $1 + " " + ">function()"; }
	| direct_declarator "(" identifier_list ")"		{ $$ = $1 + " " + ">function{ " + $3 + " }"; }
	;

pointer
	: "*" type_qualifier_list pointer	{ $$ = $2 + " " + ">pointer" + " " + $3; }
	| "*" type_qualifier_list		{ $$ = $2 + " " + ">pointer"; }
	| "*" pointer				{ $$ = std::string(">pointer") + " " + $2; }
	| "*"					{ $$ = ">pointer"; }
	;

type_qualifier_list
	: type_qualifier			{ $$ = $1; }
	| type_qualifier_list type_qualifier	{ $$ = $1 + " " + $2; }
	;


parameter_type_list
	: parameter_list "," ELLIPSIS
	| parameter_list
	;

parameter_list
	: parameter_declaration
	| parameter_list "," parameter_declaration
	;

parameter_declaration
	: declaration_specifiers declarator
	| declaration_specifiers abstract_declarator
	| declaration_specifiers
	;

identifier_list
	: IDENTIFIER				{ $$ = $1; }
	| identifier_list "," IDENTIFIER	{ $$ = $1 + " " + $3; }
	;

type_name
	: specifier_qualifier_list abstract_declarator		{ $$ = "<<<type-name>>>"; }
	| specifier_qualifier_list				{ $$ = "<<<type-name>>>"; }
	;

abstract_declarator
	: pointer direct_abstract_declarator
	| pointer
	| direct_abstract_declarator
	;

direct_abstract_declarator
	: "(" abstract_declarator ")"
	| "[" "]"
	| "[" "*" "]"
	| "[" STATIC type_qualifier_list assignment_expression "]"
	| "[" STATIC assignment_expression "]"
	| "[" type_qualifier_list STATIC assignment_expression "]"
	| "[" type_qualifier_list assignment_expression "]"
	| "[" type_qualifier_list "]"
	| "[" assignment_expression "]"
	| direct_abstract_declarator "[" "]"
	| direct_abstract_declarator "[" "*" "]"
	| direct_abstract_declarator "[" STATIC type_qualifier_list assignment_expression "]"
	| direct_abstract_declarator "[" STATIC assignment_expression "]"
	| direct_abstract_declarator "[" type_qualifier_list assignment_expression "]"
	| direct_abstract_declarator "[" type_qualifier_list STATIC assignment_expression "]"
	| direct_abstract_declarator "[" type_qualifier_list "]"
	| direct_abstract_declarator "[" assignment_expression "]"
	| "(" ")"
	| "(" parameter_type_list ")"
	| direct_abstract_declarator "(" ")"
	| direct_abstract_declarator "(" parameter_type_list ")"
	;

initializer
	: "{" initializer_list "}"
	| "{" initializer_list "," "}"
	| assignment_expression
	;

initializer_list
	: designation initializer
	| initializer
	| initializer_list "," designation initializer
	| initializer_list "," initializer
	;

designation
	: designator_list "="
	;

designator_list
	: designator
	| designator_list designator
	;

designator
	: "[" constant_expression "]"
	| "." IDENTIFIER
	;

static_assert_declaration
	: STATIC_ASSERT "(" constant_expression "," STRING_LITERAL ")" ";"
	;

statement
	: labeled_statement
	| compound_statement
	| expression_statement
	| selection_statement
	| iteration_statement
	| jump_statement
	;

labeled_statement
	: IDENTIFIER ":" statement
	| CASE constant_expression ":" statement
	| DEFAULT ":" statement
	;

compound_statement
	: "{" "}"
	| "{"  block_item_list "}"
	;

block_item_list
	: block_item
	| block_item_list block_item
	;

block_item
	: declaration
	| statement
	;

expression_statement
	: ";"
	| expression ";"	{ std::cout << "expression statement detected: " << $1 << std::endl; }
	;

selection_statement
	: IF "(" expression ")" statement ELSE statement
	| IF "(" expression ")" statement
	| SWITCH "(" expression ")" statement
	;

iteration_statement
	: WHILE "(" expression ")" statement
	| DO statement WHILE "(" expression ")" ";"
	| FOR "(" expression_statement expression_statement ")" statement
	| FOR "(" expression_statement expression_statement expression ")" statement
	| FOR "(" declaration expression_statement ")" statement
	| FOR "(" declaration expression_statement expression ")" statement
	;

jump_statement
	: GOTO IDENTIFIER ";"
	| CONTINUE ";"
	| BREAK ";"
	| RETURN ";"
	| RETURN expression ";"
	;

translation_unit
	: external_declaration
	| translation_unit external_declaration
	;

external_declaration
	: function_definition
	| declaration
	;

function_definition
	: declaration_specifiers declarator declaration_list compound_statement
	| declaration_specifiers declarator compound_statement
	;

declaration_list
	: declaration
	| declaration_list declaration
	;



%%
void
yy::calcxx_parser::error (const location_type& l,
                          const std::string& m)
{
  driver.error (l, m);
}
