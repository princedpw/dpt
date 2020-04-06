%{
  open Syntax
  open Batteries

%}

%token <Span.t * Var.t> ID
%token <Span.t * int> NUM
%token <Span.t> TRUE
%token <Span.t> FALSE
%token <Span.t> EQ
%token <Span.t> LESS
%token <Span.t> IF
%token <Span.t> THEN
%token <Span.t> ELSE
%token <Span.t> SEMI
%token <Span.t> PRINTI
%token <Span.t> LPAREN
%token <Span.t> RPAREN
%token <Span.t> LBRACKET
%token <Span.t> RBRACKET
%token <Span.t> LBRACE
%token <Span.t> RBRACE
%token <Span.t> COMMA
%token <Span.t> TBOOL
%token <Span.t> TEVENT
%token <Span.t> TINT


%token EOF

%start prog
%type  <Syntax.decls> prog

%right ELSE     /* lowest precedence */
%nonassoc LESS EQ
%left PLUS
%left LBRACKET      /* highest precedence */

%%

exp:
    | NUM                               { value_to_exp (vint (snd $1) (fst $1)) }

decl:
    | PRINTI exp SEMI                   { decl (DPrinti $2, Span.extend $1 $3) }

decls:
    | decl                              { [$1] }
    | decl decls                        { $1::$2 }

prog:
    | decls EOF                         { $1 }
;
