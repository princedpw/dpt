%{
  open Syntax
  open Batteries

%}

%token <Span.t * Id.t> ID
%token <Span.t * int> NUM
%token <Span.t> TRUE
%token <Span.t> FALSE
%token <Span.t> EQ
%token <Span.t> LESS
%token <Span.t> PLUS
%token <Span.t> ASSIGN
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

ty:
    | TINT				{ ty_sp TInt $1 }
    | TBOOL				{ ty_sp TBool $1 }
    | TEVENT LBRACKET tylist RBRACKET   { ty_sp (TEvent $3) (Span.extend $1 $4) }

tylist:
    | ty				{ [ $1 ] }
    | ty COMMA tylist			{ $1::$3 }

exp:
    | ID				{ exp_sp (EVar (snd $1)) (fst $1) }
    | NUM                               { value_to_exp (vint_sp (snd $1) (fst $1)) }
    | exp PLUS exp      		{ exp_sp (EOp(Plus, [$1; $3])) (Span.extend $1.espan $3.espan) }

decl:
    | PRINTI exp SEMI                   { decl_sp (DPrinti $2) (Span.extend $1 $3) }
    | ty ID ASSIGN exp SEMI		{ decl_sp (DVar (snd $2, $1, $4)) (Span.extend $1.tspan $5) } 

decls:
    | decl                              { [$1] }
    | decl decls                        { $1::$2 }

prog:
    | decls EOF                         { $1 }
;
