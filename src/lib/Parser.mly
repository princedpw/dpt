%{
  open Syntax
  open Batteries

%}

%token <Span.t * Id.t> ID
%token <Span.t * int> NUM
%token <Span.t> TRUE
%token <Span.t> FALSE
%token <Span.t> EQ
%token <Span.t> AND
%token <Span.t> OR
%token <Span.t> NOT
%token <Span.t> LESS
%token <Span.t> PLUS
%token <Span.t> ASSIGN
%token <Span.t> IF
%token <Span.t> ELSE
%token <Span.t> SEMI
%token <Span.t> HANDLE
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

%left PLUS          /* lowest precedence */
%left AND OR
%nonassoc LESS EQ
%left NOT           /* highest precedence */

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
    | TRUE                              { value_to_exp (vbool_sp true $1) }
    | FALSE                             { value_to_exp (vbool_sp false $1) }
    | exp PLUS exp      		{ exp_sp (EOp(Plus, [$1; $3])) (Span.extend $1.espan $3.espan) }
    | exp LESS exp			{ exp_sp (EOp(Less, [$1; $3])) (Span.extend $1.espan $3.espan) }
    | exp AND exp			{ exp_sp (EOp(And, [$1; $3])) (Span.extend $1.espan $3.espan) }
    | exp OR exp			{ exp_sp (EOp(Or, [$1; $3])) (Span.extend $1.espan $3.espan) }
    | exp EQ exp			{ exp_sp (EOp(Eq, [$1; $3])) (Span.extend $1.espan $3.espan) }
    | NOT exp				{ exp_sp (EOp(Not, [$2])) (Span.extend $1 $2.espan) }
    | LPAREN exp RPAREN			{ $2 }

decl:
    | PRINTI exp SEMI                   { decl_sp (DPrinti $2) (Span.extend $1 $3) }
    | ty ID ASSIGN exp SEMI		{ decl_sp (DVar (snd $2, $1, $4)) (Span.extend $1.tspan $5) } 
    | HANDLE ID LPAREN params RPAREN LBRACE statement RBRACE
      	     	       	      	     	{ handler_sp (snd $2) $4 $7 (Span.extend $1 $8) }
    
decls:
    | decl                              { [$1] }
    | decl decls                        { $1::$2 }

param:
    | ty ID				{ (snd $2, $1) }

params:
    | param				{ [ $1 ] }
    | param COMMA params                { $1 :: $3 } 
    
statement:
    | matched				{ $1 }
    | unmatched				{ $1 }

matched:
    | IF LPAREN exp RPAREN LBRACE statement RBRACE ELSE LBRACE statement RBRACE
                                        { sifte_sp $3 $6 $10 (Span.extend $1 $11) }
    | statement0 { $1 }

unmatched:
    | IF LPAREN exp RPAREN LBRACE statement RBRACE ELSE unmatched
                                        { sifte_sp $3 $6 $9 (Span.extend $1 $9.sspan)}
    | IF LPAREN exp RPAREN LBRACE statement RBRACE
                                        { sifte_sp $3 $6 snoop (Span.extend $1 $7)}

statement0:
    | statement1			{ $1 }
    | statement1 statement0             { sseq_sp $1 $2 (Span.extend $1.sspan $2.sspan) }

statement1:
    | ID ASSIGN exp SEMI	        { sassign_sp (snd $1) $3 (Span.extend (fst $1) $4) }
    | PRINTI exp SEMI			{ sprinti_sp $2 (Span.extend $1 $3) }

prog:
    | decls EOF                         { $1 }
;
