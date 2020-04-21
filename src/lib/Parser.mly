%{
  open Syntax
  open Batteries

%}

%token <Span.t * Id.t> ID
%token <Span.t * Id.t> GID
%token <Span.t * Integer.t> NUM
%token <Span.t * string> STRING
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
%token <Span.t> REPORTI
%token <Span.t> REPORTS
%token <Span.t> LPAREN
%token <Span.t> RPAREN
%token <Span.t> LBRACKET
%token <Span.t> RBRACKET
%token <Span.t> LBRACE
%token <Span.t> RBRACE
%token <Span.t> COMMA
%token <Span.t> DOT
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
    | TINT				{ ty_sp (TInt 32) $1 }
    | TBOOL				{ ty_sp TBool $1 }
    | ID				{ ty_sp (TId (snd $1)) (fst $1) }
    | TEVENT LBRACKET tylist RBRACKET   { ty_sp (TEvent $3) (Span.extend $1 $4) }

tylist:
    | ty				{ [ $1 ] }
    | ty COMMA tylist			{ $1::$3 }

cid:
    | ID				{ (fst $1, Cid.id (snd $1)) }
    | ID DOT cid                        { (Span.extend (fst $1) (fst $3),
                                            Compound (snd $1, snd $3) )  } 

exp:
    | cid			        { var_sp (snd $1) (fst $1) }			
    | NUM                               { value_to_exp (vint_sp (snd $1) (fst $1)) }
    | TRUE                              { value_to_exp (vbool_sp true $1) }
    | FALSE                             { value_to_exp (vbool_sp false $1) }
    | cid LPAREN args RPAREN            { call_sp (snd $1) ($3) (Span.extend (fst $1) $4) }
    | exp PLUS exp                      { op_sp Plus [$1; $3] (Span.extend $1.espan $3.espan) }     | exp LESS exp			{ op_sp Less [$1; $3] (Span.extend $1.espan $3.espan) }
    | exp AND exp			{ op_sp And [$1; $3] (Span.extend $1.espan $3.espan) }
    | exp OR exp			{ op_sp Or [$1; $3] (Span.extend $1.espan $3.espan) }
    | exp EQ exp			{ op_sp Eq [$1; $3] (Span.extend $1.espan $3.espan) }
    | NOT exp                           { op_sp Not [$2] (Span.extend $1 $2.espan) }
    | LPAREN exp RPAREN			{ $2 }

args:
    | exp                               { [$1] }
    | exp COMMA args                    { $1::$3 }
    
decl:
    | REPORTI exp SEMI                  { dprinti_sp $2 (Span.extend $1 $3) }
    | REPORTS STRING SEMI               { dprints_sp (snd $2) (Span.extend $1 $3) }
    | ty ID ASSIGN exp SEMI		{ dglobal_sp (snd $2) $1  $4 (Span.extend $1.tspan $5) } 
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
    | statement0			{ $1 }
    | statement0 statement              { sseq_sp $1 $2 (Span.extend $1.sspan $2.sspan) }
    
statement0:
    | matched				{ $1 }
    | unmatched				{ $1 }
    | statement1                        { $1 }

matched:
    | IF LPAREN exp RPAREN LBRACE statement RBRACE ELSE LBRACE statement RBRACE
                                        { sifte_sp $3 $6 $10 (Span.extend $1 $11) }

unmatched:
    | IF LPAREN exp RPAREN LBRACE statement RBRACE ELSE unmatched
                                        { sifte_sp $3 $6 $9 (Span.extend $1 $9.sspan)}
    | IF LPAREN exp RPAREN LBRACE statement RBRACE
                                        { sifte_sp $3 $6 snoop (Span.extend $1 $7)}

statement1:
    | ty ID ASSIGN exp SEMI             { slocal_sp (snd $2) $1 $4 (Span.extend $1.tspan $5) }
    | ID ASSIGN exp SEMI	        { sassign_sp (snd $1) $3 (Span.extend (fst $1) $4) }
    | REPORTI exp SEMI			{ sprinti_sp $2 (Span.extend $1 $3) }
    | REPORTS STRING SEMI	        { sprints_sp (snd $2) (Span.extend $1 $3) }

prog:
    | decls EOF                         { $1 }
;
