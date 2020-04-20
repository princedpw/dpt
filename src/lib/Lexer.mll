
{
  open Batteries
  open Parser
  open Printf
  open Span
  exception Eof

  let position lexbuf =
    {fname=(lexbuf.Lexing.lex_start_p).pos_fname; start=Lexing.lexeme_start lexbuf; finish=Lexing.lexeme_end lexbuf}

  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      { pos with Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
                 Lexing.pos_bol = pos.Lexing.pos_cnum; } ;;

  let extract_string s =
    let len = String.length s in
    String.sub s 1 (len-2)
}

let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let gid = ['A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let symbol = ['~' '`' '!' '@' '#' '$' '%' '^' '&' '|' ':' '?' '>' '<' '[' ']' '=' '-' '.']+
let num = ['0'-'9']+
let tid = ['\'']['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let wspace = [' ' '\t']
let filename = "\""(['a'-'z' 'A'-'Z' '0'-'9' '_' '\\' '/' '.' '-'])+"\""
let str = "\""['a'-'z' 'A'-'Z' '_' '0'-'9' '~' '!' '@' '#' '$' '%' '^' '&' '|' ':' '?' '>' '<' '[' ']' '=' '-' '.' ' ']*"\""

rule token = parse
  | "(*"              { comments 0 lexbuf }
  | "false"           { FALSE (position lexbuf) }
  | "true"            { TRUE (position lexbuf) }
  | "if"              { IF (position lexbuf) }
  | "else"            { ELSE (position lexbuf) }
  | "int"             { TINT (position lexbuf) }
  | "bool"            { TBOOL (position lexbuf) }
  | "event"           { TEVENT (position lexbuf) }
  | "report_int"      { REPORTI (position lexbuf) }
  | "report_string"   { REPORTS (position lexbuf) }
  | "handle"	      { HANDLE (position lexbuf) }
  | id as s           { ID (position lexbuf, Id.create s) }
  | gid as s          { GID (position lexbuf, Id.create s) }	
  | num as n          { NUM (position lexbuf, int_of_string n) }
  | "+"               { PLUS (position lexbuf) }
  | "!"               { NOT (position lexbuf) }
  | "&&"              { AND (position lexbuf) }
  | "||"              { OR (position lexbuf) }
  | "=="              { EQ (position lexbuf) }
  | "<"               { LESS (position lexbuf) }
  | ";"               { SEMI (position lexbuf) }
  | "("               { LPAREN (position lexbuf) }
  | ")"               { RPAREN (position lexbuf) }
  | "="               { ASSIGN (position lexbuf) } 
  | "{"               { LBRACE (position lexbuf) }
  | "}"               { RBRACE (position lexbuf) }
  | ","               { COMMA (position lexbuf) }
  | "."               { DOT (position lexbuf) }
  | wspace            { token lexbuf }
  | '\n'              { incr_linenum lexbuf; token lexbuf}
  | str as s          { STRING (position lexbuf, extract_string s) }
  | _ as c            { printf "[Parse Error] Unrecognized character: %c\n" c; token lexbuf }
  | eof		            { EOF }

and comments level = parse
  | "*)"  { if level = 0 then token lexbuf else comments (level-1) lexbuf }
  | "(*"  { comments (level+1) lexbuf }
  | '\n'  { incr_linenum lexbuf; comments level lexbuf}
  | _     { comments level lexbuf }
  | eof   { raise End_of_file }
