open Batteries

let debug s = print_endline s

let read ?(filename : string option = None) lexbuf =
  let get_info () =
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    (tok, line, cnum)
  in
  let err_header =
    match filename with
    | None -> Printf.sprintf "[Parser]"
    | Some s -> Printf.sprintf "[Parser] %s:" s
  in
  try Parser.prog Lexer.token lexbuf with
  | Failure x -> Console.error (Printf.sprintf "%s %s" err_header x) 
  | End_of_file -> Console.error (Printf.sprintf "%s end of file in comment" err_header)
  | _ ->
    let tok, line, cnum = get_info () in
    Console.error (
        Printf.sprintf "%s token: %s, line: %s, char: %s" err_header tok
          (string_of_int line) (string_of_int cnum)
      )

let read_from_file fname =
  let fin = open_in fname in
  let lexbuf = Lexing.from_channel fin in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname=fname};
  lexbuf.lex_start_p <- {lexbuf.lex_start_p with pos_fname=fname};
  let res = read ~filename:(Some fname) lexbuf in
  close_in fin ; res

let parse fname = read_from_file fname
