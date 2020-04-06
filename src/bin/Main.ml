open Batteries

(* the name of the file *)
let filename = Sys.argv.(1)

let main () =
  let input = open_in filename in
  let filebuf = Lexing.from_input input in
  try
    print_endline "Parsing ...";
    let ds = Parser.main Lexer.token filebuf in
    print_endline "Interpreting...";
    let _ = Interp.inter_decls ds in
    print_endline "Done";
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf)
  ;
  IO.close_in input

let _ = main ()

