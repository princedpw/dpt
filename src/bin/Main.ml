open Batteries
open Dpt

let report s = print_endline s

let main () =
  let filename = Sys.argv.(1) in
  report "Parsing ...";
  let ds = Input.parse filename in
  report "Interpreting...";
  let _ = Interp.inter_decls ds in
  report "Done"

let _ = main ()

