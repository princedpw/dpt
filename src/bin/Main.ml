open Batteries

let report s = print_endline s

let main () =
  let filename = Sys.argv.(1) in
  report "Parsing ...";
  let ds = Dpt.Input.parse filename in
  report "Interpreting...";
  let _ = Dpt.Interp.inter_decls ds in
  report "Done"

let _ = main ()

