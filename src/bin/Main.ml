open Batteries
open Dpt

let report s = print_endline s

let main () = (* report s1; report s2 *)
  let filename = Sys.argv.(1) in
  report "Parsing ...";
  let ds = Input.parse filename in
  report "Interpreting...";
  let _ = Interp.interp ds in
  report "Done" 

let _ = main ()

