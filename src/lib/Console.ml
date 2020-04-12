module T = ANSITerminal
exception Error of string
                 
let show_message msg color label =
  T.print_string [] "\n" ;
  T.print_string [T.Foreground color; T.Bold] (label ^ ": ") ;
  Printf.printf "%s" msg ;
  T.print_string [] "\n"
                 
let error msg =
  show_message msg T.Red "error" ;
  raise (Error msg)

let warning msg = show_message msg T.Yellow "warning"

let report msg = show_message msg T.Black "dpt"
