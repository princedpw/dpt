open Batteries

let process_line line =
  let error () = Console.error ("bad packet format:" ^ line) in
  match BatString.split_on_char ',' line with
  | [s1; s2] ->
     begin
       try
         let i1 = int_of_string s1 in
         let i2 = int_of_string s2 in
         Some [Syntax.vint i1; Syntax.vint i2;]
       with
         _ -> error ()
     end
  | _ -> error ()
   
let lines_of channel =
    Stream.from
      (fun _ ->
        try
          process_line (input_line channel)
        with
        | End_of_file -> close_in channel; None
        | exn -> close_in_noerr channel; raise exn)
   
let parse filename =
  lines_of (open_in filename)

