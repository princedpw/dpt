open Batteries
open Dpt

let default_packet_file = "packets"

let get_packets filename =
  match filename with
  | None ->
     if Sys.file_exists default_packet_file then
       PacketSource.parse default_packet_file
     else
       Stream.from (fun _ -> None)
  | Some name ->
     if Sys.file_exists name then
       PacketSource.parse name
     else
       Console.error ("packets file " ^ name ^ " not found")
    
let main () = (* report s1; report s2 *)
  let argc = Array.length Sys.argv in
  if argc < 2 || argc > 3 then
    Console.error "usage: dpt program [packet_file]"
  else
    begin
      let program = Sys.argv.(1) in
      let packet_file =
        if argc < 3 then
          None
        else
          Some Sys.argv.(2) in
      let packets = get_packets packet_file in
      Console.report "Parsing ...";
      let ds = Input.parse program in
      Console.report "Interpreting...";
      let _ = Interp.interp ds packets in
      Console.report "Done" 
    end
  
let _ = main ()

