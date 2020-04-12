module T = ANSITerminal
exception Error of string

val error : string -> 'a

val warning : string -> unit

val report : string -> unit
  
val show_message : string -> T.color -> string -> unit
