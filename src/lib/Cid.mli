(* Compound Identifiers *)

type t =
    Id of Id.t
  | Compound of Id.t * t

(* Constructors *)
                        
val create : string list -> t
val create_ids : Id.t list -> t
val fresh : string list -> t
val id : Id.t -> t 
val compound : Id.t -> t -> t

(* Destructors *)
                    
val to_string : t -> string
val names : t -> string list 
                        
(* Operations *)
      
val compare : t -> t -> int
val equals : t -> t -> bool
