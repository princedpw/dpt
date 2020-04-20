(* Compound Identifiers *)

type t =
    Id of Id.t
  | Compound of Id.t * t

(* Constructors *)
                        
let rec create ss =
  match ss with
    [] -> Id (Id.create "")
  | [s] -> Id (Id.create s)
  | s::ss -> Compound (Id.create s, create ss)

let rec create_ids ids =
  match ids with
    [] -> Id (Id.create "")
  | [id] -> Id id
  | id::ids -> Compound (id, create_ids ids)
           
let rec fresh ss =
  match ss with
    [] -> Id (Id.fresh "")
  | [s] -> Id (Id.fresh s)
  | s::ss -> Compound (Id.create s, fresh ss)

let id id = Id id
let compound id cid = Compound (id, cid)

(* Destructors *)
                    
let rec to_string cid =
  match cid with
  | Id id -> Id.to_string id
  | Compound (id, cid) -> Id.to_string id ^ "." ^ to_string cid

let rec names cid =
  match cid with
    Id id -> [Id.name id]
  | Compound (id, cid) -> (Id.name id) :: (names cid)
                        
(* Operations *)
      
let rec compare cid1 cid2 =
    match cid1, cid2 with
    | Id i1, Id i2 -> Id.compare i1 i2
    | Compound (id1,cid1), Compound(id2, cid2) ->
       let i = Id.compare id1 id2 in
       if i = 0 then
         compare cid1 cid2
       else
         i
    | Id _, Compound _ -> -1
    | Compound _, Id _ -> 1

let equals cid1 cid2 =
  (compare cid1 cid2 = 0)
