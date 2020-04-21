open Batteries
open Syntax

let rec sep s f xs =
  match xs with
  | [] -> ""
  | [x] -> f x
  | x :: y :: rest -> f x ^ s ^ sep s f (y :: rest)

let comma_sep f xs = sep "," f xs

let rec t_to_string t =
  match t with
  | TBool -> "bool"
  | TInt i -> "int" ^ string_of_int i
  | TId id -> Id.to_string id
  | TEvent tys -> "event[" ^ comma_sep ty_to_string tys ^ "]"
               
and ty_to_string t = t_to_string t.t
                   
let op_to_string op =
  match op with
  | And -> "&&"
  | Or -> "||"
  | Not -> "!"
  | Eq -> "=="
  | Less -> "<"
  | Plus -> "+"

let packet_to_string p =
  "{src=" ^ Integer.to_string (src p) ^ "; dst=" ^ Integer.to_string (dst p) ^ "}" 

let rec v_to_string v =
  match v with
  | VBool true -> "true"
  | VBool false -> "false"
  | VInt i -> Integer.to_string i
  | VObj (id, cid) -> Id.to_string id ^ "." ^ Cid.to_string cid
  | VEvent _ -> failwith "unimplemented vevent printer"
               
and value_to_string v = v_to_string v.v
  
let rec e_to_string e =
  match e with
  | EVal v -> v_to_string v.v
  | EVar cid -> Cid.to_string cid
  | EOp (op, [e]) -> op_to_string op ^ exp_to_string e
  | EOp (op, [e1;e2]) -> exp_to_string e1 ^ op_to_string op ^ exp_to_string e2
  | EOp (op, es) -> error ("wrong number of arguments (" ^
                             string_of_int (List.length es) ^
                               ") to " ^ op_to_string op)
  | ECall(cid, es) ->
     (Cid.to_string cid) ^ "(" ^ es_to_string es ^ ")"

and es_to_string es =
  match es with
    [] -> ""
  | [e] -> exp_to_string e
  | e::es -> exp_to_string e ^ "," ^ es_to_string es

and exp_to_string e = e_to_string e.e

let rec d_to_string d =
  match d with
  | DPrinti e -> "report_int " ^ exp_to_string e ^ ";\n"
  | DPrints s -> "report_string " ^ s ^ ";\n"
  | DGlobal (id, ty, e) -> ty_to_string ty ^ " " ^ Id.to_string id ^  " = " ^ exp_to_string e ^ ";\n"
  | DHandler (id, (params, s)) ->
     let _,_ = params, s in
     "handle " ^ Id.to_string id ^ " ... " (* TODO *)
             
and decl_to_string d = d_to_string d.d 
  
let rec decls_to_string ds =
  match ds with
  | [] -> ""
  | d::ds -> decl_to_string d ^ decls_to_string ds
