open Syntax
open Batteries
   
exception Error of string
let error s = raise (Error s)

(* Interpreter State *)

module K = struct
  type t = Id.t
  let compare = Id.compare
end
         
module Env = BatMap.Make(K)

module State = struct
  type state = {env: Syntax.value Env.t}
             
  let empty = {env=Env.empty}
            
  let lookup k s =
    try Env.find k s.env with
      Not_found -> error ("missing variable: " ^ Id.to_string k)
                 
  let update k v s = {env=Env.add k v s.env}

  let print s =
    Env.iter (fun k v ->
        let kstr = Id.to_string k in
        let vstr = Printing.value_to_string v in
        print_endline (kstr ^ ":" ^ vstr)) s.env
          
end

(* Interpreter Operations *)
             
let interp_op op vs =
  match op,vs with
  | And, [v1;v2] -> vbool (raw_bool v1 && raw_bool v2)
  | Or, [v1;v2] -> vbool (raw_bool v1 || raw_bool v2)
  | Not, [v] -> vbool (not (raw_bool v))
  | Eq, [v1;v2] -> vbool (raw_int v1 = raw_int v2)
  | Less, [v1;v2] -> vbool (raw_int v1 < raw_int v2)
  | Plus, [v1;v2] -> vint (raw_int v1 + raw_int v2)
  | _ -> error ("bad operator: " ^ Printing.op_to_string op
                ^ " with " ^ string_of_int (List.length vs) ^ " arguments")

let rec interp_exp s e =
  match e.e with
  | EVal v -> v
  | EVar id -> State.lookup id s
  | EOp (op,es) ->
     let vs = List.map (interp_exp s) es in
     interp_op op vs

let interp_decl s d =
  match d.d with
  | DPrinti e ->
     begin
       let v = interp_exp s e in
       print_endline (string_of_int (raw_int v));
       s
     end
  | DVar (id, _, e) ->
     begin
       let v = interp_exp s e in
       State.update id v s
     end
     
let rec interp_decls s ds =
  match ds with
    [] -> s
  | d::ds ->
     begin
       let s' = interp_decl s d in
       interp_decls s' ds
     end
       
let interp ds =
  let s = interp_decls State.empty ds in
  print_endline "";
  print_endline "Final Environment:";
  State.print s
