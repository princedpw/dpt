open Syntax
   
exception Error of string
let error s = raise (Error s)
                 
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

let rec interp_exp e =
  match e.e with
  | EVal value -> value 
  | EOp (op,es) ->
     let vs = List.map interp_exp es in
     interp_op op vs

let interp_decl d =
  match d.d with
  | DPrinti e ->
     let v = interp_exp e in
     print_endline (string_of_int (raw_int v))
     
let interp_decls ds =
  List.iter interp_decl ds
