open Batteries
open Syntax

let rec sep s f xs =
  match xs with
  | [] -> ""
  | [x] -> f x
  | x :: y :: rest -> f x ^ s ^ sep s f (y :: rest)

let comma_sep f xs = sep "," f xs

let op_to_string op =
  match op with
  | And -> "&&"
  | Or -> "||"
  | Not -> "!"
  | Eq -> "=="
  | Less -> "<"
  | Plus -> "+"

let rec v_to_string v =
  match v with
  | VBool true -> "true"
  | VBool false -> "false"
  | VInt i -> int_to_string i
  | VEvent vs -> fail_with "unimplemented vevent printer"
               
and value_to_string {v=v; vspan=_;} = v_to_string v
  
let rec e_to_string e =
  match e with
  | EVal v -> v_to_string v 
  | EOp (op, [e]) -> op_to_string op ^ exp_to_string e
  | EOp (op, [e1,e2]) -> exp_to_string e1 ^ op_to_string op ^ exp_to_string e2

and exp_to_string {e=e; espan=_;} = e_to_string e

let d_to_string d =
  match d with
  | DPrinti e -> "printi " ^ exp_to_string e ^ ";"
             
and decl_to_string {d=d; dspan=_;} = d_to_string d 
  
let decls_to_string ds =
  match ds with
  | [] -> ""
  | d::ds -> decl_to_string d ^ decls_to_string ds
