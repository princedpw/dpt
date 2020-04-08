(* Abstract syntax of DPT *)

type id = Id.t

type ty =
  | TBool
  | TInt
  | TEvent of ty list

type op =
  | And
  | Or
  | Not
  | Eq
  | Less
  | Plus

(* raw value *)
type v =
  | VBool of bool
  | VInt of int
  | VEvent of v list

(* value with meta data *)
and value = {v: v; vspan: Span.t;}

(* expression *)
and e =
  | EVal of value
  | EOp of op * exp list

(* expression with meta data *)
and exp = {e: e; espan: Span.t;}

(* declaration *)
type d =
  | DPrinti of exp

(* declaration with meta data *)
and decl = {d: d; dspan: Span.t;}

(* a program is a list of declarations *)
type decls = decl list


           
(* Constructors *)

(* values *)
let value_sp v span = {v; vspan=span;}
let value v = {v; vspan= Span.default;}
            
let vint i = value (VInt i)
let vbool b = value (VBool b)

let vint_sp i span = value_sp (VInt i) span

(* expressions *)
let exp e = {e; espan= Span.default;}
let exp_sp e span = {e; espan=span;}
let value_to_exp v = exp_sp (EVal v) v.vspan
                  
(* declarations *)
let decl d = {d; dspan= Span.default;}
let decl_sp d span = {d; dspan=span;}

(* Destructors *)
exception Error of string
let error s = raise (Error s)

(* values *)
let raw_int v =
  match v.v with
    VInt i -> i
  | _ -> error "not integer"

let raw_bool v =
  match v.v with
    VBool b -> b
  | _ -> error "not boolean"
