(* Abstract syntax of DPT *)

type id = Id.t

type t =
  | TBool
  | TInt
  | TEvent of ty list
            
and ty = {t:t; tspan: Span.t}

type op =
  | And
  | Or
  | Not
  | Eq
  | Less
  | Plus
              
(* values *)
type v =
  | VBool of bool
  | VInt of int
  | VEvent of event

and event = {name:id; data:value list}
            
and value = {v: v; vspan: Span.t;}
          
(* expressions *)
type e =
  | EVal of value
  | EVar of id
  | EOp of op * exp list

and exp = {e: e; espan: Span.t;}
        
(* statements *)
type s =
  | SNoop
  | SAssign of id * exp
  | SPrinti of exp
  | SIf of exp * statement * statement
  | SSeq of statement * statement
and statement = {s:s; sspan:Span.t;}        

(* event handler bodies *)
type params = (id * ty) list
type body = params * statement
           
(* declarations *)
type d =
  | DPrinti of exp
  | DVar of id * ty * exp
  | DHandler of id * body
and decl = {d: d; dspan: Span.t;}

(* a program is a list of declarations *)
type decls = decl list

(********************************)
(* Constructors and Destructors *)
(********************************)

exception Error of string
let error s = raise (Error s)           
           
(* types *)
let ty_sp t span = {t; tspan=span;}
let ty t = {t; tspan=Span.default;}
         
(* values *)
let value_sp v span = {v; vspan=span;}
let value v = {v; vspan= Span.default;}
            
let vint i = value (VInt i)
let vbool b = value (VBool b)

let vint_sp i span = value_sp (VInt i) span
let vbool_sp b span = value_sp (VBool b) span

let raw_int v =
  match v.v with
    VInt i -> i
  | _ -> error "not integer"

let raw_bool v =
  match v.v with
    VBool b -> b
  | _ -> error "not boolean"

(* packets, events *)

(* a packet will be represented as a list of 2 values for now; src then dst *)
type packet = value list
       
let packet src dst =
  [vint src; vint dst]
                   
let src p =
  match p with
  | [src; _] -> raw_int src
  | _ -> error "bad packet; wrong number of values"
       
let dst p =
  match p with
  | [_; dst] -> raw_int dst
  | _ -> error "bad packet; wrong number of values"

(* packet_in event name *)
let packet_in = Id.create "PacketIn"

let packet_in_event p = {name=packet_in; data=p;}
       
(* expressions *)
let exp e = {e; espan= Span.default;}
let exp_sp e span = {e; espan=span;}
let value_to_exp v = exp_sp (EVal v) v.vspan
                  
(* declarations *)
let decl d = {d; dspan= Span.default;}
let decl_sp d span = {d; dspan=span;}

let handler_sp id p body span =
  decl_sp (DHandler (id, (p, body))) span

(* statements *)
let statement s = {s; sspan= Span.default;}
let statement_sp s span = {s; sspan=span;}

let snoop = statement SNoop
let sseq s1 s2 = statement (SSeq (s1, s2))
let sassign id e = statement (SAssign (id, e))
let sprinti e = statement (SPrinti e)
let sifte e s1 s2 = statement (SIf (e, s1, s2))

let snoop_sp span = statement_sp SNoop span
let sassign_sp id e span = statement_sp (SAssign (id, e)) span
let sseq_sp s1 s2 span = statement_sp (SSeq (s1, s2)) span
let sprinti_sp e span = statement_sp (SPrinti e) span
let sifte_sp e s1 s2 span = statement_sp (SIf (e, s1, s2)) span

