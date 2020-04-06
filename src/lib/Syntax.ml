(* Abstract syntax of DPT *)

type var = Var.t
[@@deriving ord, eq]

type ty =
  | TBool
  | TInt
  | TEvent of ty list
[@@deriving ord, eq]

type op =
  | And
  | Or
  | Not
  | Eq
  | Less
  | Plus
[@@deriving ord, eq, show]

type v =
  | VBool of bool
  | VInt of int
  | VEvent of v list
[@@deriving ord]

and value =
  {v: v;
   vspan: Span.t [@compare fun _ _ -> 0];
  }
[@@deriving ord]

and e =
  | EVal of value
  | EOp of op * exp list
[@@deriving ord]

and exp =
  {e: e;
   espan: Span.t [@compare fun _ _ -> 0];
  }
[@@deriving ord]

type d =
  | DPrinti of exp
[@@deriving ord]
             
and decl =
  {d: d;
   dspan: Span.t [@compare fun _ _ -> 0];
  }
[@@deriving ord]
  
type decls = decl list


           
(* Useful constructors *)

let exp e =
  {e; espan= Span.default;}

let value v =
  {v; vspan= Span.default;}

let decl d = {d; dspan= Span.default;}
           

let exp_sp e span = {e; espan=span}

let value_sp v span = {v; vspan=span}

let decl_sp d span = {d; dspan=span}
                   

let vint_sp i span = value_sp (VInt i) span

let value_to_exp v = exp_sp (EVal v) v.vspan
