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

module Q = BatQueue

module State = struct
  type state = {env: Syntax.value Env.t;
                handlers: Syntax.body Env.t;
                packets: Syntax.packet Stream.t;
                events: event Q.t;}
             
  let empty =
    {env=Env.empty;
     handlers=Env.empty;
     packets=Stream.from (fun _ -> None);
     events=Q.create();}
            
  let lookup k st =
    try Env.find k st.env with
      Not_found -> error ("missing variable: " ^ Id.to_string k)
                 
  let update k v st =
    {st with env=Env.add k v st.env}

  let add_handler id lam st =
    {st with handlers=Env.add id lam st.handlers}
    
  let get_handler id st =
    try Some (Env.find id st.handlers) with
      Not_found -> None

  let rec add_params params vs st =
    match params, vs with
    | [], [] -> st
    | (id,_)::params, v::vs ->
       add_params params vs {st with env=Env.add id v st.env}
    | _, _ -> error "mismatched parameters and event values"

  let rec remove_params params st =
    match params with
    | [] -> st
    | (id,_)::params ->
       remove_params params {st with env=Env.remove id st.env}

  let push_event ev st = Q.push ev st.events
    
  let next_event st =
    try
      Some (Q.pop st.events)
    with
      Q.Empty -> None

  let add_packets packets st =
    {st with packets=packets}

  let next_packet st =
    try
      Some (Stream.next st.packets)
    with
      Stream.Failure -> None

  let print st =
    Env.iter (fun k v ->
        let kstr = Id.to_string k in
        let vstr = Printing.value_to_string v in
        print_endline (kstr ^ ":" ^ vstr)) st.env
          
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

let rec interp_exp st e =
  match e.e with
  | EVal v -> v
  | EVar id -> State.lookup id st
  | EOp (op,es) ->
     let vs = List.map (interp_exp st) es in
     interp_op op vs

let rec interp_statement st s =
  match s.s with
  | SNoop -> st
  | SAssign (id, e) ->
     let v = interp_exp st e in
     State.update id v st
  | SPrinti e ->
     begin
       let v = interp_exp st e in
       print_endline (string_of_int (raw_int v));
       st
     end
  | SPrints s ->
     begin
       print_endline s;
       st
     end
  | SIf (e, ss1, ss2) ->
     let v = interp_exp st e in
     if raw_bool v then
       interp_statement st ss1
     else
       interp_statement st ss2
  | SSeq (ss1, ss2) ->
     let st = interp_statement st ss1 in
     interp_statement st ss2
     
let interp_decl st d =
  match d.d with
  | DPrinti e ->
     begin
       let v = interp_exp st e in
       print_endline (string_of_int (raw_int v));
       st
     end
  | DPrints s ->
     begin
       print_endline s;
       st
     end
  | DVar (id, _, e) ->
     begin
       let v = interp_exp st e in
       State.update id v st
     end
  | DHandler (id, handler) ->
       State.add_handler id handler st
     
let rec interp_decls st ds =
  match ds with
    [] -> st
  | d::ds ->
     begin
       let st = interp_decl st d in
       interp_decls st ds
     end

let handle st ev =
  match State.get_handler ev.name st with
  | None ->
     begin
       Console.warning ("event " ^ Id.to_string ev.name ^ " has no handler");
       st
     end
  | Some (params, s) ->
     begin
       let st = State.add_params params ev.data st in
       let st = interp_statement st s in
       let st = State.remove_params params st in
       st 
     end
    
let rec interp_events st =
  match State.next_event st with
  | Some ev ->
     let st = handle st ev in
     interp_events st
  | None ->
     begin
       match State.next_packet st with
       | None -> st
       | Some pk ->
          let st = handle st (packet_in_event pk) in
          interp_events st
     end
  
let interp ds packets =
  let st = interp_decls State.empty ds in
  let st = State.add_packets packets st in
  let st = interp_events st in
  print_endline "";
  print_endline "Final Environment:";
  State.print st
