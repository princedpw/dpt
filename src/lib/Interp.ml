open Syntax
open Batteries

(* Error Reporting *)
   
exception Error of string
let error s = raise (Error s)

(* Interpreter Values *)
            
(* Interpreter State *)
       
module Env = BatMap.Make(Cid)
module Q = BatQueue
module CidSet = BatSet.Make(Cid)
         
module State = struct
         
  type state = {
      locals: CidSet.t;
      env: ival Env.t;
      handlers: Syntax.body Env.t;
      packets: Syntax.packet Stream.t;
      events: event Q.t;}

  and ival =
  | V of Syntax.value
  | F of code
       
  and code = state -> Syntax.value list -> Syntax.value * state
                              
  let empty =
    {locals=CidSet.empty;
     env=Env.empty;
     handlers=Env.empty;
     packets=Stream.from (fun _ -> None);
     events=Q.create();}

  let mem_env cid st =
    Env.mem cid st.env

  let lookup k st =
    try Env.find k st.env with
      Not_found -> error ("missing variable: " ^ Cid.to_string k)
                 
  let update k v st =
    {st with env=Env.add k (V v) st.env}

  let add_local cid v st =
    if Env.mem cid st.env then
      error ("local variable " ^ Cid.to_string cid ^ "  already defined")
    else
      {st with locals=CidSet.add cid st.locals; env=Env.add cid (V v) st.env;}

  let remove_env_id cid env =
    Env.remove cid env
    
  let remove_locals st =
    let env = CidSet.fold (fun cid st -> remove_env_id cid st) st.locals st.env in
    {st with locals=CidSet.empty; env=env}
    
  let add_global cid v st =
    if Env.mem cid st.env then
      error ("global variable " ^ Cid.to_string cid ^ "  already defined")
    else
      {st with env=Env.add cid (V v) st.env;}

  let add_global_function cid f st =
    if Env.mem cid st.env then
      error ("global variable " ^ Cid.to_string cid ^ "  already defined")
    else
      {st with env=Env.add cid (F f) st.env;}
    
  let is_mutable cid st =
    CidSet.mem cid st.locals
    
  let add_handler cid lam st =
    {st with handlers=Env.add cid lam st.handlers}
    
  let get_handler cid st =
    try Some (Env.find cid st.handlers) with
      Not_found -> None

  let rec add_params params vs st =
    match params, vs with
    | [], [] -> st
    | (id,_)::params, v::vs ->
       add_params params vs {st with env=Env.add (Cid.id id) (V v) st.env}
    | _, _ -> error "mismatched parameters and event values"

  let rec remove_params params st =
    match params with
    | [] -> st
    | (id,_)::params ->
       remove_params params {st with env=Env.remove (Cid.id id) st.env}

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
        let kstr = Cid.to_string k in
        match v with
        | V v -> print_endline (kstr ^ "=" ^ (Printing.value_to_string v))
        | F _ -> print_endline (kstr ^ "=" ^ "<function>")
      ) st.env
          
end

(* Counter Module *)
       
let counter_name = "Counter"
let counter_id = Id.create counter_name
let counter_error fun_name msg = error (counter_name ^ ": " ^ fun_name ^ ": " ^ msg)

let counter_create_name = "create"
let counter_create_id = Id.create counter_create_name
let counter_create_cid = Cid.create_ids [counter_id; counter_create_id]
let counter_create_error msg = counter_error counter_create_name msg
let counter_create_fun st vs =
  match vs with
  | [{v=VInt _; vspan=_} as initial] ->
    let objid = Cid.fresh [counter_name; "v"] in
    let st = State.update objid initial st in
    (vobj counter_id objid, st)
  | [_] -> counter_create_error "initializer has wrong type"
  | _ -> counter_create_error "takes one parameter only"
  
let counter_add_name = "add"
let counter_add_id = Id.create counter_add_name
let counter_add_cid = Cid.create_ids [counter_id; counter_add_id]
let counter_add_error msg = counter_error counter_add_name msg
let counter_add_fun st vs =
  match vs with
  | [{v=VObj (id,objid); vspan=_}; {v=VInt i; vspan=_}] ->
     begin
       if not (Id.equals id counter_id) then
         counter_add_error "not a counter";
       if not (State.mem_env objid st) then
         counter_add_error (Cid.to_string objid ^ " not found");
       match State.lookup objid st with
       | V {v=VInt current; vspan=_} ->
          begin
            let res = vint (current + i) in
            let st = State.update objid res st in
            (res, st)
          end
       | V _ -> counter_add_error "stored value is not an integer"
       | F _ -> counter_add_error "stored value is a function rather than an integer"
     end
  | [_] -> counter_add_error "initializer has wrong type"
  | _ -> counter_add_error "takes one parameter only"
       
(* Initial state *)
             
let pervasives = [
    (counter_create_cid, counter_create_fun);
    (counter_add_cid, counter_add_fun);
  ]
    
let initial_state =
  let add_function st (cid, implementation) =
    State.add_global_function cid implementation st
  in
  List.fold_left add_function State.empty pervasives
  
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
  | EVal v -> (v, st)
  | EVar cid ->
     begin
       match State.lookup cid st with
       | V v -> (v, st)
       | F _ -> error (Cid.to_string cid ^
                         " is a function identifier and can only be used in call positions")
     end
  | EOp (op,es) ->
     let (vs, st) = interp_exps st es in
     (interp_op op vs, st)
  | ECall(cid, es) ->
     begin
       let (vs, st) = interp_exps st es in
       match State.lookup cid st with
       | State.V _ -> error (Cid.to_string cid ^
                         " is a value identifier and cannot be used in a call")
       | State.F f -> f st vs
     end
and interp_exps st es =
  let process_exp (vs,st) e =
      let (v, st) = interp_exp st e in
      (v::vs, st)
  in
  let vs, st = List.fold_left process_exp ([], st) es in
  (List.rev vs, st)

let rec interp_statement st s =
  match s.s with
  | SNoop -> st
  | SAssign (id, e) ->
     let cid =  Cid.id id in
     if not (State.mem_env cid st) then
       error ("variable " ^ Cid.to_string cid ^ " not defined")
     else if not (State.is_mutable cid st) then
       error ("variable " ^ Cid.to_string cid ^ " not mutable")
     else
       let v, st = interp_exp st e in
       State.update cid v st
  | SLocal (id, _, e) ->
     let cid = Cid.id id in
     let v, st = interp_exp st e in
     State.add_local cid v st
  | SPrinti e ->
     begin
       let v, st = interp_exp st e in
       print_endline (string_of_int (raw_int v));
       st
     end
  | SPrints s ->
     begin
       print_endline s;
       st
     end
  | SIf (e, ss1, ss2) ->
     let v, st = interp_exp st e in
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
       let v, st = interp_exp st e in
       print_endline (string_of_int (raw_int v));
       st
     end
  | DPrints s ->
     begin
       print_endline s;
       st
     end
  | DGlobal (id, _, e) ->
     begin
       let v, st = interp_exp st e in
       State.add_global (Cid.id id) v st
     end
  | DHandler (id, handler) ->
       State.add_handler (Cid.id id) handler st
     
let rec interp_decls st ds =
  match ds with
    [] -> st
  | d::ds ->
     begin
       let st = interp_decl st d in
       interp_decls st ds
     end

let handle st ev =
  match State.get_handler ev.eid st with
  | None ->
     begin
       Console.warning ("event " ^ Cid.to_string ev.eid ^ " has no handler");
       st
     end
  | Some (params, s) ->
     begin
       let st = State.add_params params ev.data st in
       let st = interp_statement st s in
       let st = State.remove_params params st in
       let st = State.remove_locals st in
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
  let st = interp_decls initial_state ds in
  let st = State.add_packets packets st in
  let st = interp_events st in
  print_endline "";
  print_endline "Final Environment:";
  State.print st
