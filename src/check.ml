(** Static checks. *)

(* modules *) (* {{{ *)
open Format

module U = Util
module PA = PropAst
module SA = SoolAst

(* }}} *)
(* environment and other utilities *) (* {{{ *)

exception Error

let warnings = ref []
let errors = ref []

let warn p m = warnings := (sprintf "@[%s: warning: %s@]" p m) :: !warnings
let error p c m = errors := (sprintf "@[%s: %s: %s@]" p c m) :: !errors
let fatal p c m = error p c m; raise Error

(* NOTE: Fields and methods live in different namespaces. *)
module type EnvironmentT = sig
  type t
  val make : (string, string) SA.program -> t (* TODO: (variable, value) *)
  val update_line : t -> int -> t
  val add_variables : t -> SA.declaration list -> t
  val position : t -> string
  val lookup_variable : t -> string -> SA.type_
  val lookup_field : t -> SA.type_ -> string -> SA.type_
  val lookup_method : t -> SA.type_ -> string -> (SA.type_ * SA.type_ list)
  (* TODO(rgrig): expose error&fatal and use them *)
end

module Environment : EnvironmentT = struct (* {{{ *)
  type t =
    { variables : SA.type_ U.StringMap.t
    ; fields_by_class : SA.type_ U.StringMap.t U.StringMap.t
    ; methods_by_class : (SA.type_ * SA.type_ list) U.StringMap.t U.StringMap.t
    ; line : int option }

  let position env = match env.line with
    | None -> "?"
    | Some n -> string_of_int n

  let error p c s = error (position p) c s
  let fatal p c s = fatal (position p) c s

  let check_type env = function
    | SA.Bool | SA.Unit -> ()
    | SA.AnyType _ -> failwith "SA.AnyType should not be created by parser."
    | SA.Class c ->
        if not (U.StringMap.mem c env.fields_by_class) then
          error env "class not declared" c

  let check_types_exist env =
    let check_type = check_type env in
    let check_method_type (r, args) =
      check_type r; List.iter check_type args in
    let map_check check vs = U.StringMap.iter (fun _ t -> check t) vs in
    map_check check_type env.variables;
    map_check (map_check check_type) env.fields_by_class;
    map_check (map_check check_method_type) env.methods_by_class

  let add_var vs { SA.declaration_variable = v; SA.declaration_type = t } =
    U.StringMap.add v t vs

  let add_vars = List.fold_left add_var

  let process_member (fs, ms) = function
    | SA.Field d -> (add_var fs d, ms)
    | SA.Method
        { SA.method_return_type = r
        ; SA.method_name = n
        ; SA.method_formals = args
        ; SA.method_body = _ }
      ->
        let gt x = x.SA.declaration_type in
        let args = List.map gt args in
        (fs, U.StringMap.add n (r, args) ms)

  let process_class (fbc, mbc) (cn, d) =
    let fs, ms = U.StringMap.empty, U.StringMap.empty in
    let fs, ms = List.fold_left process_member (fs, ms) d in
    (U.StringMap.add cn fs fbc, U.StringMap.add cn ms mbc)

  let make { SA.program_globals=gs; SA.program_classes=cs; _ } =
    let fbc, mbc = U.StringMap.empty, U.StringMap.empty in
    let fbc, mbc = List.fold_left process_class (fbc, mbc) cs in
    let env =
      { variables = add_vars U.StringMap.empty gs
      ; fields_by_class = fbc
      ; methods_by_class = mbc
      ; line = None } in
    check_types_exist env;
    env

  let update_line env line = { env with line = Some line }

  let add_variables env d =
    List.iter (fun x -> check_type env x.SA.declaration_type) d;
    { env with variables = add_vars env.variables d }

  let lookup_variable env id =
    try U.StringMap.find id env.variables
    with Not_found -> fatal env "undefined" id

  let get_class_info e m t =
    let n = match t with
      | SA.Class n -> n
      | _ -> fatal e "not a class" "?" in
    try U.StringMap.find n m
    with Not_found -> fatal e "undefined class" n

  let lookup_method env t m =
    let ms = get_class_info env env.methods_by_class t in
    try U.StringMap.find m ms
    with Not_found -> fatal env "method not found" m

  let lookup_field env t f =
    let fs = get_class_info env env.fields_by_class t in
    try U.StringMap.find f fs
    with Not_found -> fatal env "field not found" f
end (* }}} *)

let check_types_match env t1 t2 =
  let chk t1 t2 =
    if t1 <> t2 then
      let p = Environment.position env in
      let info =
        fprintf str_formatter "@[%a and %a@]"
            SA.pp_type t1 SA.pp_type t2; flush_str_formatter () in
      error p "type mismatch" info in
  let get_type = function SA.AnyType t -> t | t -> ref (Some t) in
  match get_type t1, get_type t2 with
    | {contents=Some s}, {contents=Some t} -> chk s t
    | {contents=None}, {contents=None} -> ()
    | {contents=(Some _ as s)}, ({contents=None} as t)
    | ({contents=None} as t), {contents=(Some _ as s)} -> t := s

(* }}} *)
(* typechecking of programs *) (* {{{ *)
(*
  For each type [SoolAst.t] there's a function [t : Ast.t-> type_].  Statements
  have the type [SA.Unit], except return. Composed statements, including lists,
  inherit the type of the first return they reach, and are [SA.Unit] otherwise.
 *)

let rec call env c =
  let expression = expression env in
  let check_types_match = check_types_match env in
  let string_of_class = function
    | SA.Class c -> c
    | _ ->
        fatal (Environment.position env) "expected class, not primitive" "" in
  let tr = expression c.SA.call_receiver in
  let tmr, tma = Environment.lookup_method env tr c.SA.call_method in
  let ta = List.map expression c.SA.call_arguments in
  c.SA.call_class <- Some (string_of_class tr);
  (try List.iter2 check_types_match tma ta
  with Invalid_argument _ ->
    fatal (Environment.position env) "wrong number of arguments" c.SA.call_method);
  (match c.SA.call_lhs with
    | Some l -> check_types_match tmr (expression (SA.Ref l))
    | _ -> ());
  SA.Unit

and while_ env
  { SA.while_pre_body = b1
  ; while_condition = c
  ; while_post_body = b2 }
=
  let check_types_match = check_types_match env in
  let body = body env in
  let expression = expression env in
  let t1 = body b1 in let t2 = body b2 in
  check_types_match (expression c) SA.Bool;
  check_types_match t1 t2; t1

and expression env =
  let expression x = expression env x in
  let check_types_match = check_types_match env in
  function
    | SA.Ac (_, es) ->
        let ts = List.map expression es in
        List.iter (check_types_match SA.Bool) ts; SA.Bool
    | SA.Bin (l, _, r) -> check_types_match (expression l) (expression r); SA.Bool
    | SA.Not e -> check_types_match (expression e) SA.Bool; SA.Bool
    | SA.Deref (e, f) -> Environment.lookup_field env (expression e) f
    | SA.Ref s -> Environment.lookup_variable env s
    | SA.Literal (_, t) -> SA.AnyType t

and allocate env a =
  let expression = expression env in
  let t = expression (SA.Ref a.SA.allocate_lhs) in
  a.SA.allocate_type <- Some t; SA.Unit

and statement env {PA.ast = ast; PA.line = line} =
  let env = Environment.update_line env line in
  let allocate = allocate env in
  let body = body env in
  let call = call env in
  let check_types_match = check_types_match env in
  let expression = expression env in
  match ast with
    | SA.Return e -> expression e
    | SA.Assignment (s, e) ->
        check_types_match (expression (SA.Ref s)) (expression e); SA.Unit
    | SA.Call c -> call c
    | SA.Allocate a -> allocate a
    | SA.While w -> while_ env w
    | SA.If (e, b) ->
        check_types_match (expression e) SA.Bool; body b

and body env (SA.Body (d, s)) =
  Util.map_find_not SA.Unit (statement (Environment.add_variables env d)) s

let method_ env
  { SA.method_return_type = r
  ; SA.method_name = _
  ; SA.method_formals = args
  ; SA.method_body = b }
=
  let env = Environment.add_variables env args in
  let tr = body env b in
  check_types_match env tr r

let class_ env (c, ds) =
  let f (fs, ms) = function
    | SA.Field f -> (f :: fs, ms)
    | SA.Method m -> (fs, m :: ms) in
  let fs, ms = List.fold_left f ([], []) ds in
  let env = Environment.add_variables env fs in
  let env = Environment.add_variables env
    [{SA.declaration_type=SA.Class c; SA.declaration_variable="this"}] in
  List.iter (method_ env) (List.rev ms)

(* }}} *)
(* static checks for properties *) (* {{{ *)

module PropertyChecks = struct
  let location = ref "<INTERNAL ERROR>" (* user should not see this *)
  let set_location = function
    | None -> location := "?"
    | Some l -> location := sprintf "%d" l
  let warn m = warn !location m

  let get_source e = e.PA.source
  let get_target e = e.PA.target

  let default_find d m x = try U.StringMap.find x m with Not_found -> d

  let adjacency_of_edges source target es =
    let f acc e =
      let s, t = source e, target e in
      let old = default_find [] acc s in
      U.StringMap.add s (t :: old) acc in
    default_find [] (List.fold_left f U.StringMap.empty es)

  let rec reachable_from g s =
    let r = ref U.StringSet.empty in
    let rec f s =
      if not (U.StringSet.mem s !r) then begin
        r := U.StringSet.add s !r;
        List.iter f (g s)
      end in
    f s; !r

  let check_unused_states p =
    let succ = adjacency_of_edges get_source get_target p.PA.transitions in
    let pred = adjacency_of_edges get_target get_source p.PA.transitions in
    let fs = reachable_from succ "start" in
    let te = reachable_from pred "error" in
    let collect get s = U.add_strings s (List.map get p.PA.transitions) in
    let all = collect get_target (collect get_source U.StringSet.empty) in
    let bad = U.StringSet.diff all (U.StringSet.inter fs te) in
    if not (U.StringSet.mem "start" all) then warn "missing start";
    if not (U.StringSet.mem "error" all) then warn "missing error";
    U.StringSet.iter (fun s -> warn (sprintf "unused state: %s" s)) bad

  let error_edge msg e vs = match U.StringSet.elements vs with
    | [] -> ()
    | vs ->
        U.pp_list ", " U.pp_string str_formatter vs;
        fprintf str_formatter " on edge %s->%s" e.PA.source e.PA.target;
        error !location msg (flush_str_formatter ())

  let check_linear_patterns p =
    let check_edge e =
      let see (seen, bad) x =
        if U.StringSet.mem x seen
        then (seen, U.StringSet.add x bad)
        else (U.StringSet.add x seen, bad) in
      let vs = PA.written_vars e in
      let _, vs =
        List.fold_left see (U.StringSet.empty, U.StringSet.empty) vs in
      error_edge "multiple bindings" e vs in
    List.iter check_edge p.PA.transitions

  let bindings =
    let f _ e = U.add_strings U.StringSet.empty (PA.written_vars e) in
    U.y (U.memo f)

  let count_states p =
    let f states e =
      U.StringSet.add (get_source e) (U.StringSet.add (get_target e) states) in
    U.StringSet.cardinal (List.fold_left f U.StringSet.empty p.PA.transitions)

  let bound_variables p =
    let outgoing = adjacency_of_edges get_source (fun e->e) p.PA.transitions in
    let m = ref (U.StringMap.add "start" U.StringSet.empty U.StringMap.empty) in
    let now, nxt = ref (U.StringSet.singleton "start"), ref U.StringSet.empty in
    for i = 2 to count_states p do begin
      let relax e =
        let s, t = get_source e, get_target e in
        nxt := U.StringSet.add t !nxt;
        let r = bindings e in
        let r = U.StringSet.union r (U.StringMap.find s !m) in
        (try
          let r = U.StringSet.inter r (U.StringMap.find t !m) in
          m := U.StringMap.add t r !m
        with Not_found ->
          m := U.StringMap.add t r !m) in
      U.StringSet.iter (fun v -> List.iter relax (outgoing v)) !now;
      now := !nxt; nxt := U.StringSet.empty
    end done;
    default_find U.StringSet.empty !m

  let check_bound_variables p =
    let bound = bound_variables p in
    let check_edge e =
      let guards = U.add_strings U.StringSet.empty (PA.read_vars e) in
      let bound_here = bound (get_source e) in
      let unbound = U.StringSet.diff guards bound_here in
      error_edge "possibly unbound" e unbound in
    List.iter check_edge p.PA.transitions

  let all p =
    set_location (Some p.PA.line);
    let p = p.PA.ast in
    check_unused_states p;
    check_linear_patterns p;
    check_bound_variables p
end

(* }}} *)
let program n p =
  warnings := []; errors := [];
  let env = Environment.make p in
  List.iter (class_ env) p.SA.program_classes;
  List.iter PropertyChecks.all p.SA.program_properties;
  (try match p.SA.program_main with
    | None -> ()
    | Some m -> ignore (body env m)
  with Error -> ());
  let pp l = List.iter (fun s -> eprintf "@[%s:%s@." n s) (List.rev l) in
  pp !errors; pp !warnings;
  if !errors <> [] then raise Error
