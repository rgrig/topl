(** Static checks. *)

(* modules *) (* {{{ *)
open Format

module U = Util
module PA = PropAst

(* }}} *)
(* environment and other utilities *) (* {{{ *)

exception Error

let warnings = ref []
let errors = ref []

let warn p m = warnings := (sprintf "@[%s: warning: %s@]" p m) :: !warnings
let error p c m = errors := (sprintf "@[%s: %s: %s@]" p c m) :: !errors
let fatal p c m = error p c m; raise Error

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
let properties n ps =
  warnings := []; errors := [];
  List.iter PropertyChecks.all ps;
  let pp l = List.iter (fun s -> eprintf "@[%s:%s@." n s) (List.rev l) in
  pp !errors; pp !warnings;
  if !errors <> [] then raise Error
