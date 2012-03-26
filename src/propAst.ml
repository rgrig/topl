(* modules *) (* {{{ *)
open Format

module U = Util

(* }}} *)
(* data types *) (* {{{ *)
type 'a with_line = { ast : 'a; line : int }

type vertex = string

(* p_str = Str.regexp p_re *)
type pattern =
  { p_re : Str.regexp
  ; p_string : string }

type ('variable, 'value) value_guard =
  | Variable of 'variable * int
  | Constant of 'value * int

type event_type =
  | Call
  | Return

type ('event_type, 'method_name, 'method_arity) tag =
  { event_type : 'event_type
  ; method_name : 'method_name
  ; method_arity : 'method_arity }
  (* Common shape for [tag_guard] and [event_tag]. *)

type 'method_name tag_guard =
  ((event_type option), 'method_name, (int option)) tag

type event_tag = (event_type, string, int) tag

let check_event_tag t =
  assert (t.method_name <> "");
  assert (0 <= t.method_arity)

type ('method_name, 'value_guard) event_guard =
  { tag_guard : 'method_name tag_guard
  ; value_guards : 'value_guard list }

let check_event_guard g =
  let chk n = function Variable (_, m) | Constant (_, m) ->
    assert (0 <= m && m < n) in
  let chk_all n = List.iter (chk n) g.value_guards in
  U.option () chk_all g.tag_guard.method_arity

type 'variable action = ('variable * int) list

type 'value event =
  { event_tag : event_tag
  ; event_values : 'value U.IntMap.t }
  (* I'm using an IntMap rather than an array because I prefer immutability.
    Performance is unlikely to be a problem as the typical size is <5. *)

type ('method_name, 'value_guard, 'variable) label_p =
  { guard : ('method_name, 'value_guard) event_guard
  ; action : 'variable action }
  (* used by [Parser] *)

type ('method_name, 'variable, 'value) label =
  ('method_name, ('variable, 'value) value_guard, 'variable) label_p

type ('method_name, 'variable, 'value) transition =
  { source : vertex
  ; target : vertex
  ; labels : ('method_name, 'variable, 'value) label list }

type ('variable, 'value) t =
  { name : string
  ; message : string
  ; observable : pattern
  ; transitions: (pattern, 'variable, 'value) transition list }
(* }}} *)
(* utilities *) (* {{{ *)
let mk_pattern s =
  { p_re = Str.regexp s; p_string = s }

let pattern_matches p s =
  Str.string_match p.p_re s 0

let wvars l =
  List.map fst l.action

let rvars l =
  let f = function Variable (x, _) -> Some x | _ -> None in
  U.map_option f l.guard.value_guards

let vars_of_edge f e =
  List.concat (List.map f e.labels)

let written_vars t = vars_of_edge wvars t
let read_vars t = vars_of_edge rvars t

let mk_event et mn ma vs =
  { event_tag =
    { event_type = et
    ; method_name = mn
    ; method_arity = ma }
  ; event_values =
      let f (i, acc) v = (succ i, U.IntMap.add i v acc) in
      snd (List.fold_left f (0, U.IntMap.empty) vs) }

let mk_event_guard t v =
  let r = { tag_guard = t; value_guards = v } in
  check_event_guard r; r

let edge_length e = List.length e.labels

let outgoing a src =
  List.filter (fun e -> e.source = src) a.transitions

let guards_of_automaton {transitions=ts; _ } =
  let gol acc l = l.guard :: acc in
  let goe acc e = List.fold_left gol acc e.labels in
  List.fold_left goe [] ts

let get_tag_guards p =
  let gs = guards_of_automaton p in
  List.map (fun x -> x.tag_guard) gs

let get_value_guards p =
  let gs = guards_of_automaton p in
  List.concat (List.map (fun x -> x.value_guards) gs)

let pp_event_type f = function
  | Call -> fprintf f "call"
  | Return -> fprintf f "return"

(* TODO(rgrig): remove? *)
let ok_automaton =
  { name = "AlwaysOk"
  ; message =
      "internal error: ok_automaton should be happy with all programs"
  ; observable = mk_pattern "^$"
  ; transitions = [] }

(* }}} *)
