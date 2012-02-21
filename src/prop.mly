(* Rules for the properties. *)

%{
  type pattern =
    | Action of variable
    | GuardVar of variable
    | GuardCt of value
    | GuardAny

  type item =
    | I_message of string
    | I_observe of string
    | I_prefix of string
    | I_transitions of (string, variable, value) PA.transition list

  let is_action s = 'A' <= s.[0] && s.[0] < 'Z'
  let var = String.uncapitalize

  let re_of_glob_map =
    let f m (k, v) = U.CharMap.add k v m in
    List.fold_left f U.CharMap.empty
      [ '\\', "\\\\"
      ; '$', "\\$"
      ; '^', "\\^"
      ; '.', "\\."
      ; '+', "\\+"
      ; '?', "\\?"
      ; '[', "\\["
      ; ']', "\\]"
      ; '*', ".*"
      ; '{', "\\(\\("
      ; '}', "\\)\\)"
      ; ',', "\\)\\|\\(" ]

  let re_of_glob e g =
    let lg = String.length g in
    let r = Buffer.create (2 * lg) in
    let c = ref 0 in
    for i = 0 to lg - 1 do begin
      if g.[i] = '{' then incr c;
      if g.[i] = '}' then decr c;
      if !c < 0 then e ();
      try  Buffer.add_string r (U.CharMap.find g.[i] re_of_glob_map)
      with Not_found -> Buffer.add_char r g.[i]
    end done;
    if !c <> 0 then e (); (* syntax error *)
    let r = Buffer.contents r in
(*    printf "@[HERE: '%s' -> '%s'@." g r; *)
     r

  let mk_args_pattern a ps =
    { PA.tag_guard =
      { PA.event_type = None
      ; PA.method_name = ".*"
      ; PA.method_arity = match a with None -> None | Some x -> Some (succ x) }
        (* count the receiver *)
    ; PA.value_guards = ps }

  let any_tag_guard =
    { PA.tag_guard =
      { PA.event_type = None
      ; PA.method_name = ".*"
      ; PA.method_arity = None }
    ; PA.value_guards = [] }

  let mk_value_guards =
    let f acc i = function
      | GuardVar x -> PA.Variable (x, i) :: acc
      | GuardCt v -> PA.Constant (v, i) :: acc
      | _ -> acc in
    Util.fold_with_index f []

  let mk_action =
    let f acc i = function
      | Action v -> (v, i) :: acc
      | _ -> acc in
    Util.fold_with_index f []

  let mk_label g t vs =
    let g =
      { PA.tag_guard = { g.PA.tag_guard with PA.event_type = Some t }
      ; PA.value_guards = mk_value_guards vs } in
    PA.check_event_guard g;
    { PA.guard = g
    ; PA.action = mk_action vs }

  let mk_transitions s t lss =
    let f ls = { PA.source = s; PA.target = t; PA.labels = ls } in
    List.map f lss

  let split_items xs =
    U.map_option (function I_message x -> Some x | _ -> None) xs,
    U.map_option (function I_observe x -> Some x | _ -> None) xs,
    U.map_option (function I_prefix x -> Some x | _ -> None) xs,
    U.map_option (function I_transitions x -> Some x | _ -> None) xs

  let extract_message e n = function
    | [] -> sprintf "@[%s failed@]" n
    | [m] -> m
    | _ ->
        eprintf "@[ERROR: Property %s has more than one message.@." n; e ()

  let pp_or_re f p =
    let pp f s = fprintf f "\\(%s\\)" s in
    fprintf f "\\(%a\\)" (U.pp_list "\\|" pp) p

  let prefix_of_list = function
    | [] -> ""
    | p ->
        fprintf str_formatter "\\(%a\\.\\)?" pp_or_re p;
        flush_str_formatter ()

  let mk_property e n xs =
    let m, o, p, t = split_items xs in
    let p = prefix_of_list p in
    let pm m = Str.regexp (sprintf "^%s%s$" p m) in
    let ptg tg = { tg with PA.method_name = pm tg.PA.method_name } in
    let pg g = { g with PA.tag_guard = ptg g.PA.tag_guard } in
    let pl l = { l with PA.guard = pg l.PA.guard } in
    let pt t = { t with PA.labels = List.map pl t.PA.labels } in
    { PA.name = n
    ; PA.message = extract_message e n m
    ; PA.observable = Str.regexp
        (fprintf str_formatter "%a$" pp_or_re o; flush_str_formatter ())
    ; PA.transitions = List.map pt (List.concat t) }

%}

%%

%public property:
  PROPERTY n=ID LC xs=item* RC
  { mk_property (fun () -> $syntaxerror) n xs }

item:
    i=message
  | i=observe
  | i=prefix
  | i=transition
  { i }

observe:
    OBSERVE p=string_pattern { I_observe p }

prefix: PREFIX p=string_pattern { I_prefix p }

message: MESSAGE m=STRING { I_message m }

transition:
    s=ID ARROW t=ID COLON ls=separated_nonempty_list(COMMA, big_label)
    { I_transitions (mk_transitions s t ls) }

big_label:
    RETURN v=value_pattern ASGN g=label_rhs(any_value)
      { [mk_label g PA.Return [v]] }
  | RETURN g=label_rhs(any_value)
      { [mk_label g PA.Return []] }
  | CALL STAR ASGN g=label_rhs(value_pattern)
      { [mk_label g PA.Call g.PA.value_guards] }
  | CALL g=label_rhs(value_pattern)
      { [mk_label g PA.Call g.PA.value_guards] }
  | g=label_rhs(value_pattern)
      { [ mk_label g PA.Call g.PA.value_guards
        ; mk_label g PA.Return [GuardAny] ] }
  | v=value_pattern ASGN g=label_rhs(value_pattern)
      { [ mk_label g PA.Call g.PA.value_guards
        ; mk_label g PA.Return [v] ] }

label_rhs(ValuePattern):
    v=ValuePattern DOT m=method_pattern(ValuePattern)
      { { m with PA.value_guards = v :: m.PA.value_guards } }
  | STAR { any_tag_guard }

method_pattern(ValuePattern):
    m=string_pattern x=args_pattern(ValuePattern)
      { { x with PA.tag_guard = { x.PA.tag_guard with PA.method_name = m } } }
  | STAR { any_tag_guard }

args_pattern(ValuePattern):
    LB a=integer_pattern RB
      { mk_args_pattern a [] }
  | LP ps=separated_list(COMMA, ValuePattern) RP
      { mk_args_pattern (Some (List.length ps)) ps }

integer_pattern:
    n=NUMBER { Some n }
  | STAR { None }

string_pattern:
    x=ID { x }
  | x=CONSTANT { re_of_glob (fun () -> $syntaxerror) x }
  | STAR { ".*" }

value_pattern:
    x=ID
      { if is_action x then Action (var x) else GuardVar x }
  | v=CONSTANT
      { GuardCt v }
  | STAR
      { GuardAny }

any_value:
    STAR { GuardAny }

%%

(* TODO
  - Generate loop on vertex start? It's not completely clear that we *always*
    want it. We should record an example where it is not needed, or generate
    the loop.
  - Syntax highlight in various editors.
*)
