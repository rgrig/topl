%{
  open Debug
  open Format

  module PA = PropAst
  module U = Util

  type variable = string
  type value = string

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
  let var = String.uncapitalize_ascii

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
    if log_re then printf "@[RE '%s' -> '%s'@." g r;
    r

  let mk_args_pattern a ps =
    let arity = match a with None -> (0, None) | Some x -> (x, a) in
    { PA.tag_guard =
      { PA.event_type = None
      ; PA.method_name = ".*"
      ; PA.method_arity = arity }
    ; PA.value_guards = ps }

  let prepend_arg_guard g p =
    let io = function None -> None | Some x -> Some (succ x) in
    let ip (x, y) = (succ x, io y) in
    let ia g =
      { g with
        PA.method_arity = ip g.PA.method_arity } in
    { PA.value_guards = g :: p.PA.value_guards
    ; PA.tag_guard = ia p.PA.tag_guard }

  let any_tag_guard =
    { PA.tag_guard =
      { PA.event_type = None
      ; PA.method_name = ".*"
      ; PA.method_arity = (0, None) }
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

  let mk_label t g rvs cvs =
    let rgs, ras = mk_value_guards rvs, mk_action rvs in
    let cgs, cas = mk_value_guards cvs, mk_action cvs in
    assert (t <> Some PA.Call || (rgs = [] && ras = []));
    assert (t <> Some PA.Return || (cgs = [] && cas = []));
    let mk t' xgs xas =
      if t = t' || t' = None || (t = None && (xgs <> [] || xas <> [])) then
        let g = PA.mk_event_guard
          { g.PA.tag_guard with PA.event_type = t' } xgs in
        [ { PA.guard = g; PA.action = xas } ]
      else [] in
    match mk (Some PA.Call) cgs cas @ mk (Some PA.Return) rgs ras with
      | [] -> mk None [] []
      | l -> l

  let mk_transitions s t lss =
    let f ls = { PA.source = s; PA.target = t; PA.labels = ls } in
    List.map f lss

  let split_items xs =
    U.map_option (function I_message x -> Some x | _ -> None) xs,
    U.map_option (function I_observe x -> Some x | _ -> None) xs,
    U.map_option (function I_prefix x -> Some x | _ -> None) xs,
    U.map_option (function I_transitions x -> Some x | _ -> None) xs

  let extract_message e n = function
    | [] -> n
    | [m] -> m
    | _ ->
        eprintf "@[ERROR: Property %s has more than one message.@." n; e ()

  let rec or_re b = function
    | [] -> ()
    | [x] -> Printf.bprintf b "\\(%s\\)" x
    | x :: xs -> Printf.bprintf b "\\(%s\\)\\|%a" x or_re xs

  let prefix_of_list xs =
    let b = Buffer.create 0 in
    Printf.bprintf b "\\(\\(%a\\)\\.\\)?" or_re xs;
    Buffer.contents b

  let exact_or_re xs =
    let b = Buffer.create 0 in
    Printf.bprintf b "^%a$" or_re xs;
    Buffer.contents b

  let mk_property e n xs =
    let m, o, p, t = split_items xs in
    let p = prefix_of_list p in
    let pm m = PA.mk_pattern (Printf.sprintf "^%s%s$" p m) in
    let ptg tg = { tg with PA.method_name = pm tg.PA.method_name } in
    let pg g = { g with PA.tag_guard = ptg g.PA.tag_guard } in
    let pl l = { l with PA.guard = pg l.PA.guard } in
    let pt t = { t with PA.labels = List.map pl t.PA.labels } in
    { PA.name = n
    ; PA.message = extract_message e n m
    ; PA.observable = PA.mk_pattern (exact_or_re o)
    ; PA.transitions = List.map pt (List.concat t) }

%}

%token <int> NUMBER
%token <string> CONSTANT
%token <string> ID
%token <string> STRING
%token ARROW
%token ASGN
%token CALL
%token COLON
%token COMMA
%token DOT
%token EOF
%token LB
%token LC
%token LP
%token MESSAGE
%token OBSERVE
%token PREFIX
%token PROPERTY
%token RB
%token RC
%token RETURN
%token RP
%token STAR

%start <(string, string) PropAst.t PropAst.with_line list> properties

%%

properties:
  | h=with_line(property) t=properties { h :: t }
  | EOF { [] }

with_line(X):
    x=X { { PA.ast = x; PA.line = $startpos.Lexing.pos_lnum } }

property:
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
      { mk_label (Some PA.Return) g [v] [] }
  | RETURN g=label_rhs(any_value)
      { mk_label (Some PA.Return) g [] [] }
  | CALL g=label_rhs(value_pattern)
      { mk_label (Some PA.Call) g [] g.PA.value_guards }
  | g=label_rhs(value_pattern)
      { mk_label None g [] g.PA.value_guards }
  | v=value_pattern ASGN g=label_rhs(value_pattern)
      { mk_label None g [v] g.PA.value_guards }

label_rhs(ValuePattern):
    v=ValuePattern DOT m=method_pattern(ValuePattern) { prepend_arg_guard v m }
  | v=ValuePattern DOT STAR { prepend_arg_guard v any_tag_guard }
  | m=method_pattern(ValuePattern) { m }
  | STAR { any_tag_guard }

method_pattern(ValuePattern):
    m=string_pattern x=args_pattern(ValuePattern)
      { { x with PA.tag_guard = { x.PA.tag_guard with PA.method_name = m } } }

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

