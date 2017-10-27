(* modules *) (* {{{ *)
open Debug
open Format
open Util.Operators

module B = BaristaLibrary
module BC = BaristaLibrary.Coder
module BD = BaristaLibrary.Descriptor
module BH = BaristaLibrary.HighTypes
module BU = BaristaLibrary.Utils
module PA = PropAst
module U = Util

(* }}} *)
(* globals *) (* {{{ *)
let out_dir = ref "out"
let using_infer = ref false

(* }}} *)
(* used to communicate between conversion and instrumentation *) (* {{{ *)
type method_ =  (* TODO: Use [PropAst.event_tag] instead? *)
  { method_name : string
  ; method_arity : int }

(* }}} *)
(* representation of automata in Java *) (* {{{ *)

(*
  The instrumenter has three phases:
    - convert the automaton to an intermediate representation
    - instrument the bytecode
    - emit the Java representation of the automaton
  A pattern like "c.m()" in the property matches method m in all classes that
  extend c (including c itself). For efficiency, the Java automaton does not
  know anything about inheritance. While the bytecode is instrumented, all the
  methods m in classes extending c get unique identifiers and the pattern
  "c.m()" is mapped to the set of those identifiers.

  The (first) conversion
    - goes from edge list to adjacency list
    - glues all input properties into one
    - changes the vertex representation from strings to integers
    - changes automaton variable representation from strings to integers
    - normalizes method patterns (by processing "using prefix", ... )
    - collects all patterns
  During printing a bit more processing is needed to go to the Java
  representation, but only very simple stuff.
 *)

(* shorthands for old types, those that come from prop.mly *)
type property = (string, string) PA.t
type tag_guard = PA.pattern PA.tag_guard

(* shorthands for new types, those used in Java *)
type tag = int
type vertex = int
type variable = int
type value = string (* Java literal *)

type transition =
  { steps : (PA.pattern, variable, value) PA.label list
  ; target : vertex }

type vertex_data =
  { vertex_property : property
  ; vertex_name : PA.vertex
  ; outgoing_transitions : transition list }

type ev_argtype = (int * BD.non_void_java_type)

type automaton =
  { vertices : vertex_data array
  ; observables : (property, tag_guard) Hashtbl.t
  ; pattern_tags : (tag_guard, tag list) Hashtbl.t
  (* The keys of [pattern_tags] are filled in during the initial conversion,
    but the values (the tag list) are filled in during code instrumentation. *)
  ; event_names : (tag, string) Hashtbl.t
  ; event_argtypes : (tag, ev_argtype list) Hashtbl.t }

(* following function produces an automaton with epsilon transitions *)

let check_automaton x =
  let rec is_decreasing x = function
    | [] -> true
    | y :: ys -> x >= y && is_decreasing y ys in
  let ok _ v = assert (is_decreasing max_int v) in
  Hashtbl.iter ok x.pattern_tags

(* }}} *)
(* small functions that help handling automata *) (* {{{ *)
let to_ints xs =
  let h = Hashtbl.create 101 in
  let c = ref (-1) in
  let f x = if not (Hashtbl.mem h x) then (incr c; Hashtbl.add h x !c) in
  List.iter f xs; h

let inverse_index f h =
  let r = Array.make (Hashtbl.length h) None in
  let one k v = assert (r.(v) = None); r.(v) <- Some (f k) in
  Hashtbl.iter one h;
  Array.map U.from_some r

let get_properties x =
  x.vertices >> Array.map (fun v -> v.vertex_property) >> Array.to_list

let get_vertices p =
  let f acc t = t.PA.source :: t.PA.target :: acc in
  "start" :: "error" :: List.fold_left f [] p.PA.transitions

(* }}} *)
(* pretty printing to Java *) (* {{{ *)

type 'a pp_index =
  { to_int : 'a -> int
  ; of_int : int -> 'a
  ; count : int }

type pp_full_index =
  { idx_constant : value pp_index
  ; idx_string : string pp_index }

let array_foldi f z xs =
  let r = ref z in
  for i = 0 to Array.length xs - 1 do r := f !r i xs.(i) done;
  !r

let starts x =
  let f ks k = function
    | {vertex_name="start";_} -> k :: ks
    | _ -> ks in
  array_foldi f [] x.vertices

let errors x =
  let f = function
    | {vertex_name="error"; vertex_property={PA.message=e;_};_} -> Some e
    | _ -> None in
  x.vertices >> Array.map f >> Array.to_list

let rec pp_v_list pe ppf = function
  | [] -> ()
  | [x] -> fprintf ppf "@\n%a" pe x
  | x :: xs -> fprintf ppf "@\n%a," pe x; pp_v_list pe ppf xs

let pp_int f x = fprintf f "%d" x
let pp_string f x = fprintf f "%s" x
let pp_constant_as_int i f c = pp_int f (i.idx_constant.to_int c)
let pp_string_as_int i f s =
  let v = (match s with None -> (-1) | Some s -> i.idx_string.to_int s) in
  pp_int f v
let pp_list pe f x =
  fprintf f "@[<2>%d@ %a@]" (List.length x) (U.pp_list " " pe) x
let pp_array pe f x = pp_list pe f (Array.to_list x)

let pp_value_guard index f = function
  | PA.Variable (v, i) -> fprintf f "0 %d %d" i v
  | PA.Constant (c, i) -> fprintf f "1 %d %a" i (pp_constant_as_int index) c

let pp_pattern tags f p =
  fprintf f "%a" (pp_list pp_int) (Hashtbl.find tags p)

let pp_condition index = pp_list (pp_value_guard index)

let pp_assignment f (x, i) =
  fprintf f "%d %d" x i

let pp_guard tags index f { PA.tag_guard = p; PA.value_guards = cs } =
  fprintf f "%a %a" (pp_pattern tags) p (pp_condition index) cs

let pp_action = pp_list pp_assignment

let pp_step tags index f { PA.guard = g; PA.action = a } =
  fprintf f "%a %a" (pp_guard tags index) g pp_action a

let pp_transition tags index f { steps = ss; target = t } =
  fprintf f "%a %d" (pp_list (pp_step tags index)) ss t

let pp_vertex tags index f v =
  fprintf f "%a %a"
    (pp_string_as_int index) (Some v.vertex_name)
    (pp_list (pp_transition tags index)) v.outgoing_transitions

let list_of_hash h =
  let r = ref [] in
  for i = Hashtbl.length h - 1 downto 0 do
    r := (try Some (Hashtbl.find h i) with Not_found -> None) :: !r
  done;
  !r

let pp_automaton index f x =
  let obs_p p = Hashtbl.find x.pattern_tags (Hashtbl.find x.observables p) in
  let iop = to_ints (get_properties x) in
  let poi = inverse_index U.id iop in
  let pov =
    Array.map (fun v -> Hashtbl.find iop v.vertex_property) x.vertices in
  let obs_tags = Array.to_list (Array.map obs_p poi) in
  fprintf f "%a@\n" (pp_list pp_int) (starts x);
  fprintf f "%a@\n" (pp_list (pp_string_as_int index)) (errors x);
  fprintf f "%a@\n" (pp_array (pp_vertex x.pattern_tags index)) x.vertices;
  fprintf f "%a@\n" (pp_array pp_int) pov;
  fprintf f "%a@\n" (pp_list (pp_list pp_int)) obs_tags;
  fprintf f "%a@\n"
    (pp_list (pp_string_as_int index)) (list_of_hash x.event_names)

let mk_pp_index p =
  let mk () =
    let m = Hashtbl.create 0 in
    let c = ref (-1) in
    let add x = if not (Hashtbl.mem m x) then Hashtbl.add m x (incr c; !c) in
    add, m, c in
  let mk_idx m c =
    { to_int = Hashtbl.find m
    ; of_int = (let a = inverse_index U.id m in fun i -> a.(i))
    ; count = succ !c } in
  let add_c, ioc, cc = mk () in
  let add_s, ios, sc = mk () in
  let value_guard = function PA.Constant (c, _) -> add_c c | _ -> () in
  let event_guard g = List.iter value_guard g.PA.value_guards in
  let label l = event_guard l.PA.guard in
  let transition t = List.iter label t.steps in
  let vertex_data v =
    add_s v.vertex_name; List.iter transition v.outgoing_transitions in
  Array.iter vertex_data p.vertices;
  U.hashtbl_fold_values (fun en () -> add_s en) p.event_names ();
  List.iter (function None -> () | Some s -> add_s s) (errors p);
  { idx_constant = mk_idx ioc cc
  ; idx_string = mk_idx ios sc }

let pp_constants_table j i =
  let constants =
    let rec ct n =
      if n = i.idx_constant.count
      then []
      else i.idx_constant.of_int n :: ct (succ n) in
    ct 0 in
  let pp_ext f e =
    fprintf f "@[\"topl\"@ + java.io.File.separator@ + \"Property.%s\"@]" e in
  fprintf j "@[";
  fprintf j "package topl;@\n";
  fprintf j "@[<2>public class Property {";
  fprintf j "@\n@[<2>public static final Object[] constants =@ ";
  fprintf j   "new Object[]{%a@]};" (pp_v_list pp_string) constants;
  fprintf j "@\npublic static Checker checker = null;";
  fprintf j "@\n@[<2>public static void check(Checker.Event event) {";
  fprintf j   "@\n@[<2>if (checker != null) {";
  fprintf j     "@\nchecker.check(event);";
  fprintf j   "@]@\n}";
  fprintf j "@]@\n}";
  fprintf j "@\n@[<2>public static void start() {";
  fprintf j   "@\n@[<2>if (checker == null) {";
  fprintf j     "@\n@[<2>checker =@ Checker.Parser.checker(%a,@ %a,@ constants);@]" pp_ext "text"  pp_ext "strings";
  fprintf j   "@\nchecker.historyLength = 10;";
  fprintf j   "@\nchecker.statesLimit = 10;";
  fprintf j   "@\nchecker.captureCallStacks = false;";
  fprintf j   "@\nchecker.onlyLogEvents = false;";
  fprintf j   "@\nchecker.automatonLog = null; // type is PrintWriter";
  fprintf j   "@\nchecker.selectionStrategy = Checker.SelectionStrategy.NEWEST;";
  fprintf j   "@]@\n}";
  fprintf j "@]@\n}";
  fprintf j "@\n@[<2>public static void stop() {";
  fprintf j   "@\n@[<2>if (checker.automatonLog != null) {";
  fprintf j     "@\nchecker.automatonLog.flush();";
  fprintf j   "@]@\n}";
  fprintf j   "@\nchecker = null;";
  fprintf j "@]@\n}";
  fprintf j "@]@\n}@]"

let pp_strings_nonl f index =
  for i = 0 to pred index.idx_string.count do
    let s = index.idx_string.of_int i in
    assert (not (String.contains s '\n'));
    Printf.fprintf f "%s\n" s
  done

let generate_checkers_for_runtime out_dir extra_fs p =
  check_automaton p;
  let (/) = Filename.concat in
  U.cp_r (Config.topl_dir/"src"/"topl") out_dir;
  let topl_dir = out_dir/"topl" in
  let o ext = U.open_formatter (topl_dir/("Property."^ext)) in
  let (jc, j), (tc, t) = o "java", o "text" in
  let sc = open_out (topl_dir/"Property.strings") in
  let index = mk_pp_index p in
  fprintf j "@[%a@]@." pp_constants_table index;
  pp_strings_nonl sc index;
  fprintf t "@[%a@]@." (pp_automaton index) p;
  List.iter close_out_noerr [jc; tc; sc];
  U.compile out_dir out_dir out_dir ((topl_dir/"Property.java") :: extra_fs)

(* }}} *)
(* conversion to Java representation *) (* {{{ *)

let index_for_var ifv v =
  try
    Hashtbl.find ifv v
  with Not_found ->
    let i = Hashtbl.length ifv in
      Hashtbl.replace ifv v i; i

let transform_tag_guard ptags tg =
  Hashtbl.replace ptags tg []; tg

let transform_value_guard ifv = function
  | PA.Variable (v, i) -> PA.Variable (index_for_var ifv v, i)
  | PA.Constant (c, i) -> PA.Constant (c, i)

let transform_guard ifv ptags {PA.tag_guard=tg; PA.value_guards=vgs} =
  { PA.tag_guard = transform_tag_guard ptags tg
  ; PA.value_guards = List.map (transform_value_guard ifv) vgs }

let transform_condition ifv (store_var, event_index) =
  let store_index = index_for_var ifv store_var in
    (store_index, event_index)

let transform_action ifv a = List.map (transform_condition ifv) a

let transform_label ifv ptags {PA.guard=g; PA.action=a} =
  { PA.guard = transform_guard ifv ptags g
  ; PA.action = transform_action ifv a }

let transform_properties ps =
  let vs p = p >> get_vertices >> List.map (fun v -> (p, v)) in
  let iov = to_ints (ps >>= vs) in
  let mk_vd (p, v) =
    { vertex_property = p
    ; vertex_name = v
    ; outgoing_transitions = [] } in
  let full_p =
    { vertices = inverse_index mk_vd iov
    ; observables = Hashtbl.create 0
    ; pattern_tags = Hashtbl.create 0
    ; event_names = Hashtbl.create 0
    ; event_argtypes = Hashtbl.create 0 } in
  let add_obs_tags p =
    let obs_tag =
      { PA.event_type = None
      ; PA.method_name = p.PA.observable
      ; PA.method_arity = (0, None) } in
    Hashtbl.replace full_p.pattern_tags obs_tag [];
    Hashtbl.replace full_p.observables p obs_tag in
  List.iter add_obs_tags ps;
  let add_transition vi t =
    let vs = full_p.vertices in
    let ts = vs.(vi).outgoing_transitions in
    vs.(vi) <- { vs.(vi) with outgoing_transitions = t :: ts } in
  let ifv = Hashtbl.create 0 in (* variable, string -> integer *)
  let pe p {PA.source=s;PA.target=t;PA.labels=ls} =
    let s = Hashtbl.find iov (p, s) in
    let t = Hashtbl.find iov (p, t) in
    let ls = List.map (transform_label ifv full_p.pattern_tags) ls in
    add_transition s {steps=ls; target=t} in
  List.iter (fun p -> List.iter (pe p) p.PA.transitions) ps;
  full_p

(* }}} *)
(* bytecode instrumentation *) (* {{{ *)

let utf8 = B.Utils.UTF8.of_string
let utf8_for_class x = B.Name.make_for_class_from_external (utf8 x)
let utf8_for_field x = B.Name.make_for_field (utf8 x)
let utf8_for_method x = B.Name.make_for_method (utf8 x)
let java_lang_Object = utf8_for_class "java.lang.Object"
let out = utf8_for_field "out"
let println = utf8_for_method "println"
let event = utf8_for_class "topl.Checker$Event"
let init = utf8_for_method "<init>"
let property = utf8_for_class "topl.Property"
let check = utf8_for_method "check"

(* helpers for handling bytecode of methods *) (* {{{ *)
let bm_parameters c =
  let rec tag_from k = function
    | [] -> []
    | x :: xs -> (k, x) :: tag_from (succ k) xs in
  function
    | BH.RegularMethod m -> tag_from 0
        ((if List.mem `Static m.BH.rm_flags then [] else [`Class c])
        @ fst m.BH.rm_descriptor)
    | BH.InitMethod m -> tag_from 1 m.BH.im_descriptor
    | BH.ClinitMethod _ -> []

let bm_return c = function
  | BH.RegularMethod m -> snd m.BH.rm_descriptor
  | BH.InitMethod _ -> `Class c
  | BH.ClinitMethod _ -> `Void

let bm_name = function
  | BH.RegularMethod r ->
      B.Utils.UTF8.to_string (B.Name.utf8_for_method r.BH.rm_name)
  | BH.InitMethod _ -> "<init>"
  | BH.ClinitMethod _ -> "<clinit>"

let bm_attributes = function
  | BH.RegularMethod m -> m.BH.rm_attributes
  | BH.InitMethod m -> m.BH.im_attributes
  | BH.ClinitMethod m -> m.BH.cm_attributes

let bm_locals_count m =
  let rec f = function
    | [] -> -12436 (* to cause a crash if used later *)
    | `Code c :: _ ->
        succ
          (try fst (BU.IntMap.max_binding c.BH.cv_type_of_local)
          with Not_found -> -1)
    | _ :: xs -> f xs in
  f (bm_attributes m)

let bm_is_init = function
  | BH.InitMethod _ -> true
  | _ -> false

let bm_map_attributes f = function
  | BH.RegularMethod r ->
      BH.RegularMethod { r with BH.rm_attributes = f r.BH.rm_attributes }
  | BH.InitMethod c ->
      BH.InitMethod { c with BH.im_attributes = f c.BH.im_attributes }
  | BH.ClinitMethod i ->
      BH.ClinitMethod { i with BH.cm_attributes = f i.BH.cm_attributes }

(* }}} *)

(* bytecode generating helpers *) (* {{{ *)
let bc_ldc_int i =
  [ BH.LDC (`Int (Int32.of_int i)) ]

let bc_new_object_array size = List.concat
  [ bc_ldc_int size
  ; [ BH.ANEWARRAY (`Class_or_interface java_lang_Object) ] ]

let name_of_nonvoid_box = function
  | `Boolean -> "Boolean"
  | `Byte -> "Byte"
  | `Char -> "Character"
  | `Double -> "Double"
  | `Float -> "Float"
  | `Int -> "Integer"
  | `Long -> "Long"
  | `Short -> "Short"
  | _ -> failwith "expecting a boxable type (iryku)"

let rec name_of_nonvoid = function
  | `Array t -> name_of_nonvoid t ^ "[]"
  | `Class c -> BU.UTF8.to_string (B.Name.printable_utf8_for_class c)
  | t -> name_of_nonvoid_box t

let bc_box = function
  | `Class _ | `Array _ -> []
  | t ->
      let c = utf8_for_class ("java.lang." ^ (name_of_nonvoid_box t)) in
      [BH.INVOKESTATIC (`Methodref
          (`Class_or_interface c,
	  utf8_for_method "valueOf",
          ([t], `Class c)))]

let bc_load i = function
  | `Class _ | `Array _ -> [BH.ALOAD i]
  | `Boolean -> [BH.ILOAD i]
  | `Byte -> [BH.ILOAD i]
  | `Char -> [BH.ILOAD i]
  | `Double -> [BH.DLOAD i]
  | `Float -> [BH.FLOAD i]
  | `Int -> [BH.ILOAD i]
  | `Long -> [BH.LLOAD i]
  | `Short -> [BH.ILOAD i]
  | `Void -> []

let bc_store i = function
  | `Class _ | `Array _ -> [BH.ASTORE i]
  | `Boolean -> [BH.ISTORE i]
  | `Byte -> [BH.ISTORE i]
  | `Char -> [BH.ISTORE i]
  | `Double -> [BH.DSTORE i]
  | `Float -> [BH.FSTORE i]
  | `Int -> [BH.ISTORE i]
  | `Long -> [BH.LSTORE i]
  | `Short -> [BH.ISTORE i]
  | `Void -> []

let bc_dup t =
  [ if BD.size t = 2 then BH.DUP2 else BH.DUP ]

let as_nonvoid = function
  | #BD.non_void_java_type as t -> t
  | _ -> failwith "INTERNAL(owwqr): expecting non-void"

let bc_array_set l a t =
  let t = as_nonvoid t in
  List.concat
    [ [ BH.DUP ]
    ; bc_ldc_int a
    ; bc_load l t
    ; bc_box t
    ; [ BH.AASTORE ] ]

let bc_new_event id = List.concat
  [ [ BH.NEW event
    ; BH.DUP_X1
    ; BH.SWAP ]
  ; bc_ldc_int id
  ; [ BH.SWAP
    ; BH.INVOKESPECIAL (`Methodref (`Class_or_interface
        event, init, ([`Int; `Array (`Class java_lang_Object)], `Void) )) ] ]

let bc_call_checker =
  [ BH.INVOKESTATIC (`Methodref (`Class_or_interface
      property, check, ([`Class event], `Void) )) ]

let bc_emit_for_runtime id values = match id with
  | None -> []
  | Some id ->
      let rec set j acc = function
        | (i, t) :: ts -> set (succ j) (bc_array_set i j t :: acc) ts
        | [] -> List.concat acc in
      List.concat
        [ bc_new_object_array (List.length values)
        ; set 0 [] values
        ; bc_new_event id
        ; bc_call_checker ]

let bc_emit_for_infer id values = match id with
  | None -> []
  | Some id ->
      let rec loop_load acc = function
        | [] -> List.concat (List.rev acc)
        | (i, t) :: xs -> loop_load (bc_load i t :: acc) xs in
      let call_monitor () =
        (* NOTE: The type Object should match with what is in the monitor. *)
        let args = List.map (fun _ -> `Class java_lang_Object) values in
        let mname = utf8_for_method (sprintf "event_%d" id) in
        [ BH.INVOKESTATIC (`Methodref (`Class_or_interface
          property, mname, (args, `Void) )) ] in
      List.concat
        [ loop_load [] values
        ; call_monitor () ]

let bc_emit id values =
  if !using_infer
  then bc_emit_for_infer id values
  else bc_emit_for_runtime id values

(* }}} *)

let does_method_match
  ({ method_name=mn; method_arity=ma }, mt)
  { PA.event_type=t; PA.method_name=re; PA.method_arity=(amin, amax) }
=
  let bamin = amin <= ma in
  let bamax = U.option true ((<=) ma) amax in
  let bt = U.option true ((=) mt) t in
  let bn = PA.pattern_matches re mn in
  let r = bamin && bamax  && bt && bn in
  if log_mm then begin
    printf "@\n@[<2>%s " (if r then "✓" else "✗");
    printf "(%a, %s, %d)@ matches (%a, %s, [%d..%a])@ gives (%b, %b, (%b,%b))@]"
      PA.pp_event_type mt mn ma
      (U.pp_option PA.pp_event_type) t
      re.PA.p_string
      amin
      (U.pp_option U.pp_int) amax
      bt bn bamin bamax
  end;
  r

let get_tag x =
  let cnt = ref (-1) in
  fun t (mns, ma) mn et ->
    let en = (* event name *)
      fprintf str_formatter "%a %s" PA.pp_event_type t mn;
      flush_str_formatter () in
    let fp p acc =
      let cm mn = does_method_match ({method_name=mn; method_arity=ma}, t) p in
      if List.exists cm mns then p :: acc else acc in
    if U.hashtbl_fold_values fp x.observables [] <> [] then begin
      match U.hashtbl_fold_keys fp x.pattern_tags [] with
        | [] -> None
        | ps ->
            incr cnt;
            let at p =
              let ts = Hashtbl.find x.pattern_tags p in
              if log_mm then
                printf "added tag %d\n" !cnt;
              Hashtbl.replace x.pattern_tags p (!cnt :: ts);
              Hashtbl.replace x.event_argtypes !cnt et;
              Hashtbl.replace x.event_names !cnt en in
            List.iter at ps;
            Some !cnt
    end else None

let put_labels_on =
  List.map (fun x -> (BC.fresh_label (), x))

let bc_at_return =
  let is_return = function
    | BH.ARETURN | BH.DRETURN | BH.FRETURN
    | BH.IRETURN | BH.LRETURN | BH.RETURN -> true
    (* do not instrument RET or WIDERET *)
    | _ -> false in
  function
    | [] -> U.id
    | r :: rs ->
        let rec f r rs = function
          | [] -> []
          | (xl, xi) :: xs when is_return xi ->
              (xl, r) :: (put_labels_on (rs @ [xi])) @ (f r rs xs)
          | x :: xs -> x :: f r rs xs in
        f r rs

let bc_at_call xs lys =
  put_labels_on xs @ lys

let instrument_code is_init call_id return_id arguments return locals code =
  let if_ b xs = if b then List.concat xs else [] in
  let has = (<>) None in
  let instrument_call = bc_at_call (List.concat
    [ if_ (is_init && has return_id)
      [ bc_load 0 return
      ; bc_store locals return ]
    ; bc_emit call_id arguments ]) in
  let instrument_return = bc_at_return (if_ (has return_id)
    [ if_ (not is_init && return <> `Void)
      [ bc_dup return
      ; bc_store locals return ]
    ; bc_emit return_id (if_ (return <> `Void) [[(locals, return)]]) ]) in
  instrument_call (instrument_return code)

let get_ancestors h c =
  let cs = Hashtbl.create 0 in
  let rec ga c =
    if not (Hashtbl.mem cs c) then begin
      Hashtbl.add cs c ();
      let parents = try Hashtbl.find h c with Not_found -> [] in
      List.iter ga parents
    end in
  ga c;
  U.hashtbl_fold_keys (fun c cs -> c :: cs) cs []

let name_of_class c =
  B.Utils.UTF8.to_string (B.Name.external_utf8_for_class c)

let mk_full_method_name c mn =
  name_of_class c ^ "." ^ mn

let get_overrides h c m =
  let ancestors = get_ancestors h c in
  let qualify c =  mk_full_method_name c m.method_name in
  (List.map qualify ancestors, m.method_arity)

let instrument_method get_tag h c m =
  let method_name = bm_name m in
  let arguments = bm_parameters c m in (* int is java bytecode local *)
  let topl_numbering i (_, t) = (i, t) in (* topl numbering always from 0 *)
  let arguments_topl = List.mapi topl_numbering arguments in
  let return = bm_return c m in
  let return_event_types =
    if return <> `Void
    then [(0, as_nonvoid return)]
    else [] in
  let method_arity = List.length arguments in
  let overrides = get_overrides h c {method_name; method_arity} in
  let full_method_name = mk_full_method_name c method_name in
  let ic = instrument_code
    (bm_is_init m)
    (get_tag PA.Call overrides full_method_name arguments_topl)
    (get_tag PA.Return overrides full_method_name return_event_types)
    arguments
    return
    (bm_locals_count m) in
  let ia xs =
    (* NOTE: Uses, but doesn't update cv_type_of_local. *)
    let f = function
      | `Code c -> `Code { c with BH.cv_code = ic c.BH.cv_code }
      | a -> a in
    List.map f xs in
  bm_map_attributes ia m

let pp_class f c =
  let n = B.Name.internal_utf8_for_class c.BH.c_name in
  fprintf f "@[%s@]" (B.Utils.UTF8.to_string n)

let instrument_class get_tag h c =
  if log_cp then printf "@\n@[<2>begin instrument %a" pp_class c;
  let instrumented_methods =
    List.map (instrument_method get_tag h c.BH.c_name) c.BH.c_methods in
  if log_cp then printf "@]@\nend instrument %a@\n" pp_class c;
  {c with BH.c_methods = instrumented_methods}

let compute_inheritance in_dir =
  let h = Hashtbl.create 0 in
  let record_class c =
    let parents = match c.BH.c_extends with
      | None -> c.BH.c_implements
      | Some e -> e :: c.BH.c_implements in
    Hashtbl.replace h c.BH.c_name parents in
  ClassMapper.iter in_dir record_class;
  h

(* }}} *)
(* generate static monitor *) (* {{{ *)

let get_registers p =
  let module S = U.IntSet in (* set of registers/variables *)
  let gr_oa rs (r, _) = S.add r rs in
  let gr_a rs xs = List.fold_left gr_oa rs xs in
  let gr_l rs l = gr_a rs PropAst.(l.action) in
  let gr_t rs t = List.fold_left gr_l rs t.steps in
  let gr_vd rs v = List.fold_left gr_t rs v.outgoing_transitions in
  S.elements (Array.fold_left gr_vd S.empty p.vertices)

let guards_of_tag_cache = Hashtbl.create 1
let guards_of_tag p =
  try Hashtbl.find guards_of_tag_cache p
  with Not_found ->
    let h = Hashtbl.create 1 in
    let one_tag g t =
      let gs = try Hashtbl.find h t with Not_found -> [] in
      Hashtbl.replace h t (g :: gs) in
    let one_guard g ts = List.iter (one_tag g) ts in
    Hashtbl.iter one_guard p.pattern_tags;
    Hashtbl.add guards_of_tag_cache p h;
    h

let get_guards_of_tag p tag =
  Hashtbl.find (guards_of_tag p) tag

let get_all_tags p =
  let f tag ts xs = (tag, ts) :: xs in
  Hashtbl.fold f p.event_argtypes []

let gi_configuration f p =
  let registers = get_registers p in
  let declare_register i =
    (* TODO: Check if we need to specialize to bool, int, String.*)
    fprintf f "@\nstatic private Object r%d;" i in
  let init_register i =
    fprintf f "@\nr%d = null;" i in
  fprintf f "@\nstatic private int state;";
  List.iter declare_register registers;
  fprintf f "@\n@[<2>static public void start() {";
  let maybe_state n =
    fprintf f   "@\nif (maybe()) state = %d;" n in
  let init_state = function
    | [] -> failwith "INTERNAL: vbkzpgbiuf"
    | x :: xs -> fprintf f "@\nstate = %d;" x; List.iter maybe_state xs in
  init_state (starts p);
  List.iter init_register registers;
  fprintf f "@\nq_size = 0;";
  fprintf f "@]@\n}";
  fprintf f "@\n@[<2>static public void stop() {}@]"

let all_vertices p =
  U.range (Array.length p.vertices)

let get_vertex_length p v =
  let t z x = max z (List.length x.steps) in
  List.fold_left t 1 p.vertices.(v).outgoing_transitions

let get_max_transition_length p =
  let vs = all_vertices p in
  let ms = List.map (get_vertex_length p) vs in
  List.fold_left max 1 ms

let get_max_arity p =
  let open PropAst in
  let a z (_, x) = max z x in
  let vg z = function
    | Variable (_, x) | Constant (_, x) -> max z x in
  let l z x =
    let z = List.fold_left a z x.action in
    List.fold_left vg z x.guard.value_guards in
  let t z x = List.fold_left l z x.steps in
  let vd z x = List.fold_left t z x.outgoing_transitions in
  1 + Array.fold_left vd 0 p.vertices

let gi_letter f m i =
  let component j = fprintf f "@\nstatic Object q%dl%d;" i j in
  fprintf f "@\nstatic int q%dtag;" i;
  List.iter component (U.range m)

let gi_queue f p =
  let n = get_max_transition_length p in
  let m = get_max_arity p in
  List.iter (gi_letter f m) (U.range n);
  fprintf f "@\nstatic int q_size;"

(*
  To support arrays, we (probably) need to change the bytecode instrumentation
  so that arrays are somehow cast to Object. Note that the monitor cannot
  refer to private types/classes in the program it verifies, so that's why
  we should use either Object or, perhaps, Object[].
 *)
let warn_if_array =
  let d = ref false in
  let w = function
    | `Array _ -> d := true; eprintf "@[W: arrays not supported!@]@."
    | _ -> () in
  (function t -> if not !d then w t)

let gi_event p f (tag, ts) =
  let f_arg f (i, t) = warn_if_array t; fprintf f "Object l%d" i in
(* XXX  let a_arg f (i, _) = fprintf f "l%d" i in *)
  fprintf f "@\n@[<2>public static void event_%d(%a) {"
    tag (U.pp_list ", " f_arg) ts;
  let hint_hack i = (* Remove if Infer #717 is fixed. *)
    fprintf f "@\nif (r%d != null) r%d.hashCode();" i i in
  List.iter hint_hack (get_registers p);
  let mm = get_max_arity p in
  let m = List.length ts in (* XXX check! *)
  let n = get_max_transition_length p in
  let ppq f i = fprintf f "q_size == %d" i in
  fprintf f "@\nif (!(%a)) { while (true); }"
    (U.pp_list " || " ppq) (U.range n);
  for i = n - 1 downto 1 do begin
    let copy_component j = fprintf f "@\nq%dl%d = q%dl%d;" i j (i-1) j in
    fprintf f "@\nq%dtag = q%dtag;" i (i - 1);
    List.iter copy_component (U.range mm)
  end done;
  let save_component j = fprintf f "@\nq0l%d = l%d;" j j in
  let null_component j = fprintf f "@\nq0l%d = null;" j in
  fprintf f "@\nq0tag = %d;" tag;
  List.iter save_component (U.range m);
  List.iter null_component (U.range2 m mm);
  fprintf f "@\n++q_size;";
  let execute _ = fprintf f "@\nexecute();" in
  List.iter execute (U.range n);
(* XXX do in execute
  for state = 0 to Array.length p.vertices - 1 do begin
    fprintf f "@\n";
    if state > 0 then
      fprintf f "else ";
    fprintf f "if (state == %d) event_%d_state_%d(%a);"
      state tag state (U.pp_list ", " a_arg) ts
  end done;
*)
  fprintf f "@]@\n}"

let transitions_of_tag_vertex p tag vertex =
  let gs = get_guards_of_tag p tag in
  let ts = p.vertices.(vertex).outgoing_transitions in
  let f t = match t.steps with
    | [l]
    | l :: _ -> (* XXX: REMOVE BRANCH -- this is just for debug *)
        if not (List.mem l.PA.guard.PA.tag_guard gs) then [] else begin
          [ l.PA.guard.PA.value_guards
          , l.PA.action
          , t.target ]
        end
    | _ -> failwith "INTERNAL: non-unit transitions are not supported (mnjwq)"
  in
  ts >>= f

let gi_condition f conditions =
  let one_condition = function
    | PA.Variable (register, letter) ->
        fprintf f " && r%d == l%d" register letter
    | PA.Constant (literal, letter) ->
        (* TODO: this probably needs to use .equals() for strings *)
        fprintf f " && %s == l%d" literal letter in
  fprintf f "true";
  List.iter one_condition conditions

let gi_action f actions =
  let one_action (register, letter) =
    fprintf f "@\nr%d = l%d;" register letter in
  List.iter one_action actions

let gi_event_state p f ((tag, ts), vertex) =
  let is_error x =
    p.vertices.(x).vertex_name = "error" in
  let transitions = transitions_of_tag_vertex p tag vertex in
  let gi_maybe_transition b (condition, action, target) =
    let gi_maybe f b =
      if b then fprintf f "maybe() && " in
    fprintf f "@]@\n@[<2>} else if (%a%a) {" gi_maybe b gi_condition condition;
    fprintf f   "%a" gi_action action;
    fprintf f   "@\nstate = %d;" target;
    if is_error target then
      fprintf f "@\nwhile (true);"
  in
  let f_arg f (i, t) = warn_if_array t; fprintf f "Object l%d" i in
  fprintf f "@\n@[<2>static void event_%d_state_%d(%a) {"
    tag vertex (U.pp_list ", " f_arg) ts;
  fprintf f   "@\n@[<2>if (false) {";
  List.iter (gi_maybe_transition true) transitions;
  List.iter (gi_maybe_transition false) transitions;
  fprintf f   "@]@\n}";
  fprintf f "@]@\n}"

let gi_execute_state_queue f p vertex q_size =
  let gi_condition f steps =
    let rec loop time register_map = function
      | [] -> ()
      | l :: ls ->
          let reg v =
            try
              let time, i = Util.IntMap.find v register_map in
              sprintf "q%dl%d" time i
            with Not_found ->
              sprintf "r%d" v in
          let ppt f (a, b) = (* all tag ids in [a,b) *)
            assert (a < b);
            if a + 1 = b then
              fprintf f "q%dtag == %d" time a
            else
              fprintf f "%d <= q%dtag && q%dtag < %d" a time time b in
          let tags = Hashtbl.find p.pattern_tags PropAst.(l.guard.tag_guard) in
          let tags = U.intervals tags in
          (* XXX TODO glue tags *)
          fprintf f " && (%a)" (U.pp_list " || " ppt) tags;
          let ppg = PropAst.(function
            | Variable (v, i) ->
                fprintf f " && %s == q%dl%d" (reg v) time i
            | Constant (c, i) -> (* XXX use 'equals' for strings *)
                fprintf f " && %s == q%dl%d" c time i) in
          List.iter ppg PropAst.(l.guard.value_guards);
          let do_action register_map (v, i) =
            Util.IntMap.add v (time, i) register_map in
          let register_map =
            List.fold_left do_action register_map PropAst.(l.action) in
          loop (time - 1) register_map ls in
    loop (q_size - 1) Util.IntMap.empty steps in
  let gi_action f steps =
    let rec loop time = function
      | [] -> ()
      | l :: ls ->
          let pp (v, i) = fprintf f "@\nr%d = q%dl%d;" v time i in
          List.iter pp PropAst.(l.action);
          loop (time - 1) ls in
    loop (q_size - 1) steps in
  let gi_maybe_transition maybe t =
    let is_error x =
      p.vertices.(x).vertex_name = "error" in
    let m = if maybe then "maybe()" else "true" in
    fprintf f "else if (%s%a) {" m gi_condition t.steps;
    fprintf f   "%a" gi_action t.steps;
    fprintf f   "@\nstate = %d;" t.target;
    fprintf f   "@\nq_size -= %d;" (List.length t.steps);
    if is_error t.target then fprintf f "@\nwhile (true);";
    fprintf f "@]@\n@[<2>} " in
  let transitions = p.vertices.(vertex).outgoing_transitions in
  fprintf f "@\n@[<2>static void execute_state%d_q%d() {" vertex q_size;
  fprintf f   "@\nif (!(state == %d)) { while (true); }" vertex;
  fprintf f   "@\nif (!(q_size == %d)) { while (true); }" q_size;
  fprintf f   "@\nif (false) {@\n@[<2>} ";
  List.iter (gi_maybe_transition true) transitions;
  List.iter (gi_maybe_transition false) transitions;
  fprintf f   "else {";
  fprintf f     "@\nq_size -= 1; // skip";
  fprintf f   "@]@\n}";
  fprintf f "@]@\n}"

let gi_execute_state p f vertex =
  let nn = get_max_transition_length p in
  let n = get_vertex_length p vertex in
  let qs = U.range2 n (nn + 1) in
  let ppq i =
    fprintf f "@\nelse if (q_size == %d) execute_state%d_q%d();" i vertex i in
  fprintf f "@\n@[<2>static void execute_state%d() { // %s"
    vertex p.vertices.(vertex).vertex_name;
  fprintf f   "@\nif (!(state == %d)) return;" vertex;
  fprintf f   "@\nif (false) {}";
  List.iter ppq qs;
  fprintf f "@]@\n}";
  List.iter (gi_execute_state_queue f p vertex) qs

let gi_execute f p =
  fprintf f "@\n@[<2>static void execute() {";
  let gn i = p.vertices.(i).vertex_name in
  let pe i = fprintf f "@\nexecute_state%d(); // %s" i (gn i) in
  List.iter pe (all_vertices p);
  fprintf f "@]@\n}"

let gi_automaton f p =
  let ts = get_all_tags p in
  let ss = all_vertices p in
  fprintf f "package topl;";
  fprintf f "@\nimport java.util.Random;";
  fprintf f "@\n@[<2>public class Property {";
  fprintf f   "%a" gi_configuration p;
  fprintf f   "%a" gi_queue p;
  fprintf f   "%a" (U.pp_list "" (gi_event p)) ts;
  fprintf f   "%a" gi_execute p;
  fprintf f   "%a" (U.pp_list "" (gi_execute_state p)) ss;
  fprintf f   "@\nstatic boolean maybe() { return random.nextBoolean(); }";
  fprintf f   "@\nstatic Random random = new Random();";
  fprintf f "@]@\n}"

let generate_checkers_for_infer out_dir extra_fs p =
  check_automaton p;
  let (/) = Filename.concat in
  let topl_dir = out_dir/"topl" in
  let jf = topl_dir/"Property.java" in
  U.mkdir_p topl_dir;
  let jc, j = U.open_formatter jf in
  fprintf j "@[%a@.@]" gi_automaton p;
  close_out_noerr jc;
  U.compile out_dir out_dir out_dir (jf :: extra_fs)

(* }}} *)
(* main *) (* {{{ *)

let read_properties fs =
  fs >> List.map Helper.parse >>= List.map (fun x -> x.PA.ast)

let check_work_directory d =
  let e = Arg.Bad ("Bad work directory: " ^ d) in
  try
    let here = Unix.getcwd () in
    let dir = Filename.concat here d in
    let here, dir = (U.normalize_path here, U.normalize_path dir) in
    if U.is_prefix dir here then raise e
  with Not_found -> raise e

let generate_checkers = ref generate_checkers_for_runtime
let use_infer () =
  using_infer := true;
  generate_checkers := generate_checkers_for_infer

let () =
  Format.set_margin 80;
  printf "@[";
  let usage = Printf.sprintf
    "usage: %s -i <dir> [-o <dir>] <topls>" Sys.argv.(0) in
  try
    let fs = ref [] in
    let extra_fs = ref [] in
    let set_extra_source f = extra_fs := f :: !extra_fs in
    let in_dir = ref None in
    let out_dir = ref None in
    let set_dir r v = match !r with
      | Some _ -> raise (Arg.Bad "Repeated argument.")
      | None -> r := Some v in
    Arg.parse
      [ "-s", Arg.Unit use_infer, "generate infer checkers"
      ; "-e", Arg.String set_extra_source, "sources that refer to topl.Property"
      ; "-i", Arg.String (set_dir in_dir), "input directory"
      ; "-o", Arg.String (set_dir out_dir), "output directory" ]
      (fun x -> fs := x :: !fs)
      usage;
    if !in_dir = None then begin
      eprintf "@[Missing input directory.@\n%s@." usage;
      exit 2;
    end;
    if !out_dir = None then out_dir := !in_dir;
    let in_dir, out_dir = U.from_some !in_dir, U.from_some !out_dir in
    let backup_dir = U.temp_path "toplc_backup_" in
    List.iter check_work_directory [in_dir; out_dir];
    if Sys.file_exists out_dir then U.rename out_dir backup_dir;
    let ps = read_properties !fs in
    let h = compute_inheritance in_dir in
    let p = transform_properties ps in
    ClassMapper.map in_dir out_dir (instrument_class (get_tag p) h);
    !generate_checkers out_dir !extra_fs p;
    printf "@."
  with
    | Helper.Parsing_failed m
    | Sys_error m
        -> eprintf "@[ERROR: %s@." m;

(* }}} *)
