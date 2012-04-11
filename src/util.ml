open Format

type ('a, 'b) either = Left of 'a | Right of 'b

let either a b = function Left x -> a x | Right x -> b x

let id x = x

let from_option a = function None -> a | Some a -> a

let option d f = function
  | None -> d
  | Some x -> f x

let from_some = function
  | Some x -> x
  | None -> failwith "I was hoping to get Some."

let list_of_option = function
  | Some x -> [x]
  | None -> []

(* [map_find d f p xs] applies [f] to each [x] and returns the first
  result that satisfies [p]. Otherwise returns the default [d]. *)
let rec map_find d f p = function
  | x :: xs -> let r = f x in if p r then r else map_find d f p xs
  | [] -> d
let map_find_not d f xs = map_find d f ((<>) d) xs

let map_option f xs =
  let f' acc x = match f x with
    | None -> acc
    | Some y -> y :: acc in
  List.rev (List.fold_left f' [] xs)

let cons x xs = x :: xs

let unique l =
  let h = Hashtbl.create 0 in
  List.iter (fun x -> Hashtbl.replace h x x) l;
  Hashtbl.fold (fun _ -> cons) h []

let fold_with_index f init xs =
  let g (i, acc) x = succ i, f acc i x in
  snd (List.fold_left g (0, init) xs)

let map_with_index f xs =
  let g acc i x = f i x :: acc in
  List.rev (fold_with_index g [] xs)

let hashtbl_fold_keys f h z =
  let f k _ = f k in
  Hashtbl.fold f h z

let hashtbl_fold_values f h z =
  let f _ v = f v in
  Hashtbl.fold f h z

(** Operators go in a submodule so that we can open it, without having to
polute the namespace with everything in Util. *)
module Operators = struct
  let (@<) f g x = f (g x)
  let (@>) f g = g @< f
  let (>>) x f = f x
  let (>>=) x f = x >> List.map f >> List.concat
end

module Int = struct type t = int let compare = compare end
module OrderedPair (A:Set.OrderedType) (B:Set.OrderedType) =
struct
  type t = A.t * B.t
  let compare = compare
end

module CharMap = Map.Make (Char)
module IntMap : (Map.S with type key = int) = Map.Make (Int)
module IntSet : (Set.S with type elt = int) = Set.Make (Int)
module StringMap = Map.Make (String)
module StringPairMap = Map.Make (OrderedPair (String) (String))
module StringSet = Set.Make (String)

let flip f x y = f y x

let add_ints s = List.fold_left (flip IntSet.add) s

let add_strings s = List.fold_left (flip StringSet.add) s

let pp_string f s = fprintf f "%s" s

let pp_int f x = fprintf f "%d" x

let pp_list sep pp_element =
  let rec pp = fun f -> function
    | [] -> ()
    | [x] -> pp_element f x
    | x :: xs -> fprintf f "@[%a@]%s@," pp_element x sep; pp f xs in
  pp

let pp_option pp_e ppf = function
  | None -> fprintf ppf "None"
  | Some s -> fprintf ppf "Some(%a)" pp_e s

let rec fix f x =
  let y = f x in
  if y = x then y else fix f y

let rec y f x = f (y f) x

let rec memo f f' =
  let cache = Hashtbl.create 101 in
  fun x ->
    try
      Hashtbl.find cache x
    with Not_found -> begin
      let r = f f' x in
      Hashtbl.add cache x r; r
    end

let fresh_id () =
  let alphabet = "abcdefghijklmnopqrstuvwxyz" in
  let n = String.length alphabet in
  let count = ref (-1) in
  fun () ->
    incr count;
    if !count = 0 then String.sub alphabet 0 1 else begin
      let r = Buffer.create 5 in
      let rec f x =
        if x >= n then f (x / n);
        Buffer.add_char r alphabet.[x mod n] in
      f !count;
      Buffer.contents r
    end

let fresh_internal_id =
  let internal = fresh_id () in
  (fun () -> Printf.sprintf "<%s>" (internal ()))

let todo () = failwith "todo"

let rec rel_fs_preorder top m f =
  let (/) = Filename.concat in
  let here = top/f in
  assert (Sys.is_directory here);
  let pc c =
    m top (f/c);
    if Sys.is_directory (here/c) then
      rel_fs_preorder top m (f/c) in
  Array.iter pc (Sys.readdir here)

let rec fs_postorder m f =
  if Sys.file_exists f then begin
    if Sys.is_directory f then begin
      let children = Array.map (Filename.concat f) (Sys.readdir f) in
      Array.iter (fs_postorder m) children
    end;
    m f
  end

let fs_filter p f =
  let r = ref [] in
  fs_postorder (fun x -> if p x then r := x::!r) f; !r

let rm_r dir =
  let delete f =
    if Sys.is_directory f then Unix.rmdir f
    else Sys.remove f in
  fs_postorder delete dir

let cp source_file target_file =
  let source = open_in_bin source_file in
  let target = open_out_bin target_file in
  try
    while true do
      output_byte target (input_byte source)
    done
  with End_of_file -> (close_in source; close_out target)

let rec mkdir_p dir =
  assert (Filename.basename dir <> Filename.parent_dir_name);
  if Sys.file_exists dir then begin
    if not (Sys.is_directory dir) then
      raise (Unix.Unix_error (Unix.EEXIST, "mkdir_p", dir))
  end else begin
    mkdir_p (Filename.dirname dir);
    if Filename.basename dir <> Filename.current_dir_name then
      Unix.mkdir dir 0o755
  end

let cp_r source_file target_directory =
  let cp top path =
    let (/) = Filename.concat in
    let src = top/path in
    let tgt = target_directory/path in
    if Sys.is_directory src
    then mkdir_p tgt
    else cp src tgt in
  let top, sf = (Filename.dirname source_file, Filename.basename source_file) in
  cp top sf;
  if Sys.is_directory source_file then rel_fs_preorder top cp sf

let rename s t =
  assert (Sys.file_exists s && not (Sys.file_exists t));
  try Sys.rename s t
  with Sys_error _ ->
    let dt = Filename.dirname t in
    cp_r s dt;
    Sys.rename (Filename.concat dt (Filename.basename s)) t

let mk_tmp_dir p s =
  let tmp_file = Filename.temp_file p s in
  Sys.remove tmp_file;
  mkdir_p tmp_file;
  tmp_file

(* TODO(rgrig): Do this properly. *)
let command_escape s =
  "\"" ^ s ^ "\""

(* It is *unlikely* that the returned name is of an existing file. *)
let rec temp_path prefix =
  Filename.concat
    (Filename.temp_dir_name)
    (Printf.sprintf "%s%Lx"
      prefix
      (Int64.of_float (1000.0 *. Unix.gettimeofday ())))

(* Throws [Not_found] if [p] isn't a valid absolute path. *)
(* TODO(rgrig): test on Windows and cygwin. *)
let normalize_path p =
  let rec components acc d =
    let dn, bn = (Filename.dirname d, Filename.basename d) in
    if dn = d
    then dn :: acc
    else components (bn :: acc) dn in
  let rec clean n acc = function
    | [r] -> if n <> 0 then raise Not_found else r :: acc
    | d :: ds when d = Filename.current_dir_name -> clean n acc ds
    | d :: ds when d = Filename.parent_dir_name -> clean (succ n) acc ds
    | d :: ds ->
        if n = 0
        then clean n (d :: acc) ds
        else clean (pred n) acc ds
    | [] -> raise Not_found in
  match clean 0 [] (List.rev (components [] p)) with
    | d :: ds -> List.fold_left Filename.concat d ds
    | [] -> failwith "INTERNAL: Should have raised Not_found already"

let is_prefix s t =
  let m, n = (String.length s, String.length t) in
  let rec f i j = i = m || (j < n && s.[i] = t.[j] && f (succ i) (succ j)) in
  f 0 0
