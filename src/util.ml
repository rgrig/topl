open Debug
open Format

type ('a, 'b) either = Left of 'a | Right of 'b

let either a b = function Left x -> a x | Right x -> b x

let from_option a = function None -> a | Some a -> a

let option d f = function
  | None -> d
  | Some x -> f x

let from_some = function
  | Some x -> x
  | None -> failwith "I was hoping to get Some."

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
  let h = Hashtbl.create 51 in
  List.iter (fun x -> Hashtbl.replace h x x) l;
  Hashtbl.fold (fun _ -> cons) h []

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

let pp_s pp_f s = Format.fprintf pp_f "%s" s

let pp_list pp_sep pp_element =
  let rec f = fun pp_f -> function
    | [] -> ()
    | [x] -> pp_element pp_f x
    | x :: xs -> Format.fprintf pp_f "@[%a@]%s@,%a" pp_element x pp_sep f xs in
  f

let pp_option pp_e ppf = function
  | None -> Format.fprintf ppf "None"
  | Some s -> Format.fprintf ppf "Some %a" pp_e s

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

let list_of_option = function
  | Some x -> [x]
  | None -> []

let cartesian xss =
  let rec f acc = function
    | [] -> List.map List.rev acc
    | xs :: xss ->
        let ys = List.map (fun x -> List.map (fun ys -> x :: ys) acc) xs in
        f (List.concat ys) xss in
  f [[]] xss

let rec rel_fs_preorder top m f =
  let (/) = Filename.concat in
  let here = top/f in
  assert (Sys.is_directory here);
  let pc c =
    if log_cm then printf "@\n@[rel_fs_preorder %s %s@]" top f;
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

let open_out_p fn =
  mkdir_p (Filename.dirname fn);
  open_out fn

(* TODO:
  - nothing for now?
 *)
(*
vim:sts=2:sw=2:ts=8:et:
*)
