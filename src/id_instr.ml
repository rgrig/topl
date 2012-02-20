open Format
open Util

module B = BaristaLibrary
module BA = B.HighClass.HighAttribute
module BC = B.HighClass
module BM = B.HighClass.HighMethod

let removeLNT c =
  let not_LNT : BA.code_attribute -> bool = function
    | `LineNumberTable _ -> false
    | _ -> true in
  let rm_c c = { c with BA.attributes = List.filter not_LNT c.BA.attributes } in
  let rm_a : BA.for_method -> BA.for_method = function
    | `Code c -> `Code (rm_c c)
    | x -> x in
  let rm_mr mr = { mr with BM.attributes = List.map rm_a mr.BM.attributes } in
  let rm_mc mc = { mc with BM.cstr_attributes = List.map rm_a mc.BM.cstr_attributes } in
  let rm_mi mi = { mi with BM.init_attributes = List.map rm_a mi.BM.init_attributes } in
  let rm_m = function
    | BM.Regular mr -> BM.Regular (rm_mr mr)
    | BM.Constructor mc -> BM.Constructor (rm_mc mc)
    | BM.Initializer mi -> BM.Initializer (rm_mi mi) in
  { c with
    BC.methods = List.map rm_m c.BC.methods }

let () =
  let in_dir = ref Filename.current_dir_name in
  let out_dir = ref (Filename.concat Filename.temp_dir_name "out") in
  Arg.parse ["-i", Arg.Set_string in_dir, "input directory";
             "-o", Arg.Set_string out_dir, "output directory"]
            (fun _ -> ())
            "usage: /id_instr [-i <input directory>][-o <output directory>]";
   ClassMapper.map !in_dir !out_dir removeLNT
