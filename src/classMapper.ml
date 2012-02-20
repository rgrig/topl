open Debug
open Format

module B = BaristaLibrary
module F = Filename
module U = Util


let (/) d f =
    if d = F.current_dir_name then f
    else F.concat d f

let is_jar f = F.check_suffix f ".jar"
let is_class f = F.check_suffix f ".class"

let mk_tmp_dir p s =
  let tmp_file = F.temp_file p s in
  Sys.remove tmp_file;
  U.mkdir_p tmp_file;
  tmp_file

let ensure_dir f =
  if F.check_suffix f "/" then f else (f ^ "/")

let open_class fn =
  let read_class_channel ch =
    try
      let cl_in = B.InputStream.make_of_channel ch in
      let cf = B.ClassFile.read cl_in in
      Some (B.HighClass.decode cf)
    with
      | B.InputStream.Exception e ->
	eprintf "@[dec %s: %s@." fn (B.InputStream.string_of_error e);
	None
      | B.Version.Exception e ->
	eprintf "@[dec %s: %s@." fn (B.Version.string_of_error e);
	None
      | B.HighClass.Exception e ->
	eprintf "@[dec %s: %s@." fn (B.HighClass.string_of_error e);
	None
      | B.ClassFile.Exception e ->
	eprintf "@[dec %s: %s@." fn (B.ClassFile.string_of_error e);
	None
      | _ -> eprintf "@[dec %s: error@." fn; None in
  try
    let ch = open_in fn in
    let cd = read_class_channel ch in
    close_in ch; cd
  with
    | Sys_error msg -> eprintf "@[error opening %s: %s@." fn msg; None

let output_class version fn c =
  try
    let ch = open_out fn in
    let bytes = B.HighClass.encode (c, version) in
    B.ClassFile.write bytes (B.OutputStream.make_of_channel ch);
    close_out ch;
    true
  with
    | Sys_error msg -> eprintf "@[error opening %s: %s@." fn msg; false
    | B.Version.Exception e ->
        eprintf "@[enc %s: %s@." fn (B.Version.string_of_error e); false
    | B.Name.Exception e ->
        eprintf "@[enc %s: %s@." fn (B.Name.string_of_error e); false
    | B.AccessFlag.Exception e ->
        eprintf "@[enc %s: %s@." fn (B.AccessFlag.string_of_error e); false
    | B.HighClass.Exception e ->
        eprintf "@[enc %s: %s@." fn (B.HighClass.string_of_error e); false
    | _ -> eprintf "@[enc %s: error@." fn; false

let rec map in_dir out_dir f =
  let process_jar jf =
    if log_cm then printf "@\n@[map jar: %s@]" (in_dir / jf);
    let tmp_in_dir = mk_tmp_dir "in_" "_jar" in
    let tmp_out_dir = mk_tmp_dir "out_" "_jar" in
    let jar_in = Zip.open_in (in_dir / jf) in
    let extract e =
      let e_fn = tmp_in_dir / e.Zip.filename in
      U.mkdir_p (F.dirname e_fn);
      if not e.Zip.is_directory then
        (if log_cm then printf "@\n@[extracting %d bytes to %s@]" e.Zip.uncompressed_size e_fn;
        Zip.copy_entry_to_file jar_in e e_fn) in
    List.iter extract (Zip.entries jar_in);
    Zip.close_in jar_in;
    map tmp_in_dir tmp_out_dir f;
    if log_cm then printf "@\n@[REMOVING %s@]" tmp_in_dir;
    U.rm_r tmp_in_dir;
    let jar_out = Zip.open_out (out_dir / jf) in
    let intract _ fn =
      if Sys.is_directory (tmp_out_dir / fn) then Zip.add_entry "" jar_out (ensure_dir fn)
      else
        (if log_cm then printf "@\n@[intracting %d bytes from %s@]" (Unix.stat (tmp_out_dir / fn)).Unix.st_size (tmp_out_dir / fn);
         Zip.copy_file_to_entry (tmp_out_dir / fn) jar_out fn) in
    U.rel_fs_preorder tmp_out_dir intract "";
    Zip.close_out jar_out;
    U.rm_r tmp_out_dir in
  let process_class fn =
    if log_cm then printf "@\n@[map class: %s@]" (in_dir / fn);
    let copy () = U.cp (in_dir / fn) (out_dir / fn) in
    match open_class (in_dir / fn) with
    | None -> copy ()
    | Some (cd, version) ->
        let inst_cd = f cd in
        if not (output_class version (out_dir / fn) inst_cd) then copy () in
  let process _ fn =
    if log_cm then printf "@\n@[map: %s@]" (in_dir / fn);
    if Sys.is_directory (in_dir / fn) then U.mkdir_p (out_dir / fn)
    else begin
      if is_jar fn then process_jar fn
      else if is_class fn then process_class fn
      else U.cp (in_dir / fn) (out_dir / fn)
    end in
  if log_cm then printf "@\n@[map: %s -> %s@]" in_dir out_dir;
  U.mkdir_p out_dir;
  U.rel_fs_preorder in_dir process ""

let rec iter in_dir f =
  let iter_jar jf =
    if log_cm then printf "@[iter jar: %s@]" jf;
    let tmp_in_dir = mk_tmp_dir "iter_" "_jar" in
    let jar_in = Zip.open_in (in_dir / jf) in
    let extract e =
      let e_fn = tmp_in_dir / e.Zip.filename in
      U.mkdir_p (F.dirname e_fn);
      if not e.Zip.is_directory then Zip.copy_entry_to_file jar_in e e_fn in
    List.iter extract (Zip.entries jar_in);
    Zip.close_in jar_in;
    iter tmp_in_dir f;
    U.rm_r tmp_in_dir in
  let iter_class fn =
    if log_cm then printf "@\n@[iter class: %s@]" fn;
    match open_class (in_dir / fn) with
    | None -> ()
    | Some (cd, _) -> f cd in
  let process _ fn =
    printf "@[iterating %s@." (in_dir / fn);
    if Sys.is_directory (in_dir / fn) then printf "@[directory@."
    else if is_jar fn then iter_jar fn
    else if is_class fn then iter_class fn in
  U.rel_fs_preorder in_dir process ""

(*
vim:sts=2:sw=2:ts=8:et:
*)
