open Format

let topl_dir =
  try
    let r = Sys.getenv "TOPL_DIR" in
    if not (Sys.file_exists r) || not (Sys.is_directory r) then raise Not_found;
    r
  with Not_found ->
    match Util.binary_path () with
      | None -> eprintf "@[Please set TOPL_DIR.@."; exit 2
      | Some f -> Filename.dirname f
