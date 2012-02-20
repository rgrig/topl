(** [map src tgt f] creates directory [tgt] as a mirror of directory [src] in
which all class-files are transformed by applying [f] to them. Non-class-files
are copied unchanged. Subdirectories and jar-files are processed recursively. *)
val map : string -> string ->
  (BaristaLibrary.HighClass.t -> BaristaLibrary.HighClass.t) ->
  unit

(** [iter dir f] applies [f] to all the class-files in [dir]. *)
val iter : string -> (BaristaLibrary.HighClass.t -> unit) -> unit
