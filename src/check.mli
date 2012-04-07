exception Error

val properties :
  string (* file name *)
  -> (string, string) PropAst.t PropAst.with_line list
  -> unit
