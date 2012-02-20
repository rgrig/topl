(* Some generic utilities. Unlike util.ml, this module also depends on
  non-standard modules. *)

open Format

exception Parsing_failed of string

let parse fn =
  let f = open_in fn in
  let lexbuf = Lexing.from_channel f in
  try
    let parse =
      MenhirLib.Convert.Simplified.traditional2revised Parser.program in
    parse (Lexer.token lexbuf)
  with Parser.Error ->
    (match Lexing.lexeme_start_p lexbuf with
    { Lexing.pos_lnum=line; Lexing.pos_bol=c0;
      Lexing.pos_fname=_; Lexing.pos_cnum=c1} ->
        let msg = sprintf "@[%s:%d:%d: parse error@]" fn line (c1-c0+1) in
        raise (Parsing_failed msg));

