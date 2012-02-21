{
  open Parser

  type ('a, 'b) either = Left of 'a | Right of 'b

  let l x = Left x
  let r x = Right x

  let new_line x y lexbuf =
    let m = ref 0 in
    String.iter (fun c -> if c = '\n' then incr m) x;
    let n = String.length y in
    let lcp = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { lcp with
      Lexing.pos_lnum = lcp.Lexing.pos_lnum + !m;
      Lexing.pos_bol = lcp.Lexing.pos_cnum - n
    };
    r n

  let keyword =
    let table = Hashtbl.create 53 in
    List.iter (fun (k, v) -> Hashtbl.add table k v)
      [ "and", AND
      ; "call", CALL
      ; "class", CLASS
      ; "do", DO
      ; "else", ELSE
      ; "false", NUMBER 0
      ; "if", IF
      ; "main", MAIN
      ; "message", MESSAGE
      ; "new", NEW
      ; "observe", OBSERVE
      ; "or", OR
      ; "prefix", PREFIX
      ; "property", PROPERTY
      ; "return", RETURN
      ; "var", VAR
      ; "while", WHILE ];
    fun id -> l (try Hashtbl.find table id with Not_found -> ID id)

  let quoted = Str.regexp "\\\\\\(.\\)"
  let unquote x = Str.global_replace quoted "\\1" x
}

let id_head = ['a'-'z' 'A'-'Z']
let id_tail = ['a'-'z' 'A'-'Z' '0'-'9']*

rule tok1 = parse
  | '\t'    { raise Error }
  | ((' '* (("//" | ';') [^ '\n']*)? '\n')+ as x) (' '* as y)
            { new_line x y lexbuf }
  | ' '+    { tok1 lexbuf }
  | "/\\" | "&&"
            { l AND }
  | "->"    { l ARROW }
  | ":="    { l ASGN }
  | ":"     { l COLON }
  | ','     { l COMMA }
  | '.'     { l DOT }
  | "=="    { l EQ }
  | '['     { l LB }
  | '{'     { l LC }
  | '('     { l LP }
  | "!="    { l NE }
  | "¬" | "!"
            { l NOT }
  | "\\/" | "||"
            { l OR }
  | ']'     { l RB }
  | '}'     { l RC }
  | ')'     { l RP }
  | '*'     { l STAR }
  | '<' (([^ '>' '\\' '\n'] | ('\\' _))* as x) '>'
            { l (CONSTANT (unquote x)) }
  | '"' ([^ '"' '\n']* as x) '"'
            { l (STRING x) }
  | '-'? ['0'-'9']+ as n
            { l (NUMBER (int_of_string n)) }
  | id_head id_tail as id { keyword id }
  | eof     { l EOF }
  | _       { raise Error }

{
  (* EXERCISE: Write it with continuations. *)
  let token lexbuf =
    let indents = ref [0] in
    let scheduled_rb = ref 0 in
    let return t = (t, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p) in
    let rec return_rb n = match !indents with
      | h :: t when h > n -> incr scheduled_rb; indents := t; return_rb n
      | h :: _ when h < n -> raise Error
      | [] -> failwith "broken invariant: List.length !indents > 0"
      | _ -> tok2 ()
    and tok2 () =
      if !scheduled_rb > 0 then begin
        decr scheduled_rb;
        return RC
      end else match tok1 lexbuf with (* TODO *)
        | Right n ->
            if n > List.hd !indents then begin
              indents := n :: !indents;
              return LC
            end else return_rb n
        | Left t -> return t in
    tok2
}
