%{
  let type_of_string = function
    | "Bool" -> SA.Bool
    | "Unit" -> SA.Unit
    | x -> SA.Class x
%}

%%

%public class_:
    CLASS c=ID LC m=member* RC
      { (c, m) }

%public global:
    VAR d=type_id
      { d }

%public main:
    MAIN b=body
      { b }

member:
    VAR d=type_id
      { SA.Field d }
  | d=type_id LP a=separated_list(COMMA, type_id) RP b=body?
      { SA.Method
        { SA.method_return_type = d.SA.declaration_type
        ; SA.method_name = d.SA.declaration_variable
        ; SA.method_formals = a
        ; SA.method_body =
            match b with
              | Some b -> b
              | None -> SA.default_body $endpos.Lexing.pos_lnum } }

type_id:
    t=ID v=ID
      { { SA.declaration_type = type_of_string t; SA.declaration_variable = v } }

body:
    LC b=with_line(statement)* RC
      { let ds = ref [] in (* ugly *)
        let ss = ref [] in
        let add_to_ds x = ds := x :: !ds in
        let add_to_ss line x = ss := { PA.ast = x; PA.line = line } :: !ss in
        let process_line { PA.ast = xs; PA.line = line } =
          List.iter (either add_to_ds (add_to_ss line)) xs in
        List.iter process_line b;
        SA.Body (List.rev !ds, List.rev !ss) }

statement:
    RETURN r=expression
      { [Right(SA.Return r)] }
  | VAR d=type_id
      { [Left d] }
  | l=lhs ASGN NEW
      { fst l @ [Right(SA.mk_allocate (snd l))] }
  | l=lhs ASGN e=expression
      { fst l @
        [Right(SA.Assignment(snd l, e))] }
  | l=lhs ASGN r=expression DOT m=ID a=args
      { fst l @ [Right(SA.mk_call (Some(snd l)) r m a)] }
  | r=ID DOT m=ID a=args (* if lhs may start with (, then grammar would be ambiguous *)
      { [ Right(SA.mk_call None (SA.Ref r) m a) ] }
  | pre=do_part? WHILE c=expression post=body?
      { [Right(SA.While
        { SA.while_pre_body=from_option SA.empty_body pre
        ; SA.while_condition=c
        ; SA.while_post_body=from_option SA.empty_body post})] }
  | IF c=expression i=body e=else_?
      { [Right(SA.If(c,i))]
        @ (match e with None -> [] | Some e -> [Right(SA.If(SA.Not c, e))]) }

do_part:
    DO b=body
      { b }

lhs:
  VAR d=type_id { ([Left d], d.SA.declaration_variable) } (* sugar *)
  | r=ID { ([], r) }

expression:
    r=atom
      { r }
  | l=expression op=binop r=expression
      { SA.Bin (l, op, r) }
  | l=expression op=acop r=expression
      { SA.Ac (op, [l; r]) }
  | r=expression DOT f=ID
      { SA.Deref (r, f) }
  | NOT r=expression
      { SA.Not r }

atom:
    LP r=expression RP
      { r }
  | r=ID
      { SA.Ref r }
  | n=NUMBER
      { SA.Literal (Some n, ref None) }
  | STAR
      { SA.Literal (None, ref None) }

%inline binop:
    EQ
      { SA.Eq }
  | NE
      { SA.Ne }

%inline acop:
    OR
      { SA.Or }
  | AND
      { SA.And }

%public args:
    LP r=separated_list(COMMA, expression) RP
      { r }

else_:
    ELSE b=body
      { b } (* sugar *)

%%
