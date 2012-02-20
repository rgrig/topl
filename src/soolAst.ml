(* modules *) (* {{{ *)
open Format

module PA = PropAst
module U = Util

(* }}} *)
(* stuff towards leaves *) (* {{{ *)
type variable = string
type value = int

type binop = Eq | Ne
type acop = Or | And

type type_ =
  | Class of string
  | Bool
  | Unit
  | AnyType of type_ option ref (* used while typechecking Nondet *)

(* Expressions have no side-effects, so they don't include method calls. *)
type expression =
    Ac of acop * expression list
  | Bin of expression * binop * expression
  | Not of expression
  | Deref of expression * variable
  | Ref of variable
  | Literal of value option * type_ option ref


type declaration =
  { declaration_type : type_
  ; declaration_variable : string }

type call_statement =
  { call_lhs : string option
  ; call_receiver : expression
  ; mutable call_class : string option
  ; call_method : string
  ; call_arguments : expression list }

type allocate_statement =
  { allocate_lhs : string
  ; mutable allocate_type : type_ option }

type statement =
    Return of expression
  | Assignment of string * expression
  | Call of call_statement
  | Allocate of allocate_statement
  | While of while_
  | If of expression * body

and while_ =
  { while_pre_body : body
  ; while_condition : expression
  ; while_post_body : body }

and body = Body of declaration list * statement PA.with_line list

type method_ =
  { method_return_type : type_
  ; method_name : string
  ; method_formals : declaration list
  ; method_body : body }

type member =
    Field of declaration
  | Method of method_

type class_ = string * member list
(* }}} *)
(* Root of AST, see common.mly. *) (* {{{ *)

type ('variable, 'value) program =
  { program_globals : declaration list
  ; program_classes : class_ list
  ; program_main : body option
  ; program_properties : ('variable, 'value) PA.t PA.with_line list }
  (* TODO: should convert to [(variable, value) PA.t] after parsing *)

(* }}} *)
(* utilities *) (* {{{ *)

let mk_allocate v = Allocate { allocate_lhs = v; allocate_type = None }
let mk_call l r m a = Call
  { call_lhs = l
  ; call_receiver = r
  ; call_class = None
  ; call_method = m
  ; call_arguments = a }

let default_body line =
  Body ([], [{ PA.ast = Return(Literal (None, ref None)); PA.line = line }])
let empty_body = Body ([], [])

let rec pp_type ppf = function
  | Class n -> fprintf ppf "%s" n
  | Bool -> fprintf ppf "[Bool]"
  | Unit -> fprintf ppf "[Unit]"
  | AnyType {contents=t} -> fprintf ppf "<%a>" (U.pp_option pp_type) t
(* }}} *)
