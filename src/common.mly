%{
  open Format
  open Util

  module PA = PropAst
  module SA = SoolAst
  module U = Util

  type variable = string
  type value = string
%}

%token <int> NUMBER
%token <string> CONSTANT
%token <string> ID
%token <string> STRING
%token AND
%token ARROW
%token ASGN
%token CALL
%token CLASS
%token COLON
%token COMMA
%token DO
%token DOT
%token ELSE
%token EOF
%token EQ
%token IF
%token LB
%token LC
%token LP
%token MAIN
%token MESSAGE
%token NE
%token NEW
%token NOT
%token OBSERVING
%token OR
%token PREFIX
%token PROPERTY
%token RB
%token RC
%token RETURN
%token RP
%token STAR
%token VAR
%token WHILE

%left OR AND
%left EQ NE
%nonassoc NOT
%left DOT

%start <(string, string) SoolAst.program> program

%%

program:
    h=class_ t=program
      { { t with SA.program_classes = h :: t.SA.program_classes } }
  | h=global t=program
      { { t with SA.program_globals = h :: t.SA.program_globals } }
  | h=main t=program
      { if t.SA.program_main <> None then
          eprintf "WARNING: Only the last main matters.";
        { t with SA.program_main = Some h } }
  | h=with_line(property) t=program
      { { t with SA.program_properties = h :: t.SA.program_properties } }
  | EOF
      { { SA.program_classes = []
        ; SA.program_globals = []
        ; SA.program_main = None
        ; SA.program_properties = []  } }

%public with_line(X):
    x=X { { PA.ast = x; PA.line = $startpos.Lexing.pos_lnum } }

%%

