%token ACQ
%token AND
%token ASSERT
%token BEGIN
%token ELSE
%token END
%token FORK
%token FREE
%token FREELOCK
%token IF
%token IN
%token LET
%token MALLOC
%token NEWLOCK
%token NULL
%token REC
%token REL
%token THEN
%token WAIT

%token EQUAL
%token COMMA
%token SCOLON
%token SCOLON2
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token STAR
%token LARROW

%token <string> IDENT

%token EOF

%type <Syntax.prog> program
%start program
%%

id: IDENT { $1 }
id_list: id { [$1] } | id COMMA id_list { $1 :: $3 }

program:
  | command EOF { [], $1 }
  | definition SCOLON2 command { List.rev $1, $3 }
  ;

definition:
  | LET REC function_definition { [$3] }
  | definition AND function_definition { $3 :: $1  }

function_definition:
  | id LPAREN RPAREN EQUAL command { Syntax.varid $1, ([], $5) }
  | id LPAREN id_list RPAREN EQUAL command { Syntax.varid $1, (List.map Syntax.varid $3, $6) }
  ;

command:
  | sequence_command { $1 }
  ;

sequence_command:
  | single_command { $1 }
  | single_command SCOLON sequence_command { Syntax.Sequence ($1, $3) }
  ;

single_command:
  | primary_command { $1 }
  | let_command { $1 }
  | if_command { $1 }
  ;

let_command:
  | LET id EQUAL NULL IN sequence_command { Syntax.LetNull (Syntax.varid $2, $6) }
  | LET id EQUAL id IN sequence_command { Syntax.LetVar (Syntax.varid $2, $4, $6) }
  | LET id EQUAL STAR id IN sequence_command { Syntax.LetDeref (Syntax.varid $2, $5, $7) }
  | LET id EQUAL MALLOC LPAREN RPAREN IN sequence_command { Syntax.LetMalloc (Syntax.varid $2, $8) }
  | LET id EQUAL NEWLOCK LPAREN RPAREN IN sequence_command { Syntax.LetNewlock (Syntax.varid $2, $8) }
  | LET id EQUAL FORK grouped_command IN sequence_command { Syntax.LetFork (Syntax.varid $2, $5, $7) }
  ;

if_command:
  | IF NULL LPAREN id RPAREN THEN single_command ELSE single_command { Syntax.IfNull ($4, $7, $9) }
  ;

primary_command:
  | grouped_command { $1 }
  | FREE id { Syntax.Free $2 }
  | FREELOCK id { Syntax.Freelock $2 }
  | ACQ id { Syntax.Acquire $2 }
  | REL id { Syntax.Release $2 }
  | WAIT id { Syntax.Wait $2 }
  | assert_command { $1 }
  | assign_command { $1 }
  | funcall_command { $1 }
  ;

assign_command:
  | STAR id LARROW id { Syntax.Assign ($2, $4) }
  | STAR id LARROW NULL {
    let v = Syntax.tmpvar () in
    Syntax.LetNull (Syntax.varid v, Syntax.Assign ($2, v))
  }
  ;

assert_command:
  | ASSERT LPAREN id EQUAL id RPAREN { Syntax.AssertEqVar ($3, $5) }
  | ASSERT LPAREN id EQUAL STAR id RPAREN { Syntax.AssertEqDeref ($3, $6) }
  ;

funcall_command:
  | id LPAREN RPAREN { Syntax.FunCall ($1, []) }
  | id id { Syntax.FunCall ($1, [$2]) }
  | id LPAREN id_list RPAREN { Syntax.FunCall ($1, $3) }
  ;

grouped_command:
  | LPAREN RPAREN { Syntax.Skip }
  | BEGIN END { Syntax.Skip }
  | LPAREN command RPAREN { $2 }
  | BEGIN command END { $2 }
  ;
