{
  exception Eof
  
  let keywords = [
    "acq", Parser.ACQ;
    "and", Parser.AND;
    "assert", Parser.ASSERT;
    "begin", Parser.BEGIN;
    "else", Parser.ELSE;
    "end", Parser.END;
    "fork", Parser.FORK;
    "free", Parser.FREE;
    "freelock", Parser.FREELOCK;
    "if", Parser.IF;
    "in", Parser.IN;
    "let", Parser.LET;
    "malloc", Parser.MALLOC;
    "newlock", Parser.NEWLOCK;
    "null", Parser.NULL;
    "rec", Parser.REC;
    "rel", Parser.REL;
    "then", Parser.THEN;
    "wait", Parser.WAIT;
  ]
}


rule main = parse
  | [' ' '\009' '\012' '\r' '\n']+ { main lexbuf }

  | ";;" { Parser.SCOLON2 }
  | "=" { Parser.EQUAL }
  | "," { Parser.COMMA }
  | ";" { Parser.SCOLON }
  | "(" { Parser.LPAREN }
  | ")" { Parser.RPAREN }
  | "[" { Parser.LBRACKET }
  | "]" { Parser.RBRACKET }
  | "*" { Parser.STAR }
  | "<-" { Parser.LARROW }

  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '\'']* {
    let lexeme = Lexing.lexeme lexbuf in
    try List.assoc lexeme keywords with
      | Not_found -> Parser.IDENT lexeme
  }

  | eof { Parser.EOF }

  | "(*" { comments 0 lexbuf }
and comments level = shortest
  | _* "(*" { comments (level + 1) lexbuf }
  | _* "*)" { if level = 0 then main lexbuf else comments (level - 1) lexbuf }
