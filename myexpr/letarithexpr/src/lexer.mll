{
open Parser
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "0" { ZERO }
  | "succ" { SUCC }
  | "pred" { PRED }
  | "iszero" { ISZERO }
  | "let" { LET }
  | "in" { IN }
  | "=" { EQUALS }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
