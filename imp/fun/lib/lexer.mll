{
open Parser
}

let white = [' ' '\t' '\n']+
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let alphnum = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter alphnum*
let num = digit+

rule read =
  parse
  | white { read lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACES }
  | "}" { RBRACES }
  | "skip" { SKIP }
  | "int" { INT }
  | "bool" { BOOL }
  | "fun" { FUN }
  | "return" { RETURN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | ":=" { ASSIGN }
  | ";" { SEMICOLON }
  | "true" { TRUE }
  | "false" { FALSE }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "<=" { LEQ }
  | "=" { EQ }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { NUM (Lexing.lexeme lexbuf) }
  | eof { EOF }
