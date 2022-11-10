{
open Parser
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let dgt = ['0'-'9']
let num = dgt+

rule read =
  parse
  | white { read lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "fun" { ABS }
  | "." { DOT }
  | "id" { ID }
  | "omega" { OMEGA }
  | "tru" { TRU }
  | "fls" { FLS }
  | "ift" { IFT }
  | "and" { AND }
  | "pair" { PAIR }
  | "fst" { FST }
  | "snd" { SND }
  | "scc" { SCC }
  | "add" { ADD }
  | id { VAR (Lexing.lexeme lexbuf) }
  | num { NUM (Lexing.lexeme lexbuf) }
  | eof { EOF }
