{
  open Parse
  open Lexing
  exception Error of string
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let under = '_'
let quot = '\''
let xalpha = lower | under
let alnum = digit | lower | under

rule token = parse
          | digit + { INT (int_of_string (lexeme lexbuf)) }
          | '+'     { ADD }
          | '-'     { SUB }
          | '*'     { MUL }
          | '/'     { DIV }
          | "true"  { TRUE }
          | "false" { FALSE }
          | "->"    { ARROW }
          | "fun"   { FUN }
          | '='     { EQ }
          | '<'     { LT }
          | ">="    { GE }
          | "not"   { NOT }
          | "let"   { LET }
          | "rec"   { REC }
          | "in"    { IN }
          | '('     { LPAREN }
          | ')'     { RPAREN }
          | "if"    { IF }
          | "then"  { THEN } 
          | "else"  { ELSE }
          | xalpha alnum* quot? { VAR (lexeme lexbuf) }
          | space+  { token lexbuf } 
          | eof     { EOF }
          | ";;"    { SEMIEOL }
          | _       { raise @@ Error(Printf.sprintf "At offset %d: unexpected character.\n" @@ lexeme_start lexbuf) }

