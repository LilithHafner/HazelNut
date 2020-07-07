(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
let identifier = [^' ' '\t' '.' '?' '\\' '(' ')' '{' '}' '=' ',' '\n']+
(*['0'-'9' 'a'-'z' 'A'-'Z' '_' ]+*)
rule token = parse
    [' ' '\t' '.']     { token lexbuf }     (* skip blanks and dots *)
  | ['\n' ]                 { EOL }
  | '?'|'h'                 { UNLABELED_HOLE }
  | '?' (identifier as id)  { LABELED_HOLE(Id.of_string id) }
  | "let"                   { LET }
  | "in"                    { IN }
  | identifier as id        { ID(Id.of_string id) }
  | '\\'                    { LAMBDA }
  | '('                     { LPAREN }
  | ')'                     { RPAREN }
  | '{'                     { LBRACE }
  | '}'                     { RBRACE }
  | '='                     { EQUALS }
  | ','                     { COMMA }
  | eof                     { raise Eof }