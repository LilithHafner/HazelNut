%{
open Grammar
%}
/* File Parser.mly */
%token <Id.t> ID LABELED_HOLE
%token UNLABELED_HOLE LAMBDA LBRACE RBRACE EQUALS LPAREN RPAREN EOL COMMA LET IN
%nonassoc LAMBDA			/* low precedence */
%nonassoc APPLICATION		/* high precedence */
%start main				/* the entry point */
%type <exp> main
%%
main:
	exp EOL										{ $1 }
;
annotation:
	exp EQUALS exp COMMA annotation				{ ($1, $3)::$5 }
  | exp EQUALS exp 								{ ($1, $3)::[]}
;
annotation_hint:								{ [] }
  | LBRACE annotation RBRACE					{ $2 }
;
base_exp:
	LABELED_HOLE								{ Hole $1 }
  | UNLABELED_HOLE								{ Hole (Id.unique ()) }
  | LAMBDA ID annotation_hint exp				{ Lambda($2, $3, $4) }
  | LET ID EQUALS exp IN annotation_hint exp	{ Application(Lambda($2, $6, $7), $4) }
  | ID											{ Variable $1 }
  | LPAREN exp RPAREN							{ $2 }
;
exp:
	exp base_exp								{ Application($1, $2) }
  | base_exp									{ $1 }
;