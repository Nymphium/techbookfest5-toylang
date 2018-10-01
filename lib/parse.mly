%{
open Term
%}

%token LPAREN RPAREN
%token ARROW FUN
%token ADD SUB MUL DIV EQ LT GE
%token NOT
%token LET REC IN
%token IF THEN ELSE
%token TRUE FALSE
%token <int> INT
%token <string> VAR
%token EOF SEMIEOL

%left ADD SUB
%left MUL DIV
%left LT GE
%right EQ

%start <Term.t> parse
%%

parse:     | term EOF { $1 }
           | term SEMIEOL { $1 }

term:      | FUN x = VAR ARROW t = term { Fun(x, t) } 
           | LET x = VAR EQ t = term IN body = term { Let(x, t, body) }
           | LET REC f = VAR x = VAR EQ t = term IN body = term { LetRecFun(f, x, t, body) }
           | IF c = term THEN t1 = term ELSE t2 = term { If(c, t1, t2) }
           | NOT t = term { Not t }
           | f = term t = arg_term { App(f, t) }
           | t1 = term ADD t2 = term { Add(t1, t2) }
           | t1 = term SUB t2 = term { Sub(t1, t2) }
           | t1 = term MUL t2 = term { Mul(t1, t2) }
           | t1 = term DIV t2 = term { Div(t1, t2) }
           | t1 = term LT t2 = term { Lt(t1, t2) }
           | t1 = term GE t2 = term { Ge(t1, t2) }
           | t1 = term EQ t2 = term { Eq(t1, t2) }
           | arg_term { $1 }

arg_term:  | LPAREN term RPAREN { $2 }
           | VAR   { Var $1 }
           | TRUE  { Bool true }
           | FALSE { Bool false }
           | INT   { Int $1 }
