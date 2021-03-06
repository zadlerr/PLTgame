%{ open Ast %}

%token ASSIGN
%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA
%token RETURN
%token INT STRING CHAR BOOL VOID
%token IF ELSE WHILE
%token PLUS MINUS TIMES DIVIDE MOD 
%token LT LEQ GT GEQ EQ NEQ TRUE FALSE NOT
%token AND OR
%token <char> CHAR_LIT
%token <int> INT_LIT
%token <string> ID 
%token <string> STRING_LITERAL

%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN NOT NEG
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left EQ NEQ LT GT LEQ GEQ
%left AND OR 

%start program
%type <Ast.program> program

%%

program:		 
	decls EOF { $1 }

decls: 
	{ [], [] }
	| decls vdecl { ($2 :: fst $1), snd $1 }
	| decls fdecl { fst $1, ($2 :: snd $1) }
		
fdecl:
	typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
	{ { typ = $1;
	    fname = $2;
	    formals = $4;
	    locals = List.rev $7;
	    body = List.rev $8 
	} }

formals_opt:
	/* nothing */ { [] }
      | formal_list   { List.rev $1 }

formal_list: 
      typ ID 	{ [($1, $2)] }
    | formal_list COMMA typ ID { ($3, $4) :: $1 }

typ: /* Datatype - see MAZE */
      INT    { Int }
    | STRING { String }
    | CHAR   { Char }
    | BOOL { Bool }
    | VOID { Void }

vdecl_list: 
    /* nothing */ { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    typ ID SEMI { ($1, $2) }


stmt_list:
	/* nothing */ { [] }
	| stmt_list stmt { $2 :: $1 }

stmt:
	  expr SEMI { Expr $1 }
	| RETURN expr SEMI { Return $2 }
	| RETURN SEMI	   { Return Noexpr }
	| LBRACE stmt_list RBRACE { Block(List.rev $2) }
	| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) } /* elseless if */
	| IF LPAREN expr RPAREN stmt ELSE stmt { If($3,$5,$7) }
	| WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr:
	  INT_LIT                      { Int_Literal($1) }
	| STRING_LITERAL               { String_Lit($1) }
	| CHAR_LIT		       { Char_Literal($1) }
	| TRUE			       { BoolLit(true) }
	| FALSE			       { BoolLit(false) }
	| ID	                       { Id($1) }
	| ID ASSIGN expr	       { Assign($1, $3) }
	| ID LPAREN actuals_opt RPAREN { Call($1, $3) } /* function call */
	| expr PLUS expr	       { Binop($1, Add, $3) }
	| expr MINUS expr	       { Binop($1, Sub, $3) }
	| expr TIMES expr	       { Binop($1, Mult, $3) }
	| expr DIVIDE expr	       { Binop($1, Div, $3) }
	| expr MOD expr	               { Binop($1, Mod, $3) }
	| expr LT expr		       { Binop($1, Lessthan, $3) }
	| expr LEQ expr		       { Binop($1, Leq, $3) }
	| expr GT expr		       { Binop($1, Greaterthan, $3) }
	| expr GEQ expr		       { Binop($1, Geq, $3) }
	| expr EQ expr		       { Binop($1, Equal, $3) }
	| expr NEQ expr		       { Binop($1, Neq, $3) }
	| expr AND expr		       { Binop($1, And, $3) }
	| expr OR expr		       { Binop($1, Or, $3) }
	| MINUS expr %prec NEG 	       { Unop(Neg, $2) }
	| NOT expr		       { Unop(Not, $2) }
	| LPAREN expr RPAREN { $2 }

actuals_opt:
	  /* nothing */ { [] }
        | actuals_list { List.rev $1 }

actuals_list:
	  expr	{ [$1] }
	| actuals_list COMMA expr { $3 :: $1 }
