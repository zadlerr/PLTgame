%{ open Ast %}

%token ASSIGN
%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA
%token RETURN INT STRING CHAR 
%token PLUS MINUS TIMES DIVIDE MOD 
%token LT LEQ GT GEQ EQ NEQ
%token <char> CHAR_LIT
%token <int> INT_LIT
%token <string> ID STRING_LIT

%token EOF

%left PLUS MINUS
%left TIMES DIVIDE MOD

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

typ:
      INT    { Int }
    | STRING { String }
    | CHAR   { Char }
   /*
    | BOOL { Bool }
    | VOID { Void }
   */

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

expr:
	  INT_LIT                      { Int_Literal($1) }
	| STRING_LIT                   { String_Literal($1) }
	| CHAR_LIT		       { Char_Literal($1) }
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


actuals_opt:
	  /* nothing */ { [] }
        | actuals_list { List.rev $1 }

actuals_list:
	  expr	{ [$1] }
	| actuals_list COMMA expr { $3 :: $1 }
