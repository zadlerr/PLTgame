%{ open Ast %}

%token <int> INT_LITERAL
%token <string> ID
%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA
%token /* RETURN */ INT

%token EOF

%start program
%type <Ast.program> program

%%

program:		 
	decls EOF { $1 }

decls: /* for right now is just one function declaration */
	decls fdecl { $1 }

fdecl:
	typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
	{ { typ = $1;
	    fname = $2 } }

formals_opt:
	/* nothing */ { [] }
      | formal_list   { List.rev $1 }

formal_list: 
      typ ID 	{ [($1, $2)] }
    | formal_list COMMA typ ID { ($3, $4) :: $1 }

typ:
      INT  { Int }
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

/*	| stmt_list stmt { $2 :: $1 }

empty statement list
stmt:
*/
