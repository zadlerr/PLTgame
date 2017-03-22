{ open Parser }

rule token = parse
	[ ' ' '\t' '\r' '\n'] 	{ token lexbuf }	(* whitespace *)
	| "##"					{ comment lexbuf }	(* comment start *)	
	| '('					{ LPAREN }		(* delimiters *)
	| ')'					{ RPAREN }
	| '['					{ LBRACK }
	| ']'					{ RBRACK }
	| '{'					{ LBRACE }
	| '}'					{ RBRACE }
	| ';'					{ SEMI }
	| ','					{ COMMA }
	| '.'					{ ACCESS }		(* operators *)
	| '*'					{ TIMES }
	| '/'					{ DIVIDE }
	| '%'					{ MOD }
	| '+'					{ PLUS }
	| '-'					{ MINUS }
	| '<'					{ LT }
    | "<="                  { LEQ }
    | '>'                   { GT }
    | ">="                  { GEQ }
    | "=="                  { EQ }
    | "!="                  { NEQ }
    | '&'					{ AND }
	| '|'					{ OR }
	| '!'					{ NOT }
	| '='					{ ASSIGN }
	| "fun"					{ FUN }			(* keywords *)
	| "events"				{ EVENTS }
	| "room"				{ ROOM }
	| "item"				{ ITEM }
	| "start"				{ START }
	| "npc"					{ NPC }
	| "end"					{ END }
	| "if"					{ IF }
	| "else"				{ ELSE }
	| "while"				{ WHILE }
	| "return"				{ RETURN }
	| "int"					{ INT }			
	| "string"				{ STRING }
	| "boolean"				{ BOOLEAN }
	| "void"				{ VOID }
    | "true"					as lxm { BOOL_LITERAL(bool_of_string lxm ) }
	| "false"				as lxm { BOOL_LITERAL(bool_of_string lxm ) }
	| ['0'- '9']+				as lxm { INT_LITERAL(int_of_string lxm ) }
	|  [ 'a' - 'z'  'A' - 'Z' ][ 'a' - 'z'  'A' - 'Z'  '0' - '9'  '_' ]*   as lxm  { ID ( lxm ) }
	| eof					{ EOF }
		 	 	 		
	and comment = parse
	| "##"					{ token lexbuf }	(* comment end *)
	| _					{ comment lexbuf }	(* eat everything else *)

